#!/usr/bin/env gosh
;;; Copyright 2018 Yoshihiro Tanaka
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; Author: Yoshihiro Tanaka <contact@cordea.jp>
;;; date  : 2018-05-31

(use makiki)
(use srfi-13)
(use srfi-19)
(use rfc.http)
(use gauche.threads)
(use control.job)
(use control.thread-pool)

(define *token* "")
(define *server* "slack.com")
(define *post-message-path* "/chat.postMessage")
(define *date-format* "~Y/~m/~d-~H:~M")
(define *type-event* "event_callback")
(define *type-verification* "url_verification")
(define *pool* '())

(define-class verification () (token challenge type))

(define-class event () (token team-id api-app-id event type event-id event-time))

(define-class app-mention () (type user text ts channel event-ts))

; @mention user time text...
(define-class command () (user time text))

(define (parse-verification json)
  (let1 verification (make verification)
        (slot-set! verification 'token (assoc-ref json "token"))
        (slot-set! verification 'challenge (assoc-ref json "challenge"))
        (slot-set! verification 'type (assoc-ref json "type"))
        verification))

(define (parse-app-mention json)
  (let1 app-mention (make app-mention)
        (slot-set! app-mention 'type (assoc-ref json "type"))
        (slot-set! app-mention 'user (assoc-ref json "user"))
        (slot-set! app-mention 'text (assoc-ref json "text"))
        (slot-set! app-mention 'ts (assoc-ref json "ts"))
        (slot-set! app-mention 'channel (assoc-ref json "channel"))
        (slot-set! app-mention 'event-ts (assoc-ref json "event_ts"))
        app-mention))

(define (create-event json)
  (let1 event (make event)
        (slot-set! event 'token (assoc-ref json "token"))
        (slot-set! event 'team-id (assoc-ref json "team_id"))
        (slot-set! event 'api-app-id (assoc-ref json "api_app_id"))
        (slot-set! event 'event
                   (parse-app-mention (assoc-ref json "event")))
        (slot-set! event 'type (assoc-ref json "type"))
        (slot-set! event 'event-id (assoc-ref json "event_id"))
        (slot-set! event 'event-time (assoc-ref json "event_time"))
        event))

(define (parse-command text)
  (let ((raw (string-split text " ")) (command (make command)))
    (slot-set! command 'user (list-ref raw 1))
    (slot-set! command 'time
               (difference-from-now (parse-date (list-ref raw 2))))
    (slot-set! command 'text
               (string-concatenate (intersperse " " (drop raw 3))))
    command))

(define (parse-date date)
  (guard
    (e (else (date->time-utc
               (string->date
                 (string-append
                   (number->string (slot-ref (current-date) 'year))
                   "/"
                   date)
                 *date-format*))))
    (date->time-utc (string->date date *date-format*))))

(define (difference-from-now time)
  (time-second (time-difference time (current-time))))

(define (text-with-mention command)
  (string-concatenate `("<@" ,(slot-ref command 'user) "> " ,(slot-ref command 'text))))

(define (with-interval time expr)
  (thread-sleep! time)
  (expr))

(define (start-timer time expr)
  (add-job! *pool*
            (cut guard (e (else (report-error e) #f))
                 (with-interval time expr))))

(define (post-message channel text)
  (http-post *server*
             *post-message-path*
             `((token ,*token*) (channel ,channel) (text ,text))
             :secure #t))

(define (post-message-with-delay channel command)
  (start-timer (slot-ref command 'time)
               (cut post-message channel (text-with-mention command))))

(define (mention-handler req body)
  (let* ((event (create-event body)) (app-mention (slot-ref event 'event)))
    (let ((channel (slot-ref app-mention 'channel))
          (user (slot-ref app-mention 'user))
          (command (parse-command (slot-ref app-mention 'text))))
      (post-message-with-delay channel command)
      (post-message channel "")))
  (respond/ok req `(json ,body)))

(define (verification-handler req body)
  (let* ((verification (parse-verification body)) (challenge (slot-ref verification 'challenge)))
    (respond/ok req challenge :content-type "text/plain")))

(define (handler req app)
  (let*
    ((body (request-param-ref req "json-body" :default '()))
     (type (assoc-ref body "type")))
    (cond
      ((equal? type *type-event*) (mention-handler req body))
      ((equal? type *type-verification*) (verification-handler req body)))))

(define (main args)
  (set! *pool* (make-thread-pool 10))
  (start-http-server :access-log #t :error-log #t :port 8080)
  (terminate-all! *pool* :force-timeout 3000))

(define-http-handler "/mention" (with-post-json handler))
