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
(use rfc.http)

(define *token* "")
(define *server* "slack.com")
(define *post-message-path* "/chat.postMessage")

(define-class event () (token team-id api-app-id event type event-id event-time))

(define-class app-mention () (type user text ts channel event-ts))

(define (parse-app-mention json)
  (let1 app-mention (make app-mention)
        (slot-set! app-mention 'type (assoc-ref json "type"))
        (slot-set! app-mention 'user (assoc-ref json "user"))
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

(define (post-message channel text)
  (http-post *server*
             *post-message-path*
             `((token ,*token*) (channel ,channel) (text ,text))
             :secure #t))

(define (handler req app)
    (let1 body (request-param-ref req "json-body" :default '())
          (print (post-message "" ""))
          (respond/ok req `(json ,body))))

(define (main args)
  (start-http-server :access-log #t :error-log #t :port 8080))

(define-http-handler "/mention" (with-post-json handler))
