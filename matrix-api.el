;;; matrix-api.el --- An ELisp client for the Matrix.org RPC

;; Copyright (C) 2017-2018 Jay Kamat
;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 21 June 2015
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; matrix-api.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a pure-elisp implementation of the Matrix.org RPC protocol
;; specification 0.1. It forms the basis of the included `matrix-client' Matrix chat
;; client, and can be used as a general RPC system using `matrix-send-event' and
;; `matrix-event-poll'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'eieio)
(require 'map)

(require 'a)

(require 'matrix-helpers)
(require 'matrix-utils)

(defvar matrix-error-hook nil
  "This is a list of functions to pass Matrix errors to.")

(defcustom matrix-homeserver-base-url "https://matrix.org"
  "URI to your Matrix homeserver, defaults to the official homeserver."
  :type 'string
  :group 'matrix-client)

(defcustom matrix-insecure-connection nil
  "Whether to use insecure HTTPS connection when connecting to the homeserver."
  :type 'boolean
  :group 'matrix-client)

(defclass matrix-connection ()
  ((base-url :initarg :base-url
             :initform  "https://matrix.org"
             :type string
             :documentation "URI to your Matrix homeserver, defaults to the official homeserver.")
   (token :initarg :token
          :initform nil
          :documentation "Matrix access_token")
   (txn-id :initarg :txn-id
           :initform 1
           :type integer)))

(cl-defmethod matrix-login ((con matrix-connection) login-type content)
  "Log in to connection CON using LOGIN-TYPE and return the server response.
CONTENT is an alist of additional keys/values to add to the
submitted JSON.  After logging in, the access token is set on
CON."
  (when-let ((content (map-put content "type" login-type))
             (response (matrix-send con "POST" "/login" content)))
    (oset con :token (map-elt response 'access_token))
    response))

(cl-defmethod matrix-login-with-password ((con matrix-connection) username password)
  "Log in to connection CON with USERNAME and PASSWORD and save the access token."
  (matrix-login con "m.login.password" (a-list 'user username 'password password)))

(cl-defun matrix-request-error-handler
    (con &rest args &key error-thrown symbol-status
         &allow-other-keys)
  ;; Call err handler functions
  (dolist (handler matrix-error-hook)
    (funcall handler con symbol-status error-thrown))
  ;; Message some warnings if we know what it is
  (let ((exit-code (matrix-parse-curl-exit-code (cddr error-thrown))))
    (warn (pcase exit-code
            ((or 51 60) "Error sending request to matrix homeserver, SSL certificate is invalid")
            (`nil "Unknown error occurred sending request to matrix homeserver: %S")
            (_ (format "Matrix request exited with exit code %d" exit-code))))))

(cl-defmethod matrix-send ((con matrix-connection) method path
                           &optional content query-params headers api-version)
  "Send an event to the Matrix homeserver.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.  QUERY-PARAMS is an
optional alist of URL parameters.  HEADERS is optional HTTP
headers to add to the request.

The return value is the `json-read' response from homeserver."
  (let* ((token (oref con :token))
         (url-request-data (when content
                             (json-encode content)))
         (endpoint (concat (matrix-homeserver-api-url api-version) path))
         (method (upcase method)))
    (when token
      (map-put query-params "access_token" token))
    (let ((request-curl-options request-curl-options))
      (when matrix-insecure-connection
        (push "--insecure" request-curl-options))
      (request-response-data
       (pcase method
         ("GET"
          (request endpoint
                   :type method
                   :params query-params
                   :sync t
                   :error (apply-partially #'matrix-request-error-handler con)
                   :parser 'json-read))
         ((or "POST" "PUT")
          (request endpoint
                   :type method
                   :params query-params
                   :sync t
                   :error (apply-partially #'matrix-request-error-handler con)
                   :data (json-encode content)
                   :headers (map-put headers "Content-Type" "application/json")
                   :parser 'json-read)))))))

(cl-defmethod matrix-send-async ((con matrix-connection) method path
                                 &optional content query-params headers callback api-version)
  "Perform an asynchronous Matrix API call.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.  QUERY-PARAMS is an
optional alist of URL parameters.  HEADERS is optional HTTP
headers to add to the request.  CALLBACK is the callback which
will be called by `request' when the call completes"
  (let* ((token (oref con token))
         (endpoint (concat (matrix-homeserver-api-url api-version) path))
         (request-curl-options request-curl-options))
    (when matrix-insecure-connection
      (push "--insecure" request-curl-options))
    (when token
      (map-put query-params "access_token" token))
    (request endpoint
             :type (upcase method)
             :params query-params
             :parser 'json-read
             :data (json-encode content)
             :error (apply-partially #'matrix-request-error-handler con)
             :headers (map-put headers "Content-Type" "application/json")
             :complete (apply-partially #'matrix-async-cb-router callback con))))

(cl-defun matrix-async-cb-router (callback con &key data error-thrown symbol-status &allow-other-keys)
  (if (or error-thrown
          (eq symbol-status 'timeout))
      (dolist (handler matrix-error-hook)
        (funcall handler con symbol-status error-thrown))
    (when callback
      (funcall callback data))))

(cl-defmethod matrix-send-event ((con matrix-connection) room-id event-type content
                                 &optional &key async)
  "Send a raw event to the room ROOM-ID.
EVENT-TYPE is the matrix event type to send (see Matrix spec).
CONTENT is a `json-encode' compatible list to include in the
event.  If ASYNC is non-nil, send the message asynchronously."
  (let* ((txn-id (incf (oref con :txn-id)))
         (path (format "/rooms/%s/send/%s/%s"
                       (url-encode-url room-id)
                       (url-encode-url event-type)
                       txn-id)))
    (pcase async
      (`nil (matrix-send con "PUT" path content))
      (`t (matrix-send-async con "PUT" path content)))))

(cl-defmethod matrix-get-displayname ((con matrix-connection) user-id callback)
  "Get displayname for USER-ID and call CALLBACK."
  (let* ((path (format "/profile/%s/displayname" user-id)))
    (matrix-send-async con "GET" path
                       nil nil nil callback)))

(cl-defmethod matrix-send-message ((con matrix-connection) room-id message)
  "Send string MESSAGE to room ROOM-ID."
  (matrix-send-event con room-id "m.room.message"
                     (a-list "msgtype" "m.text"
                             ;; NOTE: We encode the message with UTF-8.  I'm not sure if this is the
                             ;; best or right place to do this.  I can't (easily) find out how or if
                             ;; Emacs and/or `json-encode' encode strings by default, but this seems
                             ;; to fix <https://github.com/jgkamat/matrix-client-el/issues/44>.
                             "body" (encode-coding-string message 'utf-8))
                     :async t))

(cl-defmethod matrix-get-messages ((con matrix-connection) room-id &key from to (direction "b") limit callback)
  "Get messages for ROOM-ID.

DIRECTION should be \"b\" (the default) or \"f\".  TO and
LIMIT (an integer, default 10) are optional.  Request is made
asynchronously, and CALLBACK is called with the result."
  ;; NOTE: API version 0.3.0 works differently, so this should be temporary, until we switch to the
  ;; new API.
  (let ((path (format "/rooms/%s/messages" room-id))
        (query-params (matrix--alist "from" from
                                     "to" to
                                     "dir" direction
                                     "limit" limit)))
    (matrix-send-async con "GET" path nil
                       query-params nil callback "r0")))

(cl-defmethod matrix-sync ((con matrix-connection) since full-state timeout callback)
  "Start an event poller starting from END-TOKEN.
It will wait at least TIMEOUT seconds before calling the
CALLBACK.  After receiving any events it will call CALLBACK with
those events as its argument."
  (let ((query-params (matrix--alist "timeout" (int-to-string timeout)
                                     "full_state" (if full-state "true" "false")
                                     "since" since)))
    (matrix-send-async con "GET" "/sync" nil
                       query-params nil callback "r0")))

(cl-defmethod matrix-event-poll ((con matrix-connection) end-token timeout callback)
  "Start an event poller starting from END-TOKEN.
It will wait at least TIMEOUT seconds before calling the
CALLBACK.  After receiving any events it will call CALLBACK with
those events as its argument."
  (matrix-send-async "GET" "/events" nil
                     (a-list "from" end-token
                             "timeout" (number-to-string timeout))
                     nil callback))

(cl-defmethod matrix-mark-as-read ((con matrix-connection) room-id event-id)
  "Mark as read EVENT-ID in ROOM-ID."
  (let ((path (format "/rooms/%s/receipt/m.read/%s" room-id event-id)))
    ;; TODO: Update API version.
    (matrix-send-async con "POST" path nil nil nil (lambda (status)) "api/v2_alpha")))

(cl-defmethod matrix-join-room ((con matrix-connection) room-id)
  "Join ROOM-ID."
  (let* ((txn-id (incf (oref con :txn-id)))
         ;; TODO: Standardize on `url-hexify-string' or `url-encode-url'
         (path (format "/join/%s" (url-hexify-string room-id))))
    ;; MAYBE: error-handling needed here?
    (matrix-send con "POST" path)))

(cl-defmethod matrix-leave-room ((con matrix-connection) room-id)
  "Leave ROOM-ID."
  (let* ((txn-id (incf (oref con :txn-id)))
         ;; TODO: Standardize on `url-hexify-string' or `url-encode-url'
         (path (format "/rooms/%s/leave" (url-encode-url room-id))))
    (matrix-send con "POST" path)))

(cl-defmethod matrix-sync-room ((con matrix-connection) room-id)
  "Perform an /initialSync of a single room ROOM-ID."
  (let* ((txn-id (incf (oref con :txn-id)))
         (path (format "/rooms/%s/initialSync" (url-hexify-string room-id))))
    (matrix-send con "GET" path)))

(provide 'matrix-api)

;;; matrix-api.el ends here
