;; -*- lexical-binding: t; -*-


;; Here is a playground for implementing the latest version of the
;; API, r0.3.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

;; TODO: Consider let-binding `json-array-type' to `list'.  I'm not
;; sure there's any advantage to using vectors, and it makes the code
;; more error-prone because some things are lists and some vectors.

;;; Code:

;;;; Requirements

;; Built-in
(require 'cl-lib)
(require 'dns)
(require 'eieio)
(require 'map)
(require 'pcase)
(require 'seq)

;; MELPA
(require 'a)
(require 'dash)
(require 'ht)
(require 'json)
(require 'request)
(require 's)

;; Local
(require 'matrix-macros)
(require 'matrix-helpers)

;;;; Variables

(defvar matrix-log nil
  "Enable logging to `matrix-log-buffer'.
NOTE: This can log sensitive data, such as passwords and access
tokens.  Logs should be sanitized before sharing.")

(defvar matrix-log-buffer "*matrix-log*"
  "Name of buffer used by `matrix-log'.")

(defvar matrix-synchronous nil
  "When non-nil, run `matrix-request' requests synchronously.")

(defvar matrix-warn-unimplemented nil
  "Give warnings for unimplemented event handlers.")

(defvar matrix-after-initial-sync-hook nil
  "Hooks run with session as argument after initial sync.
Called after the sync response is parsed into the session
objects, before returning from the sync callback.")

(defvar matrix-after-sync-hook nil
  "Hooks run with session as argument after every sync.
Called after the sync response is parsed into the session
objects, before returning from the sync callback.")

;;;; Macros

(defmacro matrix-defclass (name superclasses slots &rest options-and-doc)
  "Identical to `defclass', except...
adds nil initforms.  Also adds optional `:instance-initform'
argument to each slot.  This should be a sexp that will be
evaluated in the context of the object's slots (using
`with-slots') when it is initialized (like Python's __init__
method)."
  (declare (indent defun))
  (let* ((slot-inits (-non-nil (--map (let ((name (car it))
                                            (initer (plist-get (cdr it) :instance-initform)))
                                        (when initer
                                          (list 'setq name initer)))
                                      slots)))
         (slot-names (mapcar #'car slots))
         (around-fn-name (intern (concat (symbol-name name) "-initialize")))
         (docstring (format "Inititalize instance of %s." name)))
    ;; Add nil initforms
    (cl-loop for (slot . attrs) in slots
             unless (plist-get attrs :initform)
             ;; FIXME: Doesn't work if there are no attrs at all.
             do (nconc attrs (list :initform nil)))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options-and-doc)
       (when (> (length ',slot-inits) 0)
         (cl-defmethod initialize-instance :after ((this ,name) &rest _)
           ,docstring
           (with-slots ,slot-names this
             ,@slot-inits))))))

(cl-defmacro matrix-defcallback (name type docstring &key slots body)
  "Define callback function NAME on TYPE with DOCSTRING and BODY.
This defines a method on a TYPE object, compatible with the
`request' callback API.  The object of TYPE will be available as
TYPE without any `matrix-' prefix.  The method's name will be
`matrix-NAME-callback'. Object SLOTS are made available
automatically with `with-slots'.  Keyword arguments DATA,
ERROR-THROWN, SYMBOL-STATUS, and RESPONSE are defined
automatically, and other keys are allowed."
  (declare (indent defun) (debug (symbolp symbolp stringp ":slots" form ":body" form)))
  (let ((name (intern (concat "matrix-" (symbol-name name) "-callback")))
        (instance (intern (nth 1 (s-match (rx "matrix-" (group (1+ anything)))
                                          (symbol-name type))))))
    `(cl-defun ,name (,instance &key cbargs status headers data
                                error url query ; Used primarily for error handling
                                &allow-other-keys)
       ,docstring
       (with-slots ,slots ,instance
         ,body))))

;;;; Functions

(defun funcall-when (fn &rest args)
  "If FN is a function, return result of applying ARGS to it, otherwise nil."
  (let ((fn (intern-soft fn)))
    (when (functionp fn)
      (apply fn args))))

;;;; Classes

(matrix-defclass matrix-session ()
  ((user :initarg :user
         :type string
         :instance-initform (progn
                              (unless (s-starts-with? "@" user)
                                ;; Prepend "@"
                                (setq user (concat "@" user)))
                              (unless (string-match-p ":" user)
                                ;; Server not given: assume matrix.org
                                (setq user (concat user ":matrix.org"))
                                (message "Assuming server is matrix.org"))
                              user)
         :documentation "The fully qualified user ID, e.g. @user:matrix.org.")
   (server :initarg :server
           :instance-initform (or server
                                  (nth 2 (s-match (rx "@" (group (1+ (not (any ":"))))
                                                      ":" (group (1+ anything)))
                                                  user)))
           :documentation "FQDN of server, e.g. \"matrix.org\" for the official homeserver.  Derived automatically when not explicitly set.")
   (api-url-prefix :type string
                   :instance-initform (concat (if (string-match-p (rx bos "http" (opt "s") "://")
                                                                  server)
                                                  "" "https://")
                                              server "/_matrix/client/r0/")
                   :documentation "URL prefix for API requests.  Derived automatically from server-name and built-in API version.")
   (device-id :initarg :device-id
              :initform (md5 (concat "matrix-client.el" (system-name)))
              :documentation "ID of the client device.")
   (initial-device-display-name
    :initarg :initial-device-display-name
    :initform (concat "matrix-client.el @ " (system-name))
    :type string
    :documentation "A display name to assign to the newly-created device.  Ignored if device_id corresponds to a known device.")
   (access-token :initarg :access-token
                 :documentation "API access_token.")
   (txn-id :initarg :txn-id
           :initform 0
           ;; Initialize to a random number.  This may avoid a potential problem like:

           ;; 1.  User connects.
           ;; 2.  User sends one message with txn-id 0.
           ;; 3.  Client crashes (Emacs is killed, whatever), txn-id is not saved to disk.
           ;; 4.  User restarts Emacs and connects.
           ;; 5.  User sends another message with txn-id 0.

           ;; If this happens within 30 minutes (see links below), I guess it's possible that the
           ;; server would reject it as a duplicate txn.
           :instance-initform (random 100000)
           :type integer
           ;; NOTE: According to <https://matrix.org/docs/spec/client_server/r0.3.0.html#id183>, the
           ;; transaction ID should be scoped to the access token, so we should preserve it with the
           ;; token.  However, if the client crashes and fails to save the TID, and then reuses it
           ;; in the future...what happens?  The server seems to accept messages with already-used
           ;; TIDs.  Maybe it has some kind of heuristic...  I found these:
           ;; <https://github.com/matrix-org/synapse/issues/1481> and
           ;; <https://github.com/matrix-org/synapse/pull/1624>.
           :documentation "Transaction ID.  Defaults to 0 and should be automatically incremented for each request.  According to the API, the scope of the transaction ID is an access token, so this should be preserved with the access token.")
   (rooms :initarg :rooms
          :type list
          :documentation "List of room objects user has joined.")
   (account-data :documentation "The private data that this user has attached to this account.")
   (followed-users :initform (ht)
                   :initarg :followed-users
                   :type hash-table
                   :documentation "Hash table of user IDs whose presence this user wants to follow.")
   (next-batch :type string
               :documentation "The batch token to supply in the since param of the next /sync request.")
   ;; FIXME: After fixing bug in macro, let this be simply (initial-sync-p)
   (initial-sync-p :initarg :initial-sync-p)
   (sync-retry-delay :initform 0
                     :initarg :sync-retry-delay
                     :type integer
                     :documentation "When a /sync request fails, wait this long before syncing again.
The sync error handler should increase this for consecutive errors, up to a maximum, and the success handler should reset it to 0.")
   (disconnect :initform nil
               :documentation "Set to non-nil to cancel further /sync requests.")
   (pending-syncs :initarg :pending-syncs
                  :type list
                  :initform nil
                  :documentation "List of response buffers for pending /sync requests.  This should generally be a list of zero or one buffers.  This is used to cancel pending /sync requests when the user disconnects.")
   (client-data :initarg :client-data
                :documentation "Reserved for users of the library, who may store whatever they want here.")
   (login-attempts :initarg :login-attempts
                   :initform 0
                   :type integer
                   :documentation "Number of login attempts."))
  :allow-nil-initform t)

;;;;; Room

(matrix-defclass matrix-room ()
  ((session :initarg :session
            :type matrix-session)
   (id :documentation "Fully-qualified room ID."
       :initarg :id
       :type string)
   (avatar :initarg :avatar
           :documentation "A string containing the room avatar image in its text properties.")
   (typers :initarg :typers)
   (name :initarg :name)
   (topic :initarg :topic)
   (aliases :initarg :aliases)
   (canonical-alias :initarg :canonical-alias)
   (members :documentation "List of room members, as user objects."
            :type list)
   (direct-p :documentation "Non-nil if the room is an \"m.direct\" room."
             :type boolean)
   (state :documentation "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true).")
   (state-new :documentation "List of new state events.  Clients should clear this list by calling `matrix-clear-state'.")
   (tags :documentation "Room tags.
An alist in which room tags, as symbols, are the CAR of each element, and the CDR is an alist of attributes, like tag order."
         :type list)
   (timeline :documentation "List of timeline events."
             :type list)
   (timeline-new :documentation "List of new timeline events.  Clients may clear this list by calling `matrix-clear-timeline'."
                 :type list)
   (timeline-event-ids :documentation "Hash table of event IDs already stored in the timeline.  Used for deduplication."
                       :initform (ht))
   (prev-batch :documentation "A token that can be supplied to to the from parameter of the rooms/{roomId}/messages endpoint.")
   (last-full-sync :documentation "The oldest \"since\" token for which the room has been synced completely.")
   (ephemeral :documentation "The ephemeral events in the room that aren't recorded in the timeline or state of the room. e.g. typing.")
   (account-data :documentation "The private data that this user has attached to this room.")
   (unread-notifications :documentation "Counts of unread notifications for this room.")
   (hook :initarg :hook
         :documentation "List of functions called when room is updated.  Function is called with one argument, this room object.")
   (end-token :initarg :end-token
              :initform nil
              :documentation "The most recent event-id in a room, used to push read-receipts to the server.")
   (client-data :initarg :client-data
                :initform (matrix-room-client-data)
                :documentation "Reserved for users of the library, who may store whatever they want here.")
   (display-name :initarg :display-name
                 :documentation "Room's computed display name."))
  :allow-nil-initform t)

(defcustom matrix-client-direct-chat-prefer-room-name nil
  ;; FIXME: This violates the API/client boundary, but since the room-display-name
  ;; function lives here now, it must be here.  Maybe that needs to change back.
  "In direct chat rooms, prefer the room name over the other user's name.
In 1-on-1 chat rooms, if this option is disabled, the room will
be displayed using the name of the other user.  If enabled, the
room's set name will be used when available."
  :group 'matrix-client
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (when (bound-and-true-p matrix-client-sessions)
           (--each matrix-client-sessions
             (--each (oref it rooms)
               (oset it display-name (matrix--room-display-name it)))
             (matrix-client-rename-room-buffers it)))))

(defun matrix--room-display-name (room)
  "Compute and return display name for ROOM."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id267
  (cl-macrolet
      ((displaynames-sorted-by-id (members)
                                  `(--> ,members
                                        (-sort (-on #'string< #'car) it)
                                        (--map (matrix-user-displayname room (car it))
                                               it)))
       (members-without-self () `(cl-remove self members :test #'string= :key #'car))
       (pick-name (&rest choices)
                  ;; This macro allows short-circuiting the choice forms, only evaluating them when needed.
                  `(or ,@(cl-loop for choice in choices
                                  collect `(--when-let ,choice
                                             ;; NOTE: We check to see if strings are empty,
                                             ;; because apparently it can happen that an
                                             ;; mxid is something like "@:hostname", with
                                             ;; an empty displayname.  Sigh.
                                             (if (listp it)
                                                 (cl-loop for this-choice in (-non-nil (-flatten it))
                                                          unless (string-empty-p this-choice)
                                                          return this-choice)
                                               (unless (string-empty-p it)
                                                 it)))))))
    (pcase-let* (((eieio id name avatar aliases canonical-alias members session) room)
                 ((eieio (user self)) session)
                 (avatar (when (and avatar matrix-client-show-room-avatars-in-buffer-names)
                           ;; Make a new image to avoid modifying the avatar in the header.
                           (setq avatar (cl-copy-list (get-text-property 0 'display avatar)))
                           (setf (image-property avatar :max-width) matrix-client-room-avatar-in-buffer-name-size)
                           (setf (image-property avatar :max-height) matrix-client-room-avatar-in-buffer-name-size)
                           (setq avatar (concat (propertize " " 'display avatar) " ")))))
      (concat avatar
              (pcase (1- (length members))
                (1 (if matrix-client-direct-chat-prefer-room-name
                       (pick-name name (matrix-user-displayname room (caar (members-without-self)))
                                  canonical-alias aliases id)
                     (pick-name (matrix-user-displayname room (caar (members-without-self)))
                                name canonical-alias aliases id)))
                (2 (pick-name name canonical-alias aliases
                              (s-join ", " (displaynames-sorted-by-id (members-without-self)))
                              id))
                ((or `nil (pred (< 0))) ;; More than 2
                 (pick-name name canonical-alias aliases
                            (format "%s and %s others"
                                    (car (displaynames-sorted-by-id (members-without-self)))
                                    (- (length members) 2))
                            id))
                (_ (pick-name name canonical-alias aliases
                              ;; FIXME: The API says to use names of previous room
                              ;; members if nothing else works, but I don't feel like
                              ;; coding that right now, so we'll just use the room ID.
                              id)))))))

(defun matrix-user-displayname (room user-id)
  "Return display name for USER-ID in ROOM."
  (pcase-let* (((eieio members) room)
               (displayname (a-get* members user-id 'displayname)))
    (or displayname user-id)))

;;;; Functions

(defun matrix-log (&rest args)
  "Log MESSAGE with ARGS to Matrix log buffer and return non-nil.
MESSAGE and ARGS should be a string and list of strings for
`format'."
  (when matrix-log
    (when (stringp (car args))
      (setq args (a-list 'message args)))
    (map-put args 'timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
    (let* ((print-circle t)
           (print-level 5)
           (print-length 10))
      (with-current-buffer (get-buffer-create matrix-log-buffer)
        (setq buffer-undo-list t)
        (save-excursion
          (goto-char (point-max))
          (insert (pp-to-string args) "\n")))))
  ;; Returning t is more convenient than nil, which is returned by `message'.
  t)

(defun matrix-error (&rest args)
  "Log ARGS to Matrix log buffer and signal error."
  ;; FIXME: Improve this and the docstring.
  (apply #'matrix-log args)
  (display-warning 'matrix-client (pp-to-string args) :error))

(defun matrix-unimplemented (&rest args)
  (when matrix-warn-unimplemented
    (apply #'matrix-log args)))

(defun matrix-get (&rest args)
  "Call `matrix-request' with ARGS for a \"GET\" request."
  (declare (indent defun))
  (apply #'matrix-request args ))

(defun matrix-post (&rest args)
  "Call `matrix-request' with ARGS for a \"POST\" request."
  (declare (indent defun))
  (nconc args (list :method 'post))
  (apply #'matrix-request args))

(defun matrix-put (&rest args)
  "Call `matrix-request' with ARGS for a \"PUT\" request."
  (declare (indent defun))
  (nconc args (list :method 'put))
  (apply #'matrix-request args))

(defun matrix-delete (&rest args)
  "Call `matrix-request' with ARGS for a \"DELETE\" request."
  (declare (indent defun))
  (nconc args (list :method 'delete))
  (apply #'matrix-request args))

(defun matrix-lookup (domain)
  "Return hostname of Matrix server for DOMAIN.
If DNS lookup fails, return DOMAIN."
  ;; TODO: Also return port, and actually use that port elsewhere.
  (pcase-let* ((`(_ _ (port ,_port) (target ,target))
                (dns-query (concat "_matrix._tcp." domain) 'SRV)))
    (or target domain)))

;;;; Methods

;;;;; Request

(cl-defun matrix-request (session endpoint &key data success
                                  raw-data (content-type "application/json")
                                  (method "GET") (error #'matrix-request-error-callback) timeout
                                  (query-on-exit t))
  "Make request to ENDPOINT on SESSION with DATA and call SUCCESS on success.
Request is made asynchronously.  METHOD should be a symbol or
string, `get' (the default) or `post' (it will be upcased).  ENDPOINT may be a string
or symbol and should represent the final part of the API
URL (e.g. for \"/_matrix/client/r0/login\", it should be
\"login\".  DATA should be an alist which will be automatically
encoded to JSON.  SUCCESS should be a method specialized on
`matrix-session', FIXME whose subsequent arguments are defined in
accordance with the `request' package's API.  ERROR, if
set, will be called if the request fails."
  (declare (indent defun))
  (with-slots (api-url-prefix access-token txn-id) session
    (let* ((url (url-encode-url
                 ;; MAYBE: Come up with a nicer way to use alternate API URL prefixes.
                 (if (and (stringp endpoint)
                          (s-prefix? "http" endpoint))
                     endpoint
                   (concat api-url-prefix (cl-typecase endpoint
                                            (string endpoint)
                                            (symbol (symbol-name endpoint)))))))
           ;; FIXME: Maybe don't send/increment txn-id for every
           ;; request, but only those that require it.  But it's
           ;; simpler to do it here, because we can't forget.
           ;; (data (map-put data 'txn-id (incf txn-id)))
           (data (map-filter
                  ;; Remove keys with null values
                  ;; TODO: Benchmark this against cl-loop.
                  (lambda (k v)
                    v)
                  data))
           (success (cl-typecase success
                      ;; If success is a symbol, apply session to
                      ;; it.  If it's an already-partially-applied
                      ;; function, use it as-is.
                      ;; FIXME: Add to docstring.
                      (symbol (apply-partially success session))
                      (t success)))
           (error (cl-typecase error
                    ;; If error is a symbol, apply session to
                    ;; it.  If it's an already-partially-applied
                    ;; function, use it as-is.
                    ;; FIXME: Add to docstring.
                    (symbol (apply-partially error session))
                    (t error)))
           (method (upcase (cl-typecase method
                             (string method)
                             (symbol (symbol-name method))))))
      ;; NOTE: This can log sensitive data in the `data' var, e.g. passwords and access tokens
      (matrix-log (a-list 'event 'matrix-request
                          'url url
                          'method method
                          'data data
                          'timeout timeout))
      (pcase method
        ("GET" (matrix-url-with-retrieve-async url
                 :query-on-exit query-on-exit
                 :silent t
                 :inhibit-cookies t
                 :extra-headers (a-list "Authorization" (concat "Bearer " access-token))
                 :query data
                 :parser #'json-read
                 :success success
                 :error error
                 :timeout timeout))
        ((or "POST" "PUT" "DELETE") (matrix-url-with-retrieve-async url
                                      :query-on-exit query-on-exit
                                      :silent t
                                      :inhibit-cookies t
                                      :method method
                                      :extra-headers (a-list "Content-Type" content-type
                                                             "Authorization" (concat "Bearer " access-token))
                                      :data (or raw-data (json-encode data))
                                      :parser #'json-read
                                      :success success
                                      :error error
                                      :timeout timeout))))))

(cl-defun matrix-request-request (session endpoint &key data success
                                          raw-data (content-type "application/json")
                                          (method "GET") (error #'matrix-request-error-callback) timeout
                                          (query-on-exit t))
  "Using `request', make request to ENDPOINT on SESSION with DATA and call SUCCESS on success.
Request is made asynchronously.  METHOD should be a symbol or
string, `get' (the default) or `post' (it will be upcased).  ENDPOINT may be a string
or symbol and should represent the final part of the API
URL (e.g. for \"/_matrix/client/r0/login\", it should be
\"login\".  DATA should be an alist which will be automatically
encoded to JSON.  SUCCESS should be a method specialized on
`matrix-session', FIXME whose subsequent arguments are defined in
accordance with the `request' package's API.  ERROR, if
set, will be called if the request fails."
  ;; NOTE: This is necessary because of the crazy bugs in url.el and request.el.  We can't just use
  ;; one, we have to use both.  This is a copy of `matrix-request' that only changes the
  ;; `matrix-url-with-retrieve-async' calls to `request' calls.  This will do for now.  Later we can
  ;; figure out a DRYer way.
  (declare (indent defun))
  (with-slots (api-url-prefix access-token txn-id) session
    (let* ((url (url-encode-url
                 ;; MAYBE: Come up with a nicer way to use alternate API URL prefixes.
                 (if (and (stringp endpoint)
                          (s-prefix? "http" endpoint))
                     endpoint
                   (concat api-url-prefix (cl-typecase endpoint
                                            (string endpoint)
                                            (symbol (symbol-name endpoint)))))))
           ;; FIXME: Maybe don't send/increment txn-id for every
           ;; request, but only those that require it.  But it's
           ;; simpler to do it here, because we can't forget.
           ;; (data (map-put data 'txn-id (incf txn-id)))
           (data (map-filter
                  ;; Remove keys with null values
                  ;; TODO: Benchmark this against cl-loop.
                  (lambda (k v)
                    v)
                  data))
           (success (cl-typecase success
                      ;; If success is a symbol, apply session to
                      ;; it.  If it's an already-partially-applied
                      ;; function, use it as-is.
                      ;; FIXME: Add to docstring.
                      (symbol (apply-partially success session))
                      (t success)))
           (error (cl-typecase error
                    ;; If error is a symbol, apply session to
                    ;; it.  If it's an already-partially-applied
                    ;; function, use it as-is.
                    ;; FIXME: Add to docstring.
                    (symbol (apply-partially error session))
                    (t error)))
           (method (upcase (cl-typecase method
                             (string method)
                             (symbol (symbol-name method))))))
      ;; NOTE: This can log sensitive data in the `data' var, e.g. passwords and access tokens
      (matrix-log (a-list 'event 'matrix-request
                          'url url
                          'method method
                          'data data
                          'timeout timeout))
      (pcase method
        ("GET" (request url
                        :headers (a-list "Authorization" (concat "Bearer " access-token))
                        :params data
                        :parser #'json-read
                        :success success
                        :error error
                        :timeout timeout))
        ((or "POST" "PUT") (request url
                                    :type method
                                    :headers (a-list "Content-Type" content-type
                                                     "Authorization" (concat "Bearer " access-token))
                                    :data (or raw-data (json-encode data))
                                    :parser #'json-read
                                    :success success
                                    :error error
                                    :timeout timeout))))))

(matrix-defcallback request-error matrix-session
  "Callback function for request error."
  :slots (user)
  :body (matrix-error (a-list 'event 'matrix-request-error-callback
                              'error error
                              'url url
                              'query query
                              'data data)))

;;;;; Login/logout

(defun matrix-login (session password)
  "Log in to SESSION with PASSWORD.
Session should already have its USER slot set, and optionally its
DEVICE-ID and INITIAL-DEVICE-DISPLAY-NAME."
  (with-slots (user device-id initial-device-display-name initial-sync-p login-attempts) session
    (setq initial-sync-p t)
    (cl-incf login-attempts)
    (matrix-post session 'login
      :data (a-list 'type "m.login.password"
                    'user user
                    'password password
                    'device_id device-id
                    'initial_device_display_name initial-device-display-name)
      :success #'matrix-login-callback
      :error (apply-partially #'matrix-login-error-callback session
                              ;; Pass password so we can use it again if necessary.
                              :cbargs (a-list 'password password)))))

(matrix-defcallback login matrix-session
  "Callback function for successful login.
Set access_token and device_id in session."
  :slots (access-token device-id)
  :body (pcase-let* (((map access_token device_id) data))
          (matrix-log (a-list 'event 'matrix-login-callback))
          (setq access-token access_token
                device-id device_id)
          (run-hook-with-args 'matrix-login-hook session)))

(matrix-defcallback login-error matrix-session
  "Callback function for unsuccessful login."
  :slots (server user login-attempts)
  :body (progn
          (matrix-log (a-list 'event 'matrix-login-error-callback
                              'error error
                              'url url
                              'query query
                              'data data))
          (pcase error
            (`(error http 403) (matrix-error "403 Unauthorized (probably invalid username or password)"))
            (_ (pcase login-attempts
                 (1 (progn
                      ;; One failed attempt, maybe because server hostname differs from user ID.
                      ;; Try looking up the hostname and logging in again.  We make a new session to
                      ;; reset the api-url-prefix to use the new server.
                      (matrix-login (matrix-session :user user
                                                    :server (matrix-lookup server)
                                                    :login-attempts login-attempts)
                                    (a-get cbargs 'password))))
                 (_ (matrix-error (format$ "Login failed.  Error: $error"))))))))

(defun matrix-logout (session)
  "Log out of SESSION."
  (with-slots (user device-id initial-device-display-name) session
    (matrix-post session 'logout
      :success #'matrix-logout-callback)))

(matrix-defcallback logout matrix-session
  "Callback function for successful logout.
Unset access_token and device_id in session."
  :slots (access-token device-id)
  ;; TODO: Do we need to set the device_id to nil?

  ;; FIXME: This doesn't stop outstanding polls, so when one returns,
  ;; it will raise a warning since the access token is now nil.
  :body (setq access-token nil
              device-id nil))

;;;;; Sync

(cl-defun matrix-sync (session &key full-state set-presence (timeout 30))
  "Start making /sync API requests.
Every TIMEOUT seconds, the server should return any outstanding
requests, and we make a new request."
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126

  ;; TODO: Probably should add a watchdog in case the server drops the connection and doesn't
  ;; actually return when timed-out.  We should be able to check the `symbol-status' arg to the
  ;; callback, and if it's `timeout' or `abort', we call /sync again.  That would let us avoid
  ;; using a timer.  To do this, we also need to set the timeout arg to `request' to, say, a
  ;; few seconds more than the timeout sent in the API request.

  ;; MAYBE: Don't make new /sync request if timeout is 0/nil.  It would have to be passed
  ;; through to the callback.

  ;; MAYBE: Can we just set the :complete arg to `request' to `matrix-sync' so that it will be
  ;; called repeatedly?  Or should we call `matrix-sync' again in the success callback?  Do we
  ;; want to stop syncing if there's an error?

  ;; NOTE: I guess we should stop syncing if there's an error, because otherwise, if next-batch
  ;; doesn't get updated, it will call sync repeatedly to get the same batch of data that's
  ;; failing, and likely enter an infinite loop (an asynchronous one, at least).  So we'll call
  ;; `matrix-sync' again in the success callback.

  ;; FIXME: Need to handle gaps according to the API: "Normally, all new events which are
  ;; visible to the client will appear in the response to the /sync API. However, if a large
  ;; number of events arrive between calls to /sync, a \"limited\" timeline is returned,
  ;; containing only the most recent message events. A state \"delta\" is also returned,
  ;; summarising any state changes in the omitted part of the timeline. The client may
  ;; therefore end up with \"gaps\" in its knowledge of the message timeline. The client can
  ;; fill these gaps using the /rooms/<room_id>/messages API. This situation looks like this:"

  ;; So IIUC, we need to see if "limited" is true FOR EACH ROOM, and if so, get the
  ;; "prev_batch" token FOR THAT ROOM and call `matrix-messages' with it.  But it seems that we
  ;; also need to call it with an "end" token, which I guess should be the "next_batch" token
  ;; from the last non-limited /sync request (which starts to get confusing).  The API docs
  ;; aren't especially clear here.  NOTE: I guess this will be difficult or impossible to test,
  ;; because I don't know how often the server returns limited timelines (i.e. how busy a room
  ;; has to be), or how we could make it do so, because there is no "limit" param for /sync.

  ;; I think what I will do for now is just raise a warning if "limited" is ever set.

  ;; The /messages call does NOT return a "limited", so I guess there is no way to know when
  ;; you've retrieved all of the messages that /sync WOULD have sent you if there were no
  ;; limit.  So I guess we have to decide for ourselves how many messages we want to show on
  ;; initial sync, and fetch backward until we have that many--unless, of course, we hit the
  ;; beginning of the room, in which case I guess the start/end tokens from /messages will be
  ;; the same...?  Or we'll receive fewer messages than the limit...?

  (with-slots (access-token next-batch pending-syncs disconnect) session
    (matrix-log (a-list 'event 'matrix-sync
                        'next-batch next-batch
                        'timeout timeout))
    (cl-case disconnect
      ;; This seems a bit inelegant, but it may be the best way to stop syncs from continuing after
      ;; the user has decided to disconnect.
      ('t (matrix-log (a-list 'event 'matrix-sync
                              'disconnect disconnect)))
      ('nil (unless access-token
              ;; This should never happen.
              (error "Missing access token for session"))
            (when-let ((response-buffer (matrix-get session 'sync
                                          :data (a-list 'since next-batch
                                                        'full_state full-state
                                                        'set_presence set-presence
                                                        ;; Convert timeout to milliseconds
                                                        'timeout (* timeout 1000))
                                          :success #'matrix-sync-callback
                                          :error #'matrix-sync-error-callback
                                          ;; Add 5 seconds to timeout to give server a bit of grace period before we
                                          ;; consider it unresponsive.
                                          ;; MAYBE: Increase grace period substantially, maybe up to 60 seconds.
                                          :timeout (+ timeout 5)
                                          ;; Don't prompt the user if Emacs is exited while a /sync is waiting
                                          :query-on-exit nil)))
              (push response-buffer pending-syncs))))))

(matrix-defcallback sync matrix-session
  "Callback function for successful sync request."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  :slots (next-batch initial-sync-p sync-retry-delay pending-syncs)
  :body (progn
          (matrix-log (a-list 'type 'matrix-sync-callback
                              'headers headers
                              'data data))
          (cl-loop for param in '(rooms presence account_data to_device device_lists)
                   ;; Assume that methods called will signal errors if anything goes wrong, so
                   ;; ignore return values.
                   do (if-fn-apply (concat "matrix-sync-" (symbol-name param))
                          (list session (a-get data param))
                        (matrix-unimplemented (format$ "Unimplemented API method: $fn-name"))))
          (when initial-sync-p
            (run-hook-with-args 'matrix-after-initial-sync-hook session))
          (setq initial-sync-p nil
                next-batch (a-get data 'next_batch)
                sync-retry-delay 0)
          (setq pending-syncs (delete (current-buffer) pending-syncs))
          (matrix-log "Sync callback complete.")
          (run-hook-with-args 'matrix-after-sync-hook session)))

(matrix-defcallback sync-error matrix-session
  "Callback function for sync request error."
  :slots (rooms next-batch initial-sync-p sync-retry-delay disconnect)
  :body (cl-case disconnect
          ('t (matrix-log (a-list 'event 'matrix-sync-error-callback
                                  'disconnect disconnect)))
          ('nil (matrix-log (a-list 'event 'matrix-sync-error-callback
                                    'error error
                                    'data data
                                    'sync-retry-delay sync-retry-delay))
                ;; NOTE: We do NOT run sync again here.  It's the client
                ;; side's responsibility to do so again in the watchdog-alert.
                (setq sync-retry-delay (cond ((>= sync-retry-delay 60) 60)
                                             (t (* sync-retry-delay 2)))))))

(defun matrix-sync-presence (session state-changes)
  "Process presence STATE-CHANGES."
  ;; TODO: Test this.
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id294
  (with-slots (followed-users) session
    (seq-doseq (change (a-get state-changes 'events))
      (pcase-let* (((map content) change)
                   ((map avatar_url currently_active last_active_ago presence user_id) content))
        (ht-set followed-users user_id (a-list 'avatar_url avatar_url
                                               'currently_active currently_active
                                               'last_active_ago last_active_ago
                                               'presence presence))))))

(defun matrix-sync-rooms (session rooms)
  "Process ROOMS from sync response on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (cl-loop for room in rooms
           do (progn
                (pcase room
                  (`(join . ,_) (matrix-sync-join session room))
                  (`(invite .  ,_) (matrix-unimplemented (format$ "Would process room invites: $room")))
                  (`(leave . ,_) (matrix-unimplemented (format$ "Would process room leaves: %s" room)))))))

(defun matrix-sync-join (session join)
  "Sync JOIN, a list of joined rooms, on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (with-slots (rooms) session
    (cl-loop for it in (cdr join)
             do (pcase-let* ((`(,joined-room-id . ,joined-room) it)
                             ;; Room IDs are decoded from JSON as symbols, so we convert to strings.
                             (room-id (symbol-name joined-room-id))
                             (params '(state timeline ephemeral account_data unread_notifications))
                             (room (or (--first (equal (oref it id) room-id)
                                                rooms)
                                       ;; Make and return new room
                                       (car (push (matrix-room :session session
                                                               :id room-id)
                                                  rooms))))
                             (prev-batch (a-get* joined-room 'timeline 'prev_batch)))
                  (cl-loop for param in params
                           ;; If the event array is empty, the function will be
                           ;; called anyway, so ignore its return value.
                           do (if-fn-apply (concat "matrix-sync-" (symbol-name param))
                                  (list room (a-get joined-room param))
                                (matrix-unimplemented (format$ "Unimplemented API method: $fn-name")))
                           ;; Always return t for now, so that we think the sync succeeded
                           ;; and we can set next_batch in `matrix-sync-callback'.
                           finally return t)
                  ;; Run client hooks
                  (run-hook-with-args 'matrix-room-update-hook room)
                  ;; FIXME: Shouldn't matter if we return t anymore.
                  t))))

(defun matrix-sync-state (room data)
  "Process state DATA in ROOM."
  (with-slots (state state-new) room
    (pcase-let (((map events) data))
      ;; events is an array, not a list, so we can't use --each.
      (seq-doseq (event events)
        (push event state)
        (push event state-new)
        (matrix-event room event)))))

;; (defvar matrix-sync-timeline-hook nil
;;   "List of functions called for new timeline events.
;; Each function is called with ROOM and EVENT.")

(defun matrix-sync-timeline (room data)
  "Sync timeline DATA in ROOM."
  (with-slots* (((id session timeline timeline-new timeline-event-ids prev-batch last-full-sync) room)
                ((next-batch) session))
    (pcase-let (((map events limited prev_batch) data))
      (matrix-log (a-list 'event 'matrix-sync-timeline
                          'room-id id
                          'prev-batch prev-batch
                          'last-full-sync last-full-sync
                          'data data))
      (seq-doseq (event events)
        (let ((id (a-get event 'event_id)))
          (unless (ht-get timeline-event-ids id)
            (ht-set timeline-event-ids id t)
            (push event timeline)
            (push event timeline-new)
            ;; Run API handler for event.
            (matrix-event room event))))
      ;; Reverse new events so that they are processed in chronological order.
      ;; MAYBE: Do this in client code rather than here.
      (setq timeline-new (nreverse timeline-new))
      (setq prev-batch prev_batch)
      (if (and (not (equal limited :json-false))
               last-full-sync)
          ;; Timeline is limited and we have a token to fill to: fill the gap.  If
          ;; `last-full-sync' is nil, this should mean that we are doing an initial sync, and
          ;; since we have no previous "since" token to fetch up to, we do not bother to fetch
          ;; more messages, even if the timeline is limited.
          ;; MAYBE: Add setting for minimum number of events/messages to initially fetch.
          (progn
            (matrix-log "Limited timeline.  Calling `matrix-messages'...")
            (matrix-messages room))
        ;; Timeline is not limited: save the not-yet-updated next-batch token.  If the next
        ;; timeline is limited, we use this token to know when we have filled the timeline gap.
        (matrix-log "Room fully synced.")
        (setq last-full-sync next-batch)))))

(defun matrix-event (room event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (if-fn-apply (concat "matrix-event-" type)
        (list room event)
      (matrix-unimplemented (format$ "Unimplemented API handler for event $type in room %s." (oref room id))))))

(defun matrix-event-m.tag (room event)
  "Process m.tag EVENT in ROOM."
  ;; NOTE: Assuming that the event is a comprehensive list of tags
  ;; that currently apply to the room, not a diff.
  (with-slots (tags) room
    (let-alist event
      (setf tags .content.tags))))

(defun matrix-event-m.room.member (room event)
  "Process m.room.member EVENT in ROOM."
  (with-slots (members id display-name session) room
    (pcase-let* (((map ('state_key user-id) content) event)
                 ((map membership displayname avatar_url) content))
      (pcase membership
        ;; TODO: Support all membership changes: invite, join, knock, leave, ban.
        ("join" (map-put members user-id (a-list 'displayname displayname
                                                 'avatar-url avatar_url)))
        ("leave" (setq members (map-delete members user-id)))))
    (unless (oref session initial-sync-p)
      (setf display-name (matrix--room-display-name room)))
    ;; FIXME: Don't think we need this hook, the client can just process the event from timeline-new.
    (run-hook-with-args 'matrix-event-m.room.member-hook room event)))

(defun matrix--set-room-display-names (session)
  "Set display names for all rooms in SESSION.
To be used in the after-initial-sync hook.  Computing it can be
expensive in large rooms, so rather than do it for every
m.room.member event received on initial sync, we do it once,
here, after initial sync."
  (dolist (room (oref session rooms))
    (oset room display-name (matrix--room-display-name room))))

(add-hook 'matrix-after-initial-sync-hook #'matrix--set-room-display-names)

(defun matrix-event-m.room.name (room event)
  "Process m.room.name EVENT in ROOM."
  (with-slots (name display-name) room
    (setq name (a-get* event 'content 'name)
          display-name (matrix--room-display-name room))
    (run-hook-with-args 'matrix-room-metadata-hook room)))

(defun matrix-event-m.room.canonical_alias (room event)
  "Process m.room.canonical_alias EVENT in ROOM."
  (with-slots (canonical-alias display-name) room
    (setq canonical-alias (a-get* event 'content 'alias)
          display-name (matrix--room-display-name room))
    (run-hook-with-args 'matrix-room-metadata-hook room)))

(defvar matrix-room-metadata-hook nil
  "List of functions called when a room's metadata is updated.
Each function is called with one argument, the room object that
was updated.")

(defvar matrix-room-update-hook nil
  ;; FIXME: Rename to matrix-room-timeline-hook
  "List of functions called when a room's timeline is updated.
Each function is called with one argument, the room object that
was updated.")

(defun matrix-clear-timeline (room)
  "Clear ROOM's `timeline-new' list."
  (with-slots (timeline-new) room
    (setq timeline-new nil)))

(defun matrix-clear-state (room)
  "Clear ROOM's `state-new' list."
  (with-slots (state-new) room
    (setq state-new nil)))

(cl-defun matrix-messages (room &key (direction "b") (limit 10))
  "Request messages for ROOM-ID in SESSION.
DIRECTION must be \"b\" (the default) or \"f\".  LIMIT is the
maximum number of events to return (default 10)."
  ;; TODO: As written, this may only work going backward.  Needs testing.
  (with-slots (id session prev-batch last-full-sync) room
    (matrix-get session (format$ "rooms/$id/messages")
      :data (a-list 'from prev-batch
                    'to last-full-sync
                    'dir direction
                    'limit limit)
      :success (apply-partially #'matrix-messages-callback room))))

(matrix-defcallback messages matrix-room
  "Callback for /rooms/{roomID}/messages."
  :slots (id timeline timeline-new timeline-event-ids prev-batch last-full-sync)
  :body (pcase-let* (((map start end chunk) data)
                     ;; Disable notifications while loading old messages.
                     (matrix-client-notifications nil)
                     (new-events-p))

          (matrix-log (a-list 'type 'matrix-messages-callback
                              'room-id id
                              'data data
                              'prev-batch prev-batch
                              'last-full-sync last-full-sync))
          ;; NOTE: API docs:
          ;; start: The token the pagination starts from. If dir=b
          ;; this will be the token supplied in from.
          ;; end: The token the pagination ends at. If dir=b this
          ;; token should be used again to request even earlier
          ;; events.
          (seq-doseq (event chunk)
            (let ((id (a-get event 'event_id)))
              (unless (ht-get timeline-event-ids id)
                (setq new-events-p t)
                (ht-set timeline-event-ids id t)
                (push event timeline)
                (push event timeline-new))))
          (setq timeline-new (nreverse timeline-new))
          (setq prev-batch end)
          (setq last-full-sync nil)
          (if new-events-p
              (run-hook-with-args 'matrix-room-update-hook room :old-messages t)
            (when (> (length chunk) 0)
              ;; Only got events we already had: go back further
              (matrix-messages room)))

          ;; NOTE: I don't think this code is necessary, but I'm temporarily leaving it for future reference.
          ;; (if (equal end last-full-sync)
          ;;     ;; Gap has been filled: clear the last-full-sync token (NOTE: Not sure if this is correct)
          ;;     (progn
          ;;       (matrix-log "Gap is filled")
          ;;       )
          ;;   ;; Gap not yet filled: continue filling
          ;;   (matrix-log "Gap not filled" id data)
          ;;
          ;;   (matrix-messages room))
          ))

(defun matrix-sync-ephemeral (room data)
  "Sync EPHEMERAL in ROOM."
  (with-slots (ephemeral) room
    (pcase-let (((map events) data))
      (seq-doseq (event events)
        (push event ephemeral)
        (matrix-event room event)))))

;; NOTE: Leaving these as methods for now.

(cl-defmethod matrix-sync-account_data ((session matrix-session) data)
  "Sync ACCOUNT-DATA in SESSION."
  (with-slots (account-data) session
    (pcase-let (((map events) data))
      (seq-doseq (event events)
        (push event account-data)))))

(cl-defmethod matrix-sync-account_data ((room matrix-room) data)
  "Sync ACCOUNT-DATA in ROOM."
  (with-slots (account-data) room
    (pcase-let (((map events) data))
      (seq-doseq (event events)
        (push event account-data)
        ;; Run API handler for event.
        (matrix-event room event)))))

(defun matrix-sync-to_device (session data)
  "Sync to_device data in SESSION."
  ;; FIXME: Implement.
  ;; (matrix-log "Received to_device data: %s" data)
  )

(defun matrix-sync-device_lists (session data)
  "Sync device_lists data in SESSION."
  ;; FIXME: Implement.
  ;; (matrix-log "Received device_lists data: %s" data)
  )

(defun matrix-sync-unread_notifications (room unread-notifications)
  "Sync UNREAD-NOTIFICATIONS in ROOM."
  ;; (pcase-let (((eieio id) room)
  ;;             ((map highlight_count notification_count) unread-notifications))
  ;;   (matrix-log "Would process highlight_count in %s: " id highlight_count)
  ;;   (matrix-log "Would process notification_count in %s: " id notification_count)
  ;;   t)
  )

;;;;; Rooms

(cl-defun matrix-create-room (session &key (visibility "private") alias name topic invite preset (is-direct t))
  "Create new room on SESSION.
When IS-DIRECT is non-nil, set that flag on the new room."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id190

  ;; MAYBE: Add other parameters: invite_3pid, creation_content,
  ;; initial_state.  Not sure how useful these would be for us.

  (matrix-post session 'createRoom
    :data (a-list 'visibility visibility
                  'room_alias_name alias
                  'name name
                  'topic topic
                  'preset preset
                  'is-direct is-direct)
    :success #'matrix-create-room-callback))

(matrix-defcallback create-room matrix-session
  "Callback for create-room.
Add new room to SESSION."
  ;; TODO: Should we add the room directly here, or should we do that after /sync?
  :slots (rooms)
  :body (pcase-let* (((map room_id) data)
                     (room (matrix-room :session session
                                        :id room_id)))
          (push room rooms)))

(defun matrix-join-room (session room-id)
  "Join ROOM-ID on SESSION.
If ROOM-ID does not have a server part, SESSION's server will be
added."
  (let* ((room-id (if (not (s-match (rx (1+ (not space)) ":" (1+ (not space)))
                                    room-id))
                      ;; Add server
                      (concat room-id ":" (oref session server))
                    ;; Already has server
                    room-id))
         (endpoint (concat "join/" (url-hexify-string room-id))))
    (matrix-post session endpoint
      :success #'matrix-join-room-callback
      :error #'matrix-join-room-error-callback)))

(matrix-defcallback join-room matrix-session
  "Callback for join-room."
  ;; Just log it, because it will be handled on the next sync.
  :body (matrix-log "JOINED ROOM: %s" (a-get data 'room_id)))

(matrix-defcallback join-room-error matrix-session
  "Error callback for join-room."
  ;; Just log it, because it will be handled on the next sync.
  :body (progn
          (matrix-log (a-list 'event 'matrix-join-room-error-callback
                              'data data
                              'url url
                              'status status))
          (let ((room-id (url-unhex-string (-last-item (s-split "/" url)))))
            (pcase error
              (`(error http 404) (matrix-error (format$ "Room not found: $room-id")))
              (_ (matrix-error (format$ "Error joining room $room-id: $error")))))))

(cl-defun matrix-send-message (room message &key (msgtype "m.text")
                                    override-txn-id extra-content success error)
  "Send MESSAGE of MSGTYPE to ROOM.
SUCCESS should be a function which will be called when the server
acknowledges the message; if nil, `matrix-send-message-callback'
will be called.  ERROR should be a function which will be called
if sending the message fails.  If OVERRIDE-TXN-ID is non-nil, use
it as the transaction ID; otherwise, automatically increment and
use the session's.  EXTRA-CONTENT is an alist to merge with the
standard event content object."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id182
  (with-slots* (((id session) room)
                ((txn-id) session))
    ;; Use `with-slots*' instead of `pcase-let*' so we can `incf' the txn-id.
    (let* ((type "m.room.message")
           (content (a-list 'msgtype msgtype
                            'body message))
           (txn-id (or override-txn-id (cl-incf txn-id)))
           (endpoint (format$ "rooms/$id/send/$type/$txn-id"))
           (success (or success
                        (apply-partially #'matrix-send-message-callback room))))
      (when extra-content
        (setq content (append content extra-content)))
      ;; FIXME: I just received a send-message reply from the server, with the event_id, 16
      ;; (sixteen) minutes after the HTTP PUT request.  I guess we need to set a timeout, now that
      ;; we're implementing resend.
      (matrix-put session endpoint
        :data content
        :success success
        :error error
        ;; Trying a 30 second timeout.  However, given that 16-minute reply I experienced, who knows
        ;; if this is a good idea.  Theoretically, if we resend with the same transaction ID, the
        ;; server won't duplicate the message...
        :timeout 30)
      ;; Return txn-id
      txn-id)))

(matrix-defcallback send-message matrix-room
  "Callback for send-message."
  ;; For now, just log it, because we'll get it back when we sync anyway.
  :slots (id)
  :body (matrix-log (a-list 'event 'matrix-send-message-callback
                            'room-id id
                            'data data)))

(cl-defun matrix-send-state (room &key data type success error)
  "Send state event of TYPE to ROOM with DATA.

TYPE should be, e.g. \"m.room.topic\"."
  (with-slots* (((id session) room))
    (let* ((endpoint (format$ "rooms/$id/state/$type"))
           (success (or success (apply-partially #'matrix-send-state-callback room))))
      (matrix-put session endpoint
        :data data
        :success success
        :error error
        :timeout 30))))

(matrix-defcallback send-state matrix-room
  "Callback for send-state."
  ;; For now, just log it, because we'll get it back when we sync anyway.
  :slots (id)
  :body (matrix-log (a-list 'event 'matrix-send-state-callback
                            'room-id id
                            'data data)))

(cl-defun matrix-room-tags (room tags &key (action 'put) success error)
  "Change TAGS on ROOM.
Each tag in TAGS may be a tag or a (TAG . ORDER) cons.

ACTION may be `put' or `delete', as appropriate."
  ;; e.g. /_matrix/client/r0/user/%40alice%3Aexample.com/rooms/%21726s6s6q%3Aexample.com/tags/u.work
  (let* ((session (oref room session))
         (room-id (url-hexify-string (oref room id)))
         (user-id (url-hexify-string (oref session user)))
         (success (or success (apply-partially #'matrix-set-tags-callback room)))
         (fn (intern (format "matrix-%s" action)))
         name data)
    ;; FIXME: When setting a tag, the server returns 400 Bad Request
    ;; "content must be a JSON object" unless I send the "order".  Not
    ;; sure if caused by JSON-encoding nil or if bug in server.  So
    ;; for now we always set the order to 1 unless specified, and
    ;; never send content when deleting.
    (dolist (tag tags)
      (pcase tag
        (`(,n . ,o) (setf name n
                          data (a-list 'order o)))
        (n (setf name n
                 data (a-list 'order 1))))
      (pcase action
        ('delete (setf data nil)))
      (funcall fn session (format$ "user/$user-id/rooms/$room-id/tags/$name")
               :data data
               :success success
               :error (apply-partially #'matrix-set-tags-error-callback room)
               :timeout 30))))

(matrix-defcallback set-tags matrix-room
  "Callback for set-tags."
  ;; For now, just log it, because we'll get it back when we sync anyway.
  :slots (id)
  :body (matrix-log (a-list 'event 'matrix-set-tags-callback
                            'room-id id
                            'data data)))

(matrix-defcallback set-tags-error matrix-room
  "Error Callback for set-tags."
  ;; For now, just log it, because we'll get it back when we sync anyway.
  :slots (id)
  :body (matrix-log (a-list 'event 'matrix-set-tags-error-callback
                            'room-id id
                            'data data)))

(defun matrix-client-room-user-tags (room)
  "Return list of user tags set in ROOM."
  (with-slots (tags) room
    (->> tags
         (-map #'car)
         (-map #'symbol-name)
         (--select (s-prefix? "u." it)))))

(defun matrix-set-name (room name)
  "Set ROOM name to NAME."
  (matrix-send-state room :type "m.room.name" :data (a-list 'name name)))

(defun matrix-set-topic (room topic)
  "Set ROOM topic to TOPIC."
  (matrix-send-state room :type "m.room.topic" :data (a-list 'topic topic)))

(defun matrix-leave (room)
  "Leave room."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id203
  (with-slots (id session) room
    (let* ((endpoint (format$ "rooms/$id/leave")))
      (matrix-post session endpoint
        :success (apply-partially #'matrix-leave-callback room)))))

(matrix-defcallback leave matrix-room
  "Leave room callback."
  :slots (session)
  ;; TODO: Verify that this works in more circumstances.  `equal' is
  ;; used, and it works in the test, but will it always work?
  ;; TODO: Optionally kill room buffer.
  :body (object-remove-from-list session :rooms room))

(defun matrix-forget (room)
  "Forget ROOM."
  ;; TODO: Maybe use room ID instead of object, since if we've left a
  ;; room, the object should be gone.  Alternatively, instead of
  ;; removing the room when it's left, change its status, or put it in
  ;; a list of left-but-not-forgotten rooms.  Should look at how the
  ;; API does it more, probably in /sync with the different lists of
  ;; rooms.

  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id204
  (with-slots (id session) room
    (let* ((endpoint (format$ "rooms/$id/forget")))
      (matrix-post session endpoint
        :success (apply-partially #'matrix-forget-callback room)))))

(matrix-defcallback forget matrix-room
  "Forget room callback."
  :slots (id)
  :body (matrix-log "FORGOT ROOM: %s" id))

(defun matrix-room-direct-p (room-id session)
  "Return non-nil if ROOM-ID in SESSION is \"m.direct\"."
  ;; Yes, this nesting is ugly, but this is how the API presents it.
  (pcase-let* ((`(((type . "m.direct")
                   (content . ,users)))
                (matrix-account-data "m.direct" session)))
    (cl-loop for (user-id . room-ids) in users
             thereis (seq-contains room-ids room-id #'string=))))

;;;;; Typing notifications

(cl-defun matrix-typing (room typing-p)
  "Send typing notification for ROOM.
TYPING-P should be t or nil."
  (pcase-let* (((eieio id session) room)
               ((eieio user) session)
               (endpoint (format$ "rooms/$id/typing/$user"))
               (data (a-list 'typing (or typing-p :json-false)
                             'timeout 30000)))
    (matrix-put session endpoint
      :data data
      :success (lambda (&rest args)
                 (matrix-log (a-list 'fn 'matrix-typing-success-callback
                                     'args args)))
      :error (lambda (&rest args)
               (matrix-log (a-list 'fn 'matrix-typing-error-callback
                                   'args args))))))

;;;;; Misc

(defun matrix-account-data (type session)
  "Return account data of TYPE for SESSION."
  (with-slots (account-data) session
    (--select (equal (alist-get 'type it) type)
              account-data)))

(defun matrix-transform-mxc-uri (session uri)
  "Return HTTPS URL for MXI URI to be accessed through SESSION."
  (pcase-let* (((eieio server) session)
               (`(,protocol _ ,mxc-server ,file) (split-string uri "/")))
    (format$ "https://$server/_matrix/media/v1/download/$mxc-server/$file")))

(defun matrix-upload (room path)
  "Upload file at PATH to SESSION's server."
  (pcase-let* (((eieio session) room)
               ((eieio server) session)
               (filename (file-name-nondirectory path))
               (extension (file-name-extension filename))
               (mime-type (cond (extension (mailcap-extension-to-mime extension))
                                (t (mailcap-extension-to-mime (symbol-name (or (image-type-from-file-header path)
                                                                               (error "Can't determine image type'")))))))
               (file-contents (with-temp-buffer
                                (insert-file-contents path)
                                (buffer-string)))
               (file-size (f-size path))
               (endpoint (url-encode-url (format$ "https://$server/_matrix/media/r0/upload?filename=$filename"))))
    (matrix-request-request session endpoint
      :method "POST"
      :success (apply-partially #'matrix-upload-callback room
                                :cbargs (list :path path
                                              :filename filename
                                              :mime-type mime-type
                                              :size file-size))
      :content-type mime-type
      :raw-data file-contents)))

(matrix-defcallback upload matrix-room
  "Callback for `matrix-upload'.
Post the uploaded file to the room as an m.image or m.file
message."
  :slots (id)
  :body (-let* (((&plist :path path :filename filename :mime-type mime-type :size size) cbargs)
                ((&alist 'content_uri url) data)
                (msgtype (cond ((s-prefix? "image/" mime-type) "m.image")
                               (t "m.file")))
                (info (a-list 'mimetype mime-type
                              'size size)))
          (matrix-log (a-list 'fn 'matrix-upload-callback
                              'room id
                              'data data))
          (pcase msgtype
            ("m.image" ;; Get image dimensions
             (-when-let* ((image (create-image path))
                          ((width . height) (image-size image t)))
               (map-put info 'w width)
               (map-put info 'h height))))
          (matrix-send-message room filename
                               :msgtype msgtype
                               :extra-content (a-list 'url url
                                                      'info info))))

;;;; Footer

(provide 'matrix-api-r0.3.0)

;;; matrix-api-r0.3.0.el ends here
