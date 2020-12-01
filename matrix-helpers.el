;;; matrix-helpers.el --- Helpers for matrix-client

;; Copyright (C) 2015-2016 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 4 March 2016
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.3.0

;; This file is not part of GNU Emacs.

;; matrix-helpers.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-helpers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpers for [`matrix-api'].

;;; Code:

;;;; Macros

(defmacro oref* (&rest slots)
  "Access SLOTS of nested EIEIO objects.
The first of SLOTS should be an object, while the rest should be
slot symbols.  Accessing each slot should return an object for
which the next slot is valid, except for the last slot, which may
return any value."
  (cl-labels ((rec (slots)
                   `(oref ,(if (and (consp (cdr slots))
                                    (cddr slots))
                               (rec (cdr slots))
                             (cadr slots))
                          ,(car slots))))
    (rec (nreverse slots))))

(defmacro oset-multi (object &rest pairs)
  "Set slot values for OBJECT.
PAIRS should be of the form (SLOT VALUE SLOT VALUE...)."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (slot value) on pairs by #'cddr
                collect (list 'oset object slot value))))

(defmacro a-get* (&rest keys)
  ;; See https://github.com/plexus/a.el/issues/7
  (cl-labels ((rec (keys)
                   `(a-get ,(if (and (consp (cdr keys))
                                     (cddr keys))
                                (rec (cdr keys))
                              (cadr keys))
                           ,(car keys))))
    (rec (nreverse keys))))

;;;; Functions

(defun matrix-pp-string (object)
  "Return pretty-printed representation of OBJECT as string."
  (with-temp-buffer
    (pp object (current-buffer))
    (buffer-string)))

(defun matrix-client-buffer-list-update-hook ()
  "Set buffer's modified status and move last-seen overlay when focused."
  ;; NOTE: Since this hook is added to the `buffer-list-update-hook', it
  ;; is ABSOLUTELY NECESSARY that this function be COMPLETELY bug
  ;; free.  If there is any bug, it can make it virtually impossible
  ;; to use Emacs, because nearly any command (especially if using
  ;; something like Helm) calls this hook, and the bug causes an error
  ;; before the command the user is running actually does anything.
  ;; Practically the only solution is to kill and restart Emacs (even
  ;; C-x C-c doesn't work).

  ;; FIXME: Using `when-let' to test for `matrix-client-room-object'
  ;; should be safe, but given the risk of using
  ;; `buffer-list-update-hook', and the fact that it's called in so
  ;; many places in Emacs, it would be better to do this a different
  ;; way altogether.  Maybe we could use `window-configuration-change-hook'.
  (-when-let* ((room matrix-client-room)
               (buffer (oref* room client-data buffer))
               (window (get-buffer-window buffer))
               (window-active-p (equal window (selected-window)))
               (not-one-window-p (not (= 1 (length (window-list))))))
    (when (get-buffer-window buffer 'visible)
      ;; FIXME: Need a way to move the seen line when there's one
      ;; window visible and the user has seen it.  Unfortunately,
      ;; there seems to be no way to detect whether the Emacs frame
      ;; ("window" in X) has focus, other than using focus hooks and
      ;; tracking state ourselves, which seems messy.  This is good
      ;; enough for now.
      (set-buffer-modified-p nil)
      ;; NOTE: Commenting out the last-seen-line-moving for now.
      ;; Maybe it should be an option, but I guess we shouldn't update
      ;; it just because the Emacs window happens to have focus and
      ;; the room's buffer happens to be in the selected window,
      ;; because the user might not be at the screen.  The user can
      ;; move the line by simply pressing RET, even without typing a
      ;; message.
      ;; (matrix-client-update-last-seen room)
      )))

;; (defun matrix-client-buffer-visible-p (&optional buffer)
;; FIXME: Remove this if we don't need it anymore.
;;   "Return non-nil if BUFFER is currently visible.
;; If BUFFER is nil, use the current buffer."
;;   (let ((buffer (or buffer (current-buffer))))
;;     (get-buffer-window buffer 'visible)))

(defun matrix-client-delete-backward-char (n &optional kill-flag)
  "Delete backward unless the point is at the prompt or other read-only text."
  (interactive "p\nP")
  (unless (get-text-property (- (point) 2) 'read-only)
    (call-interactively #'delete-backward-char n kill-flag)))

(defun matrix-homeserver-api-url (&optional version)
  "Message `matrix-homeserver-base-url' in to a fully-qualified API endpoint URL."
  (let ((version (or version "api/v1")))
    (format "%s/_matrix/client/%s" matrix-homeserver-base-url version)))

(defun matrix-client-room-for-id (connection room-id)
  "Return room for ROOM-ID on CONNECTION."
  (a-get (oref connection :rooms) room-id))

(defun matrix-parse-curl-exit-code (error-string)
  "Return exit code from ERROR-STRING as a number, or nil if none found."
  (cond
   ;; If we get a list with a number, use that '(401)
   ((and (listp error-string)
         (> (length error-string) 0)
         (numberp (first error-string)))
    (first error-string))
   ((stringp error-string)
    (when (string-match "exited abnormally with code \\([[:digit:]]+\\).*" error-string)
      (ignore-errors
        ;; Ignore errors to avoid any problems that might be caused by
        ;; the error string not matching.  I don't think this is
        ;; strictly necessary, but the old code caught errors, so just
        ;; in case...
        (string-to-number (match-string-no-properties 1 error-string)))))
   (t
    nil)))

(defun matrix--alist (&rest pairs)
  "Return an alist of the key-value pairs in PAIRS whose value is non-nil.
PAIRS is a spliced plist."
  ;; e.g. (matrix--alist "direction" "b" "limit" nil) => (("direction" . "b"))
  (cl-loop for (key value) on pairs by #'cddr
           when value
           collect (cons key value)))

(defun matrix--url-http-create-request (&optional ref-url)
  "Create an HTTP request for `url-http-target-url', referred to by REF-URL.

This is a copy of `url-http-create-request' from Emacs 26.1,
modified to allows binary uploads, which are prevented by the
\"fix\" for bug #23750 near the bottom of the function."
  (let* ((extra-headers)
	 (request nil)
	 (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
	 (using-proxy url-http-proxy)
	 (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
					      url-http-extra-headers))
			     (not using-proxy))
			 nil
		       (let ((url-basic-auth-storage
			      'url-http-proxy-basic-auth-storage))
			 (url-get-authentication url-http-proxy nil 'any nil))))
	 (real-fname (url-filename url-http-target-url))
	 (host (url-http--encode-string (url-host url-http-target-url)))
	 (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
		   nil
		 (url-get-authentication (or
					  (and (boundp 'proxy-info)
					       proxy-info)
					  url-http-target-url) nil 'any nil))))
    (if (equal "" real-fname)
	(setq real-fname "/"))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
	(setq auth (concat "Authorization: " auth "\r\n")))
    (if proxy-auth
	(setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

    ;; Protection against stupid values in the referrer
    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
					   (string= ref-url "")))
	(setq ref-url nil))

    ;; We do not want to expose the referrer if the user is paranoid.
    (if (or (memq url-privacy-level '(low high paranoid))
	    (and (listp url-privacy-level)
		 (memq 'lastloc url-privacy-level)))
	(setq ref-url nil))

    ;; url-http-extra-headers contains an assoc-list of
    ;; header/value pairs that we need to put into the request.
    (setq extra-headers (mapconcat
			 (lambda (x)
			   (concat (car x) ": " (cdr x)))
			 url-http-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
	(setq extra-headers (concat extra-headers "\r\n")))

    ;; This was done with a call to `format'.  Concatenating parts has
    ;; the advantage of keeping the parts of each header together and
    ;; allows us to elide null lines directly, at the cost of making
    ;; the layout less clear.
    (setq request
          (concat
           ;; The request
           (or url-http-method "GET") " "
           (url-http--encode-string
            (if using-proxy (url-recreate-url url-http-target-url) real-fname))
           " HTTP/" url-http-version "\r\n"
           ;; Version of MIME we speak
           "MIME-Version: 1.0\r\n"
           ;; (maybe) Try to keep the connection open
           "Connection: " (if (or using-proxy
                                  (not url-http-attempt-keepalives))
                              "close" "keep-alive") "\r\n"
           ;; HTTP extensions we support
           (if url-extensions-header
               (format
                "Extension: %s\r\n" url-extensions-header))
           ;; Who we want to talk to
           (if (/= (url-port url-http-target-url)
                   (url-scheme-get-property
                    (url-type url-http-target-url) 'default-port))
               (format
                "Host: %s:%d\r\n" (puny-encode-domain host)
                (url-port url-http-target-url))
             (format "Host: %s\r\n" (puny-encode-domain host)))
           ;; Who its from
           (if url-personal-mail-address
               (concat
                "From: " url-personal-mail-address "\r\n"))
           ;; Encodings we understand
           (if (or url-mime-encoding-string
		   ;; MS-Windows loads zlib dynamically, so recheck
		   ;; in case they made it available since
		   ;; initialization in url-vars.el.
		   (and (eq 'system-type 'windows-nt)
			(fboundp 'zlib-available-p)
			(zlib-available-p)
			(setq url-mime-encoding-string "gzip")))
               (concat
                "Accept-encoding: " url-mime-encoding-string "\r\n"))
           (if url-mime-charset-string
               (concat
                "Accept-charset: "
                (url-http--encode-string url-mime-charset-string)
                "\r\n"))
           ;; Languages we understand
           (if url-mime-language-string
               (concat
                "Accept-language: " url-mime-language-string "\r\n"))
           ;; Types we understand
           "Accept: " (or url-mime-accept-string "*/*") "\r\n"
           ;; User agent
           (url-http-user-agent-string)
           ;; Proxy Authorization
           proxy-auth
           ;; Authorization
           auth
           ;; Cookies
	   (when (url-use-cookies url-http-target-url)
             (url-http--encode-string
              (url-cookie-generate-header-lines
               host real-fname
               (equal "https" (url-type url-http-target-url)))))
           ;; If-modified-since
           (if (and (not no-cache)
                    (member url-http-method '("GET" nil)))
               (let ((tm (url-is-cached url-http-target-url)))
                 (if tm
                     (concat "If-modified-since: "
                             (url-get-normalized-date tm) "\r\n"))))
           ;; Whence we came
           (if ref-url (concat
                        "Referer: " ref-url "\r\n"))
           extra-headers
           ;; Length of data
           (if url-http-data
               (concat
                "Content-length: " (number-to-string
                                    (length url-http-data))
                "\r\n"))
           ;; End request
           "\r\n"
           ;; Any data
           url-http-data))
    ;; Bug#23750
    (unless (or
             ;; Our local fix to the "fix" is to not do the string-bytes/length comparison when
             ;; POSTing.
             (equal url-http-method "POST")
             (= (string-bytes request)
                (length request)))
      (error "Multibyte text in HTTP request: %s" request))
    (url-http-debug "Request is: \n%s" request)
    request))

(provide 'matrix-helpers)
;;; matrix-helpers.el ends here
