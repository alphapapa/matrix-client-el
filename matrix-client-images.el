;; -*- lexical-binding: t; -*-

(require 'shr)

(require 'request)
(require 's)

(require 'matrix-macros)

(defcustom matrix-client-show-images nil
  "Download and show images posted to rooms."
  :type 'boolean
  :group 'matrix-client)

(defcustom matrix-client-image-url-prefixes
  (list (rx bow "http" (optional "s") "://"
            (or "i.imgur.com"
                "i.redd.it"
                "i.redditmedia.com"
                )
            "/"))
  "List of regexps matching parts of URLs to images that should be downloaded and displayed.
Each regexp should match from the beginning of the URL, including
the protocol type, if desired.  It will automatically be extended
to match until the next whitespace character."
  :type '(repeat string)
  :group 'matrix-client)

(defconst matrix-client-can-show-images (or (fboundp 'imagemagick-types) (version< "27.0.49" emacs-version))
  "Flag indicating if emacs has the requirements to show the images in matrix-client.")

(defun matrix-client--image-urls (text)
  "Return list of supported image URLs in TEXT."
  (cl-loop for regexp in matrix-client-image-url-prefixes
           for re = (rx-to-string `(seq (regexp ,regexp) (1+ (not space))))
           append (-map #'car (s-match-strings-all re text))))

(defun matrix-client-insert-image (room message-id url)
  "Download image from URL and insert it at message MESSAGE-ID in ROOM."
  (matrix-url-with-retrieve-async url
    :silent t
    :inhibit-cookies t
    :query-on-exit nil
    :parser (apply-partially #'matrix-client-parse-image room)
    :success (apply-partially #'matrix-client-insert-image-callback
                              :room room
                              :message-id message-id
                              :url url)))

(defun matrix-client-parse-image (room &rest rescale-args)
  "Parse image from current HTTP response buffer and return image object.
RESCALE-ARGS are passed to `matrix-client-rescale-image'."
  (pcase-let* ((data (progn
                       ;; Disabling multibyte is required for reading binary data.
                       ;; FIXME: require or autoload this function
                       (mm-disable-multibyte)
                       ;; Point is where the body starts, after the headers
                       (buffer-substring (point) (point-max))))
               ((eieio client-data) room)
               ((eieio buffer) client-data))
    (with-current-buffer buffer
      ;; Rescale image in room buffer to get proper size
      (apply #'matrix-client-rescale-image data rescale-args))))

(cl-defun matrix-client-rescale-image (data &key max-width max-height &allow-other-keys)
  "Rescale DATA, if too big, to fit the current buffer.
MAX-WIDTH and MAX-HEIGHT are used if set, otherwise they are
determined by the size of the buffer's window."
  ;; Based on image.el
  (if (null matrix-client-can-show-images)
      nil
    (cond ((and max-width max-height)
           ;; Use given size
           )
          ((get-buffer-window (current-buffer))
           ;; Use window size
           (setq max-width (or max-width (window-pixel-width))
                 max-height (or max-height (/ (window-pixel-height) 2))))
          ((current-buffer)
           ;; Buffer not displayed; use frame
           (setq max-width (or max-width (frame-pixel-width))
                 max-height (or max-height (/ (frame-pixel-height) 2))))
          (t
           ;; This should not happen with the fixes above, but just in case:
           (warn "Weird error rescaling image, please report.  Buffer: %s" (current-buffer))))
    (if (version< "27.0.49" emacs-version)
        (create-image data nil 'data-p
                      :ascent 'center
                      :max-width max-width
                      :max-height max-height)
        (create-image data 'imagemagick 'data-p
                      :ascent 'center
                      :max-width max-width
                      :max-height max-height))))

(cl-defun matrix-client-insert-image-callback (&key room message-id url data &allow-other-keys)
  "Insert image into proper place at URL in message MESSAGE-ID in ROOM.
Image is passed from parser as DATA, which should be an image
object made with `create-image'.  This function should be called
as an async callback when the image is downloaded."
  (with-room-buffer room
    (save-excursion
      ;; Starting with last message, search backward to find message
      (cl-loop initially do (goto-char (1- (matrix-client--prompt-position)))
               for event_id = (get-text-property (point) 'event_id)
               until (equal event_id message-id)
               do (goto-char (previous-single-property-change (point) 'event_id)))
      ;; Find beginning and end of message text
      (let* ((beg (point))
             (end (or (next-single-property-change beg 'event_id)
                      (matrix-client--prompt-position)))
             (inhibit-read-only t)
             (props (text-properties-at beg))
             (string (with-temp-buffer
                       ;; NOTE: These space characters before the newlines
                       ;; are REQUIRED for some reason.  Without them,
                       ;; images disappear seemingly at random when
                       ;; the user scrolls, moves point, or types.
                       (insert " \n")
                       (insert-image data)
                       (insert " \n")
                       (apply #'propertize (buffer-string) props))))
        ;; Find URL and insert image after it
        (goto-char beg)
        (cond ((re-search-forward (regexp-quote url) end 'noerror)
               ;; Found plain URL string in buffer
               (goto-char (match-end 0)))
              ;; No plain URL found; might be a text-button.  Just insert at end of message.
              ((goto-char end)))
        (insert string)
        (forward-char)))))

(provide 'matrix-client-images)
