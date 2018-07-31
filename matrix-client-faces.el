;;;;; Faces

(defface matrix-client-metadata
  '((((class color) (background light)) (:foreground "#000088" :weight bold))
    (((class color) (background dark)) (:foreground "#4444FF" :weight bold))
    (t (:weight bold)))
  "Face for chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-metadata
  '((((class color) (background light)) (:foreground "#268bd2" :weight bold))
    (((class color) (background dark)) (:foreground "#268bd2" :weight bold))
    (t (:weight bold)))
  "Face for user's own chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-messages
  '((((class color) (background light)) (:foreground "#586e75" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "#586e75" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's own chat messages."
  :group 'matrix-client)

(defface matrix-client-pending-messages
  '((((class color) (background light)) (:foreground "#586e75" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "#586e75" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's pending chat messages."
  :group 'matrix-client)

(defface matrix-client-failed-messages
  '((((class color) (background light)) (:foreground "red" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "red" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's failed chat messages."
  :group 'matrix-client)

(defface matrix-client-notice
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-notice-metadata
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-last-seen
  '((t (:inherit 'highlight :height 0.1)))
  "Face for last-seen overlay."
  :group 'matrix-client)

(defface matrix-client-date-header
  '((t (:inherit highlight :weight bold)))
  "Face for date headers."
  :group 'matrix-client)

(provide 'matrix-client-faces)
