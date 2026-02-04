;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun make-bold (beg end)
  (interactive "r")
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face '(:weight bold :width condensed))
    (overlay-put ov 'priority 100)
    (message "Make bold style done!")))

;; ;;;###autoload
;; (defun make-default (beg end)
;;   (interactive "r")
;;   (let ((ov (make-overlay beg end)))
;;     (overlay-put ov 'face '(:weight regular :width condensed))
;;     (overlay-put ov 'priority 100)
;;     (message "bold done!")))

;;;###autoload
(defun remove-bold (beg end)
  (interactive "r")
  (remove-overlays beg end 'face '(:weight bold :width condensed))
  ;; (remove-overlays beg end) 
  (message "Bold styles cleared!"))
