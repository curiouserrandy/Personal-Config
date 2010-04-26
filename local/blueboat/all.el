(require 'rs-frames)

(server-start)
(let ((display-size (list (x-display-pixel-width) (x-display-pixel-height))))
  (cond
   ((equal display-size '(1440 852))
    ;; Just my standard mac screen
    (setq initial-frame-alist
	  '((top . 1) (left . -1) (width . 80) (height . 48))))
   ((equal display-size '(2720 1024))
    ;; Home dual monitors, 1440x852 + 1280x1024
    (setq initial-frame-alist
	  '((top . 1) (left . 838) (width . 80) (height . 48))))
   ;; Leave it all alone if we don't recognize the config.
   (t nil)))
