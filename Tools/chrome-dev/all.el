
;; Try and setup C++ bindings properly for chrome
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq large-file-warning-threshold
      (max large-file-warning-threshold (* 25 1024 1024))) ;For TAGS file.
