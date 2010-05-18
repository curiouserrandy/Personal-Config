
;; Try and setup C++ bindings properly for chrome
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
