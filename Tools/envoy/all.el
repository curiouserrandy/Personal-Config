;; Try and setup C++ bindings properly for envoy
(require 'google-c-style)
(require 'rs-underpoint)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Make sure C++ shows up before C for .h files.
(setq auto-mode-alist (cons '("\\.h" . c++-mode) auto-mode-alist))

(defconst envoy-codesearch-search-template
  "https://cs.corp.google.com/search/?q=%s+file://depot/google3/third_party/envoy&type=cs"
  "URL to visit for searching for a particular string in codesearch.")

(setq envoy-codesearch-history '())

(defun envoy-codesearch (search-string)
  (interactive (list
		(read-from-minibuffer
		 "Tag to Codesearch: " 
		 (randy-word-under-point)
		 nil nil envoy-codesearch-history)))
  (browse-url (format envoy-codesearch-search-template search-string)))

(provide 'envoy-dev)
