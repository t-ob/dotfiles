(let ((org-lisp-dir (expand-file-name "lisp" (concat (getenv "HOME") "/.emacs.d/src/org-mode"))))
  (when (file-directory-p org-lisp-dir)
    (add-to-list 'load-path org-lisp-dir)
    (require 'org)))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq base-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (concat (getenv "HOME") "/.emacs.d/src/org-mode")
                  (file-directory-p (expand-file-name "lisp"
                                                      (concat (getenv "HOME") "/.emacs.d/src/org-mode"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "config.org" base-dir))))
