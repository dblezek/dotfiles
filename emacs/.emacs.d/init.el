;;; .xemacs/init.el

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; default to unified diffs
(setq diff-switches "-u")

(setq load-path "~/.emacs.d/")
(setq bkup-backup-directory-info
      '((t "~/Temp/XEmacsBackups" ok-create full-path prepend-name)))
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)

;; Font color changes
;; (global-font-lock-mode t)
(setq font-lock-auto-fontify t)   ; XEmacs
(setq load-path (cons "~/.emacs.d/" load-path))

;; Generally don't like tabs
(setq-default indent-tabs-mode nil)

(autoload 'ispell "ispell" "Start ispell." t)
(setq ispell-prefer-aspell t)

(add-hook 'tex-mode-hook 'flyspell-mode)
;; (add-hook 'latex-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'latex-mode-hook 'turn-on-font-lock)        


;;Move autosave files to a sensible place
;; (require 'auto-save) 
(require 'backup-dir) 

;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  ;;(print (concat "buffer-filename = " (buffer-file-name)))
  ;;(print (concat "buffer-name     = " (buffer-name)))
  (when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
    ;;(setq file-name (file-name-nondirectory (buffer-file-name)))
    (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
    ;;(print (concat "parent-dir = " parent-dir))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    ;;(print (concat "new-buffer-name= " new-buffer-name))
    (rename-buffer new-buffer-name t)
    )
  )

(add-hook 'cmake-mode-hook (function cmake-rename-buffer))
(setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit
;; (desktop-save-mode 1)
