(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(groovy-indent-level 2 t)
 '(js-indent-level 2)
 '(longlines-show-hard-newlines nil)
 '(longlines-wrap-follows-window-size t)
 '(matlab-auto-fill f)
;; '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(spell-command "aspell")
 '(standard-indent 2)
 '(tcl-continued-indent-level 2)
 '(tcl-indent-level 2)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g"'goto-line)
(global-set-key "\M-s" 'save-buffer)
(setq kill-whole-line t)
;; (load "/Users/blezek/.emacs.d/lua-mode.el")
(setq auto-mode-alist
      (append
       '(("\\.txx\\'" . c++-mode))
       '(("\\.config\\'" . lua-mode))
       '(("\\.h\\'" . c++-mode))
       '(("\\.json\\'" . js-mode))
       auto-mode-alist))
;; '(("\.lua\'" . lua-mode))
;; '(("\.pro\'" . makefile-mode))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'longlines-mode)
(add-hook 'latex-mode-hook 'longlines-mode)
(add-hook 'java-mode-hook
          '(lambda ()
                                        ; (push-new '(access-label . /) c-offsets-alist)
             (setq c++-complete-class-prototype t)
             (setq c-basic-offset 2)
             (set-default 'indent-tabs-mode nil)
             ;; now fix it so \C-m indents
             (define-key c++-mode-map "\C-m"
               'reindent-then-newline-and-indent)
             ))
;; If we match the text on the first line, set the mode
(when (fboundp 'magic-mode-alist)
  (add-to-list 'magic-mode-alist '( ".*?iMI3C" . lua-mode))
  (add-to-list 'magic-mode-alist '( ".*?tclsh" . tcl-mode))
  )

(add-to-list 'interpreter-mode-alist '(".*?iMI3C" . lua-mode))
(add-to-list 'interpreter-mode-alist '(".*?tclsh" . tcl-mode))

(setq interpreter-mode-alist ( cons '("#!/bin/env iMI3C\\b" . lua-mode) interpreter-mode-alist))
(setq interpreter-mode-alist ( cons '("iMI3C" . lua-mode) interpreter-mode-alist))
(setq interpreter-mode-alist ( cons '("tclsh" . tcl-mode) interpreter-mode-alist))

(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))
(autoload 'cmake-mode "cmake-mode""CMake editing mode." t)

;; Autofill mode
(setq auto-mode-alist (cons '("COMMIT_EDITMSG$" . auto-fill-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("svn-commit\\.tmp$" . auto-fill-mode) auto-mode-alist))

;; Spaces not tabs
(setq-default indent-tabs-mode nil)
(setq kill-whole-line t)       ;;; Killing line also deletes \n
(setq require-final-newline t) ;;; Put \n at end of last line

;; F3 and F4 do the right thing!
;; (global-set-key [f8] 'defining-kbd-macro)
;; (global-set-key [f9] 'end-kbd-macro)
;; (global-set-key [f10] 'call-last-kbd-macro)
(global-set-key '[(f6)] 'join-line)

;; (add-hook 'c++-mode-hook (function (lambda () (c-set-style "itk"))))
;; (add-hook 'c++-mode-hook 'itk-c-mode-hook)

;; (setq auto-mode-alist (cons '("\\.txx$" . itk-c-mode-hook) auto-mode-alist))
(when (fboundp 'magic-mode-alist)
  (add-to-list 'magic-mode-alist '( "^#ifndef[[:space:]]?__itk.*" . itk-mode))
)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(require 'backup-dir)
(setq bkup-backup-directory-info
      '((t "~/Temp/XEmacsBackups" ok-create full-path prepend-name)))
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)
;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

(set-face-attribute 'default nil :height 140)
(setq font-lock-use-colors t)
(setq font-lock-use-fonts nil)


(autoload 'groovy-mode "groovy-mode""Groovy editing mode." t)
;; (setq-default groovy-indent-level 2)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(autoload 'longlines-mode "longlines.el""Minor mode for automatically wrapping long lines." t)


(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;;   (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(
                ("\\.cl\\'" . c-mode)
                ("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; (add-hook 'cmake-mode-hook (function cmake-rename-buffer))
(setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit


(setq cc-other-file-alist
      '(("\\.cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".txx" ".cxx"))
        ("\\.cxx$" (".h"))
        ("\\.txx$" (".h"))
        ))

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "M-<up>") 'ff-find-other-file)))


(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

(autoload 'itk-mode "itk-mode" "ITK editing mode." t)
(setq auto-mode-alist (cons '("itk*" . itk-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("sitk*$" . itk-mode) auto-mode-alist))

;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Cedet system
;; (load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu
;; (require 'semantic-ia)

