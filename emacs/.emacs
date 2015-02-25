(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".DS_Store")))
 '(go-command "~/Source/go/bin/go")
 '(groovy-indent-level 2 t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "/opt/local/bin/aspell")
 '(js-indent-level 2)
 '(longlines-show-hard-newlines nil)
 '(longlines-wrap-follows-window-size t)
 '(matlab-auto-fill nil)
 '(sh-basic-offset 2)
 '(sh-indent-comment 4 t)
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(spell-command "aspell")
 '(standard-indent 2)
 '(tcl-continued-indent-level 2)
 '(tcl-indent-level 2)
 '(tool-bar-mode nil))
;; '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch))))

;; (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Monaco")))))

;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)
;; Also in v23, moving was line by line visually, not by logical line
(setq line-move-visual nil)

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g"'goto-line)
(global-set-key "\M-s" 'save-buffer)
;; Move to the next window/frame
(global-set-key "\M-`" 'next-multiframe-window)
(setq kill-whole-line t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (load "/Users/blezek/.emacs.d/lua-mode.el")
(setq auto-mode-alist
      (append
       '(("\\.txx\\'" . c++-mode))
       '(("\\.config\\'" . lua-mode))
       '(("\\.h\\'" . c++-mode))
       '(("\\.json\\'" . js-mode))
       '(("\.lua\'" . lua-mode))
       auto-mode-alist))
;; '(("\.pro\'" . makefile-mode))

;;
;; Setup puppet-mode for autoloading
;;
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


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

;; always indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)


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
(autoload 'cmake-mode "cmake-mode" "CMake editing mode." t)

;; Autofill mode
(setq auto-mode-alist (cons '("COMMIT_EDITMSG$" . auto-fill-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("COMMIT_EDITMSG$" . flyspell-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("svn-commit\\.tmp$" . auto-fill-mode) auto-mode-alist))

;; Spaces not tabs
(setq-default indent-tabs-mode nil)
(setq kill-whole-line t)       ;;; Killing line also deletes \n
(setq require-final-newline t) ;;; Put \n at end of last line

;; Bash editing
(setq sh-indent-comment 4)

;; F3 and F4 do the right thing!
;; (global-set-key [f8] 'defining-kbd-macro)
;; (global-set-key [f9] 'end-kbd-macro)
;; (global-set-key [f10] 'call-last-kbd-macro)
(global-set-key '[(f6)] 'join-line)

;; (add-hook 'c++-mode-hook (function (lambda () (c-set-style "itk"))))
;; (add-hook 'c++-mode-hook 'itk-c-mode-hook)

(autoload 'itk-mode "itk-mode" "ITK editing mode." t)
;; (setq auto-mode-alist (cons '("\\.txx$" . itk-c-mode-hook) auto-mode-alist))
(setq auto-mode-alist (cons '("^[s]?itk" . itk-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("^sitk" . itk-mode) auto-mode-alist))
;; (when (fboundp 'magic-mode-alist)
;;   (add-to-list 'magic-mode-alist '( "^#ifndef[[:space:]]?__itk.*" . itk-mode))
;;)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/themes"))

(require 'color-theme) 
(color-theme-initialize) 
(setq color-theme-is-global t)
(color-theme-dark-laptop)

;; Useful color themes:
;; charcoal-black
;; arjen
;; clarity
;; dark-laptop
;; hober
;; matrix
;; midnight
;; hacker

;; (require 'icicles)

(require 'minimap)

(require 'backup-dir)
(setq bkup-backup-directory-info
      '((t "~/Temp/XEmacsBackups" ok-create full-path prepend-name)))
;; Keep Tramp happy...
(setq tramp-bkup-backup-directory-info nil)
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)
;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

(set-face-attribute 'default nil :height 140)
(setq font-lock-use-colors t)
(setq font-lock-use-fonts nil)

;; Font
(set-face-attribute 'default nil :font "Source Code Pro-16")

(autoload 'groovy-mode "groovy-mode""Groovy editing mode." t)
;; (setq-default groovy-indent-level 2)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(autoload 'longlines-mode "longlines.el""Minor mode for automatically wrapping long lines." t)

;; In Makefiles, set the tab-width to 8
(add-hook 'makefile-mode-hook
          (function
           (lambda()
           (setq tab-width 8))))


(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;;   (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(
                ("\\.rst.inc\\'" . rst-mode)
                ("\\.cl\\'" . c-mode)
                ("\\.cu\\'" . c-mode)
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
;; Remove trailing white space in C/C++ code
(add-hook 'c-mode-common-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; reStructuredText mode
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'rst-mode-hook 'longlines-mode)

;; IDO, allows smart search
(require 'ido)
(ido-mode t)

;; Smart tabs?

;; Package manager
(require 'package)
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)

;; Auto pair parens
(electric-pair-mode 1)

;; Make M-/ comment and uncomment lines
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "M-/") 'comment-eclipse)

;; Line numbers
(global-linum-mode)

;; Go configuration
(defun my-go-mode-hook ()
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; Use goimports
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Tab width
(setq default-tab-width 2)

;; Next / previous buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)


;; Neotree / projectile
(require 'projectile)
(require 'neotree)
(setq projectile-switch-project-action 'neotree-projectile-action)

;; Get our path from the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "build"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class"))))
(require 'git)
;; (require 'egg)

;; Trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'rst-mode-hook
;;          (lambda()
;;            (remove-hook 'before-save-hook 'delete-trailing-whitespace 'true) ) )


;; Cedet system
;; (load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu
;; (require 'semantic-ia)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 145 :width normal :foundry "apple" :family "Source Code Pro")))))

;; Kill the scratch buffer
(kill-buffer "*scratch*")
