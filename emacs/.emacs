(setenv "PATH" "/Users/blezek/.macosx/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/blezek/Source/go/bin:/Users/blezek/Source/go-bin/bin")
(setenv "GOPATH" "/Users/blezek/Source/go-bin")
(setenv "GOROOT" "/Users/blezek/Source/go")

;; Package manager
(require 'package)
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Go Configuration
(require 'go-mode)

(require 'auto-complete-config)
(ac-config-default)
(require 'go-autocomplete)
(require 'yasnippet)
(yas-global-mode 1)
(setq ac-go-expand-arguments-into-snippets t)

(require 'projectile)
(setq projectile-mode t)
;; No toolbar
(tool-bar-mode nil)

;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)
;; Also in v23, moving was line by line visually, not by logical line
(setq line-move-visual nil)

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g"'goto-line)
(global-set-key "\M-s" 'save-buffer)
;; Move to the next window/frame
(global-set-key "\M-`" 'next-multiframe-window)

(setq kill-whole-line t)       ;;; Killing line also deletes \n

;; always indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Spaces not tabs
(setq-default indent-tabs-mode nil)
(setq require-final-newline t) ;;; Put \n at end of last line

;; Bash editing
(setq sh-indent-comment 4)

;; F3 and F4 do the right thing!
;; (global-set-key [f8] 'defining-kbd-macro)
;; (global-set-key [f9] 'end-kbd-macro)
;; (global-set-key [f10] 'call-last-kbd-macro)
(global-set-key '[(f6)] 'join-line)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-dark-laptop)

;; (require 'backup-dir)
;; (setq bkup-backup-directory-info
;;       '((t "~/Temp/XEmacsBackups" ok-create full-path prepend-name)))
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

;; ;; Font
(set-face-attribute 'default nil :font "Source Code Pro-14")

;; In Makefiles, set the tab-width to 8
(add-hook 'makefile-mode-hook
          (function
           (lambda()
           (setq tab-width 8))))


;; auto reload
(global-auto-revert-mode 1)

;; (setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit


(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; IDO, allows smart search
(require 'ido)
(ido-mode t)

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
(setq tab-width 2)

;; Next / previous buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)


;; Neotree / projectile
;; (require 'projectile)
;; (require 'neotree)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; Get our path from the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "build"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class"))))
;; (require 'git)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 160 :width normal :foundry "apple" :family "Source Code Pro")))))

;; Kill the scratch buffer
(kill-buffer "*scratch*")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
