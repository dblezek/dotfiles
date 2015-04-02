;; Package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
(projectile-global-mode)
(load-file "~/.emacs.d/rename.el")

;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)
;; Also in v23, moving was line by line visually, not by logical line
(setq line-move-visual nil)

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-?" 'auto-complete)
(global-set-key "\M-s" 'save-buffer)

;; Move to the next window/frame
(global-set-key "\M-`" 'next-multiframe-window)

(setq kill-whole-line t)       ;;; Killing line also deletes \n

;; Set the frame title
(setq-default frame-title-format "%b (%f)")

;; always indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\M-i" 'indent-buffer)

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

;; Set the backup directory
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
(setq auto-save-file-name-transforms
      `((".*" , "~/.saves" t)))

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

(set-face-attribute 'default nil :height 150)
(setq font-lock-use-colors t)
(setq font-lock-use-fonts nil)

;; ;; Font
(set-face-attribute 'default nil :font "Source Code Pro-15")

;; In Makefiles, set the tab-width to 8
(add-hook 'makefile-mode-hook
          (function
           (lambda()
           (setq tab-width 8))))


;; auto reload
(global-auto-revert-mode 1)

(setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit


;; Gives us unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;; IDO, allows smart search
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Auto pair parens
(electric-pair-mode 1)
(show-paren-mode 1)

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

;; Next / previous buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)


;; Neotree / projectile
;; (require 'projectile)
;; (require 'neotree)
;; (setq projectile-switch-project-action 'neotree-projectile-action)


;; directory tree
(require 'direx)
(require 'popwin)
(popwin-mode 1)
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;; If you are using popwin, you can use directory viewer as temporary "side-bar", like this:
(push '(direx:direx-mode :position right :width 30 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-x C-k") 'direx-project:jump-to-project-root-other-window)

;; Align with spaces, not tabs...
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Get our path from the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; (setenv "PATH" "~/.macosx/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:~/Source/go/bin")
(setenv "GOPATH" (shell-command-to-string ". ~/.bashrc; echo $GOPATH"))
(setenv "GOROOT" (shell-command-to-string ". ~/.bashrc; echo $GOROOT"))
;; (setenv "GOROOT" "~/Source/go")


;; Fuzzy finder
(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "build" "DEWEYLocal"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class" "*.dcm"))))

;; (require 'git)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 150 :width normal :foundry "apple" :family "Source Code Pro")))))

;; Kill the scratch buffer
(kill-buffer "*scratch*")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" ".git" "node_modules" "bower_components")))
 '(js-indent-level 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil))

;; Start the server
(server-start)
