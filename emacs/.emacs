;; -*-no-byte-compile: t; -*-

;; Autofill for GIT COMMIT Messages
(setq auto-mode-alist (cons '("COMMIT_EDITMSG$" . auto-fill-mode) auto-mode-alist))

;; Package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; keep old clipboard contents
(setq save-interprogram-paste-before-kill 't)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Go Configuration
(require 'go-mode)

;; Emacs parses the buffer and creates a suggestion list
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'go-autocomplete)
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (setq ac-go-expand-arguments-into-snippets t)

;; convert a buffer to unix from DOS
(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; PROJECTILE MODE
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
;; disable remote file caching (https://github.com/bbatsov/projectile)
(setq projectile-file-exists-remote-cache-expire nil)
(load-file "~/.emacs.d/rename.el")
;; Remove for TRAMP connections
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))


;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)

;; Company (complete anything), see http://company-mode.github.io/
(add-hook 'after-init-hook 'global-company-mode)

;; tern, very handy for Javascript.  Be sure to install in ~/.macosx or the like
(add-hook 'js-mode-hook (lambda() (tern-mode t)))

;; bind to M-? for autocomplete now
(global-set-key "\M-?" 'company-complete-common)
;; in text, company mode always suggests lowercase options.
;; see http://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text for fix
(setq company-dabbrev-downcase nil)

;; NB: these two go together, let option be the "super" key (not sure how this will work for CLI)
;; (setq mac-option-modifier 'meta)
;; (global-set-key (kbd "s-/") 'auto-complete)
;; Also in v23, moving was line by line visually, not by logical line
(setq line-move-visual nil)

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g" 'goto-line)
;; (global-set-key "\M-?" 'auto-complete)
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
;; Don't indent, just insert a new line
(global-set-key (kbd "C-<return>") (lambda () (interactive) (insert "\n")))

;; Spaces not tabs
(setq-default indent-tabs-mode nil)
(setq require-final-newline t) ;;; Put \n at end of last line

;; F3 and F4 do the right thing!
;; (global-set-key [f8] 'defining-kbd-macro)
;; (global-set-key [f9] 'end-kbd-macro)
;; (global-set-key [f10] 'call-last-kbd-macro)
(global-set-key '[(f6)] 'join-line)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-dark-laptop)

;; Set the backup directoryto $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
;; No more # files
(setq auto-save-default nil)

;; restore opened files
(desktop-save-mode 1)

;; (require 'backup-dir)
;; (setq bkup-backup-directory-info
;;       '((t "~/Temp/XEmacsBackups" ok-create full-path prepend-name)))
;; Keep Tramp happy...
(setq tramp-bkup-backup-directory-info nil)
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)
(setq tramp-verbose 5)
;; controlmaster options for tramp
;; See https://lists.gnu.org/archive/html/help-gnu-emacs/2013-04/msg00317.html
;; and https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00890.html
;; (setq tramp-ssh-controlmaster-options
;;       (concat
;;         "-o ControlPath=~/.ssh/tramp-%%C "
;;         "-o ControlMaster=auto -o ControlPersist=yes"))

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

;; In Python, set to 4 character tabs
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))
(add-hook 'python-mode-hook 'subword-mode)

;; GLSL mode
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

;; Web mode http://web-mode.org/
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
(add-to-list 'web-mode-comment-formats '("javascript" . "//"))


;; auto reload
(global-auto-revert-mode t)

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)          


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
  ;; (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Kill by camel case
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'subword-mode)

;; Next / previous buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)


;; Neotree / projectile
;; (require 'projectile)
;; (require 'neotree)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; Markdown mode should include visual-line-mode and flyspell mode
(add-hook `markdown-mode-hook `flyspell-mode)
(add-hook `markdown-mode-hook `visual-line-mode)

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
;; (setenv "GOROOT" (shell-command-to-string ". ~/.bashrc; echo $GOROOT"))
;; (setenv "GOROOT" "~/Source/go")


;; Insert the date
(defun insert-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%F)")))

;; Fuzzy finder
(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "build" "DEWEYLocal"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class" "*.dcm"))))

;; (require 'git)

;; Find tags without the prompt
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting"
  (interactive)
  (xref-find-definitions (find-tag-default)))
;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'find-tag-no-prompt)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 150 :width normal :foundry "apple" :family "Source Code Pro")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" ".git" "node_modules" "bower_components")))
 '(js-indent-level 2)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(package-selected-packages
   (quote
    (company-tern web-mode sqlite sql-indent company-shell company-ansible company-lua company-go company markdown-preview-mode cmake-font-lock color-theme-solarized color-theme-modern yaml-mode toml-mode terraform-mode tabbar scss-mode scala-mode2 scala-mode popwin neotree markdown-mode lua-mode groovy-mode gradle-mode go-projectile go-errcheck go-direx go-autocomplete glsl-mode ggtags fiplr exec-path-from-shell dockerfile-mode direx-grep color-theme cmake-mode autopair)))
 '(python-guess-indent t)
 '(python-indent 2)
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 2)
 '(sh-basic-offset 2)
 '(sh-indent-comment 2)
 '(sh-indentation 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-script-padding 0))

;; Start the server
(require 'server)
(unless (boundp 'server-process) (server-start))
;; Kill the scratch buffer
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  )
