;; -*-no-byte-compile: t; -*-

;; Autofill for GIT COMMIT Messages
(setq auto-mode-alist (cons '("COMMIT_EDITMSG$" . auto-fill-mode) auto-mode-alist))

;; Package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package try
	:ensure t)

(use-package which-key
	:ensure t 
	:config
	(which-key-mode))

(use-package expand-region
	:ensure t
	:config
	(global-set-key (kbd "C-@") 'er/expand-region))

;; ;; org mode
;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; (setq org-image-actual-width '(400))
;; (setq org-descriptive-links nil)

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; pending delete mode...
(pending-delete-mode t)


;; Get our path from the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setenv "GOPATH" (shell-command-to-string ". ~/.bashrc; echo $GOPATH"))

;; keep old clipboard contents
(setq save-interprogram-paste-before-kill 't)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Go Configuration
(use-package go-mode
  :ensure t)

;; convert a buffer to unix from DOS
(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; clear the echo area
(defun clear-message-area ()
  (interactive)
  (message nil)
  )

;; Latex mode
(add-hook 'tex-mode-hook 'visual-line-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)

;; Highight indent mode
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)

;; Company (complete anything), see http://company-mode.github.io/
(global-company-mode)

;; tern, very handy for Javascript.  Be sure to install in ~/.macosx or the like
;; (add-hook 'js-mode-hook (lambda() (tern-mode t)))

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
(global-set-key '[(f6)] 'join-line)

;; (require 'color-theme)
;; (color-theme-initialize)
;; (setq color-theme-is-global t)
;; (color-theme-dark-laptop)

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

;; Keep Tramp happy...
(setq tramp-bkup-backup-directory-info nil)
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)
(setq tramp-verbose 5)

;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

(set-face-attribute 'default nil :height 150)
(setq font-lock-use-colors t)
(setq font-lock-use-fonts nil)

;; ;; Font
;; (set-face-attribute 'default nil :font "Source Code Pro-15")

;; In Makefiles, set the tab-width to 8
(add-hook 'makefile-mode-hook
          (function
           (lambda()
           (setq tab-width 8))))

;; In Python, set to 4 character tabs
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 2)))
(add-hook 'python-mode-hook 'subword-mode)

;; GLSL mode
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

;; Web mode http://web-mode.org/
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))

;; C mode for ITK
(add-to-list 'auto-mode-alist '("\\.txx?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?\\'" . c++-mode))

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

;; Auto pair parens!  cool
(electric-pair-mode 1)
(show-paren-mode 1)

;; Need to match what eclipse does...
;; Make M-/ comment and uncomment lines
(setq mac-option-modifier 'super) ; make opt key do Super

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
(global-set-key (kbd "s-/") 'dabbrev-expand)

;; helpers, so that option works like I expect...
(global-set-key (kbd "s-b") 'backward-word)
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-d") 'kill-word)
(global-set-key (kbd "<s-backspace>") 'backward-kill-word)

;; Line numbers
(global-linum-mode)

;; open current buffer in a new frame(window)
(global-set-key [?\C-x ?5 ?c]
                '(lambda(newname display-flag)
                   "Like `clone-indirect-buffer-other-window' but display in another frame."
                   (interactive
                    (progn
                      (if (get major-mode 'no-clone-indirect)
                          (error "Cannot indirectly clone a buffer in %s mode" mode-name))
                      (list (if current-prefix-arg
                                (read-buffer "Name of indirect buffer: " (current-buffer))) t)))
                   (save-window-excursion
                     (let ((newbuf (clone-indirect-buffer newname display-flag)))
                       (switch-to-buffer-other-frame newbuf)
                       )
                     )
                   )
                )


;; Go configuration
(setq gofmt-command "goimports")
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


;; Markdown mode should include visual-line-mode and flyspell mode
(add-hook `markdown-mode-hook `flyspell-mode)
(add-hook `markdown-mode-hook `visual-line-mode)
;; Make back-ticks be an electric-pair in markdown mode
;; (add-hook `markdown-mode-hook (lambda()
;;                                 (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))
;;                                 (setq-local elctric-pair-text-pairs electric-pair-pairs)
;;                                 ))

;; Align with spaces, not tabs...
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Insert the date
(defun insert-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%F)")))

;; Fuzzy finder
(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "build" "DEWEYLocal"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class" "*.dcm"))))

;; Find tags without the prompt
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting"
  (interactive)
  (xref-find-definitions (find-tag-default)))
;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'find-tag-no-prompt)
(require 'etags-select)
(require 'etags-table)
(setq etags-table-search-up-depth 10)
;; (global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "M-*") 'pop-tag-mark)
;; never add tags, keeps annoying waring away
;; https://emacs.stackexchange.com/questions/14802/never-keep-current-list-of-tags-tables-also
(setq tags-add-tables nil)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :height 150 :width normal :foundry "apple" :family "Source Code Pro"))))
 '(fixed-pitch ((t (:height 150 :family "Source Code Pro"))))
 '(highlight-indent-guides-character-face ((t (:foreground "#a9a9a9")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(c-basic-offset 2)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(groovy-indent-offset 2)
 '(highlight-indent-guides-method (quote character))
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" ".git" "node_modules" "bower_components")))
 '(js-indent-level 2)
 '(latex-run-command "latex --synctex=1")
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(markdown-fontify-code-blocks-natively t)
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("`" org-code verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-src-preserve-indentation nil)
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (edit-indirect-region-latex expand-region elm-mode matlab-mode edit-indirect highlight-indent-guides smart-mode-line json-navigator json-mode org-bullets which-key try use-package rib-mode package-lint ## etags-select etags-table go-mode company-tern web-mode sqlite sql-indent company-shell company-ansible company-lua company-go company markdown-preview-mode cmake-font-lock yaml-mode toml-mode terraform-mode tabbar scss-mode scala-mode2 scala-mode popwin neotree markdown-mode lua-mode groovy-mode gradle-mode go-errcheck go-direx go-autocomplete glsl-mode ggtags fiplr exec-path-from-shell dockerfile-mode direx-grep cmake-mode autopair)))
 '(python-guess-indent t)
 '(python-indent 4)
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

;; SML needs to be after custom-set-variables
;; https://github.com/Malabarba/smart-mode-line/issues/88
(sml/setup)

;; Kill the scratch buffer
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  )

;; Start the server, only the graphic UI
;; see: https://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
(when (display-graphic-p)
  (server-start)
  )

