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

(defun package-reinstall-activated ()
  "Reinstall all activated packages."
  (interactive)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name )
                (warn "Package %s failed to reinstall" package-name))))))

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


;; different theme
(use-package zenburn-theme
  :ensure t
  )

(use-package nimbus-theme
  :ensure t
  :disabled
  )
(use-package monokai-theme
  :ensure t
  :disabled
  )

;; try helm http://tuhdo.github.io/helm-intro.html
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (global-set-key (kbd "C-s") 'helm-occur)
  (global-set-key (kbd "C-S-s") 'isearch-forward)
  (global-set-key (kbd "M-.") 'helm-etags-select)
  (global-set-key (kbd "M-*") 'pop-tag-mark)
  
  (setq helm-follow-mode-persistent t )
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  ;; https://www.reddit.com/r/emacs/comments/7rho4f/now_you_can_use_helm_with_frames_instead_of/
  (setq
     ;; helm-display-function 'helm-display-buffer-in-own-frame
     helm-display-function 'helm-default-display-buffer
     helm-display-buffer-reuse-frame t
     helm-use-undecorated-frame-option t
     helm-always-two-windows nil
     helm-split-window-in-side-p nil
     helm-source-names-using-follow '("Occur")
     helm-follow-mode-persistent t
     )
  (helm-mode 1)
  )

;; better helm search
(use-package helm-swoop
  :ensure t
  :config
  
  ;; this is perhaps the best search tool ever
  ;; https://github.com/ShingoFukuyama/helm-swoop
  (defun helm-swoop-multiline-4 ()
    (interactive)
    (helm-swoop :$query "" :$multiline 4))

  ;; Always use the previous search for helm. Remember C-<backspace> will delete entire line
  (setq helm-swoop-pre-input-function
        (lambda () (if (boundp 'helm-swoop-pattern)
                       helm-swoop-pattern "")))
  
  (setq helm-swoop-split-direction 'split-window-vertically)
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key [(control shift s)] 'helm-swoop-multiline-4)
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key [(meta shift i)] 'helm-swoop-multiline-4)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  )

;; Highight indent mode
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-odd-face-perc 35)
  (setq highlight-indent-guides-auto-even-face-perc 35)
  (setq highlight-indent-guides-auto-character-face-perc 30)  
  )

;; Gives us unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
  )

;; Tree view of projects on F8
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :init
  (bind-key '[(f8)] 'treemacs)
  )

(use-package desktop
  :config
  ;; see https://stackoverflow.com/questions/4053708/emacs-desktop-doesnt-remember-tramp-connections?rq=1
  ;; for details
  (setq desktop-buffers-not-to-save "^$")
  (setq desktop-files-not-to-save "^$")
)

(use-package tramp
  :config
  ;; (setq tramp-default-method "ssh")
  (tramp-change-syntax 'simplified)
  )

(use-package powerline
  :ensure t
  :disabled t
  :config
  (powerline-center-theme)
)

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; pending delete mode...
(pending-delete-mode t)


;; Set path
;; add /usr/local/bin
(setq exec-path (append exec-path '("/usr/local/bin/")))
;; Get our path from the shell
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))
;; (setenv "GOPATH" (shell-command-to-string ". ~/.bashrc; echo $GOPATH"))

;; keep old clipboard contents
(setq save-interprogram-paste-before-kill 't)

;; No startup message
(setq inhibit-startup-message t)

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

;; In version 23, the command key was mapped to 'super' to allow common mac shortcuts
(setq mac-command-modifier 'meta)

;; Company (complete anything), see http://company-mode.github.io/
(use-package company
  :ensure t
  :config
(global-company-mode)
;; bind to M-? for autocomplete now
(global-set-key "\M-?" 'company-complete-common)
;; in text, company mode always suggests lowercase options.
;; see http://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text for fix
(setq company-dabbrev-downcase nil)
  )

;; tern, very handy for Javascript.  Be sure to install in ~/.macosx or the like
;; (add-hook 'js-mode-hook (lambda() (tern-mode t)))

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
;; (add-hook 'makefile-mode-hook
;;           (function
;;            (lambda()
;;              (setq tab-width 8))))

;; In Python, set to 4 character tabs
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil
;;                   tab-width 2)))
;; (add-hook 'python-mode-hook 'subword-mode)

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

;; IDO, allows smart search
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

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
                    (gn
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
(use-package fiplr
  :ensure t
  :disabled
  :config
  (global-set-key (kbd "C-x f") 'fiplr-find-file)
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "build" "DEWEYLocal"))
                              (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class" "*.dcm"))))

  )

;; ;; Find tags without the prompt
;; (defun find-tag-no-prompt ()
;;   "Jump to the tag at point without prompting"
;;   (interactive)
;;   (xref-find-definitions (find-tag-default)))
;; ;; don't prompt when finding a tag
;; (global-set-key (kbd "M-.") 'find-tag-no-prompt)
;; ;; (use-package etags-select
;; ;;   :ensure t
;; ;;   )
;; ;; (require 'etags-table)
;; ;; (setq etags-table-search-up-depth 10)
;; ;; (global-set-key (kbd "M-.") 'etags-select-find-tag)
;; (global-set-key (kbd "M-*") 'pop-tag-mark)
;; ;; never add tags, keeps annoying waring away
;; ;; https://emacs.stackexchange.com/questions/14802/never-keep-current-list-of-tags-tables-also
;; (setq tags-add-tables nil)


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
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(desktop-save-mode t)
 '(desktop-restore-eager 1)
 '(groovy-indent-offset 2)
 '(helm-mode t)
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" ".git" "node_modules" "bower_components")))
 '(js-indent-level 2)
 '(latex-run-command "latex --synctex=1")
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(markdown-fontify-code-blocks-natively t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
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
    (spaceline telephone-line powerline minimap sublimity-map sublimity helm-swoop magit anaconda-mode elpy monokai-theme nimbus-theme treemacs highlight-indent-guides-mode helm zenburn-theme edit-indirect-region-latex expand-region elm-mode matlab-mode edit-indirect highlight-indent-guides smart-mode-line json-navigator json-mode org-bullets which-key try use-package rib-mode package-lint ## etags-select etags-table go-mode company-tern web-mode sqlite sql-indent company-shell company-ansible company-lua company-go company markdown-preview-mode cmake-font-lock yaml-mode toml-mode terraform-mode tabbar scss-mode scala-mode2 scala-mode popwin neotree markdown-mode lua-mode groovy-mode gradle-mode go-errcheck go-direx go-autocomplete glsl-mode ggtags fiplr exec-path-from-shell dockerfile-mode direx-grep cmake-mode autopair)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

;; SML needs to be after custom-set-variables
;; https://github.com/Malabarba/smart-mode-line/issues/88
(use-package sml
  :ensure t
  :config
  (sml/setup)
  )

;; Kill the scratch buffer
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*")
  )

;; Start the server, only the graphic UI
;; see: https://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
(when (display-graphic-p)
  (server-start)
  )

