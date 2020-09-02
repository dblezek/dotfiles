;; -*-no-byte-compile: t; -*-

;; To recompile all the packages...
;; see: https://stackoverflow.com/a/24735377
;; M-: (byte-recompile-directory package-user-dir nil 'force)

;; To update all packages
;; M-x package-list-packages
;; Ux


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


;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in asterisks (*name*) in the current directory and changes the prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; updates packages https://emacs.stackexchange.com/a/31904
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Tree view of projects on F8
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (progn
    (setq
     treemacs-is-never-other-window         t
     )
    )
  :init
  (bind-key '[(f8)] 'treemacs)
  )

(use-package try
  :ensure t)

(use-package magit
  :ensure t
  :disabled
  :config
  (setq vc-handled-backends nil)
)

(use-package which-key
  :ensure t 
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-@") 'er/expand-region))

;; try neo tree
(use-package neotree
  :ensure t
  :disabled
  :config
  (global-set-key [f8] 'neotree-toggle)
  )

;; different theme
(use-package zenburn-theme
  :ensure t
  :disabled
  )

(use-package nimbus-theme
  :ensure t
  :disabled
  )

(use-package monokai-theme
  :ensure t
  :disabled
  )

;; fix broken titlebar brightess
(use-package ns-auto-titlebar
  :ensure t
  )

;; try helm http://tuhdo.github.io/helm-intro.html
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; keep the old one around
  (global-set-key (kbd "C-x f") 'find-file)
  ;; (global-set-key (kbd "C-s") 'helm-occur)
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
   ;; Don't try to be clever when opening files
   helm-find-files-ignore-thing-at-point t
   )
  (helm-mode 1)
  )

;; better helm search
;; NB: this doesn't seem to work with treemacs!?!
(use-package helm-swoop
  :ensure t
  :config
  
  ;; ;; this is perhaps the best search tool ever
  ;; ;; https://github.com/ShingoFukuyama/helm-swoop
  ;; (defun helm-swoop-multiline-4 ()
  ;;   (interactive)
  ;;   (helm-swoop :$query "" :$multiline 4))

  ;; ;; search across buffers
  ;; (global-set-key [(meta @)] 'helm-multi-swoop-all)
  
  ;; ;; Always use the previous search for helm. Remember C-<backspace> will delete entire line
  ;; (setq helm-swoop-pre-input-function
  ;;       (lambda () ""))
  
  ;; (setq helm-swoop-split-direction 'split-window-vertically)
  ;; ;; (global-set-key (kbd "C-s") 'helm-swoop)
  ;; ;; keep regular search around
  ;; (global-set-key (kbd "C-c C-s") 'isearch-forward)
  ;; (global-set-key [(control shift s)] 'helm-swoop-multiline-4)
  ;; (global-set-key (kbd "M-i") 'helm-swoop)
  ;; (global-set-key [(meta shift i)] 'helm-swoop-multiline-4)
  ;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  ;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  ;; ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  ;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; ;; Move up and down like isearch
  ;; (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  ;; (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  ;; (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  ;; (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  )

(use-package helm-descbinds :ensure t :config ( helm-descbinds-mode ) )

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

;; Save buffers and reload at startup
(use-package desktop
  :config
  ;; see https://stackoverflow.com/questions/4053708/emacs-desktop-doesnt-remember-tramp-connections?rq=1
  ;; for details
  (setq desktop-buffers-not-to-save "^$")
  (setq desktop-files-not-to-save "^$")
  )

(use-package yasnippet
  :disabled
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.dotfiles/snippits"))
  (yas-global-mode 1)
  )


(use-package tramp
  :ensure t
  :bind ("<f5>" . tramp-cleanup-all-connections)
  :config
  (tramp-change-syntax 'simplified)
  ;; Keep Tramp happy...
  (setq tramp-bkup-backup-directory-info nil)
  (setq delete-old-versions t
        kept-old-versions 1
        kept-new-versions 3
        version-control t)
  (setq tramp-verbose 5)
  )

(use-package projectile
  :disabled
  :config
  (projectile-mode +1)
  (global-set-key (kbd "M-p") 'projectile-command-map)
  )
  
  

;; languages
;; (use-package matlab-mode :ensure t)
(use-package edit-indirect :ensure t)
(use-package smart-mode-line :ensure t)
(use-package json-mode :ensure t)
(use-package rib-mode :ensure t)
(use-package go-mode :ensure t)
(use-package web-mode :ensure t)
(use-package sqlite :ensure t)
(use-package company-ansible :ensure t)
(use-package company-lua :ensure t)
(use-package company-go :ensure t)
(use-package company :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package groovy-mode :ensure t)
(use-package gradle-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package autopair :ensure t)

;; nicer status bar
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

;; keep old clipboard contents
(setq save-interprogram-paste-before-kill 't)

;; No startup message
(setq inhibit-startup-message t)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; collapse spaces
(global-set-key (kbd "s-SPC") 'just-one-space)

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

;; NB: these two go together, let option be the "super" key (not sure how this will work for CLI)
;; (setq mac-option-modifier 'meta)
;; (global-set-key (kbd "s-/") 'auto-complete)
;; Also in v23, moving was line by line visually, not by logical line
(setq line-move-visual nil)


;; Insert the current date
 (defun insert-current-date () (interactive) (insert (format-time-string "%Y-%m-%d")))
(global-set-key (kbd "C-S-d") 'insert-current-date)
(defun insert-current-timestamp () (interactive) (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "C-S-t") 'insert-current-timestamp)

;; since we like the screenshot tool in macOS, replace becomes (Meta-shift-7)
(global-set-key (kbd "M-&") 'query-replace-regexp)

(global-set-key [C-tab] 'other-window)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'save-buffer)

;; Move to the next window/frame
(global-set-key "\M-`" 'next-multiframe-window)

(setq kill-whole-line t)       ;;; Killing line also deletes \n

;; collapse all spaces between words
(global-set-key (kbd "M-?") 'just-one-space)
(global-set-key (kbd "S-SPC") 'just-one-space)


;; Set the frame title
(setq-default frame-title-format "%b (%f)")

;; always indent on return
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

;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

(set-face-attribute 'default nil :height 150)
(setq font-lock-use-colors t)
(setq font-lock-use-fonts nil)

;; ;; Font
;; (set-face-attribute 'default nil :font "Source Code Pro-15")
(set-face-attribute 'default nil :font "Input Mono-16")

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

;; Fuzzy finder
(use-package fiplr
  :ensure t
  :disabled
  :config
  (global-set-key (kbd "C-x f") 'fiplr-find-file)
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "build" "DEWEYLocal"))
                              (files ("*.jpg" "*.png" "*.zip" "*~" "*.a" "*.class" "*.dcm"))))

  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auto-compression-mode nil)
 '(c-basic-offset 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(smart-mode-line-dark monokai))
 '(custom-safe-themes
   '("f3ab34b145c3b2a0f3a570ddff8fabb92dafc7679ac19444c31058ac305275e1" "4b6deee4167dfdb24ead4b3f717fa4b8109dd1cf71cdc9b59e05cc0f6588ee33" "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "fe39cdf19d576f14f8a0abd8bcad9eb7aa07599d81e0be8dba99248802c6dc4d" "f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" "83ae405e25a0a81f2840bfe5daf481f74df0ddb687f317b5e005aa61261126e9" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(desktop-restore-eager 1)
 '(desktop-save-mode t)
 '(fci-rule-color "#383838")
 '(groovy-indent-offset 2)
 '(helm-mode t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(ido-ignore-directories
   '("\\`CVS/" "\\`\\.\\./" "\\`\\./" ".git" "node_modules" "bower_components"))
 '(js-indent-level 2)
 '(latex-run-command "latex --synctex=1")
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(magit-diff-use-overlays nil)
 '(markdown-fontify-code-blocks-natively t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-auto-titlebar-mode t nil (ns-auto-titlebar))
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("`" org-code verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t))))
 '(org-src-preserve-indentation nil)
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(auto-package-update centaur-tabs all-the-icons doom-themes ns-auto-titlebar ns-aut-titlebar sml-modeline matlab-mode sml spaceline telephone-line powerline minimap sublimity-map sublimity helm-swoop magit anaconda-mode elpy monokai-theme nimbus-theme treemacs highlight-indent-guides-mode helm zenburn-theme edit-indirect-region-latex expand-region elm-mode edit-indirect highlight-indent-guides smart-mode-line json-navigator json-mode org-bullets which-key try use-package rib-mode package-lint ## etags-select etags-table go-mode company-tern web-mode sqlite sql-indent company-shell company-ansible company-lua company-go company markdown-preview-mode cmake-font-lock yaml-mode toml-mode terraform-mode tabbar scss-mode scala-mode2 scala-mode popwin neotree markdown-mode lua-mode groovy-mode gradle-mode go-errcheck go-direx go-autocomplete glsl-mode ggtags fiplr exec-path-from-shell dockerfile-mode direx-grep cmake-mode autopair))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-syntax 'simplified nil (tramp))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

;; SML needs to be after custom-set-variables
;; https://github.com/Malabarba/smart-mode-line/issues/88
(use-package sml-modeline
  :ensure t
  :config
  (sml/setup)
  (sml-modeline-mode t)
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

