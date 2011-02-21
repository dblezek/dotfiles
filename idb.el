;; The file contains the lisp variables and functions that enable 
;; the user to use idb in the Emacs GUD framework.
;;
(require 'gud)

(defvar gud-idb-history nil)

(defun gud-idb-massage-args (file args)
  (cons "-emacs" args))

(defvar easy-mmode-support (fboundp 'easy-mmode-define-keymap))

(if easy-mmode-support
    (progn
      (easy-mmode-defmap gud-idb-menu-map
			 '(([refresh]	"Refresh" . gud-refresh)
			   ([run]	"Run/Rerun" . gud-run)
			   ([down]	"Down Stack" . gud-down)
			   ([up]	"Up Stack" . gud-up)
			   ([finish]	"Finish Function" . gud-finish)
			   ([stepi]	"Step Instruction" . gud-stepi)
			   ([nexti]	"Next Instruction" . gud-nexti)
			   ([step]	"Step Line" . gud-step)
			   ([next]	"Next Line" . gud-next)
			   ([print]	"Print Expression" . gud-print)
			   ([break]	"Set Breakpoint" . gud-break)
			   ([where]	"Print Stack" . gud-where)
			   ([cont]	"Continue" . gud-cont))
			 "Menu for `gud-idb-mode'."
			 :name "Gud/Idb")
      
;;      (easy-mmode-defmap gud-idb-minor-mode-map
	;;		 `(([menu-bar debug] . ("Gud" . ,gud-idb-menu-map)))
		;;	 "Map used in visited files.")
      
       ;;(let ((m (assq 'gud-minor-mode minor-mode-map-alist)))
	;;(if m (setcdr m gud-idb-minor-mode-map)
	 ;;(push (cons 'gud-minor-mode gud-minor-mode-map) minor-mode-map-alist)))
))
      
(defun idb (command-line)
  "Run idb on program <FILE> in buffer *gud-<FILE>*. The directory 
containing <FILE> becomes the current working directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run idb (like this): "
			       (if (consp gud-idb-history)
				   (car gud-idb-history)
				   "idb ")
			       nil nil
			       '(gud-idb-history . 1))))

  (gud-common-init command-line
		   'gud-idb-massage-args)
;;  (set (make-local-variable 'gud-minor-mode) 'gdb)

  (gud-def gud-break  "stop at \"%f\":%l" "\C-b" "Set breakpoint at current line of current buffer.")
  (gud-def gud-step   "step"       "\C-s" "Step one line (into functions).")
  (gud-def gud-stepi  "stepi"      "\C-i" "Step one instruction.")
  (gud-def gud-nexti  "nexti"      "\C-j" "Next one instruction.")
  (gud-def gud-next   "next"       "\C-n" "Step one line (over functions).")
  (gud-def gud-cont   "cont"	   "\C-r" "Continue.")
  (gud-def gud-finish "return"	   "\C-f" "Return from current function.")
  (gud-def gud-up     "up %p"	   "<"    "Up (numeric arg) stack frames.")
  (gud-def gud-down   "down %p"	   ">"    "Down (numeric arg) stack frames.")
  (gud-def gud-print  "print %e"   "\C-p" "Evaluate expression at point.")
  (gud-def gud-run    "run"        "\C-u" "Run/Rerun the debuggee.")
  (gud-def gud-where  "where"      "\C-w" "Print the stack.")

  (if (not easy-mmode-support)
      (progn 
	(local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
	(local-set-key [menu-bar debug up]     '("Up Stack"    . gud-up))
	(local-set-key [menu-bar debug down]   '("Down Stack"  . gud-down))))

  (setq comint-prompt-regexp  "^[^)]*idb) *")
  (setq paragraph-start comint-prompt-regexp)

  (run-hooks 'idb-mode-hook)
)

(provide 'idb)
