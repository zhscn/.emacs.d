;;; init-keybinds.el -*- lexical-binding: t; -*-

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
	hook-list
      (cl-loop for hook in hook-list
	       if (eq (car-safe hook) 'quote)
	       collect (cadr hook)
	       else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
			     vars)
			 (while args
			   (push (if singles
				     (list (pop args))
				   (cons (pop args) (pop args)))
				 vars))
			 (nreverse vars))
	   for hook in (doom--resolve-hook-forms hooks)
	   for mode = (string-remove-suffix "-hook" (symbol-name hook))
	   append
	   (cl-loop for (var . val) in vars
		    collect
		    (list var val hook
			  (intern (format "doom--setq-%s-for-%s-h"
					  var mode))))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
	hook-list
      (cl-loop for hook in hook-list
	       if (eq (car-safe hook) 'quote)
	       collect (cadr hook)
	       else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
		     (goto-char indent-point)
		     (when (looking-at-p "\\s-*(")
		       (lisp-indent-defform state indent-point))))
	   (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
	 (func-forms ())
	 (defn-forms ())
	 append-p
	 local-p
	 remove-p
	 forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
	(:append (setq append-p t))
	(:local  (setq local-p t))
	(:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
	     (setq func-forms rest))

	    ((eq first 'defun)
	     (setq func-forms (mapcar #'cadr rest)
		   defn-forms rest))

	    ((memq first '(quote function))
	     (setq func-forms
		   (if (cdr rest)
		       (mapcar #'doom-unquote rest)
		     (doom-enlist (doom-unquote (car rest))))))

	    ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
	(dolist (func func-forms)
	  (push (if remove-p
		    `(remove-hook ',hook #',func ,local-p)
		  `(add-hook ',hook #',func ,append-p ,local-p))
		forms)))
      (macroexp-progn
       (append defn-forms
	       (if append-p
		   (nreverse forms)
		 forms))))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
	(list (if (or (not (bound-and-true-p byte-compile-current-file))
		      (require package nil 'noerror))
		  #'progn
		#'with-no-warnings)
	      (let ((body (macroexp-progn body)))
		`(if (featurep ',package)
		     ,body
		   ;; We intentionally avoid `with-eval-after-load' to prevent
		   ;; eager macro expansion from pulling (or failing to pull) in
		   ;; autoloaded macros/packages.
		   (eval-after-load ',package ',body)))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
	     `(after! (:and ,@package) ,@body))
	    ((memq p '(:or :any))
	     (macroexp-progn
	      (cl-loop for next in (cdr package)
		       collect `(after! ,next ,@body))))
	    ((memq p '(:and :all))
	     (dolist (next (cdr package))
	       (setq body `((after! ,next ,@body))))
	     (car body))))))

(defvar doom-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar doom-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar doom-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar doom-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")

(defvar doom-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape ()
  "Run `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 ;; quit the minibuffer if open.
	 (abort-recursive-edit))
	;; Run all escape hooks. If any returns non-nil, then stop there.
	((run-hook-with-args-until-success 'doom-escape-hook))
	;; don't abort macros
	((or defining-kbd-macro executing-kbd-macro) nil)
	;; Back to the default
	((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'doom/escape)


;;
;;; General + leader/localleader keys

(use-package general
  :straight t
  :init
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'unmap! #'general-unbind))

;; HACK `map!' uses this instead of `define-leader-key!' because it consumes
;; 20-30% more startup time, so we reimplement it ourselves.
(defmacro doom--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
	    (def (pop keys)))
	(if (keywordp key)
	    (when (memq key '(:prefix :infix))
	      (setq prefix def))
	  (when prefix
	    (setq key `(general--concat t ,prefix ,key)))
	  (let* ((udef (cdr-safe (doom-unquote def)))
		 (bdef (if (general--extended-def-p udef)
			   (general--extract-def (general--normalize-extended-def udef))
			 def)))
	    (unless (eq bdef :ignore)
	      (push `(define-key doom-leader-map (general--kbd ,key)
		       ,bdef)
		    forms))
	    (when-let (desc (cadr (memq :which-key udef)))
	      (prependq!
	       wkforms `((which-key-add-key-based-replacements
			   (general--concat t doom-leader-alt-key ,key)
			   ,desc)
			 (which-key-add-key-based-replacements
			   (general--concat t doom-leader-key ,key)
			   ,desc))))))))
    (macroexp-progn
     (cons `(after! which-key ,@(nreverse wkforms))
	   (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `doom-leader-key' and `doom-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'doom-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `doom-localleader-key' and `doom-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
	:states '(normal visual motion emacs insert)
	:major-modes t
	:prefix doom-localleader-key
	:non-normal-prefix doom-localleader-alt-key
	,@args)
    `(general-define-key
      :major-modes t
      :prefix doom-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Doom's startup time!
(define-prefix-command 'doom/leader 'doom-leader-map)
(define-key doom-leader-map [override-state] 'all)

;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'doom-after-init-modules-hook
  (defun doom-init-leader-keys-h ()
    "Bind `doom-leader-key' and `doom-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
	  (progn
	    (cond ((equal doom-leader-alt-key "C-c")
		   (set-keymap-parent doom-leader-map mode-specific-map))
		  ((equal doom-leader-alt-key "C-x")
		   (set-keymap-parent doom-leader-map ctl-x-map)))
	    (define-key map (kbd doom-leader-alt-key) 'doom/leader))
	(evil-define-key* '(normal visual motion) map (kbd doom-leader-key) 'doom/leader)
	(evil-define-key* '(emacs insert) map (kbd doom-leader-alt-key) 'doom/leader))
      (general-override-mode +1))))
(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
	    collect `(defun ,fn (&rest _)
		       ,(format "%s = %s" var (pp-to-string val))
		       (setq-local ,var ,val))
	    collect `(remove-hook ',hook #',fn) ; ensure set order
	    collect `(add-hook ',hook #',fn))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
	    in (doom--setq-hook-fns hooks vars 'singles)
	    collect `(remove-hook ',hook #',fn))))



;;
;;; Packages

(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements doom-leader-key "<leader>")
  (which-key-add-key-based-replacements doom-localleader-key "<localleader>")

  (which-key-mode +1))


;;
;;; `map!' macro

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun doom--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
	   if (cdr (assq l doom-evil-state-alist)) collect it
	   else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :prefix-map   'lisp-indent-function 'defun)

;; specials
(defvar doom--map-forms nil)
(defvar doom--map-fn nil)
(defvar doom--map-batch-forms nil)
(defvar doom--map-state '(:dummy t))
(defvar doom--map-parent-state nil)
(defvar doom--map-evil-p nil)
(after! evil (setq doom--map-evil-p t))

(defun doom--map-process (rest)
  (let ((doom--map-fn doom--map-fn)
	doom--map-state
	doom--map-forms
	desc)
    (while rest
      (let ((key (pop rest)))
	(cond ((listp key)
	       (doom--map-nested nil key))

	      ((keywordp key)
	       (pcase key
		 (:leader
		  (doom--map-commit)
		  (setq doom--map-fn 'doom--define-leader-key))
		 (:localleader
		  (doom--map-commit)
		  (setq doom--map-fn 'define-localleader-key!))
		 (:after
		  (doom--map-nested (list 'after! (pop rest)) rest)
		  (setq rest nil))
		 (:desc
		  (setq desc (pop rest)))
		 (:map
		  (doom--map-set :keymaps `(quote ,(doom-enlist (pop rest)))))
		 (:mode
		  (push (cl-loop for m in (doom-enlist (pop rest))
				 collect (intern (concat (symbol-name m) "-map")))
			rest)
		  (push :map rest))
		 ((or :when :unless)
		  (doom--map-nested (list (intern (doom-keyword-name key)) (pop rest)) rest)
		  (setq rest nil))
		 (:prefix-map
		  (cl-destructuring-bind (prefix . desc)
		      (doom-enlist (pop rest))
		    (let ((keymap (intern (format "doom-leader-%s-map" desc))))
		      (setq rest
			    (append (list :desc desc prefix keymap
					  :prefix prefix)
				    rest))
		      (push `(defvar ,keymap (make-sparse-keymap))
			    doom--map-forms))))
		 (:prefix
		  (cl-destructuring-bind (prefix . desc)
		      (doom-enlist (pop rest))
		    (doom--map-set (if doom--map-fn :infix :prefix)
				   prefix)
		    (when (stringp desc)
		      (setq rest (append (list :desc desc "" nil) rest)))))
		 (:textobj
		  (let* ((key (pop rest))
			 (inner (pop rest))
			 (outer (pop rest)))
		    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
				 (:map evil-outer-text-objects-map ,key ,outer))
			  doom--map-forms)))
		 (_
		  (condition-case _
		      (doom--map-def (pop rest) (pop rest)
				     (doom--map-keyword-to-states key)
				     desc)
		    (error
		     (error "Not a valid `map!' property: %s" key)))
		  (setq desc nil))))

	      ((doom--map-def key (pop rest) nil desc)
	       (setq desc nil)))))

    (doom--map-commit)
    (macroexp-progn (nreverse (delq nil doom--map-forms)))))

(defun doom--map-append-keys (prop)
  (let ((a (plist-get doom--map-parent-state prop))
	(b (plist-get doom--map-state prop)))
    (if (and a b)
	`(general--concat nil ,a ,b)
      (or a b))))

(defun doom--map-nested (wrapper rest)
  (doom--map-commit)
  (let ((doom--map-parent-state (doom--map-state)))
    (push (if wrapper
	      (append wrapper (list (doom--map-process rest)))
	    (doom--map-process rest))
	  doom--map-forms)))

(defun doom--map-set (prop &optional value)
  (unless (equal (plist-get doom--map-state prop) value)
    (doom--map-commit))
  (setq doom--map-state (plist-put doom--map-state prop value)))

(defun doom--map-def (key def &optional states desc)
  (when (or (memq 'global states)
	    (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
		  (keywordp (car-safe (setq unquoted (doom-unquote def)))))
	     (setq def (list 'quote (plist-put unquoted :which-key desc))))
	    ((setq def (cons 'list
			     (if (and (equal key "")
				      (null def))
				 `(:ignore t :which-key ,desc)
			       (plist-put (general--normalize-extended-def def)
					  :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
	  (alist-get state doom--map-batch-forms)))
  t)

(defun doom--map-commit ()
  (when doom--map-batch-forms
    (cl-loop with attrs = (doom--map-state)
	     for (state . defs) in doom--map-batch-forms
	     if (or doom--map-evil-p (not state))
	     collect `(,(or doom--map-fn 'general-define-key)
		       ,@(if state `(:states ',state)) ,@attrs
		       ,@(mapcan #'identity (nreverse defs)))
	     into forms
	     finally do (push (macroexp-progn forms) doom--map-forms))
    (setq doom--map-batch-forms nil)))

(defun doom--map-state ()
  (let ((plist
	 (append (list :prefix (doom--map-append-keys :prefix)
		       :infix  (doom--map-append-keys :infix)
		       :keymaps
		       (append (plist-get doom--map-parent-state :keymaps)
			       (plist-get doom--map-state :keymaps)))
		 doom--map-state
		 nil))
	newplist)
    (while plist
      (let ((key (pop plist))
	    (val (pop plist)))
	(when (and val (not (plist-member newplist key)))
	  (push val newplist)
	  (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

Properties
  :leader [...]                   an alias for (:prefix doom-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
				  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
				  where the following keys will be bound. DO NOT
				  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds."
  (doom--map-process rest))

(provide 'init-keybinds)
;;; init-keybinds.el ends here
