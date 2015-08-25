(require 'cc-mode)

;; CSharp mode comment says: These are only required at compile time to get the sources for the language
;; constants.  (The cc-fonts require and the font-lock related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is necessary to get them compiled.)
(eval-when-compile
    (require 'cc-mode)
    (require 'cc-fonts)
    (require 'cc-langs)
    (require 'cc-bytecomp)

    ; There seems to be a problem in Emacs 24.3 and 24.4 when batch-byte-compiling this file. It's to do
    ; with one of the symbols c-lang-defconst and c-identifier-ops. These stem from CC Mode. From
    ; http://debbugs.gnu.org/db/18/18845.html it seems there is a bug that should be fixed in 24.5.

    (if (and (= emacs-major-version 24) (< emacs-minor-version 5))
        (require 'cl))
)

(eval-and-compile
  (c-add-language 'nix-mode 'java-mode))

;;  Nix allows `?.' as well as `.' for creating identifiers.
(c-lang-defconst c-identifier-ops
                 nix '((left-assoc "." "?.")))

;; Nix allows operators such as `*.', `?.', `.&' and `.@'.  Java mode puts `*' here to deal with
;; import statement usage which we need for Nix.
(c-lang-defconst c-after-id-concat-ops
  nix '( "*" "&" "@" ))

;;;;  Should really do something with `c-string-escaped-newlines' and `c-multiline-string-start-char' to
;;;;  handle the triple delimeter multiline strings.

;; Because of the above we have to redefine `c_operators' because no other language has `.&' and
;; `.@' operators.

(c-lang-defconst c-operators
  "List describing all operators, along with their precedence and
associativity.  The order in the list corresponds to the precedence of
the operators: The operators in each element is a group with the same
precedence, and the group has higher precedence than the groups in all
following elements.  The car of each element describes the type of of
the operator group, and the cdr is a list of the operator tokens in
it.  The operator group types are:

'prefix         Unary prefix operators.
'postfix        Unary postfix operators.
'postfix-if-paren
		Unary postfix operators if and only if the chars have
		parenthesis syntax.
'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
'right-assoc-sequence
                Right associative operator that constitutes of a
                sequence of tokens that separate expressions.  All the
                tokens in the group are in this case taken as
                describing the sequence in one such operator, and the
                order between them is therefore significant.

Operators containing a character with paren syntax are taken to match
with a corresponding open/close paren somewhere else.  A postfix
operator with close paren syntax is taken to end a postfix expression
started somewhere earlier, rather than start a new one at point.  Vice
versa for prefix operators with open paren syntax.

Note that operators like \".\" and \"->\" which in language references
often are described as postfix operators are considered binary here,
since CC Mode treats every identifier as an expression."

  nix `(
           ;; Primary.
           ,@(c-lang-const c-identifier-ops)

             (postfix-if-paren "<" ">") ; Templates.

             (prefix "super")

             ;; Postfix.
             (left-assoc "." "*." "?." ".&" ".@")

             (postfix "++" "--" "[" "]" "(" ")" "<:" ":>")

             ;; Unary.
             (prefix "++" "--" "+" "-" "!" "~" "new" "(" ")")

             ;; Multiplicative.
             (left-assoc "*" "/" "%")

             ;; Additive.
             (left-assoc "+" "-")

             ;; Shift.
             (left-assoc "<<" ">>" ">>>")

             ;; Relational.
             (left-assoc "<" ">" "<=" ">=" "instanceof" "<=>")

             ;; Matching.
             (left-assoc "=~" "==~" )

             ;; Equality.
             (left-assoc "==" "!=" )

             ;; Bitwise and.
             (left-assoc "&")

             ;; Bitwise exclusive or.
             (left-assoc "^")

             ;; Bitwise or.
             (left-assoc "|")

             ;; Logical and.
             (left-assoc "&&")

             ;; Logical or.
             (left-assoc "||")

             ;; Conditional.
             (right-assoc-sequence "?" ":")

             ;; Assignment.
             (right-assoc ,@(c-lang-const c-assignment-operators))

             ;; Exception.
             ;(prefix "throw") ; Java mode didn't have this but c++ mode does.  Humm...

             ;; Sequence.
             (left-assoc ",")

             ;; Separator for parameter list and code in a closure.
             (left-assoc "->")
             ))

;;  Nix can overload operators where Java cannot.
(c-lang-defconst c-overloadable-operators
                 nix '("+" "-" "*" "/" "%"
                          "&" "|" "^" "~" "<<" ">>" ">>>"
                          "==" "!=" ">" "<" ">=" "<="
                          "<=>"
                          "=~" "==~"
                          "++" "--" "+=" "-=" "*=" "/=" "%="
                          "&=" "|=" "^=" "~=" "<<=" ">>=" ">>>="
                          "!" "&&" "||"))

;; Nix allows newline to terminate a statement unlike Java and like Awk.  We draw on the Awk
;; Mode `Virtual semicolon material.  The idea is to say when an EOL is a `virtual semicolon,
;; i.e. a statement terminator.

(when (version< c-version "5.32.2")
  (c-lang-defconst c-stmt-delim-chars
                   nix "^;{}\n\r?:")

  (c-lang-defconst c-stmt-delim-chars-with-comma
                   nix "^;,{}\n\r?:"))

;;  Is there a virtual semicolon at POS or point?
;;
;;  A virtual semicolon is considered to lie just after the last non-syntactic-whitespace
;; character on a line where the EOL is the statement terminator.  A real semicolon never
;; counts as a virtual one.
(defun nix-at-vsemi-p ( &optional pos )
  (save-excursion
	(let ((pos-or-point (if pos (goto-char pos) (point))))
	  (if (eq pos-or-point (point-min))
		  nil
		(and
		 (not (char-equal (char-before) ?\;))
		 (nix-ws-or-comment-to-eol-p pos-or-point)
		 (nix-not-in-statement-p pos-or-point)
		 (nix-not-if-or-else-etc-p pos-or-point))))))

(c-lang-defconst c-at-vsemi-p-fn
                 nix 'nix-at-vsemi-p)

;; see if end of line or comment on rest of line
(defun nix-ws-or-comment-to-eol-p ( pos )
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
       (or
        (char-equal (char-after) ?\n)
        (looking-at "/[/*].*"))))

(defun nix-not-in-statement-p ( pos )
  (save-excursion
    (goto-char pos)
    (if (equal (point) (point-min))
        nil
      (backward-char 1)
      (or
       (not (looking-at "[=+*%<{:]"))
       (if (char-equal (char-after) ?>)
           (if (equal (point) (point-min))
               nil
             (char-equal (char-before) ?-)))))))

;; check for case of if(stuff) and nothing else on line
;; ie
;; if(x > y)
;;
;; if(x < y) do something will not match
;; else blah blah will not match either
(defun nix-not-if-or-else-etc-p ( pos )
  (save-excursion
    (goto-char pos)
	(back-to-indentation)
	(not
	 (or
	  (and (looking-at "if") ; make sure nothing else on line
		   (progn (forward-sexp 2)
				  (nix-ws-or-comment-to-eol-p (point))))
	  (and (looking-at "}?else")
		   (progn (forward-char)
				  (forward-sexp 1)
				  (nix-ws-or-comment-to-eol-p (point))))))))

(defun nix-vsemi-status-unknown-p () nil)

(c-lang-defconst c-vsemi-status-unknown-p-fn
                 nix 'c-nix-vsemi-status-unknown-p)


;;  Java does not do this but perhaps it should?
(c-lang-defconst c-type-modifier-kwds
                 nix '("volatile" "transient"))

(c-lang-defconst c-typeless-decl-kwds
                 nix (append (c-lang-const c-class-decl-kwds)
                                (c-lang-const c-brace-list-decl-kwds)
                                '("def")))

;;;;  Should we be tinkering with `c-block-stmt-1-key' or `c-block-stmt-2-key' to deal with closures
;;;;  following what appears to be function calls or even field names?

;; Nix allows use of `<%' and `%>' in template expressions.
;(c-lang-defconst c-other-op-syntax-tokens
;  nix '( "<%" "%>" ))

;; Nix does not allow the full set of Java keywords in the modifier category and, of course, there is the
;; `def' modifier which Nix introduces to support dynamic typing.  Should `const' be treated
;; as reserved here as it is in Java?
(c-lang-defconst c-modifier-kwds
                 nix '( "abstract" "def" "final" "private" "protected" "public" "static" "synchronized" ))

;;  Java does not define these pseudo-kewords as keywords, why not?

(c-lang-defconst c-constant-kwds
  nix '( "true" "false" "null" ))

;;  Why does Java mode not put `super' into the `c-primary-expr-kwds?

(c-lang-defconst c-primary-expr-kwds
  nix '( "this" "super" ))

;;  Nix does not allow anonymous classes as Java does.
(c-lang-defconst c-inexpr-class-kwds
                 nix nil)

(c-lang-defconst c-inexpr-brace-list-kwds
                 nix nil)

;;;;  Should we be changing `c-opt-inexpr-brace-list-key' to deal with closures after function calls and
;;;;  field expressions?

;; We need to include the "as" for the cast and "in" for for.
(c-lang-defconst c-other-kwds
                 nix '( "in" "as" ))


(defconst nix-font-lock-keywords-1 (c-lang-const c-matchers-1 nix)
  "Minimal highlighting for Nix mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst nix-font-lock-keywords-2 (c-lang-const c-matchers-2 nix)
  "Fast normal highlighting for Nix mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `java-font-lock-extra-types', and the doc
comment styles specified by `c-doc-comment-style'.")

(defconst nix-font-lock-keywords-3 (c-lang-const c-matchers-3 nix)
  "Accurate normal highlighting for Nix mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar nix-font-lock-keywords nix-font-lock-keywords-3
  "Default expressions to highlight in Nix mode.")

(defun nix-font-lock-keywords-2 ()
  (c-compose-keywords-list nix-font-lock-keywords-2))
(defun nix-font-lock-keywords-3 ()
  (c-compose-keywords-list nix-font-lock-keywords-3))
(defun nix-font-lock-keywords ()
  (c-compose-keywords-list nix-font-lock-keywords))

(defvar nix-mode-syntax-table nil
  "Syntax table used in Nix mode buffers.")
(or nix-mode-syntax-table
    (setq nix-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table nix))))

;(modify-syntax-entry ?: "_" nix-mode-syntax-table)

(defvar nix-mode-abbrev-table nil
  "Abbreviation table used in nix-mode buffers.")
(c-define-abbrev-table 'nix-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the syntactic context, and which
  ;; therefore should trigger reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

;;  Jim Morris proposed changing to the following definition of nix-mode-map 2009-11-27, but this change
;;  has not made so as to continue to use the same code structure as still used in the Java mode.

;(defvar nix-mode-map (let ((map (c-make-inherited-keymap)))
;                                                  ;; Add bindings which are only useful for Nix
;                                                  map)
;  "Keymap used in nix-mode buffers.")

(defvar nix-mode-map ()
  "Keymap used in nix-mode buffers.")
(if nix-mode-map
    nil
  (setq nix-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Nix
  )

;(easy-menu-define c-nix-menu nix-mode-map "Nix Mode Commands"
;                (cons "Nix" (c-lang-const c-mode-menu nix)))

;;----------------------------------------------------------------------------
;;;###autoload (add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . nix-mode))

;; Custom variables
;;;###autoload
(defcustom nix-mode-hook nil
  "*Hook called by `nix-mode'."
  :type 'hook
  :group 'c)

;;; The following are used to overide cc-mode indentation behavior to match nix

;; if we are in a closure that has an argument eg ends with -> (excluding comment) then
;; change indent else lineup with previous one
(defun nix-mode-fix-closure-with-argument (langelem)
  (save-excursion
	(back-to-indentation)
	(c-backward-syntactic-ws)
	(backward-char 2)
	(if (looking-at "->")                                  ; if the line has a -> in it
		(vector (+ (current-indentation) c-basic-offset))  ; then indent from base
	  0)))

;; A helper function from: http://mihai.bazon.net/projects/emacs-javascript-mode/javascript.el
;; Originally named js-lineup-arglist, renamed to nix-lineup-arglist
(defun nix-lineup-arglist (langelem)
  ;; the "DWIM" in c-mode doesn't Do What I Mean.
  ;; see doc of c-lineup-arglist for why I redefined this
  (save-excursion
    (let ((indent-pos (point)))
      ;; Normal case.  Indent to the token after the arglist open paren.
      (goto-char (c-langelem-2nd-pos c-syntactic-element))
      (if (and c-special-brace-lists
               (c-looking-at-special-brace-list))
          ;; Skip a special brace list opener like "({".
          (progn (c-forward-token-2)
                 (forward-char))
        (forward-char))
      (let ((arglist-content-start (point)))
        (c-forward-syntactic-ws)
        (when (< (point) indent-pos)
          (goto-char arglist-content-start)
          (skip-chars-forward " \t"))
        (vector (current-column))))))

(defun is-nix-mode ()
  "return t if we are in nix mode else nil"
  (eq major-mode 'nix-mode))

(defun nix-is-label (the-list)
  (let ((ret nil))
    (dolist (elt the-list)
      (if (eq 'label (car elt))
	  (setq ret t)))
    ret))

(defun nix-backtrack-open-paren ()
  (let ((counter 0))
    (while (<= 0 counter)
      (skip-chars-backward "^}]){[(")
      (cond ((or (equal ?\] (preceding-char))
		 (equal ?\) (preceding-char))
		 (equal ?} (preceding-char)))
	     (setq counter (1+ counter)))
	    ((or (equal ?\[ (preceding-char))
		 (equal ?\( (preceding-char))
		 (equal ?{ (preceding-char)))
	     (setq counter (1- counter))))
      (backward-char 1))))

(defun nix-named-parameter-list-anchor-points ()
  (save-excursion
    (beginning-of-line)
    (c-backward-syntactic-ws)
    (if (equal ?, (preceding-char))
	(let* ((second-anchor (progn (nix-backtrack-open-paren)
				     (point)))
	       (first-anchor (progn (beginning-of-line)
				    (c-forward-syntactic-ws)
				    (point))))
	  (cons first-anchor second-anchor))
      nil)))

;; use defadvice to override the syntactic type if we have a
;; statement-cont, see if previous line has a virtual semicolon and if
;; so make it statement.
(defadvice c-guess-basic-syntax (after c-guess-basic-syntax-nix activate)
  (catch 'exit-early
    (when (is-nix-mode)
      (if (nix-is-label ad-return-value)
	  (progn
	    (let ((anchor-points (nix-named-parameter-list-anchor-points)))
	      (if anchor-points
		  (setq ad-return-value `((arglist-cont-nonempty ,(car anchor-points) ,(cdr anchor-points))))
      		(throw 'exit-early 1)))))

      (save-excursion
	(let* ((ankpos (progn
			 (beginning-of-line)
			 (c-backward-syntactic-ws)
			 (beginning-of-line)
			 (c-forward-syntactic-ws)
			 (point))) ; position to previous non-blank line
	       (curelem (c-langelem-sym (car ad-return-value))))
	  (end-of-line)
	  (cond
	   ((eq 'statement-cont curelem)
	    (when (nix-at-vsemi-p) ; if there is a virtual semi there then make it a statement
	      (setq ad-return-value `((statement ,ankpos)))))

	   ((eq 'topmost-intro-cont curelem)
	    (when (nix-at-vsemi-p) ; if there is a virtual semi there then make it a top-most-intro
	      (setq ad-return-value `((topmost-intro ,ankpos)))))

	   ))))))

;; This disables bracelists, as most of the time in nix they are closures
;; We need to check we are currently in nix mode
(defadvice c-inside-bracelist-p (around nix-c-inside-bracelist-p activate)
  (if (not (is-nix-mode))
	  ad-do-it
 	(setq ad-return-value nil)))


;; based on java-function-regexp
;; Complicated regexp to match method declarations in interfaces or classes
;; A nasty test case is:
;;    else if(foo instanceof bar) {
;; which will get mistaken for a function as Nix does not require types on arguments
;; so we need to check for empty parens or comma separated list, or type args
(defvar nix-function-regexp
  (concat
   "^[ \t]*"                                   ; leading white space
   "\\(public\\|private\\|protected\\|"        ; some of these 8 keywords
   "abstract\\|final\\|static\\|"
   "synchronized\\|native|def"
   "\\|[ \t\n\r]\\)*"                          ; or whitespace
   "[a-zA-Z0-9_$]*"                            ; optional return type
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\([a-zA-Z0-9_$]+\\)"                      ; the name we want
   "[ \t\n\r]*"                                ; optional whitespace
   "("                                         ; open the param list
   "[ \t]*"                                    ; optional whitespace
   "\\("
   "[ \t\n\r]*\\|"                             ; empty parens or
   "[a-zA-Z0-9_$]+\\|"                         ; single param or
   ".+?,.+?\\|"                                ; multi comma separated params or
   "[a-zA-Z0-9_$]+"                            ; a type
   "[ \t\n\r]*[[]?[]]?"                        ; optional array
   "[ \t\n\r]+[a-zA-Z0-9_$]+"                  ; and param
   "\\)"
   "[ \t\n\r]*"                                ; optional whitespace
   ")"                                         ; end the param list
   "[ \t\n\r]*"                                ; whitespace
;   "\\(throws\\([, \t\n\r]\\|[a-zA-Z0-9_$]\\)+\\)?{"
   "\\(throws[^{;]+\\)?"                       ; optional exceptions
   "[;{]"                                      ; ending ';' (interfaces) or '{'
										       ; TODO nix interfaces don't need to end in ;
   )
  "Matches method names in nix code, select match 2")

(defvar nix-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in nix code, select match 2")

(defvar nix-interface-regexp
  "^[ \t\n\r]*\\(abstract\\|public\\|[ \t\n\r]\\)*interface[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;]*;"
  "Matches interface names in nix code, select match 2")

(defvar nix-imenu-regexp
  (list (list nil nix-function-regexp 2)
        (list ".CLASSES." nix-class-regexp 2)
        (list ".INTERFACES." nix-interface-regexp 2)
		(list ".CLOSURES." 	"def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*{" 1))
  "Imenu expression for Nix")


;; Setup imenu to extract functions, classes, interfaces and closures assigned to variables
(defvar cc-imenu-nix-generic-expression
  nix-imenu-regexp
  "Imenu generic expression for Nix mode.  See `imenu-generic-expression'.")

;; For compatibility with Emacs < 24
(defalias 'nix-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode nix-mode nix-parent-mode "Nix"
  "Major mode for editing Nix code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `nix-mode-hook'.

Key bindings:
\\{nix-mode-map}"
  (c-initialize-cc-mode t)
  (setq local-abbrev-table nix-mode-abbrev-table
        abbrev-mode t)
  (use-local-map nix-mode-map)
  (c-init-language-vars nix-mode)
  (c-common-init 'nix-mode)
  ;;(easy-menu-add nix-menu)
  (cc-imenu-init cc-imenu-nix-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'nix-mode-hook)

  ;; quick fix for misalignment of statements with =
  (setq c-label-minimum-indentation 0)

  ;; fix for indentation after a closure param list
  (c-set-offset 'statement 'nix-mode-fix-closure-with-argument)

  ;; get arglists (in nix lists or maps) to align properly
  (c-set-offset 'arglist-close '(c-lineup-close-paren))
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-cont-nonempty '(nix-lineup-arglist))
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'label '+)

  (c-update-modeline))

;;----------------------------------------------------------------------------

(provide 'nix-mode)

;;; nix-mode.el ends here
