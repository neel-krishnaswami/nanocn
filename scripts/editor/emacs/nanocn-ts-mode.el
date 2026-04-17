;;; nanocn-ts-mode.el --- Tree-sitter major mode for nanoCN -*- lexical-binding: t; -*-

;; Author: nanoCN contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/neel-krishnaswami/nanocn

;;; Commentary:

;; A tree-sitter-backed major mode for nanoCN surface (.cn) and
;; refined (.rcn) programs.  Provides syntax highlighting,
;; indentation, Imenu, defun navigation, and Eglot (LSP) registration.
;;
;; Prerequisites:
;;   - Emacs 29.1+ compiled with tree-sitter support
;;   - The nanocn tree-sitter grammar installed (see below)
;;
;; Install the grammar:
;;   M-x treesit-install-language-grammar RET nanocn RET
;;
;; Or manually:
;;   cd scripts/editor/tree-sitter-nanocn
;;   tree-sitter generate && tree-sitter build
;;   cp nanocn.so ~/.emacs.d/tree-sitter/libtree-sitter-nanocn.so
;;
;; Use:
;;   (require 'nanocn-ts-mode)   ; or autoload via package system
;;   ;; .cn and .rcn files open in nanocn-ts-mode automatically.

;;; Code:

(require 'treesit)

;; ---------------------------------------------------------------------------
;; Grammar installation recipe
;; ---------------------------------------------------------------------------

;;;###autoload
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(nanocn "https://github.com/neel-krishnaswami/nanocn"
                        nil                                       ; REVISION (default branch)
                        "scripts/editor/tree-sitter-nanocn/src"   ; SOURCE-DIR (relative to repo root)
                        )))

;; ---------------------------------------------------------------------------
;; Font-lock
;; ---------------------------------------------------------------------------

(defvar nanocn-ts-font-lock-rules
  '(;; Level 1: comment, definition
    :language nanocn
    :feature comment
    ((comment) @font-lock-comment-face)

    :language nanocn
    :feature definition
    ((fun_decl name: (lower_ident) @font-lock-function-name-face))
    :language nanocn
    :feature definition
    ((rfun_decl name: (lower_ident) @font-lock-function-name-face))
    :language nanocn
    :feature definition
    ((sort_decl name: (upper_label) @font-lock-type-face))
    :language nanocn
    :feature definition
    ((type_decl name: (upper_label) @font-lock-type-face))

    ;; Level 2: keyword, type
    :language nanocn
    :feature keyword
    (["fun" "rfun" "sort" "type" "main"
      "let" "case" "of" "iter"
      "if" "then" "else"
      "take" "do" "return"
      "unfold" "core" "log" "res" "not"
      "open-ret" "open-take" "make-ret" "make-take"]
     @font-lock-keyword-face)

    :language nanocn
    :feature keyword
    ((eff_level) @font-lock-keyword-face)
    :language nanocn
    :feature keyword
    ((fail_expr) @font-lock-keyword-face)
    :language nanocn
    :feature keyword
    ((crt_exfalso) @font-lock-keyword-face)
    :language nanocn
    :feature keyword
    ((lpf_auto) @font-lock-keyword-face)

    :language nanocn
    :feature type
    ((int_sort) @font-lock-type-face)
    :language nanocn
    :feature type
    ((bool_sort) @font-lock-type-face)
    :language nanocn
    :feature type
    (["Pred" "Ptr"] @font-lock-type-face)
    :language nanocn
    :feature type
    ((app_sort name: (upper_label) @font-lock-type-face))
    :language nanocn
    :feature type
    ((name_sort (upper_label) @font-lock-type-face))

    ;; Level 3: constant, number, function, builtin
    :language nanocn
    :feature number
    ((int_lit) @font-lock-number-face)

    :language nanocn
    :feature constant
    ((bool_lit) @font-lock-constant-face)

    :language nanocn
    :feature function
    ((fun_call func: (lower_ident) @font-lock-function-call-face))
    :language nanocn
    :feature function
    ((crt_call func: (lower_ident) @font-lock-function-call-face))
    :language nanocn
    :feature function
    ((lpf_unfold func: (lower_ident) @font-lock-function-call-face))

    :language nanocn
    :feature builtin
    ((state_prim) @font-lock-builtin-face)

    :language nanocn
    :feature constructor
    ((ctor_decl label: (upper_label) @font-lock-variable-use-face))
    :language nanocn
    :feature constructor
    ((ctor_pat label: (upper_label) @font-lock-variable-use-face))
    :language nanocn
    :feature constructor
    ((inject_expr label: (upper_label) @font-lock-variable-use-face))
    :language nanocn
    :feature constructor
    ((crt_case_branch label: (upper_label) @font-lock-variable-use-face))

    ;; Level 4: operator, delimiter, bracket
    :language nanocn
    :feature operator
    ([(or_expr op: _ @font-lock-operator-face)
      (and_expr op: _ @font-lock-operator-face)
      (eq_expr op: _ @font-lock-operator-face)
      (cmp_expr op: _ @font-lock-operator-face)
      (add_expr op: _ @font-lock-operator-face)
      (mul_expr op: _ @font-lock-operator-face)])

    :language nanocn
    :feature operator
    (["@" "->" ":"] @font-lock-operator-face)

    :language nanocn
    :feature delimiter
    (["," ";" "|" "=" "*"] @font-lock-delimiter-face)

    :language nanocn
    :feature bracket
    (["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face))
  "Font-lock rules for `nanocn-ts-mode'.")

;; ---------------------------------------------------------------------------
;; Indentation
;; ---------------------------------------------------------------------------

(defvar nanocn-ts-indent-offset 2
  "Number of spaces for each indentation level in `nanocn-ts-mode'.")

;; The tree-sitter node at BOL is typically an anonymous keyword token
;; (e.g. `let`), not the named `let_expr`.  Anonymous tokens don't
;; carry field names, so the built-in `match` matcher can't detect "am
;; I inside the body field?"  These helpers walk the ancestor chain to
;; find the enclosing sequencing form and align the body with its
;; keyword — giving flat ML-style layout for let/take chains.

(defconst nanocn-ts--sequencing-types
  '("let_expr" "take_expr"
    "crt_let" "crt_let_log" "crt_let_log_annot"
    "crt_let_res" "crt_let_res_annot" "crt_let_core")
  "Node types whose `body' field should stay at the same indent level.")

(defun nanocn-ts--seq-semicolon-end (seq-node)
  "Return the end position of the `;' token in SEQ-NODE, or nil."
  (let ((count (treesit-node-child-count seq-node))
        (i 0)
        result)
    (while (and (< i count) (not result))
      (let ((child (treesit-node-child seq-node i)))
        (when (equal (treesit-node-type child) ";")
          (setq result (treesit-node-end child))))
      (setq i (1+ i)))
    result))

(defun nanocn-ts--same-line-p (pos1 pos2)
  "Non-nil if buffer positions POS1 and POS2 are on the same line."
  (= (line-number-at-pos pos1 t) (line-number-at-pos pos2 t)))

(defun nanocn-ts--search-seq-from (start-node bol)
  "Walk up from START-NODE looking for a sequencing form covering BOL.
Match when:
  - The body is MISSING (incomplete parse, cursor on next line), or
  - BOL is on the same line as the body's start (first line of body).
Do NOT match when BOL is deep inside a multi-line body (e.g. inside
a case_expr that is the body of a take_expr) — those lines should
be governed by the body's own indent rules."
  (let ((n start-node))
    (catch 'found
      (while n
        (when (member (treesit-node-type n) nanocn-ts--sequencing-types)
          (let ((body (treesit-node-child-by-field-name n "body")))
            ;; Complete parse: body exists, is non-empty, BOL is in
            ;; range, AND BOL is on the body's first line.
            (when (and body
                       (/= (treesit-node-start body) (treesit-node-end body))
                       (<= (treesit-node-start body) bol)
                       (< bol (treesit-node-end body))
                       (nanocn-ts--same-line-p (treesit-node-start body) bol))
              (throw 'found n))
            ;; Incomplete parse: body is absent or zero-width
            ;; (MISSING), `;' exists, and BOL is past the `;'.
            (let ((semi-end (nanocn-ts--seq-semicolon-end n)))
              (when (and semi-end
                         (>= bol semi-end)
                         (or (null body)
                             (= (treesit-node-start body)
                                (treesit-node-end body))))
                (throw 'found n)))))
        (setq n (treesit-node-parent n)))
      nil)))

(defun nanocn-ts--enclosing-seq-body (node _parent bol &rest _)
  "If BOL is in the body position of a let/take chain, return the form node.
Tries the node at BOL first; if that fails (e.g. cursor is on an empty
line past the end of the tree), retries from the node just before BOL
on the previous line."
  (or (nanocn-ts--search-seq-from (or (treesit-node-parent node) node) bol)
      ;; Fallback: tree ends before BOL (MISSING body, cursor on
      ;; the next line).  Look at the last token before BOL.
      (let ((prev-pos (save-excursion
                         (goto-char bol)
                         (skip-chars-backward " \t\n")
                         (max (1- (point)) (point-min)))))
        (nanocn-ts--search-seq-from
         (treesit-node-at prev-pos) bol))))

(defun nanocn-ts--seq-body-anchor (node parent bol &rest _)
  "Anchor: position of the enclosing sequencing form's keyword.
Uses the node's own start position (not BOL) so that a `let' mid-line
\(e.g. `Nil u -> let x = 0;') anchors at the `let', not at `Nil'."
  (when-let ((seq-node (nanocn-ts--enclosing-seq-body node parent bol)))
    (treesit-node-start seq-node)))

;; Branch body indentation: after `| pat ->` RET, indent to the
;; pattern's column + offset.  Same previous-line fallback as the
;; sequencing rule because the tree ends at the MISSING body.

(defconst nanocn-ts--branch-types
  '("branch" "case_branch" "crt_case_branch")
  "Node types that are pattern-match branches with a `body' field.")

(defun nanocn-ts--branch-body-incomplete-p (branch-node)
  "Non-nil if BRANCH-NODE has a MISSING (zero-width) or absent body."
  (let ((body (treesit-node-child-by-field-name branch-node "body")))
    (or (null body)
        (= (treesit-node-start body) (treesit-node-end body)))))

(defun nanocn-ts--last-branch-before (container-node bol)
  "In CONTAINER-NODE, find the last branch child that starts before BOL."
  (let ((count (treesit-node-child-count container-node))
        (best nil))
    (dotimes (i count)
      (let ((child (treesit-node-child container-node i t)))
        (when (and child
                   (member (treesit-node-type child) nanocn-ts--branch-types)
                   (<= (treesit-node-start child) bol))
          (setq best child))))
    best))

(defun nanocn-ts--branch-body-p (node parent bol &rest _)
  "If BOL should be the body of a branch, return the branch node.
Uses two strategies:
  1. If PARENT is a case/fun container, scan its branch children for
     the last one before BOL with an incomplete body.
  2. Text-based fallback: walk ancestors of the previous line's node."
  (or
   ;; Strategy 1: parent is a branch container.
   (let ((container (cond
                     ((member (treesit-node-type parent)
                              '("case_expr" "crt_case" "fun_decl"))
                      parent)
                     ((member (treesit-node-type node)
                              '("case_expr" "crt_case" "fun_decl"))
                      node)
                     (t nil))))
     (when container
       (let ((br (nanocn-ts--last-branch-before container bol)))
         (when (and br
                    (>= bol (treesit-node-end br))
                    (nanocn-ts--branch-body-incomplete-p br))
           br))))
   ;; Strategy 2: text-based fallback.
   (let ((prev-pos (save-excursion
                     (goto-char bol)
                     (skip-chars-backward " \t\n")
                     (max (1- (point)) (point-min)))))
     (let ((n (treesit-node-at prev-pos)))
       (catch 'found
         (while n
           (when (member (treesit-node-type n) nanocn-ts--branch-types)
             (when (and (>= bol (treesit-node-end n))
                        (nanocn-ts--branch-body-incomplete-p n))
               (throw 'found n)))
           (setq n (treesit-node-parent n)))
         nil)))))

(defun nanocn-ts--branch-body-anchor (node parent bol &rest _)
  "Anchor: the branch node's start position (= the pattern's start).
Adding `nanocn-ts-indent-offset' gives pat_column + 2."
  (when-let ((br (nanocn-ts--branch-body-p node parent bol)))
    (save-excursion
      (goto-char (treesit-node-start br))
      (point))))

(defun nanocn-ts--toplevel-keyword-p (node _parent _bol &rest _)
  "Non-nil if NODE is a top-level declaration keyword.
Handles incomplete parses where the declaration is inside an ERROR
node rather than directly under source_file."
  (let ((type (treesit-node-type node)))
    ;; Check if this is a declaration-start keyword at column 0-ish,
    ;; or a named declaration node whose parent is source_file or ERROR.
    (or (member type '("fun_decl" "rfun_decl" "sort_decl" "type_decl" "main_decl"))
        ;; Anonymous keyword token whose grandparent is source_file or ERROR/root.
        (and (member type '("fun" "rfun" "sort" "type" "main"))
             (let* ((p (treesit-node-parent node))
                    (gp (and p (treesit-node-parent p))))
               (or (null gp)
                   (member (treesit-node-type gp)
                           '("source_file" "ERROR"))))))))

;; Smart case-expr indentation: when BOL follows a branch with an
;; incomplete body, indent relative to that branch (pat + offset)
;; rather than the case keyword.

(defconst nanocn-ts--case-container-types
  '("case_expr" "crt_case" "fun_decl")
  "Node types that contain branch children.")

(defun nanocn-ts--case-indent-matcher (_node parent _bol &rest _)
  "Match when PARENT is a case/fun container."
  (member (treesit-node-type parent) nanocn-ts--case-container-types))

(defun nanocn-ts--deepest-incomplete-seq (node)
  "Find the deepest sequencing form with an incomplete body in NODE's subtree.
Walks from NODE into its `body' field recursively.  Returns the
deepest sequencing form whose body is MISSING, or nil."
  (when (and node (member (treesit-node-type node) nanocn-ts--sequencing-types))
    (let ((body (treesit-node-child-by-field-name node "body")))
      (if (or (null body)
              (= (treesit-node-start body) (treesit-node-end body)))
          ;; This node has an incomplete body — it's our target.
          node
        ;; Body exists; recurse into it (it might be another let/take).
        (nanocn-ts--deepest-incomplete-seq body)))))

(defun nanocn-ts--case-indent-anchor (_node parent bol &rest _)
  "Anchor for children of case containers.
Three cases, checked in order:
  1. Last branch has an incomplete body that is entirely MISSING →
     anchor at the branch start (pat + offset).
  2. Last branch has a body containing a sequencing form (let/take)
     whose tail is MISSING → anchor at that sequencing form (flat
     let continuation).
  3. Normal case → anchor at PARENT's BOL."
  (let ((br (nanocn-ts--last-branch-before parent bol)))
    (cond
     ;; Case 1: entirely missing branch body.
     ((and br
           (>= bol (treesit-node-end br))
           (nanocn-ts--branch-body-incomplete-p br))
      (save-excursion
        (goto-char (treesit-node-start br))
        (point)))
     ;; Case 2: branch body contains a sequencing form with missing tail.
     ((and br
           (>= bol (treesit-node-end br)))
      (let* ((body (treesit-node-child-by-field-name br "body"))
             (seq (and body (nanocn-ts--deepest-incomplete-seq body))))
        (if seq
            (save-excursion
              (goto-char (treesit-node-start seq))
              (point))
          ;; No incomplete sequencing form — normal case.
          (save-excursion
            (goto-char (treesit-node-start parent))
            (back-to-indentation)
            (point)))))
     ;; Case 3: normal.
     (t (save-excursion
          (goto-char (treesit-node-start parent))
          (back-to-indentation)
          (point))))))

(defvar nanocn-ts-indent-rules
  `((nanocn
     ;; Top-level declarations always at column 0, even when the
     ;; tree wraps them in ERROR during incomplete edits.
     (nanocn-ts--toplevel-keyword-p column-0 0)

     ;; Sequencing bodies (let/take chains) stay flat.  This rule must
     ;; come first so it takes priority over the per-form rules below.
     ;;
     ;;   let x = 1;
     ;;   let y = 2;
     ;;   x + y
     ;;
     (nanocn-ts--enclosing-seq-body nanocn-ts--seq-body-anchor 0)

     ;; Branch bodies: `| pat ->` RET → indent to pat + offset.
     ;;
     ;;   | Cons p ->
     ;;       body_here       (pat at col 4, body at col 6)
     ;;
     (nanocn-ts--branch-body-p nanocn-ts--branch-body-anchor ,nanocn-ts-indent-offset)

     ;; Closing delimiters align with the opening line.
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)

     ;; The "|" separator in branches/ctors aligns with the container
     ;; keyword (case, fun, sort, type), giving the style:
     ;;
     ;;   case x of {
     ;;     Nil u -> 0
     ;;   | Cons p -> ...
     ;;   }
     ((node-is "|") parent-bol 0)

     ;; Branch and ctor nodes themselves indent by offset (one step
     ;; past the "|"), so patterns align across branches.
     ((node-is "branch") parent-bol ,nanocn-ts-indent-offset)
     ((node-is "case_branch") parent-bol ,nanocn-ts-indent-offset)
     ((node-is "crt_case_branch") parent-bol ,nanocn-ts-indent-offset)
     ((node-is "ctor_decl") parent-bol ,nanocn-ts-indent-offset)

     ;; Top-level declarations sit at column 0.
     ((parent-is "source_file") column-0 0)

     ;; Bodies inside declarations indent.
     ((parent-is "fun_decl") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "rfun_decl") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "sort_decl") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "type_decl") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "main_decl") parent-bol ,nanocn-ts-indent-offset)

     ;; Sequencing forms: non-body children (value on continuation
     ;; line, etc.) indent relative to the keyword.
     ((parent-is "let_expr") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "take_expr") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let_log") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let_log_annot") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let_res") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let_res_annot") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_let_core") parent-bol ,nanocn-ts-indent-offset)

     ;; if/case/iter indent their children.
     ((parent-is "if_expr") parent-bol ,nanocn-ts-indent-offset)
     (nanocn-ts--case-indent-matcher nanocn-ts--case-indent-anchor ,nanocn-ts-indent-offset)
     ((parent-is "iter_expr") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_if") parent-bol ,nanocn-ts-indent-offset)
     ((parent-is "crt_iter") parent-bol ,nanocn-ts-indent-offset)

     ;; Catch-all: continue at current indentation.
     (no-node parent-bol 0)))
  "Indentation rules for `nanocn-ts-mode'.")

;; ---------------------------------------------------------------------------
;; Defun navigation & Imenu
;; ---------------------------------------------------------------------------

(defun nanocn-ts-defun-name (node)
  "Return the definition name for NODE, or nil."
  (pcase (treesit-node-type node)
    ((or "fun_decl" "rfun_decl")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ((or "sort_decl" "type_decl")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("main_decl" "main")
    (_ nil)))

(defvar nanocn-ts-imenu-settings
  `(("Function" "\\`\\(fun_decl\\|rfun_decl\\)\\'" nil nil)
    ("Sort" "\\`sort_decl\\'" nil nil)
    ("Type" "\\`type_decl\\'" nil nil))
  "Imenu settings for `nanocn-ts-mode'.")

;; ---------------------------------------------------------------------------
;; Major mode
;; ---------------------------------------------------------------------------

;;;###autoload
(define-derived-mode nanocn-ts-mode prog-mode "nanoCN"
  "Major mode for editing nanoCN .cn/.rcn files, powered by tree-sitter."
  :syntax-table nil

  (unless (treesit-ready-p 'nanocn)
    (error "nanoCN tree-sitter grammar is not installed; run M-x treesit-install-language-grammar RET nanocn"))

  (treesit-parser-create 'nanocn)

  ;; Font-lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules nanocn-ts-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword type)
                (constant number function builtin constructor)
                (operator delimiter bracket)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules nanocn-ts-indent-rules)

  ;; Defun navigation (C-M-a / C-M-e)
  (setq-local treesit-defun-type-regexp
              (rx bos (or "fun_decl" "rfun_decl"
                          "sort_decl" "type_decl"
                          "main_decl") eos))
  (setq-local treesit-defun-name-function #'nanocn-ts-defun-name)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings nanocn-ts-imenu-settings)

  ;; Comment support
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; ElDoc: keep echo area to one line; full content in *eldoc* buffer.
  (setq-local eldoc-echo-area-use-multiline-p 1)

  ;; Activate
  (treesit-major-mode-setup))

;; ---------------------------------------------------------------------------
;; File associations
;; ---------------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cn\\'"  . nanocn-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rcn\\'" . nanocn-ts-mode))

;; ---------------------------------------------------------------------------
;; Eglot (LSP) registration
;; ---------------------------------------------------------------------------

(defcustom nanocn-lsp-server "nanocn-lsp"
  "Command to run the nanoCN LSP server.
Can be an absolute path (e.g. during development:
\"/path/to/nanocn/_build/default/bin/nanocn_lsp.exe\")
or a bare name that must be on `exec-path'."
  :type 'string
  :group 'nanocn)

;;;###autoload
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nanocn-ts-mode . nanocn-ts--eglot-contact)))

(defun nanocn-ts--eglot-contact (_interactive)
  "Return the LSP server contact for Eglot.
Evaluates `nanocn-lsp-server' at connection time so customization
changes take effect without reloading."
  (list nanocn-lsp-server))

(provide 'nanocn-ts-mode)

;;; nanocn-ts-mode.el ends here
