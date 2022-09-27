;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Major mode for IEEE 1687-2014 ICL
;; https://standards.ieee.org/ieee/1687/3931/
;;
;; This major mode provides basic syntax highlighting, indentation, and imenu
;; support for the Instrument Connectivity Language (ICL) as defined by IEEE
;; 1687.

;;; Code:

;;;###autoload
(define-derived-mode icl-mode c-mode "ICL"
  "Major mode for IEEE 1687 ICL"
  (setq-local c-basic-offset 3)
  (setq-local indent-line-function 'icl-indent-line)
  (setq-local font-lock-defaults '(icl-font-lock-keywords))
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression
              `(("modules" ,(rx bol "Module" (+ " ")(group (1+ (any alnum "_-"))) symbol-end) 1)
                ("instances" ,(rx bol (+ " ") "Instance" (+ " ") (group (1+ (any alnum "_-"))) symbol-end) 1)))
  (modify-syntax-entry ?\' "." icl-mode-syntax-table)
  (modify-syntax-entry ?$ "." icl-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.icl\\'" . icl-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pdl\\'" . tcl-mode))

(with-eval-after-load 'tcl-mode
  (push "iProc" tcl-proc-list)
  (push "iTopProc" tcl-proc-list)
  (push "iProc" tcl-keyword-list)
  (push "iTopProc" tcl-keyword-list)
  (push "iProcsForModule" tcl-keyword-list)
  (tcl-set-proc-regexp)
  (tcl-set-font-lock-keywords)
  (setq tcl-imenu-generic-expression
        `((nil ,(concat tcl-proc-regexp "\\([-A-Za-z0-9_:+*]+\\)") 2))))

(defun icl-broken-line-p ()
  "Check if the line is split across a declaration.
For example:

Instance x Of
    y {}"
  (save-excursion
    (previous-line)
    (end-of-line)
    (skip-syntax-backward " " (line-beginning-position))
    (save-match-data
      (looking-back (rx (or "Of" "SelectedBy" "=")) (line-beginning-position)))))

(defun icl-indent-line ()
  "Indent like C, unless we have a split declaration."
  (interactive)
  (let ((c-basic-offset (if (icl-broken-line-p) (+1 c-basic-offset) c-basic-offset)))
    (c-indent-line)))

(defvar icl-font-lock-keywords
  ;; Attribute is a non-standard keyword, but sometimes seen in vendor ICL
  `((,(rx symbol-start "Attribute" symbol-end) 0 font-lock-constant-face)
    ;; The three types of declarations we want to highlight
    (,(rx symbol-start (group (or "Instance" "Module" "Enum")) (+ " ") (group (1+ (any alnum "_"))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ;; Keywords are PascalCase. So we check for upper followed by lower, but
    ;; also explicitly handle the tap keywords which don't have the second
    ;; letter lowercase
    (,(rx bol (0+ " ") (group (or (and upper lower) "TRST" "TCK" "TMS") (1+ alnum)) " ") 1 font-lock-keyword-face)
    ;; For some reason this keyword does not get highlighted consistently,
    ;; so adding it explicitly
    (,(rx symbol-start "InputPort" symbol-end) 0 font-lock-keyword-face)
    ;; Highlight numbers
    (,(rx (char " [':+-") (group (opt (char "bh")) (1+ digit))) 1 font-lock-type-face)
    ;; Make the hierachy seperator stand out
    (,(rx ".") 0 'error)
    ;; variables like $WIDTH
    (,(rx "$" (group alpha (1+ (any alnum "_")))) 1 font-lock-variable-name-face)
    ;; Used in Instance and Mux statements
    (,(rx (+ " ") (group (or "Of" "SelectedBy"))) 1 font-lock-builtin-face))
  "Keywords for ICL selected by try to maximize readability of collateral.")


(provide 'icl-mode)

;;; icl-mode.el ends here
