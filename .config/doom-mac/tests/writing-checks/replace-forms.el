;;; replace-forms.el --- swap, insert or delete top-level forms by name  -*- lexical-binding: t; -*-
;;
;; emacs --batch -Q -l replace-forms.el TARGET FORMS
;;
;; FORMS holds complete top-level forms, applied in order to TARGET (an .el
;; file, or an .org file whose src blocks hold the forms):
;;   a plain form     replaces the one definition of the same name;
;;   ";;@new"         puts the next form right after the form handled before it;
;;   ";;@after NAME"  puts the next form right after the definition of NAME;
;;   ";;@delete NAME" removes the definition of NAME.
;; A name defined zero or several times is an error, and nothing is written.

(require 'cl-lib)

(defun rf--bounds (name)
  "Return (START . END) of the one top-level definition of NAME."
  (goto-char (point-min))
  (let ((re (concat "^(def[a-z*-]* " (regexp-quote name) "\\_>"))
        found)
    (while (re-search-forward re nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-sexp)
        (push (cons start (point)) found)))
    (unless (= (length found) 1)
      (error "%s: expected one definition, found %d" name (length found)))
    (car found)))

(defun rf--defined-p (name)
  (condition-case nil (progn (rf--bounds name) t) (error nil)))

(defun rf--read-ops (file)
  "Return the operations in FILE, in order."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (ops pending)
      (while (not (eobp))
        (cond ((looking-at ";;@delete \\(.+\\)$")
               (push (list 'delete (match-string 1)) ops)
               (forward-line 1))
              ((looking-at ";;@after \\(.+\\)$")
               (setq pending (list 'after (match-string 1)))
               (forward-line 1))
              ((looking-at ";;@new$")
               (setq pending (list 'new))
               (forward-line 1))
              ((looking-at "(")
               (let* ((start (point))
                      (end (progn (forward-sexp) (point)))
                      (text (buffer-substring-no-properties start end)))
                 (push (list (or (car pending) 'replace)
                             (symbol-name (nth 1 (read text)))
                             text
                             (cadr pending))
                       ops)
                 (setq pending nil)))
              (t (forward-line 1))))
      (nreverse ops))))

(let* ((target (nth 0 command-line-args-left))
       (ops (rf--read-ops (nth 1 command-line-args-left)))
       (last nil)
       (counts (list :replaced 0 :inserted 0 :deleted 0)))
  (setq command-line-args-left nil)
  (with-temp-buffer
    (insert-file-contents target)
    (emacs-lisp-mode)
    (dolist (op ops)
      (pcase-let ((`(,kind ,name ,text ,anchor) op))
        (pcase kind
          ('delete
           (let ((b (rf--bounds name)))
             (delete-region (car b) (cdr b))
             (goto-char (car b))
             (let ((s (progn (skip-chars-backward "\n") (point)))
                   (e (progn (skip-chars-forward "\n") (point))))
               (delete-region s e)
               (goto-char s)
               (insert (if (looking-at "[(;]") "\n\n" "\n")))
             (cl-incf (plist-get counts :deleted))))
          ('replace
           (let ((b (rf--bounds name)))
             (delete-region (car b) (cdr b))
             (goto-char (car b))
             (insert text)
             (setq last name)
             (cl-incf (plist-get counts :replaced))))
          ((or 'after 'new)
           (when (rf--defined-p name)
             (error "%s is already defined" name))
           (let ((where (if (eq kind 'after) anchor last)))
             (unless where (error "%s: @new with no form before it" name))
             (goto-char (cdr (rf--bounds where)))
             (insert "\n\n" text)
             (setq last name)
             (cl-incf (plist-get counts :inserted)))))))
    (write-region (point-min) (point-max) target nil 'quiet))
  (princ (format "%S -> %s\n" counts target)))

;;; replace-forms.el ends here
