#!/bin/bash
# Load named top-level forms from the tangled config.el into the running Doom
# daemon, so a change to config.org is live without a restart.
#   hotload.sh NAME...          defun / defface / define-minor-mode / defvar by name
#   hotload.sh --force NAME...  also re-set defvars (defvar never overwrites a bound one)
# Run doom sync first.  Key bindings (map!, evil-define-key*) are not def forms:
# evaluate those by hand with emacsclient.
set -uo pipefail
force=nil
[ "${1:-}" = "--force" ] && { force=t; shift; }
[ $# -gt 0 ] || { echo "usage: hotload.sh [--force] NAME..." >&2; exit 2; }
names=$(printf '%s ' "$@")
emacsclient -s doom --eval "
(let ((names (mapcar #'intern (split-string \"$names\")))
      done)
  (with-temp-buffer
    (insert-file-contents (expand-file-name \"config.el\" doom-user-dir))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (memq (car-safe form) '(defun defvar defface define-minor-mode defcustom))
                       (memq (cadr form) names))
              (if (and $force (eq (car form) 'defvar))
                  (set (cadr form) (eval (nth 2 form) nil))
                (eval form nil))
              (push (cadr form) done))))
      (end-of-file nil)))
  (list :loaded (length done) :missing (seq-difference names done)))"
