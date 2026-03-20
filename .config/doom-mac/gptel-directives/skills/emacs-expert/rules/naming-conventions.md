---
title: Apply Emacs Lisp Naming Conventions
impact: HIGH
impactDescription: Improves readability and prevents namespace collisions
tags: elisp, naming, style-guide, maintainability, namespace
---

## Apply Emacs Lisp Naming Conventions

Emacs Lisp operates in a single, global namespace. To avoid conflicts with Emacs core functions or other packages, all custom definitions **must use a personal prefix**. This rule outlines the standard conventions for naming different types of symbols in the user's configuration.

The user's designated prefix is `kyo/`.

---

### 1. Functions

Functions should be prefixed and use `kebab-case` (lowercase words separated by hyphens).

**Incorrect:** A generic, un-prefixed name that could easily conflict.

```emacs-lisp
;; This could clash with a function from another package or a future
;; built-in Emacs function.
(defun open-project (path)
  "Opens a project at the given PATH."
  ;; ... implementation ...
  )
```

**Correct:** A safely namespaced function that is clearly user-defined.

```emacs-lisp
;; The "kyo/" prefix prevents conflicts, and kebab-case is idiomatic.
(defun kyo/open-project (path)
  "Opens a project at the given PATH."
  ;; ... implementation ...
  )
```

---

### 2. Variables

Variables should follow the same rules as functions: use a prefix and `kebab-case`.

**Incorrect:** `camelCase` is not idiomatic, and the lack of a prefix makes it unsafe.

```emacs-lisp
;; This variable name is not conventional and could be overwritten by another
;; package that doesn't follow best practices.
(defvar currentProject nil
  "Holds the path to the current project.")
```

**Correct:** A prefixed, idiomatic variable name.

```emacs-lisp
;; The name is safe, easy to identify as custom, and follows Elisp style.
(defvar kyo/current-project nil
  "Holds the path to the current project.")
```

---

### 3. Constants

Constants should be defined with `defconst`, be prefixed, use `UPPER-KEBAB-CASE`, and be wrapped in `+` signs for immediate visual identification.

**Incorrect:** Using `defvar` for a value that should not change makes its intent unclear.

```emacs-lisp
;; This looks like a regular variable, but it's meant to be a constant.
(defvar kyo/default-notes-path "~/org/notes/"
  "The default path where notes are stored.")
```

**Correct:** A clearly defined constant that stands out.

```emacs-lisp
;; `defconst` and the +plus-sign+ convention make it clear this value
;; is fixed and should not be changed at runtime.
(defconst +kyo/default-notes-path+ "~/org/notes/"
  "The default path where notes are stored.")
```

---

### 4. Predicates

Predicate functions (those that return `t` for true or `nil` for false) must end with a `-p` suffix.

**Incorrect:** The function name doesn't signal that it returns a boolean value.

```emacs-lisp
;; Without the '-p' suffix, one might think this function returns the
;; project object itself, not a boolean.
(defun kyo/is-project (dir)
  "Checks if DIR is a project."
  ;; ... returns t or nil ...
  )
```

**Correct:** The `-p` suffix makes the function's purpose immediately obvious.

```emacs-lisp
;; The "-p" suffix (for "predicate") clearly indicates this function will
;; return true or false.
(defun kyo/project-p (dir)
  "Return t if DIR is a project directory."
  ;; ... returns t or nil ...
  )
```
