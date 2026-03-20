---
title: Mandatory Scratch Buffer Testing
impact: CRITICAL
impactDescription: Prevents configuration failure and editor startup issues
tags: elisp, testing, stability, doom-emacs, workflow, debugging
---

## Mandatory Scratch Buffer Testing

All new or modified Emacs Lisp code **must** be tested in isolation *before* being integrated into the main configuration. A syntax error or runtime bug in a tangled file like `config.el` can prevent Emacs from starting, forcing a manual fix from outside the editor. The Doom Scratch Buffer provides a safe, ephemeral environment for this critical testing step.

---

### Incorrect: Editing the Live Configuration Directly

This workflow introduces significant risk. A single mistake can render the entire Emacs environment unusable upon the next restart.

**The Risky Process:**

1.  A new function is written directly into a source block in `config.org`.
2.  The user saves the file, which automatically tangles the (potentially broken) code into `config.el`.
3.  The user runs `doom sync` or restarts Emacs to apply the changes.
4.  **Result:** Emacs fails to start. The user must now debug their configuration using another editor or the command line, which is slow and frustrating.

```emacs-lisp
;; Example of a bug that would break startup if placed directly in config.org
;; The user forgot the closing parenthesis for the 'let' block.
(defun kyo/get-project-name ()
  "A function with a syntax error."
  (let ((project-root (projectile-project-root)))
    (f-filename project-root)
  ; <- Missing closing parenthesis here
```

---

### Correct: Testing in the Doom Scratch Buffer

This workflow isolates development from the live configuration, ensuring that only verified, working code is ever committed.

**The Safe Process:**

1.  **Open the Scratch Buffer:** Press `SPC b x` to open the Doom Scratch Buffer, which is already in `emacs-lisp-mode`.
2.  **Write the Code:** Draft the new function in this buffer.
3.  **Evaluate:** Place the cursor after the function's closing parenthesis and press `C-x C-e`. This loads the function into the current Emacs session's memory without saving it to any file.
4.  **Test:** Write a call to your new function in the scratch buffer (e.g., `(kyo/my-new-function)`) and evaluate it with `C-x C-e`. The result will appear in the echo area at the bottom of the screen. Repeat this step until you have confirmed the function works as expected.
5.  **Integrate:** Once fully tested and verified, copy the final, working code from the scratch buffer and paste it into the appropriate location in `config.org`.
6.  **Reload:** Save `config.org` and run `doom/reload` (`SPC h r r`) to apply the change to your live configuration safely.

```emacs-lisp
;; Step 2 & 3: Write and evaluate the function in the scratch buffer.
(defun kyo/get-greeting (name)
  "Return a greeting string for NAME."
  (concat "Hello, " name "!"))
;; Press C-x C-e here

;; Step 4: Write and evaluate a test call in the scratch buffer.
(kyo/get-greeting "Kyonax")
;; Press C-x C-e here
;; The echo area will display: "Hello, Kyonax!"
;; Now the code is verified and safe to move to config.org.
```

This discipline is the most important practice for maintaining a stable and reliable Emacs configuration.
