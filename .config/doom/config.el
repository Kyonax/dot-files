(use-package beacon
  :config
  (beacon-mode 1))

(setq bookmark-default-file "~/.brain.d/bookmarks/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(setq doom-font (font-spec :family "SpaceMono Nerd Font Mono" :size 11)
      doom-variable-pitch-font (font-spec :family "SpaceMono Nerd Font Mono" :size 11)
      doom-big-font (font-spec :family "SpaceMono Nerd Font Mono" :size 18))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

(use-package doom-modeline
  :config
  (set-face-attribute 'mode-line nil :font "SpaceMono Nerd Font Mono")
  (setq doom-modeline-support-imenu t
      doom-modeline-buffer-state-icon t
      doom-modeline-icon t
      doom-modeline-height 33
      doom-modeline-bar-width 6
      doom-modeline-persp-name t
      doom-modeline-persp-icon t))

(use-package server
  :config
  (setq server-use-tcp t
      server-socket-dir "~/.config/emacs/server"))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "I am Kyo\nkyonax_on_tech - iam@kyo.wtf - github.com/Kyonax")))

(defun kyo/my-shit-is-always-greater ()
  (let* ((banner '(
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣧⡀⣿⣤⣀⣾⣅⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣤⣤⠲⠶⣦⡿⣿⣿⣿⣿⣿⣿⣿⣴⣴⠖⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⢲⣦⣿⣟⠛⡓⣀⠐⠋⢽⠟⡿⢿⣿⣿⢛⢷⣤⣤⠄⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠠⢤⣦⣤⡦⠆⢈⢋⣡⡔⠁⢀⡠⠐⢻⡿⢃⡄⠀⣿⣧⣤⣀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⣀⣴⣾⡿⠗⡪⢛⣿⣿⣷⢾⡟⠀⣊⣴⣷⣿⢧⣦⣿⣿⢿⣁⣀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⢈⡬⠟⠂⠀⠖⠀⠞⠉⡽⠃⢈⠡⢋⠏⡰⠀⡿⠈⠛⣿⡇⠸⢢⡀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠊⠄⠁⠀⠐⣀⣪⢀⣔⡤⠂⣠⢀⡴⠀⣡⡀⠁⠈⠃⡢⠡⣄⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠄⠀⢤⣜⣉⡛⠻⢿⣷⣿⣿⣾⣷⡾⠿⠷⠆⢁⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⣾⠧⣬⣍⣑⠢⣽⣿⣿⣋⡤⠴⠒⢛⣳⣧⠀⠈⠀⢤⡄⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⣤⣧⠀⡄⢿⣧⡙⠛⠻⣤⣿⣿⣿⣿⣠⣿⡿⠟⣹⡧⠐⠠⠠⣿⡧⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⣠⣾⣿⣿⣧⡀⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣾⡿⠁⢋⡄⠐⣿⣿⠀⠀⠀⠀"
                   "⠀⠀⢀⣼⣿⣿⣿⣿⣟⠀⠀⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠃⠀⣿⡧⠀⣿⣿⡀⣧⠀⠀"
                   "⠀⠀⣿⣿⣿⣿⣿⠛⢿⣷⡄⢢⠘⢿⣿⣿⣿⣿⣿⣿⣿⣿⠟⢁⠀⠀⢛⣩⣴⣿⡿⢃⡌⠀⠀"
                   "⠀⠀⠙⣿⣿⣿⣿⣿⣶⣌⠻⢦⡁⠀⠻⣿⣿⣿⣿⠿⠋⠀⣘⣡⣶⣿⡿⠟⣛⣡⠶⢋⠄⠀⠀"
                   "⠀⠀⠀⠈⠻⠿⡿⡿⣿⣿⣿⣦⣙⠢⠀⠀⠈⠈⣀⣤⣶⣿⣟⢛⡩⠔⣒⣩⣥⣤⣶⣿⠂⠀⠀"
                   "⠀⠀⠀⠀⠀⠰⣶⣦⣦⣦⡙⢿⡿⠓⣀⣠⣴⣿⣿⣿⡿⢛⣩⣴⣾⣿⡿⠿⠛⠋⠉⠁⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠈⠙⠻⢿⣿⣦⣅⠘⠿⠋⠁⢈⠟⣫⣿⣿⠿⠛⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠙⠃⠐⡄⠀⠁⡺⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'kyo/my-shit-is-always-greater)

(custom-set-faces!
  '(doom-dashboard-banner :foreground "#9FE573" :background "#000000" :weight bold)
  '(doom-dashboard-loaded :foreground "#9FE573" :background "#000000" :weight bold))

(setq doom-theme 'gruber-darker)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package treemacs
  :config
  (setq evil-treemacs-state-cursor t
        treemacs-show-cursor t
        treemacs-width 40))

(setq display-line-numbers-type 'relative
      display-line-numbers-mode t
      line-number-mode t)

(defun kyo/js2-mode-setup ()
  (js2-minor-mode))

(use-package js2-mode
  :mode (("\\.plugin\\.js\\'" . js2-mode)
         ("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode)
         ("\\.cjs\\'" . js2-mode)
         ("\\.es6\\'" . js2-mode))
  :interpreter ("javascript" . js2-mode)
  :hook ((js2-mode . lsp-deferred)
         (js2-mode . kyo/js2-mode-setup)))

(defun kyo/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-sideline-toggle-symbols-info))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js2-mode . lsp-mode)
         (web-mode . lsp-mode)
         (lsp-mode . kyo/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(after! org
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/org"
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("" "" "" "󰺕" "󰻂" "󰪥" "󰻃")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-hide-emphasis-markers t
        org-todo-keywords `((sequence ,@my-org-todo-keywords))))

(setq org-agenda-block-separator 175)

(defun kyo/tangle-on-save ()
  "If the current Org buffer has the auto_tangle tag, execute all code blocks and tangle the file."
  (when (and (derived-mode-p 'org-mode)
             ;; Check for the #+auto_tangle: t tag in the file
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+auto_tangle:[ \t]*t" nil t)))
    (org-babel-tangle)))

;; Add kyo/tangle-on-save to the after-save-hook for Org mode buffers only.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'kyo/tangle-on-save nil t)))

(defun kyo/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'kyo/insert-auto-tangle-tag)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   (javascript . t)
   (emacs-lisp . t)
   (js . t)
   (json . t)
   (php . t)
   ))

(setq org-babel-command:typescript "npx -p typescript -- tsc")
(setq org-confirm-babel-evaluate nil)

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("" "󱡞" "󰝨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight regular)
     (?B :foreground "#98be65" :weight regular)
     (?C :foreground "#c678dd" :weight regular))))

(after! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory "~/.brain.d/roam-nodes")
  (setq org-roam-dailies-directory "~/.brain.d/agenda")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates `,my-org-roam-capture-templates))

(map! :leader
      (:prefix ("n D" . "Create Roam Nodes")
       :desc "Daily Nodes - Yesterday" "Y" #'org-roam-dailies-capture-yesterday
       :desc "Daily Nodes - Today" "H" #'org-roam-dailies-capture-today
       :desc "Daily Nodes - Calendar" "C" #'org-roam-dailies-capture-date
       :desc "Daily Nodes - Tomorrow" "T" #'org-roam-dailies-capture-tomorrow))

(map! :leader
      (:prefix ("n e" . "Find Roam Nodes")
       :desc "Daily Nodes - Date" "d" #'org-roam-dailies-goto-date
       :desc "Daily Nodes - Yesterday" "Y" #'org-roam-dailies-goto-yesterday
       :desc "Daily Nodes - Today" "H" #'org-roam-dailies-goto-today
       :desc "Daily Nodes - Tomorrow" "T" #'org-roam-dailies-goto-tomorrow))

(use-package! websocket
    :after org-roam)
(use-package! org-roam-ui
    :after org
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(defun kyo/web-mode-setup ()
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-block-padding 2))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.twig\\'" . web-mode))
  :hook (web-mode . kyo/web-mode-setup))

(use-package whitespace-mode
  :hook ((js2-mode . whitespace-mode)
         (web-mode . whitespace-mode)
         (php-mode . whitespace-mode)))

(setq user-full-name "Cristian D. Moreno - Kyonax"
      user-mail-address "iam@kyo.wtf")

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
              (:map dired-mode-map
               :desc "Peep-dired image previews" "d p" #'peep-dired
               :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(defun kyo/open-directory (dir)
  "Open the specified directory DIR in Dired mode."
  (interactive "DChoose directory: ") ; Prompt for directory
  (dired dir))

(setq! gptel-directives '(
 (default . "Test 32")
 (studying .
"I am working in Doom Emacs using Org Mode to document research in a `.org` file. My goal is to improve the quality of the file, making it more readable, accessible, and well-structured for learning and reference. Depending on my request, you will provide one of the following types of support:

1. **Rewriting the Whole File**:
   - If I ask to rewrite the entire `.org` file, provide **only the new structured `.org` file** as the output, without any explanations, documentation, or unrelated content. Follow these guidelines:
     - Use clear and consistent title structures (all titles must be in UPPER CASE; plural words like CATs should have a lowercase 's').
     - Apply Org Mode best practices for documenting research, such as logical hierarchy, tags, and metadata.
     - Do not modify the existing configuration data (e.g., `:PROPERTIES:`, `#+title:`, `#+filetags:`, etc.).
     - Organize content with sections, subsections, and bullet points.
     - Include `.org` code blocks to demonstrate improvements and practical exercises (if applicable).

2. **Support for Small Pieces of Research**:
   - If I ask for help with a small piece of research or a specific point, focus on providing clear, concise, and well-structured support. Follow these strategies:
     - Use human-readable and learning-friendly formats.
     - Explain complex terms or concepts if necessary.
     - Provide examples, references, or documentation if requested.
     - Structure the response to make it easy to integrate into the `.org` file.

3. **Improving Text Structure**:
   - If I ask for help structuring text to make it more readable, focus on organizing the content logically. Use headings, bullet points, and clear formatting to enhance readability.

4. **Providing Examples or References**:
   - If I ask for examples, provide `.org` code blocks or practical demonstrations.
   - If I ask for references or documentation, suggest reliable sources or links to further information.

5. **Translation into Other Languages**:
  - If I ask for translation support, provide an accurate and context-aware translation of the specified content into the requested language. Ensure the translation maintains the original meaning, tone, and structure while adapting to the target language's conventions.

**General Guidelines**:
- Always prioritize clarity, readability, and usability.
- Adapt your response to the specific type of support I request.
- Do not include unnecessary explanations, documentation, or unrelated content unless explicitly requested.

**Deliverables**:
 - Provide only the requested output, whether it’s a rewritten .org file, support for a small piece of research, improved text structure, examples, references, or translations. Do not include additional explanations or unrelated content unless explicitly requested.
 ")
 (documentation .
  "
You are an expert developer specialized in refining code documentation. Your sole focus is improving the clarity and readability of the documentation without altering the code or structure. Follow these guidelines strictly:
Description

    Clearly and concisely explain the purpose of the function, class, or module.
    Cross-check the description with the code to ensure accuracy. The explanation must fully align with what the code does.
    Avoid jargon or buzzwords like scalable, intuitive, or robust.
    Provide enough context for someone new to coding to understand, but donÔÇÖt oversimplify.

Usage

    If a usage example exists, ensure it reflects the most common and simplest use case for the code.
    Verify that the usage aligns with the codeÔÇÖs behavior. Refine it to be precise, accurate, and functional out of the box.
    Be precise. No room for vagueness.

Parameters/Attributes

    If parameters or attributes are documented, verify they match the code exactly. Ensure names, types, and purposes are accurate and specific.
    Describe each parameter/attribute in a short, one-line summary that eliminates ambiguity.
    Do not add parameter documentation if itÔÇÖs not already present in the code.

Important Notes:

    Do not modify the code itself.
    Do not add new sections or alter the documentation structure.
    Always verify the documentation matches the code, ensuring accuracy and relevance.
    Keep explanations terse, professional, and focused. Skip pleasantries or unnecessary details.
    Always return just the answer and only the answer. Do not enclose it in code blocks, as we are already in a code editor.
")
 ))

(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "p" #'ivy-push-view
       :desc "Ivy switch view" "s" #'ivy-switch-view))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                \\newcommand{\\checkbox}{\\text{[ ]}} % Define \checkbox
                \\newcommand{\\checkedbox}{\\text{[X]}} % Define \checkedbox
                [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! org
  ;; Minted config for much better syntax highlightig to src blocks.
  (setq
   org-latex-listings 'minted
   ;; minted calls the pygmentize process and thus needs shell escaping
   org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   ;; NOTE I don't actually add minted to the package list, I'll just \usepackage it when needed

   ;;(setq org-latex-pdf-process
   ;;'("latexmk -f -pdf %f")) ; Doesn't work with pdflatex
   ;; Margins
   ;;(add-to-list 'org-latex-packages-alist '("a4paper,margin=1.1in" "geometry"))

   ;; Add 'colorlinks' option to hyperrref, its much prettier
   org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks=true,
 linkcolor={cyan}
}"
   ;; page break after TOC
   org-latex-toc-command "\\tableofcontents\n\\pagebreak\n\n")
  ;; org-latex-minted-options '(("bgcolor" "{white!98!black}"))
  ;; org-latex-minted-options '(("bgcolor" "\\color{white!96!black}"))
  ;; In case I configured hyperref in a separate latex preamble
  ;;(setq org-latex-hyperref-template ""))
  (add-hook! 'org-roam-buffer-prepare-hook
    (setq display-line-numbers nil)))


(setq org-highlight-latex-and-related '(native))

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

;; Define the custom checklist symbols in LaTeX
(defun my-org-latex-checklist-filter (text backend info)
  "Replace Org checklist symbols with custom LaTeX symbols."
  (when (org-export-derived-backend-p backend 'latex)
    (let ((text (replace-regexp-in-string "\\[ \\]" "\\\\checkbox" text))
          (text (replace-regexp-in-string "\\[X\\]" "\\\\checkedbox" text)))
      text)))

;; Add the custom filter to the export process
(add-hook 'org-export-filter-final-output-functions
          'my-org-latex-checklist-filter)

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :hook (pdf-tools-enabled . hide-mode-line-mode)
  :config
  (setq pdf-view-midnight-colors '("#888888" . "#111111")))

(setq password-cache-expiry nil)
