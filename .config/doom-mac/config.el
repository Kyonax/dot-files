(use-package beacon
  :config
  (beacon-mode 1))

(setq evil-escape-key-sequence "jk")

(setq bookmark-default-file "~/.brain.d/bookmarks/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(setq +file-templates-dir (expand-file-name "templates" doom-user-dir))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode)
  :config
  (setq editorconfig-exclude-modes '(archive-mode
                                     image-mode
                                     pdf-view-mode))
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

  ;; Espera a que cargue el major-mode antes de aplicar settings
  (setq editorconfig-lazy-major-mode t)
  (setq editorconfig-verbose nil))

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
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 33
        doom-modeline-bar-width 6
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info t
        doom-modeline-vcs-max-length 15
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-github-interval (* 15 60)
        doom-modeline-env-version t
        doom-modeline-mu4e nil
        doom-modeline-irc nil)
  :config
  (when (display-graphic-p)
    (set-face-attribute 'mode-line nil :font "SpaceMono Nerd Font Mono"))
  (doom-modeline-mode 1))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "@kyonax_on_tech")))

(defun kyo/my-shit-is-always-greater ()
  (let* ((banner '(
                   "                                                             "
                   "                         ███████████      █████              "
                   "                     █████████████████████████               "
                   "                  ███████████████████████████                "
                   "                █████████████████████████████                "
                   "               ████████████       ████████████               "
                   "              ██████████          █████████████              "
                   "              █████████          ██████████████              "
                   "             ██████████         █████ ██████████             "
                   "             ██████████       ██████  ██████████             "
                   "             ██████████      ██████   ██████████             "
                   "             ██████████     █████     ██████████             "
                   "             ██████████   ██████      ██████████             "
                   "             ██████████  ██████       ██████████             "
                   "             ██████████ █████         ██████████             "
                   "              ██████████████         ██████████              "
                   "              █████████████         ███████████              "
                   "               ████████████       ████████████               "
                   "                █████████████████████████████                "
                   "                ███████████████████████████                  "
                   "               █████████████████████████                     "
                   "             ██████     ████████████                         "
                   "                                                             "
                   "                        zerønet labs                         "
                   "                     Cristian D. Moreno                      "
                   "                                                             "
                   "                                                             "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line
                        (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'kyo/my-shit-is-always-greater)

(custom-set-faces!
  '(doom-dashboard-banner :foreground "#9FE573" :background "#000000" :weight bold)
  '(doom-dashboard-loaded :foreground "#9FE573" :background "#000000" :weight bold))

(setq doom-theme 'doom-monokai-spectrum)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package treemacs
  :config
  (setq evil-treemacs-state-cursor t
        treemacs-show-cursor t
        treemacs-width 39))

(setq display-line-numbers-type 'relative
      display-line-numbers-mode t
      line-number-mode t)

(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-delay 0.2)

(map! :leader
      :desc "Show LSP UI Doc"
      "c d" #'lsp-ui-doc-show)

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :hook ((scss-mode . lsp-deferred)
         (scss-mode . flycheck-mode)
         (scss-mode . whitespace-mode)
         (scss-mode . prettier-js-mode))
  :config
  (setq css-indent-offset 2))

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :hook ((css-mode . lsp-deferred)
         (css-mode . flycheck-mode)
         (css-mode . whitespace-mode)
         (css-mode . prettier-js-mode))
  :config
  (setq css-indent-offset 2))

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

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . lsp-deferred))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(defun kyo/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js2-mode . lsp-mode)
         (typescript-mode . lsp-mode)
         (web-mode . lsp-mode)
         (lsp-mode . kyo/lsp-mode-setup))

  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook ((js2-mode . lsp-ui-mode)
         (typescript-mode . lsp-ui-mode)
         (web-mode . lsp-ui-mode)))

(defvar my-org-todo-keywords
  '("TODO(t)"        ; A task that is ready to be tackled
    "CODE(m)"        ; Coding Tasks
    "TEST(s)"        ; Blog writing assignments
    "DEVELOP(o)"     ; Things to develop
    "MEET(5)"        ; A Meeting
    "PROYECT(p)"     ; A project that contains other tasks
    "REVIEW(r)"      ; A project that contains other tasks
    "WAIT(w)"        ; Something is holding up this task
    "|"              ; Separates active from inactive states
    "DONE(d)"        ; Task has been completed
    "CANCELLED(c)")  ; Task has been cancelled
  "List of Org todo keywords for the sequence.")

(after! org
  (setq org-directory "~/org"
        org-ellipsis " ▼ "
        org-hide-emphasis-markers t))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("" "" "" "󰺕" "󰻂" "󰪥" "󰻃")
        org-superstar-itembullet-alist '((?+ . ?➤)
                                         (?- . ?✦))))

(after! org
  (setq org-todo-keywords `((sequence ,@my-org-todo-keywords))))

(setq org-agenda-block-separator 175)

(after! org
  (setq org-agenda-files '("~/.brain.d/roam-nodes/2025-02-13-$S-work_s_org_agenda_file.org")))

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "madison-reed"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Maritz:")))
          (tags "agile-engine"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Softtek:")))
          (tags "dot-com"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Shoptron:")))
          (tags "work"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Work Tasks:")))
          (tags "meeting"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Important Meetings:")))
          (agenda "")
          (alltodo "")))))

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
   (web . t)
   ))

(setq org-babel-command:typescript "npx -p typescript -- tsc")
(setq org-confirm-babel-evaluate nil)

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  (org-mode . org-superstar-mode)
  :config
  (setq org-fancy-priorities-list '("" "󱡞" "󰝨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight regular)
     (?B :foreground "#98be65" :weight regular)
     (?C :foreground "#c678dd" :weight regular))))

(defvar my-org-roam-capture-templates
  '(
    ("d" "default" plain "%?"
     :if-new (file+head "%<%Y-%m-%d-$S>-${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t)
    ("s" "Not Time Stamp File" plain "%?"
     :if-new (file+head "${slug}.org"
                        "#+title: ${title}\n")
     :unnarrowed t)
    ("f" "New Feature Azure DevOps" plain
     (file "~/.brain.d/roam-nodes/templates/NEWNodeTemplate.org")
     :if-new (file+head "%<%Y-%m-%d-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+DESCRIPTION: %^{Description}\n#+FILETAGS: %^{File Tags}\n#+AUTHOR: %^{Author}\n")
     :unnarrowed t)
    ("i" "New Sentinel Inspection" plain
     (file "~/.brain.d/roam-nodes/templates/NEWNodeSentinelInspection.org")
     :if-new (file+head "%<%Y-%m-%d-%S>-${slug}.org"
                        "#+TITLE: Kyonax's Daily Sentinel Inspection ~ %<%d/%m/%Y> \n")
     :unnarrowed t)
    ("v" "New Invoice" plain
     (file "~/.brain.d/roam-nodes/templates/NEWNodeInvoice.org")
     :if-new (file+head "%<%Y-%m-%d-%S>-${slug}.org"
                        "#+TITLE: Invoice ${Client Name}\n#+AUTHOR: %^{Author | Cristian D. Moreno - Kyonax}\n")
     :unnarrowed t)
    ("p" "New PBI Azure DevOps" plain
     (file "~/.brain.d/roam-nodes/templates/NEWNodeProject.org")
     :if-new (file+head "%<%Y-%m-%d-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+DESCRIPTION: %^{Description}\n#+FILETAGS: %^{File Tags}\n#+AUTHOR: %^{Author}\n")
     :unnarrowed t))
  "My custom Org Roam personal capture templates.")

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

(use-package flycheck
  :init (global-flycheck-mode)
  :hook ((js2-mode . flycheck-mode)
         (typescript-mode . flycheck-mode)))

(use-package whitespace-mode
  :hook ((js2-mode . whitespace-mode)
         (typescript-mode . whitespace-mode)
         (web-mode . whitespace-mode)
         (php-mode . whitespace-mode)
         (json-mode . whitespace-mode)))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(defun kyo/run-prettier ()
  "Format the current file using the Prettier CLI if available."
  (interactive)
  (when (and buffer-file-name
             (string-match-p
              "\\.\\(js\\|jsx\\|ts\\|tsx\\|mjs\\|cjs\\|es6\\|json\\|html?\\)\\'"
              buffer-file-name))
    (let* ((project-root (locate-dominating-file buffer-file-name "node_modules"))
           (local-prettier (and project-root (expand-file-name "node_modules/.bin/prettier" project-root)))
           (prettier-bin (or (and local-prettier (file-executable-p local-prettier) local-prettier)
                             (executable-find "prettier"))))
      (when prettier-bin
        (shell-command-to-string
         (format "%s --write %s"
                 (shell-quote-argument prettier-bin)
                 (shell-quote-argument buffer-file-name)))
        (revert-buffer t t t)))))

(setq user-full-name "Cristian D. Moreno - Agile Engine"
      user-mail-address "cristian.moreno@agileengine.com")

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
