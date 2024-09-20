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
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
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
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
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

(setq user-full-name "Kyonax - Cristian Moreno"
      user-mail-address "cristian@kyo.wtf")

(set-buffer-file-coding-system 'unix)

(after! org
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/org"
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("" "" "" "󰺕" "󰻂" "󰪥" "󰻃")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-hide-emphasis-markers t
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "CONTENT(n)"        ; Content to accomplish
           "STREAM(s)"         ; Stream planned
           "WEB(e)"            ; Website Tasks
           "CODE(m)"           ; Coding Tasks
           "STORY(y)"          ; Storytelling TODO
           "TEST(c)"           ; Blog writing assignments
           "DEVELOP(d)"        ; Things to develop
           "DAILY(a)"          ; A Daily Task
           "MONDAY(1)"         ; The Tasks of the Monday
           "ODDT(3)"           ; The Wednesday and Friday Tasks
           "WEEKLY(k)"         ; A Weekly Task
           "DATE(4)"         ; A Weekly Task
           "EVENT(5)"         ; A Weekly Task
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROYECT(p)"           ; A project that contains other tasks
           "REVIEW(r)"         ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))) ; Task has been cancelled                        )

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun dt/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'dt/insert-auto-tangle-tag)

(after! org
  (setq org-agenda-files '("~/.brain.d/roam-nodes/20240912084617-agenda.org")))

(setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("🐲" "🐉" "🀄")
   ;; org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-agenda-block-separator 175)

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
          (tags "maritz"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Maritz:")))
          (tags "softtek"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Softtek:")))
          (tags "shoptron"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks for Shoptron:")))
          (tags "homea"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#A] Home Daily Tasks:")))
          (tags "homeb"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#B] Home Wed-Fri Day Tasks:")))
          (tags "homec"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#C] Home Weekly Tasks:")))
          (tags "health"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Family Health:")))
          (tags "work"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Work Tasks:")))
          (tags "kyo"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Kyonax's Projects:")))
          (tags "event"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Important Events:")))
          (agenda "")
          (alltodo "")))))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (typescript . t)
   (javascript . t)
   (emacs-lisp . t)
   (js . t)
   (json . t)
   (php . t)
   ))

(setq org-babel-command:typescript "npx -p typescript -- tsc")
(setq org-confirm-babel-evaluate nil)

(setq bookmark-default-file "~/.brain.d/bookmarks/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(setq doom-theme 'doom-molokai)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-mode t)
(setq line-number-mode t)

(setq doom-font (font-spec :family "SpaceMono Nerd Font Mono" :size 11)
      doom-variable-pitch-font (font-spec :family "SpaceMono Nerd Font Mono" :size 11)
      doom-big-font (font-spec :family "SpaceMono Nerd Font Mono" :size 18))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

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

(after! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory "~/.brain.d/roam-nodes")
  (setq org-roam-dailies-directory "~/.brain.d/agenda")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
            :if-new (file+head "%<%Y-%m-%d-$S>-${slug}.org"
                               "#+title: ${title}\n") :unnarrowed t)
          ("s" "Not Time Stamp File" plain "%?"
            :if-new (file+head "${slug}.org"
                               "#+title: ${title}\n") :unnarrowed t)
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
          ("p" "New PBI Azure DevOps" plain
           (file "~/.brain.d/roam-nodes/templates/NEWNodeProject.org")
           :if-new (file+head "%<%Y-%m-%d-%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+DESCRIPTION: %^{Description}\n#+FILETAGS: %^{File Tags}\n#+AUTHOR: %^{Author}\n")
           :unnarrowed t))))

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'powerline)
(powerline-default-theme)

(beacon-mode 1)

(after! ccls
  (setq ccls-executable "C:/ProgramData/chocolatey/bin/ccls.exe")
  (set-lsp-priority! 'ccls 0))

(after! lsp-tailwindcss
  (setq lsp-tailwindcss-major-modes '(typescript-tsx-mode rjsx-mode web-mode html-mode css-mode svelte-mode)))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

(set-face-attribute 'mode-line nil :font "SpaceMono Nerd Font Mono")
(setq doom-modeline-support-imenu t ;;
      doom-modeline-buffer-state-icon t ;;
      doom-modeline-icon t ;;
      doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(after! treemacs
  (setq evil-treemacs-state-cursor t
        treemacs-show-cursor t
        treemacs-width 40))

(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

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
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Ivy switch view" "v s" #'ivy-switch-view))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

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

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "kyonax_on_tech - is.kyonax")))

                (defun my-weebery-is-always-greater ()
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

(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)

(custom-set-faces!
  '(doom-dashboard-banner :foreground "#9FE573" :background "#000000" :weight bold)
  '(doom-dashboard-loaded :foreground "#9FE573" :background "#000000" :weight bold))

(setq password-cache-expiry nil)

(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

(defun open-directory (dir)
  "Open the specified directory DIR in Dired mode."
  (interactive "DChoose directory: ") ; Prompt for directory
  (dired dir))

(defun shoptron ()
  "Open the Shoptron main Directory"
  (interactive)
  (open-directory "/plinkx:Shoptron:~/html"))

(defun shtheme ()
  "Open the ShoptronTheme main Directory"
  (interactive)
  (open-directory "/plinkx:Shoptron:~/html/custom/plugins/ShoptronTheme"))

(defun shconfigurator ()
  "Open the ShoptronConfiurator main Directory"
  (interactive)
  (open-directory "/plinkx:Shoptron:~/html/custom/plugins/ShoptronConfigurator"))

(require 'server)
(setq server-use-tcp t
      server-socket-dir "~/.emacs.d/server")
(unless (server-running-p)
    (server-start))
