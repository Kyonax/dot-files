(setq user-full-name "Kyonax - Cristian Moreno"
      user-mail-address "kyonax25@gmail.com")

(set-buffer-file-coding-system 'unix)

(after! org
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/.brain.d/org-mode"
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-hide-emphasis-markers t
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "DAILY(a)"          ; A Daily Task
             "MONDAY(1)"         ; The Tasks of the Monday
             "ODDT(3)"           ; The Wednesday and Friday Tasks
             "WEEKLY(k)"         ; A Weekly Task
             "CITA(4)"         ; A Weekly Task
             "EVENTO(5)"         ; A Weekly Task
             "CONTENT(n)"        ; Content to accomplish
             "STREAM(s)"         ; Stream planned
             "WEB(e)"            ; Website Tasks
             "CODE(m)"           ; Coding Tasks
             "STORY(y)"          ; Storytelling TODO
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
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
  (setq org-agenda-files '("~/.brain.d/agenda/agenda.org")))

(setq
   org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("🐲" "🐉" "🀄")
   ;; org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-agenda-block-separator 175)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         (
          (tags "homea"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#A] Home Daily Tasks:")))
          (tags "homeb"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#B] Home Wed-Fri Day Tasks:")))
          (tags "homec"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "[#C] Home Weekly Tasks:")))
          (tags "salud"
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
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "High-priority Unfinished Tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Medium-priority Unfinished Tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done 'wait))
                 (org-agenda-overriding-header "Low-priority Unfinished Tasks:")))

          (agenda "")
          (alltodo "")))))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🐲" "🐉" "🀄")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight regular)
     (?B :foreground "#98be65" :weight regular)
     (?C :foreground "#c678dd" :weight regular))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   ))

(setq org-babel-command:typescript "npx -p typescript -- tsc")

(setq bookmark-default-file "~/.brain.d/bookmarks/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(setq doom-theme 'doom-horizon)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-mode t)
(setq line-number-mode t)

(setq doom-font (font-spec :family "JetBrainsMono NF" :size 11)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono NF" :size 14)
      doom-big-font (font-spec :family "JetBrainsMono NF" :size 24))
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
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n") :unnarrowed t)
          ("s" "Not Time Stamp File" plain "%?"
            :if-new (file+head "${slug}.org"
                               "#+title: ${title}\n") :unnarrowed t)
          ("n" "new org document" plain
           (file "~/.brain.d/roam-nodes/templates/NEWNodeTemplate.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+subtitle: %^{Subtitle}\n#+description: %^{Desctiption}\n#+filetags: %^{File Tags}\n#+author: %^{Author}\n#+date: %U\n")
           :unnarrowed t)
          ("p" "new org project" plain
           (file "~/.brain.d/roam-nodes/templates/NEWNodeProject.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+subtitle: %^{Subtitle}\n#+description: %^{Description}\n#+filetags: %^{File Tags}\n#+author: %^{Author}\n#+date: %U\n")
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
  (setq ccls-executable "/path/to/shell/wrapper")
  (set-lsp-priority! 'ccls 0))

(after! lsp-tailwindcss
  (setq lsp-tailwindcss-major-modes '(typescript-tsx-mode rjsx-mode web-mode html-mode css-mode svelte-mode)))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

(set-face-attribute 'mode-line nil :font "MesloLGL Nerd Font")
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
      '((swiper                     . ivy-posframe-display-at-window-center)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-posframe-display-at-window-center)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-posframe-display-at-window-center)
        (counsel-recentf            . ivy-posframe-display-at-window-center)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-window-center)
        (t                          . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Search in Current Directory" "s d" #'+ivy/project-search-from-cwd
       :desc "Search in Project" "s p" #'+ivy/project-search
       :desc "Ivy switch view" "v s" #'ivy-switch-view))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

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
