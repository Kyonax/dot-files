;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! beacon)

(package! bibtex-completion)
(package! parsebib)
;;(package! ivy-bibtex)

(package! calfw)
(package! calfw-org)
(package! org-gcal)

(package! cape)

(package! clippy)

(package! corfu)
(package! nerd-icons-corfu)
(package! orderless)
(package! corfu-terminal)

(package! drag-stuff)

(package! dired-open)

(package! dired-subtree)

(package! editorconfig)

(package! elfeed-goodies)

(package! emojify)

(package! evil-tutor)
(package! evil-nerd-commenter)

(package! flx)

(package! treesit-fold)

(package! gptel)
(package! gptel-quick)
(package! gptel-magit)

(package! helm)
(package! helm-company)
(package! helm-c-yasnippet)
(package! helm-descbinds)
(package! helm-describe-modes)
(package! helm-flx)
(package! helm-icons)
(package! helm-org)
(package! helm-posframe)
(package! helm-projectile)
(package! helm-rg)
(package! helm-swoop)

(package! opencl-mode)
(package! ccls)

(package! flycheck-cask)

(package! cdlatex)

(package! grip-mode)
(package! markdown-ts-mode)

(package! org-fancy-priorities)
(package! org-superstar)
(package! org-contacts)
(package! org-download)
(package! gnuplot)
(package! gnuplot-mode)
(package! ox-hugo)
(package! jupyter)
(package! org-noter)
(package! ox-pandoc)
(package! org-passwords)
(package! org-pomodoro)
(package! centered-window)
(package! org-tree-slide)
(package! org-re-reveal)
(package! org-appear)
(package! org-modern)
(package! org-roam)

(package! restclient-jq)
(package! jq-mode)

(package! swift-ts-mode)
(package! lsp-sourcekit)

(package! counsel-css)
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! rainbow-mode)
(package! sass-mode)
(package! slim-mode)
(package! stylus-mode)
(package! sws-mode)
(package! web-mode)
(package! yaml-mode)

(when (modulep! :tools lookup +docsets)
  (package! dash-docs)
  (package! counsel-dash))

(package! osx-dictionary)

(package! lsp-mode)
(package! lsp-ui)
;;(package! lsp-ivy)

(package! evil-magit)
(package! forge)
(package! code-review)

(package! osx-trash)

(package! multiple-cursors)

(package! olivetti)

(package! orderless)

(package! org-auto-tangle)

(package! org-bullets)

(package! org-web-tools)

(package! password-store)

(package! peep-dired)

(package! rainbow-mode)

(package! resize-window)

(package! flycheck-posframe)

(package! tldr)

(package! lsp-treemacs)

(package! wc-mode)

(package! web-mode)

(package! winum)

(package! visual-fill-column)

(unpin! org-roam company-org-roam)
(package! org-roam-ui)

(package! yasnippet-capf)
