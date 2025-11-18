;;; misc.el -*- lexical-binding: t; -*-

;; Config Stuff
(map! :leader :desc "Upgrade Doom Emacs on Rails" "h U" #'upgrade-doom-emacs-on-rails)
(map! :leader :desc "Visit handbook" "f h" #'visit-handbook)

(defun visit-handbook ()
  "Visit the user-settings.el."
  (interactive)
  (find-file (concat doom-private-dir "docs/emacs-handbook.org"))
  (message "Welcome to Doom Emacs Handbook!")
  (read-only-mode))


(add-to-list '+doom-dashboard-menu-sections '("Open Doom Emacs on Rails Handbook"
                                              :icon (all-the-icons-octicon "ruby" :face 'doom-dashboard-menu-title)
                                              :action visit-handbook) t)

(add-to-list '+doom-dashboard-menu-sections '("Upgrade Doom Emacs On Rails"
                                              :icon (all-the-icons-octicon "cloud-upload" :face 'doom-dashboard-menu-title)
                                              :action upgrade-doom-emacs-on-rails) t)




(defun upgrade-doom-emacs-on-rails ()
  "Pull, Sync and upgrade el file."
  (interactive)
  (message "Upgrading... Please wait...")
  (package-refresh-contents)
  (+vterm--create-term-with-command (concat "cd " doom-private-dir "; git pull -f; " doom-emacs-dir "bin/doom sync -u; echo 'update complete.  Press enter to close.'; read; exit") "Doom On Emacs On Rails - Upgrade")
  (message "Upgrade done!  Please restart your emacs."))

(defun visit-config-utils ()
  "Visit the config.el."
  (interactive)
  (find-file (concat doom-private-dir "user/config.el"))
  (message "Welcome to your settings file!"))

(defun visit-user-packages ()
  "Visit the packages.el."
  (interactive)
  (find-file (concat doom-private-dir "user/packages.el"))
  (message "Welcome to your packages file!"))

(defun visit-user-init ()
  "Visit the init.el."
  (interactive)
  (find-file (concat doom-private-dir "user/init.el"))
  (message "Welcome to your init file!"))

(map! :leader :desc "Visit User Config" "fm" 'visit-config-utils)
(map! :leader :desc "Visit User Init" "fi" 'visit-user-init)
(map! :leader :desc "Visit User Packages" "fI" 'visit-user-packages)

(when (not (file-exists-p "~/.pryrc")) (shell-command "cp ~/.config/doom/user/examples/.pry-example ~/.pryrc"))
(when (not (file-exists-p "~/.irbrc")) (shell-command "cp ~/.config/doom/user/examples/.irbrc-example ~/.irbrc"))

(defun open-mr-miyagi ()
  (interactive)
  (load (expand-file-name "modules/libraries/miyagi.el" doom-private-dir))
  (mr-miyagi))

(defun reload-user-settings ()
  "Pull, Sync and upgrade el file"
  (interactive)
  (load (expand-file-name "user/config.el" doom-private-dir))
  (doom/reload-font)
  (doom/reload-theme))
