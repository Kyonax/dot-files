# config.nu
#
# Installed by:
# version = "0.108.0"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# Nushell sets "sensible defaults" for most configuration settings,
# so your `config.nu` only needs to override these defaults if desired.
#
# You can open this file in your default editor using:
#     config nu
#
# You can also pretty-print and page through the documentation for configuration
# options using:
#     config nu --doc | nu-highlight | less -R

$env.STARSHIP_USER = "kyonax"

# ~/.config/nushell/config.nu
$env.LANG = "en_US.UTF-8"
$env.EMACS = "/Applications/Emacs.app/Contents/MacOS/Emacs"
$env.EMACSLOADPATH = "/Applications/Emacs.app/Contents/Resources/lisp"
$env.EMACSDATA = "/Applications/Emacs.app/Contents/Resources/etc"
$env.EMACSLIBEXEC = "/Applications/Emacs.app/Contents/MacOS/libexec"

mkdir ($nu.data-dir | path join "vendor/autoload")
starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")
fastfetch
