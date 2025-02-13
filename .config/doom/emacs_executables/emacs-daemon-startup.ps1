<#

 Emacs Daemon startup script emacs-daemon.ps1
 Cristian Moreno - Kyonax, kyo@kyo.wtf

 Reuse this Script as your wish ;D

#>

$emacs_binaries_path = 'C:\Program Files\Emacs\emacs-29.4\bin'
$emacs_server_file = '~\.emacs.d\server\local'

if (Test-Path $emacs_server_file) {
    Remove-Item $emacs_server_file
}

# Run Emacs Daemon
emacs-29.4 --bg-daemon=local
emacs-29.4 --bg-daemon=work
