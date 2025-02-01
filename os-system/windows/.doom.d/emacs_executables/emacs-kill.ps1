<#

 Emacs Daemon startup script emacs-daemon.ps1
 Cristian Moreno - Kyonax, kyo@kyo.wtf

 Reuse this Script as your wish ;D

#>


$emacs_binaries_path = 'C:\Program Files\Emacs\emacs-29.4\bin'
$emacs_server_file = '~\.emacs.d\server\server'
$emacs_server_reference = 'server'

if ($args.Length -eq 1) {
    $emacs_server_reference = $args[0]
    $emacs_server_file = "~\.emacs.d\server\$emacs_server_reference"
}

# Run Emacs Daemon
emacsclient --server-file=$emacs_server_reference --eval "(kill-emacs)"
if (Test-Path $emacs_server_file) { Remove-Item $emacs_server_file }
