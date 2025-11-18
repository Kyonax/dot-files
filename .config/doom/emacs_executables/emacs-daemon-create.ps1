<#

 Emacs Daemon startup script emacs-daemon.ps1
 Cristian Moreno - Kyonax, kyo@kyo.wtf

 Reuse this Script as your wish ;D

#>

if ($args.Length -eq 1) {
    $emacs_server_reference = $args[0]
    $emacs_binaries_path = 'C:\Program Files\Emacs\emacs-29.4\bin'
    $emacs_server_file = "~\.emacs.d\server\$emacs_server_reference"

    if (Test-Path $emacs_server_file) {
        emacsclient --server-file=$emacs_server_reference --eval "(kill-emacs)"
        Remove-Item $emacs_server_file
    }

    # Run Emacs Daemon
    emacs-29.4 --bg-daemon=$emacs_server_reference
} else {
    throw "Error: Usage emacs-kill <server-file>"
}
