<#

 Emacs Daemon startup script emacs-daemon.ps1
 Cristian Moreno - Kyonax, kyo@kyo.wtf

 I’ve reused a lot of this from Emacs users
 who shared their work online.
 Thanks for saving me the trouble! :)

#>

$emacs_server_reference = $args[0]
$emacs_binaries_path = 'C:\Program Files\Emacs\emacs-29.4\bin'
$emacs_server_file = '~\.config\emacs\server\server'
$emacs_exe_file = 'emacs-29.4'

if ($args.Length -gt 0) {
    $emacs_server_reference = $args[0]
    $emacs_server_file = "~\.config\emacs\server\$emacs_server_reference"
}

if (Test-Path $emacs_server_file) {
    $first_line = Get-Content $emacs_server_file -First 1
    $server_pid = $first_line -split ' ' | Select-Object -Index 1

    ## Check if a process with the Server PID is running
    ## and is named as the emacs_exe_file
    $process = Get-Process -Id $server_pid -ErrorAction SilentlyContinue
    if ($process -and $process.ProcessName -eq $emacs_exe_file) {
        if($args.Length -gt 0) {
            $emacs_arguments = $args[1..($args.Length - 1)]
            emacsclient --server-file=$emacs_server_reference -c $emacs_arguments
        } else {
            throw "Error: Usage emacs-client <server-file> <commands>"
        }
    } else {
        Remove-Item $emacs_server_file
    }
}
