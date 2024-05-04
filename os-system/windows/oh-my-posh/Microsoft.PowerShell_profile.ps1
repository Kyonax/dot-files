oh-my-posh init pwsh --config '~\AppData\Local\Programs\oh-my-posh\themes\negligible.omp.json' | Invoke-Expression
Import-Module -Name Terminal-Icons
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Windows
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
winfetch
figlet -c -f digital coding my world
echo ""
