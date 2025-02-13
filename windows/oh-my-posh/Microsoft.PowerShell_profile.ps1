oh-my-posh init pwsh --config 'C:\Users\MORENOC0\AppData\Local\Programs\oh-my-posh\themes\negligible.omp.json' | Invoke-Expression
Set-PSReadLineOption -PredictionViewStyle ListView
winfetch
figlet -c -f digital coding my world
echo ""
#Commands Shortcuts for Powershell
function Update-Apps {
    choco upgrade all -y
    scoop update --all
    winget update --all
    winfetch
    figlet -c -f digital everything is now updated
    echo ""
}

function Docker-Shoptron {
    docker exec -it shoptron bash
}
Set-Alias shoptron Docker-Shoptron
Set-Alias upall Update-Apps
# Import the Chocolatey Profile that contains the necessary code to enable
# tab-completions to function for `choco`.
# Be aware that if you are missing these lines from your profile, tab completion
# for `choco` will not function.
# See https://ch0.co/tab-completion for details.
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
