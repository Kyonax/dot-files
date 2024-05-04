$DotFiles = Get-Content './x-dotfiles.json' | ConvertFrom-JSON
$Apps = @($DotFiles.dotfiles)

$ePackage = [System.Convert]::toInt32("1F4E6",16)
Write-Host " "
Write-Host -ForegroundColor DarkYellow ([System.Char]::ConvertFromUtf32($ePackage)) -NoNewLine
Write-Host " Creating Path's Config Files and Files - "
Write-Host " "

foreach ($app in $Apps) {
    $file = $app.path
    $fileName = $app.name
    if (-not(Test-Path -Path $file -PathType Leaf)) {
        try {
            $null = New-Item -ItemType File -Path $file -Force -ErrorAction Stop

            $eCheck = [System.Convert]::toInt32("2714",16)
            Write-Host -ForegroundColor Green ([System.Char]::ConvertFromUtf32($eCheck)) -NoNewLine
            Write-Host -ForegroundColor Magenta "  [$fileName]" -NoNewLine
            Write-Host " The file" -NoNewLine
            Write-Host -ForegroundColor Yellow " [$file]" -NoNewLine
            Write-Host " has been created"
        } catch {
            throw $_.Exception.Message
        }
    } else {
        $eCancel = [System.Convert]::toInt32("274C",16)
        Write-Host -ForegroundColor Red ([System.Char]::ConvertFromUtf32($eCancel)) -NoNewLine
        Write-Host -ForegroundColor Magenta " [$fileName]" -NoNewLine
        Write-Host " Cannot create" -NoNewLine
        Write-Host -ForegroundColor Yellow " [$file]" -NoNewLine
        Write-Host " because a file with that name already exists"
    }
}

Write-Host " "
Write-Host -ForegroundColor DarkYellow ([System.Char]::ConvertFromUtf32($ePackage)) -NoNewLine
Write-Host " Copying Dot-Files in System - "
Write-Host " "

foreach ($app in $Apps) {
    $file = $app.path
    $fileName = $app.name
    $dotFile = $app.dotFile

    try {
        Copy-Item -Path $dotFile -Destination $file -Force

        $eCheck = [System.Convert]::toInt32("2714",16)
        Write-Host -ForegroundColor Green ([System.Char]::ConvertFromUtf32($eCheck)) -NoNewLine
        Write-Host -ForegroundColor Magenta "  [$fileName]" -NoNewLine
        Write-Host " The file" -NoNewLine
        Write-Host -ForegroundColor Yellow " [$dotFile]" -NoNewLine
        Write-Host " has been succesfully copied to" -NoNewLine
        Write-Host -ForegroundColor Yellow " [$file]"
    } catch {
        throw $_.Exception.Message
    }
}

Write-Host " "
