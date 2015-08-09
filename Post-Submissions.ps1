Get-ChildItem *.solution.json | % {
    $command = "curl --user :mlzBHPsKo5ANdYQaj73UsluHpl39mCCEidAf7obtTjk= -X POST -H `"Content-Type: application/json`" -d @$_  https://davar.icfpcontest.org/teams/83/solutions --insecure"

    Write-Host $command
    cmd /c $command
}
