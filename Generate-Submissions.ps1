if (Test-Path server.json) {
    rm server.json
}

rm *.solution.json

cmd /c "curl --user `":mlzBHPsKo5ANdYQaj73UsluHpl39mCCEidAf7obtTjk=`" https://davar.icfpcontest.org/teams/83/solutions --insecure > server.json"
1..24 | % {
    $problem = "problem_$_.json"
    sbt "run -f $problem -scores server.json -output problem_$_.solution.json"
}
