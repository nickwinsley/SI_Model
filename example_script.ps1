for ($i = 1; $i -lt 501; $i++) {
    & ".\read.exe" $i $i /run
    Start-Sleep -Seconds 1
}