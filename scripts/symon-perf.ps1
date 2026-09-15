param([int]$Interval = 4)

$total = [math]::Round((Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory / 1MB)

while ($true) {
    $samples = (Get-Counter '\Processor(_Total)\% Processor Time',
                             '\Memory\Available MBytes',
                             '\Network Interface(*)\Bytes Received/sec',
                             '\Network Interface(*)\Bytes Sent/sec').CounterSamples
    $cpu = [math]::Round($samples[0].CookedValue)
    $mem = [math]::Round(100 * (1 - $samples[1].CookedValue / $total))
    $rx = [math]::Round(($samples | Where-Object { $_.Path -like '*bytes received/sec' } |
           Measure-Object -Property CookedValue -Sum).Sum / 1024)
    $tx = [math]::Round(($samples | Where-Object { $_.Path -like '*bytes sent/sec' } |
           Measure-Object -Property CookedValue -Sum).Sum / 1024)
    Write-Output "cpu:$cpu mem:$mem rx:$rx tx:$tx"
    Start-Sleep -Seconds $Interval
}
