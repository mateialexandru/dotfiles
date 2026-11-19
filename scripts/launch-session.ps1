#Requires -Version 7.3
[CmdletBinding(SupportsShouldProcess)]
param(
    [Parameter(Mandatory)][string]$Path,
    [string]$Command,
    [string[]]$CommandArguments = @(),
    [string]$Title
)
$ErrorActionPreference = 'Stop'
$directory = Get-Item -LiteralPath $Path -ErrorAction Stop
if ($directory.PSProvider.Name -ne 'FileSystem' -or -not $directory.PSIsContainer) {
    throw 'An existing filesystem directory is required.'
}
if (-not $Command) {
    $Command = if ($env:CLAUDE_CODE_ENTRYPOINT) { 'claude' } else { 'copilot' }
}
$executable = Get-Command $Command -CommandType Application,ExternalScript -ErrorAction Stop |
    Select-Object -First 1
if (-not $Title) { $Title = $directory.Name }
function ConvertTo-PowerShellLiteral([string]$Value) { "'" + $Value.Replace("'", "''") + "'" }
$expression = '$Host.UI.RawUI.WindowTitle = ' + (ConvertTo-PowerShellLiteral $Title) +
    '; & ' + (ConvertTo-PowerShellLiteral $executable.Source)
foreach ($argument in $CommandArguments) {
    $expression += ' ' + (ConvertTo-PowerShellLiteral $argument)
}
$encoded = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($expression))
$shell = (Get-Command pwsh -CommandType Application -ErrorAction Stop).Source
if ($PSCmdlet.ShouldProcess($directory.FullName, "Launch $Command in a new PowerShell session")) {
    $process = Start-Process -FilePath $shell -WorkingDirectory $directory.FullName `
        -ArgumentList @('-NoLogo', '-NoExit', '-EncodedCommand', $encoded) -PassThru
    Write-Output "Started session process $($process.Id) in $($directory.FullName)"
}
