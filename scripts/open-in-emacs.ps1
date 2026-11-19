#Requires -Version 7.3
param(
    [Parameter(Mandatory)][string]$Path,
    [ValidateSet('File', 'Patch', 'Status', 'Unstaged', 'Staged', 'Range')]
    [string]$View = 'File',
    [ValidateRange(1, 2147483647)][int]$Line = 1,
    [ValidateRange(1, 2147483647)][int]$Column = 1,
    [string]$Base,
    [string]$Target = 'HEAD',
    [string]$EmacsClient
)

$ErrorActionPreference = 'Stop'
$clientArguments = @('--alternate-editor=false')
if (-not $EmacsClient) {
    $command = Get-Command emacsclient.exe -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if ($command) {
        $EmacsClient = $command.Source
    } else {
        $runtimeHelper = Join-Path $PSScriptRoot 'emacs-windows.ps1'
        if (-not (Test-Path -LiteralPath $runtimeHelper -PathType Leaf)) {
            throw "Emacs runtime helper is missing: $runtimeHelper"
        }
        . $runtimeHelper
        $EmacsClient = (Get-DotfilesEmacsRuntime).Client
        if (-not $env:EMACS_SERVER_FILE) {
            $clientArguments += @('--server-file', (Join-Path $env:USERPROFILE '.config\emacs\server\server'))
        }
    }
}

function ConvertTo-ElispString([string]$Value) {
    '"' + $Value.Replace('\', '\\').Replace('"', '\"').
        Replace("`r", '\r').Replace("`n", '\n').Replace("`t", '\t') + '"'
}

$item = Get-Item -LiteralPath $Path -ErrorAction Stop
if ($item.PSProvider.Name -ne 'FileSystem') { throw 'A filesystem path is required.' }
$resolved = $item.FullName.Replace('\', '/')
$quotedPath = ConvertTo-ElispString $resolved

if ($View -in @('File', 'Patch')) {
    if ($item.PSIsContainer) { throw 'File and Patch views require an existing file.' }
    $mode = if ($View -eq 'Patch') { '(diff-mode)' } else { '' }
    $action = "(find-file $quotedPath) $mode (goto-char (point-min)) (forward-line $($Line - 1)) (move-to-column $($Column - 1))"
} else {
    if (-not $item.PSIsContainer) { throw 'Magit views require a repository directory.' }
    $repo = & git -C $item.FullName rev-parse --show-toplevel
    if ($LASTEXITCODE -ne 0) { throw 'The directory is not in a Git working tree.' }
    $repoPath = ConvertTo-ElispString (($repo | Select-Object -First 1).Replace('\', '/') + '/')
    $magitAction = switch ($View) {
        'Status' { "(magit-status $repoPath)" }
        'Unstaged' { '(magit-diff-unstaged)' }
        'Staged' { '(magit-diff-staged)' }
        'Range' {
            if (-not $Base) { throw 'Range requires -Base (and optionally -Target, default HEAD).' }
            $commits = foreach ($revision in @($Base, $Target)) {
                $commit = & git -C $item.FullName rev-parse --verify --end-of-options "${revision}^{commit}"
                if ($LASTEXITCODE -ne 0) { throw "Cannot resolve revision: $revision" }
                $commit.Trim()
            }
            '(magit-diff-range ' + (ConvertTo-ElispString ($commits -join '..')) + ')'
        }
    }
    $action = "(require 'magit) (let ((default-directory $repoPath)) $magitAction)"
}

# Explicitly disable alternate-editor fallback: never start another daemon.
$graphicalFrame = & $EmacsClient @clientArguments --eval `
    "(progn (require 'seq) (if (seq-some #'display-graphic-p (frame-list)) t nil))"
if ($LASTEXITCODE -ne 0) {
    throw 'Cannot reach the existing Emacs daemon. No daemon was started or restarted.'
}
switch (($graphicalFrame -join "`n").Trim()) {
    'nil' {
        & $EmacsClient @clientArguments --create-frame --no-wait
        if ($LASTEXITCODE -ne 0) { throw 'Emacs could not create a graphical client frame.' }
    }
    't' {}
    default { throw 'Emacs returned an unexpected graphical-frame status.' }
}
$expression = @"
(progn
  (require 'seq)
  (let ((frame (or (seq-find (lambda (f) (and (display-graphic-p f) (eq (frame-visible-p f) t))) (frame-list))
                   (seq-find (lambda (f) (display-graphic-p f)) (frame-list))
                   (error "No graphical client frame is available"))))
    (select-frame-set-input-focus frame)
    $action
    (list :opened (buffer-name) :view "$View" :daemon-pid (emacs-pid))))
"@
& $EmacsClient @clientArguments --eval $expression
if ($LASTEXITCODE -ne 0) { throw "Emacs could not open the requested $View view." }
