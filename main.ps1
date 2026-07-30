$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

# Parameters
$seasonalDf = 4
$temperatureDf = 4

# Assumes this .ps1 file is saved in the project root
$rootDir = $PSScriptRoot

if ([string]::IsNullOrWhiteSpace($rootDir)) {
    $rootDir = (Get-Location).Path
}

# Make the root directory available inside the R scripts
$env:POLLEN_ROOT_DIR = $rootDir

# Find Rscript.exe
$rscriptCommand = Get-Command "Rscript.exe" -ErrorAction SilentlyContinue

if ($null -ne $rscriptCommand) {
    $rscript = $rscriptCommand.Source
}
else {
    # Try to find R automatically under C:\Program Files\R
    $rscript = Get-ChildItem `
        -Path (Join-Path $env:ProgramFiles "R") `
        -Filter "Rscript.exe" `
        -File `
        -Recurse `
        -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending |
        Select-Object -First 1 |
        ForEach-Object FullName
}

if ([string]::IsNullOrWhiteSpace($rscript) -or -not (Test-Path $rscript)) {
    throw @"
Rscript.exe could not be found.

Install R for Windows or add the R bin directory to PATH.
A typical location is:

C:\Program Files\R\R-4.x.x\bin\Rscript.exe
"@
}

Write-Host "Using Rscript: $rscript"
Write-Host "Project root: $rootDir"

# PowerShell's equivalent of running Rscript and stopping on failure
function Invoke-RScript {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Script,

        [string[]]$ScriptArgs = @()
    )

    Write-Host ""
    Write-Host "Running: $Script $($ScriptArgs -join ' ')"

    & $rscript $Script @ScriptArgs

    if ($LASTEXITCODE -ne 0) {
        throw "R script failed with exit code $LASTEXITCODE`: $Script"
    }
}

Push-Location $rootDir

try {
    # Make directories if they do not exist
    New-Item `
        -ItemType Directory `
        -Path (Join-Path $rootDir "plots") `
        -Force |
        Out-Null

    New-Item `
        -ItemType Directory `
        -Path (Join-Path $rootDir "data") `
        -Force |
        Out-Null

    # Preprocess data
    Invoke-RScript -Script "src/preprocessing.R"

    # Run models
    $variations = @(
        "perc75",
        "perc80",
        "perc85",
        "abs25",
        "abs50",
        "abs75"
    )

    foreach ($variation in $variations) {
        Invoke-RScript `
            -Script "src/modeling.R" `
            -ScriptArgs @(
                "bisection_variation=$variation",
                "seasonal_df=$seasonalDf",
                "temperature_df=$temperatureDf"
            )
    }

    # Run sensitivity analysis
    Invoke-RScript -Script "src/sensitivity.R"

    # Run t-test
    Invoke-RScript -Script "src/t_test.R"

    # Create plots
    Invoke-RScript -Script "src/plots.R"

    Write-Host ""
    Write-Host "All scripts completed successfully."
}
finally {
    Pop-Location
}