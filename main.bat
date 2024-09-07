@echo off

REM Set constants
set seasonal_df=4
set temperature_df=3
set root_dir=C:\Users\kyuhur\Documents\Github\pollen_respiratory_mortality

REM Make directories if they don't exist
if not exist "%root_dir%\plots" mkdir "%root_dir%\plots"
if not exist "%root_dir%\data" mkdir "%root_dir%\data"

REM Preprocess data
Rscript.exe proj/preprocessing.R
if errorlevel 1 exit /b

REM Run models
Rscript.exe proj/modeling.R bisection_variation=perc75 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe proj/modeling.R bisection_variation=perc80 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe proj/modeling.R bisection_variation=perc85 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe proj/modeling.R bisection_variation=abs25 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe proj/modeling.R bisection_variation=abs50 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe proj/modeling.R bisection_variation=abs75 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

REM Create plots
Rscript.exe proj/plots.R
if errorlevel 1 exit /b

REM Run sensitivity analysis
Rscript.exe proj/sensitivity.R
if errorlevel 1 exit /b
