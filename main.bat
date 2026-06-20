@echo off

REM Set constants
set seasonal_df=4
set temperature_df=4
set root_dir=C:\Users\kyuhur\Documents\Github\pollen-respiratory-mortality

REM Make directories if they don't exist
if not exist "%root_dir%\plots" mkdir "%root_dir%\plots"
if not exist "%root_dir%\data" mkdir "%root_dir%\data"

REM Preprocess data
Rscript.exe src/preprocessing.R
if errorlevel 1 exit /b

REM Run models
Rscript.exe src/modeling.R bisection_variation=perc75 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe src/modeling.R bisection_variation=perc80 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe src/modeling.R bisection_variation=perc85 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe src/modeling.R bisection_variation=abs25 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe src/modeling.R bisection_variation=abs50 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b

Rscript.exe src/modeling.R bisection_variation=abs75 seasonal_df=%seasonal_df% temperature_df=%temperature_df%
if errorlevel 1 exit /b


REM Run sensitivity analysis
Rscript.exe src/sensitivity.R
if errorlevel 1 exit /b

REM Create plots
Rscript.exe src/plots.R
if errorlevel 1 exit /b

