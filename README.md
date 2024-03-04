# Pollen, Respiratory, and Mortality Project

## Data Cleaning

1. Obtaining raw data:
- Pollen
    - `Sugi`
    - `Hinoki`
- Air pollution
    - `SPM`
    - `PM2.5`
    - `SO2`
    - `NO2`
- Mortality
    - `all`
    - `respiratory`
    - `cardiovascular`
- Environmental Variables
    - `Relative Humidity`
    - `Temperature`
- Station geocodes
- Japan Holidays
- Asian Dust Days

2. Clean and aggregate the data
3. Create lagged and moving average values for the data
4. Create moving averages for select environmental variables
- Temperature
    - `ma05`
    - `ma07`
    - `ma10`
    - `ma20`
5. Save as three files
    - `flist.csv`
    - `sublist.csv`
    - `laglist.csv`

**WIP**

## Modeling

This project makes use of [generalized linear model](https://www.sciencedirect.com/topics/mathematics/generalized-linear-model), or `glm` for short. 

1. A `glm` equation is determined, accounting for various environmental and time variables
    - Asian dust days
    - Holidays
    - Day of week
    - Day of year
    - Year
    - Temperature Mean
    - Relative Humidity Mean
    - Date
2. Each `city`, `exposure`, `outcome`, and `lag` (0 to 7) combination is evaluated and results are recorded:
    - relative risk - `rr`
    - confidence interval lower - `cil`
    - confidence interval upper - `ciu`
3. Do the same as step 2, but add pollen (`SuHi`) as an interactive exposure to the other four exposures. Save the results as `rr`, `cil`, `ciu`.

**WIP**

## Results Aggregation

The results are then aggregated by the meta-analysis method with the R package [metafor](https://www.metafor-project.org/doku.php/metafor).

**WIP**
