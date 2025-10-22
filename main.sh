#!/bin/zsh

# set constants
seasonal_df=4
temperature_df=4
root_dir="/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"

# make directories if they don't exist
mkdir -p "$root_dir/plots"
mkdir -p "$root_dir/data"

# preprocess data
Rscript src/preprocessing.R 

# run models
Rscript src/modeling.R bisection_variation=perc75 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript src/modeling.R bisection_variation=perc80 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript src/modeling.R bisection_variation=perc85 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript src/modeling.R bisection_variation=abs25 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript src/modeling.R bisection_variation=abs50 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript src/modeling.R bisection_variation=abs75 seasonal_df=$seasonal_df temperature_df=$temperature_df

# run sensitivity analysis
Rscript src/sensitivity.R

# create plots
Rscript src/plots.R 

