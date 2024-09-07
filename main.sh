#!/bin/zsh

cleanup_data() {
    find "$root_dir/data" -type f ! -name "lagdata.csv" -delete
    echo "All files except lagdata.csv have been deleted from /data."
}

# set constants
seasonal_df=4
temperature_df=3
root_dir="/Users/kyuhur/Documents/Github/pollen_respiratory_mortality"

# delete all data except lagdata.csv if an error occurs
trap 'cleanup_data' ERR

# make directories if they don't exist
mkdir -p "$root_dir/plots"
mkdir -p "$root_dir/data"

# preprocess data
Rscript proj/preprocessing.R 

# run models
Rscript proj/modeling.R bisection_variation=perc75 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript proj/modeling.R bisection_variation=perc80 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript proj/modeling.R bisection_variation=perc85 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript proj/modeling.R bisection_variation=abs25 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript proj/modeling.R bisection_variation=abs50 seasonal_df=$seasonal_df temperature_df=$temperature_df
Rscript proj/modeling.R bisection_variation=abs75 seasonal_df=$seasonal_df temperature_df=$temperature_df

# create plots
Rscript proj/plots.R 

# run sensitivity analysis
Rscript proj/sensitivity.R

trap - ERR  # remove trap if no error

