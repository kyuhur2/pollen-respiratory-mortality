#setwd("C:/Users/iwann/OneDrive/桌面/Pollen/metafor_bisection_data")

library(dplyr)
library(tidyr)

# file suffixes
suffixes <- c(
  "abs25",
  "abs50",
  "abs75",
  "perc75",
  "perc80",
  "perc85"
)

for (suf in suffixes) {
  
  # input file name
  infile <- paste0("metafor_bisection_", suf, ".csv")
  
  # read data
  test <- read.csv(infile)
  
  # select variables
  test1 <- test[, c(1,2,9,10,11,13)]
  
  # remove NA rows
  test1 <- na.omit(test1)
  
  # convert long to wide
  test1_wide <- test1 %>%
    pivot_wider(
      id_cols = c(exposure, outcome, lag),
      names_from = quantile,
      values_from = c(B, se),
      names_sep = "_q"
    )
  
  # t-test
  test1_wide$t_value <- with(
    test1_wide,
    (B_q1 - B_q2) / sqrt(se_q1^2 + se_q2^2)
  )
  
  # p-value
  test1_wide$p_value <- 2 * (1 - pnorm(abs(test1_wide$t_value)))
  
  # output file name
  outfile <- paste0("t-test__", suf, ".csv")
  
  # write csv
  write.csv(
    test1_wide,
    file = outfile,
    row.names = FALSE
  )
  
  cat("Finished:", outfile, "\n")
}
