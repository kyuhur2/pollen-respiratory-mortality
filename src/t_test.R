#setwd("C:/Users/iwann/OneDrive/桌面/Pollen/metafor_bisection_data")

library(dplyr)
library(tidyr)

proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality/"

# file suffixes
suffixes <- c("abs25", "abs50", "abs75", "perc75", "perc80", "perc85")

for (suf in suffixes) {
  for (model in c("", "no2_", "so2_")) {
    infile <- paste0(proj, "data/metafor_bisection_", model, suf, ".csv")
    test <- read.csv(infile)
    test1 <- test[, c(1,2,9,10,11,13)]
    test1 <- na.omit(test1)
    
    # convert long to wide
    test1_wide <- test1 %>%
      pivot_wider(
        id_cols = c(exposure, outcome, lag),
        names_from = quantile,
        values_from = c(B, se),
        names_sep = "_q"
      )
    
    # t-test, p-value
    test1_wide$t_value <- with(
      test1_wide,
      (B_q1 - B_q2) / sqrt(se_q1^2 + se_q2^2)
    )
    test1_wide$p_value <- 2 * (1 - pnorm(abs(test1_wide$t_value)))
    test1_wide$bisection_method <- suf

    outfile <- paste0(proj, "data/t-test__", model, suf, ".csv")
    write.csv(
      test1_wide,
      file = outfile,
      row.names = FALSE
    )
  
    cat("Finished:", outfile, "\n")
  }
}
