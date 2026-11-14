
rm(list = ls())

library(dplyr)

os <- Sys.info()[["sysname"]]
if ("Darwin" %in% os) {
  root_dir <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"
} else {
  root_dir <- "C:/Users/kyuhu/OneDrive/Documents/Github/pollen-respiratory-mortality"
}

# 1. Import ----------------------------------------------------------------

lagdata <- read.csv(paste0(root_dir, "/data/lagdata-copy.csv"))
fdata   <- read.csv(paste0(root_dir, "/data/fdata-copy.csv"))

cat("\nCHECK 1: imported\n")
cat("lagdata:", nrow(lagdata), "rows,", ncol(lagdata), "columns\n")
cat("fdata:  ", nrow(fdata),   "rows,", ncol(fdata),   "columns\n")

# 2. Make SuHi and SuHiout identical ---------------------------------------

i <- match(
  paste(fdata$city, gsub("/", "-", fdata$date)),
  paste(lagdata$city, gsub("/", "-", lagdata$date))
)
stopifnot(!anyNA(i))
fdata$SuHiout <- lagdata$SuHiout[i]

sync_suhi <- function(data) {
  if (!"SuHiout" %in% names(data) && !"SuHi" %in% names(data)) {
    stop("Neither SuHi nor SuHiout exists.")
  }
  
  source <- if ("SuHiout" %in% names(data)) data$SuHiout else data$SuHi
  
  data$SuHi    <- source
  data$SuHiout <- source
  data
}

lagdata <- sync_suhi(lagdata)
fdata   <- sync_suhi(fdata)

stopifnot(
  identical(lagdata$SuHi, lagdata$SuHiout),
  identical(fdata$SuHi, fdata$SuHiout)
)

cat("\nCHECK 2: SuHi and SuHiout are identical\n")
aggregate(SuHi ~ city, subset(lagdata, month %in% 2:4), max, na.rm = TRUE)
aggregate(SuHiout ~ city, subset(lagdata, month %in% 2:4), max, na.rm = TRUE)
aggregate(SuHi ~ city, fdata, max, na.rm = TRUE)
aggregate(SuHiout ~ city, fdata, max, na.rm = TRUE)
aggregate(SPM ~ city, subset(lagdata, month %in% 2:4), max, na.rm = TRUE)
aggregate(SPMout ~ city, subset(lagdata, month %in% 2:4), max, na.rm = TRUE)
aggregate(SPM ~ city, fdata, max, na.rm = TRUE)
aggregate(SPMout ~ city, fdata, max, na.rm = TRUE)

# 3. Recreate SPMout: remove top 0.1% within each city ---------------------

recreate_spmout <- function(data) {
  data %>%
    group_by(city) %>%
    mutate(
      cutoff = quantile(SPM, 0.999, na.rm = TRUE, type = 7),
      SPMout = ifelse(is.na(SPM) | SPM > cutoff, NA_real_, SPM)
    ) %>%
    ungroup() %>%
    select(-cutoff)
}

lagdata <- recreate_spmout(lagdata)
fdata   <- recreate_spmout(fdata)

cat("\nCHECK 3: SPM values removed by city\n")
aggregate(SPM ~ city, lagdata, max, na.rm = TRUE)
aggregate(SPMout ~ city, lagdata, max, na.rm = TRUE)
aggregate(SPM ~ city, fdata, max, na.rm = TRUE)
aggregate(SPMout ~ city, fdata, max, na.rm = TRUE)

# 4. Recreate lags 1-7 and moving averages 0-1 through 0-7 -----------------

recreate_lags <- function(data, prefix, max_lag = 7) {
  data <- data %>%
    mutate(
      .row_id = row_number(),
      .date_sort = as.Date(gsub("/", "-", date))
    ) %>%
    arrange(city, .date_sort) %>%
    group_by(city)
  
  for (k in 1:max_lag) {
    name <- paste0(prefix, k)
    data <- data %>%
      mutate(!!name := lag(.data[[prefix]], k))
  }
  
  data <- ungroup(data)
  
  for (k in 1:max_lag) {
    cols <- c(prefix, paste0(prefix, 1:k))
    data[[paste0(prefix, "ma0.", k)]] <-
      rowMeans(data[cols], na.rm = FALSE)
  }
  
  data %>%
    arrange(.row_id) %>%
    select(-.row_id, -.date_sort)
}

for (prefix in c("SPMout", "SuHi", "SuHiout")) {
  lagdata <- recreate_lags(lagdata, prefix)
  fdata   <- recreate_lags(fdata, prefix)
}

suhi_suffixes <- c("", 1:7, paste0("ma0.", 1:7))

stopifnot(
  all(vapply(
    suhi_suffixes,
    function(x) identical(
      lagdata[[paste0("SuHi", x)]],
      lagdata[[paste0("SuHiout", x)]]
    ),
    logical(1)
  )),
  all(vapply(
    suhi_suffixes,
    function(x) identical(
      fdata[[paste0("SuHi", x)]],
      fdata[[paste0("SuHiout", x)]]
    ),
    logical(1)
  ))
)

cat("\nCHECK 4: lags and moving averages recreated\n")
cat("SuHi and SuHiout families are identical\n")
sapply(lagdata[paste0("SPMout", 1:7)], max, na.rm = TRUE)
sapply(fdata[paste0("SPMout", 1:7)], max, na.rm = TRUE)
sapply(lagdata[paste0("SuHiout", 1:7)], max, na.rm = TRUE)
sapply(fdata[paste0("SuHioutma0.", 1:7)], max, na.rm = TRUE)
sapply(lagdata[paste0("SPMoutma0.", 1:7)], max, na.rm = TRUE)
sapply(fdata[paste0("SPMout", 1:7)], max, na.rm = TRUE)


# 5. Final Validation -----------------------------------------------------

spring_check <- function(data) {
  data %>%
    filter(month %in% 2:4) %>%
    group_by(city) %>%
    summarise(
      SPM_mean = mean(SPMout, na.rm = TRUE),
      SPM_sd = sd(SPMout, na.rm = TRUE),
      SPM_median = median(SPMout, na.rm = TRUE),
      SPM_IQR = IQR(SPMout, na.rm = TRUE),
      SPM_max = max(SPMout, na.rm = TRUE),
      pollen_mean = mean(SuHi, na.rm = TRUE),
      pollen_sd = sd(SuHi, na.rm = TRUE),
      pollen_median = median(SuHi, na.rm = TRUE),
      pollen_IQR = IQR(SuHi, na.rm = TRUE),
      pollen_p75 = quantile(SuHi, 0.75, na.rm = TRUE),
      pollen_p80 = quantile(SuHi, 0.80, na.rm = TRUE),
      pollen_p85 = quantile(SuHi, 0.85, na.rm = TRUE),
      pollen_max = max(SuHi, na.rm = TRUE),
      .groups = "drop"
    )
}

spring_check(lagdata)
spring_check(fdata)

# 6. Export ----------------------------------------------------------------

write.csv(
  lagdata, paste0(root_dir, "/data/lagdata.csv"), row.names = FALSE
)

write.csv(
  fdata, paste0(root_dir, "/data/fdata.csv"), row.names = FALSE
)

cat("\nCHECK 5: exported\n")
