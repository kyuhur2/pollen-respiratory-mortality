
# import
proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality/"
x <- read.csv(paste0(proj, "data/data_abs25.csv"))

# check which give just 2 levels
for (city in unique(x$city)) {
  cat("\n", city, "\n")
  print(table(x[x$city == city, "pol_0_bisection"], useNA = "ifany"))
}

pol_cols <- c(
  "pol_0_bisection", "pol_1_bisection", "pol_2_bisection",
  "pol_3_bisection", "pol_4_bisection", "pol_5_bisection",
  "pol_ma1_bisection", "pol_ma2_bisection", "pol_ma3_bisection",
  "pol_ma4_bisection", "pol_ma5_bisection"
)

for (city in unique(x$city)) {
  for (col in pol_cols) {
    vals <- unique(na.omit(x[x$city == city, col]))
    if (length(vals) < 2) {
      cat(city, col, "only has level:", vals, "\n")
    }
  }
}

# check pollen range / summary
summary(x$pol_0)
range(x$pol_0, na.rm = TRUE)

for (city in unique(x$city)) {
  cat("\n", city, "\n")
  print(summary(x[x$city == city, "pol_0"]))
  print(range(x[x$city == city, "pol_0"], na.rm = TRUE))
}

# see if cubic transform back to original (>10,000 as max)
summary(x$pol_0)
summary(x$pol_0^3)

range(x$pol_0, na.rm = TRUE)
range(x$pol_0^3, na.rm = TRUE)

table(x$pol_0^3 > 25, useNA = "ifany")
table(x$pol_0^3 > 50, useNA = "ifany")
table(x$pol_0^3 > 75, useNA = "ifany")

# reverse cubic transform
input_path <- file.path(proj, "data", "laglist.csv")
backup_path <- file.path(proj, "data", "laglist_cube_root_backup.csv")
laglist <- read.csv(input_path)

write.csv(laglist, backup_path, row.names = FALSE)
pollen_cols <- c(
  "SuHi",
  "SuHiout",
  paste0("SuHi", 1:7),
  paste0("SuHima0.", 1:7),
  paste0("SuHiout", 1:7),
  paste0("SuHioutma0.", 1:7),
  "SuHi05",
  "SuHi05out",
  paste0("SuHi05", 1:7),
  paste0("SuHi05ma0.", 1:7),
  paste0("SuHi05out", 1:7),
  paste0("SuHi05outma0.", 1:7)
)

pollen_cols <- intersect(pollen_cols, names(laglist))
laglist[pollen_cols] <- lapply(laglist[pollen_cols], function(x) x^3)

write.csv(laglist, input_path, row.names = FALSE)

# check laglist
x <- read.csv(paste0(proj, "data/laglist.csv"))

summary(x$SuHiout)
range(x$SuHiout, na.rm = TRUE)

# check only feb-april for laglist
y <- x[x$month %in% c(2, 3, 4), ]

cat("Rows in full laglist:", nrow(x), "\n")
cat("Rows in Feb-Apr:", nrow(y), "\n\n")
cat("SuHiout NAs in full laglist:", sum(is.na(x$SuHiout)), "\n")
cat("SuHiout NA rate in full laglist:", round(mean(is.na(x$SuHiout)), 4), "\n\n")
cat("SuHiout NAs in Feb-Apr:", sum(is.na(y$SuHiout)), "\n")
cat("SuHiout NA rate in Feb-Apr:", round(mean(is.na(y$SuHiout)), 4), "\n\n")
summary(y$SuHiout)
range(y$SuHiout, na.rm = TRUE)

# check after
x <- read.csv(paste0(proj, "data/data_abs25.csv"))

z <- x[x$city == "Fukuoka", ]

table(z$pol_0_bisection, useNA = "ifany")
table(z$spm_0, useNA = "ifany")
table(z$holiday, useNA = "ifany")
table(z$dow, useNA = "ifany")
table(z$ad, useNA = "ifany")

# check fukuoka only
z <- x[x$city == "Fukuoka", ]

summary(z$pol_0)
range(z$pol_0, na.rm = TRUE)
table(z$pol_0 > 25, useNA = "ifany")
table(z$pol_0_bisection, useNA = "ifany")

# check miyazaki
proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

x <- read.csv(file.path(proj, "data/data_abs75.csv"))
z <- x[x$city == "Miyazaki", ]
vars <- c(
  "resp",
  "spm_ma3",
  "pol_ma3_bisection",
  "ad",
  "holiday",
  "dow",
  "doy",
  "year",
  "tave07",
  "date",
  "rhave"
)

m <- z[complete.cases(z[, vars]), vars]

cat("Rows before complete-case filtering:", nrow(z), "\n")
cat("Rows after complete-case filtering:", nrow(m), "\n\n")

cat("pol_ma3_bisection:\n")
print(table(m$pol_ma3_bisection, useNA = "ifany"))

cat("\nresp summary:\n")
print(summary(m$resp))

cat("\nspm_ma3 summary:\n")
print(summary(m$spm_ma3))

cat("\ntave07 summary:\n")
print(summary(m$tave07))

cat("\nrhave summary:\n")
print(summary(m$rhave))

cat("\nAny non-finite numeric values?\n")
numeric_cols <- names(m)[sapply(m, is.numeric)]
print(sapply(m[numeric_cols], function(v) sum(!is.finite(v))))

# check fdata.csv

proj <- "/Users/kyuhur/Documents/Github/pollen-respiratory-mortality"

check_file <- function(path, required_cols) {
  cat("\n==============================\n")
  cat("FILE:", path, "\n")
  cat("==============================\n")

  if (!file.exists(path)) {
    cat("MISSING FILE\n")
    return(invisible(NULL))
  }

  x <- read.csv(path, check.names = TRUE)

  cat("Rows:", nrow(x), "\n")
  cat("Cols:", ncol(x), "\n\n")

  missing <- setdiff(required_cols, names(x))
  present <- intersect(required_cols, names(x))

  cat("Present required columns:\n")
  print(present)

  cat("\nMissing required columns:\n")
  print(missing)

  cat("\nPossible matching columns:\n")
  for (col in missing) {
    pattern <- gsub("\\.", ".*", col)
    hits <- grep(pattern, names(x), ignore.case = TRUE, value = TRUE)
    cat(col, "=>", paste(hits, collapse = ", "), "\n")
  }

  invisible(x)
}

# What plots.R expects from fdata.csv
fdata_required <- c(
  "city", "date", "month",
  "all", "circ", "resp",
  "all.age65", "circ.age65", "resp.age65",
  "SPM", "SuHi", "SO2", "NO2", "Tave", "RHave"
)

fdata <- check_file(
  file.path(proj, "data/fdata.csv"),
  fdata_required
)

cat("\n\nActual fdata.csv names:\n")
print(names(fdata))

cat("\n\nClasses of likely mortality columns:\n")
possible_cols <- c(
  "all", "All",
  "circ", "Cardiovascular",
  "resp", "Respiratory",
  "all.age65", "All..Above.65.",
  "circ.age65", "Cardiovascular..Above.65.",
  "resp.age65", "Respiratory..Above.65."
)

possible_cols <- intersect(possible_cols, names(fdata))
print(sapply(fdata[possible_cols], class))

cat("\n\nFirst few likely mortality columns:\n")
print(head(fdata[possible_cols]))

