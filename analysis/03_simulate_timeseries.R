# ------------------------------------------------------------------------------
# Simulates monthly salbutamol sales
# ------------------------------------------------------------------------------

set.seed(42)
output_dir <- "./data/raw"

# Monthly observations from January 2015 through December 2025
month <- seq(as.Date("2015-01-01"), as.Date("2025-12-01"), by = "month")
time <- seq_along(month)
month_number <- as.integer(format(month, "%m"))

# Sales have a gradual upward trend
trend <- 75 * time

# Sales have seasonality - higher in winter months (Dec, Jan, Feb)
winter_effect <- ifelse(month_number %in% c(12, 1, 2), 2000, 0)

# Temporary demand spike at the start of the COVID-19 pandemic
march_2020_spike <- ifelse(month == as.Date("2020-03-01"), 15000, 0)

# Simulate time series
sales <- round(
  25000 + trend + winter_effect + march_2020_spike + rnorm(length(month), 0, 1500)
)

# Compile wide data: one row and one column per year-month
salbutamol_sales <- as.data.frame(t(sales), check.names = FALSE)
names(salbutamol_sales) <- format(month, "%b-%Y")

# Export data
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(salbutamol_sales, file.path(output_dir, "salbutamol_sales.csv"), row.names = FALSE)
