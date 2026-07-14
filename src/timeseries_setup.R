# Answers to time series exercise

data_dir <- here("data", "raw")
output_image_dir <- here("output", "images")
covid_date <- as.Date("2020-03-01")

salbutamol_sales_wide <- read_csv(file.path(data_dir, "salbutamol_sales.csv"))
