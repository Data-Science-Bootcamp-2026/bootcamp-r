# ------------------------------------------------------------------------------
# Visualize simulated salbutamol sales
# ------------------------------------------------------------------------------

# Instructions -----------------------------------------------------------------




# Exercise ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(readr)
library(tidyr)

data_dir <- here("data", "raw")
output_image_dir <- here("output", "images")
covid_date <- as.Date("2020-03-01")

salbutamol_sales_wide <- read_csv(
  file.path(data_dir, "salbutamol_sales.csv"),
  show_col_types = FALSE
)


# Reshape data ----------------------------------------------------------------

# The source file has one row and a column for each month. Convert it to *long
# form* so that each row is one monthly observation, as ggplot2 expects.
salbutamol_sales <- salbutamol_sales_wide %>%
  pivot_longer(
    cols = everything(),
    names_to = "month",
    values_to = "sales"
  ) %>%
  mutate(month = my(month))


# Visualize time series -------------------------------------------------------

salbutamol_plot <- ggplot(salbutamol_sales, aes(x = month, y = sales)) +
  geom_line(color = "skyblue", linewidth = 0.8) +
  geom_vline(
    xintercept = covid_date,
    linetype = "11",
    color = "dimgray",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = covid_date + 45,
    y = max(salbutamol_sales$sales) * 1.04,
    label = "COVID-19",
    color = "dimgray",
    hjust = 0
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    date_minor_breaks = "1 year"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(salbutamol_sales$sales), by = 10000),
    minor_breaks = seq(0, max(salbutamol_sales$sales), by = 5000)
  ) +
  labs(
    title = "Monthly salbutamol sales",
    x = NULL,
    y = "Sales"
  ) +
  theme_minimal()

salbutamol_plot


# Save image ------------------------------------------------------------------

if (!dir.exists(output_image_dir)) {
  dir.create(output_image_dir, recursive = TRUE)
}
ggsave(
  file.path(output_image_dir, "timeseries.png"),
  salbutamol_plot,
  width = 10,
  height = 6,
  dpi = 300
)
