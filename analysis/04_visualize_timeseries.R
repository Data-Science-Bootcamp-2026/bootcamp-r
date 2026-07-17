# ------------------------------------------------------------------------------
# Visualize simulated salbutamol sales
# ------------------------------------------------------------------------------
# Now it is your turn to try to write some code. In the next section (titled
# "Exercise"), follow the instructions in comments to complete the right hand
# side of the assignment operators.


# Exercise ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(readr)
library(tidyr)

### TO DO: use here() to define the relative path to the folder where
### "salbutamol_sales.csv" is stored
data_dir <- 

### TO DO: use here() to define the relative path to the folder where the output
### (which is an image) should be saved
output_image_dir <- 

### TO DO: define March 1, 2020 as a Date object
covid_date <- 

### TO DO: using data_dir, which you created above, read "salbutamol_sales.csv"
salbutamol_sales_wide <- 


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
  geom_line(color = "skyblue", linewidth = 1) +
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


# Solutions --------------------------------------------------------------------

# Uncomment the next line and run it
# file.show("src/timeseries_setup.R")
