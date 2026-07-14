# ------------------------------------------------------------------------------
# Analyze the simulated cohort
# ------------------------------------------------------------------------------
# In this activity, we are going to construct a "Table 1" for a simulated
# dataset. To do that, we need to do some data exploration, cleaning, and
# transformation.


# Working directory ------------------------------------------------------------

# If you opened the bootcamp-r.Rproj file, then getwd() should correctly point
# to the project root
?getwd
getwd()

# You can also set the working directory to something else. This example moves
# into the raw-data directory, then restores the project root for the imports.
setwd("data/raw")
getwd()

# Now our working directory has drifted away from the project root, which may be
# undesirable. This is why I prefer to use here::here(), which searches for
# landmarks like .Rproj files.
library(here)
here()
here("data", "raw")


# Read data --------------------------------------------------------------------

# A simulated diabetes cohort has been created for you, but in various file
# types. Explore how to read (import) the different types.
data_dir <- here("data", "raw")

# Read CSV using base R
cohort_csv_base <- read.csv(file.path(data_dir, "cohort.csv"))

# Read CSV using Tidyverse's readr package
library(readr)
cohort_csv_readr <- read_csv(file.path(data_dir, "cohort.csv"))

# View the two objects by clicking on their names in the Environment pane (top
# right). They look the same. But are they the same?
identical(cohort_csv_base, cohort_csv_readr)
str(cohort_csv_base)
str(cohort_csv_readr)

# What about speed test?
system.time(read.csv(file.path(data_dir, "cohort.csv")))
system.time(read_csv(file.path(data_dir, "cohort.csv"), show_col_types = FALSE))

# Read Excel using Tidyverse's readxl package
cohort_xlsx <- readxl::read_xlsx(file.path(data_dir, "cohort.xlsx"))
str(cohort_xlsx)

# Read SAS data set using Tidyverse's haven package
cohort_sas <- haven::read_sas(file.path(data_dir, "cohort.sas7bdat"))
str(cohort_sas)

# Read R data format
cohort_rds <- readRDS(file.path(data_dir, "cohort.rds"))
str(cohort_rds)

# Compare the imported object classes. CSV is just a plain text file; Excel and
# SAS store rich metadata about the column types, so you see a small difference
# that indicates read_csv() had to guess the column type.
sapply(
  list(
    csv_base = cohort_csv_base,
    csv_readr = cohort_csv_readr,
    xlsx = cohort_xlsx,
    sas = cohort_sas,
    rds = cohort_rds
  ),
  class
)

# From here onwwards, let us use the csv_readr version to mimic real world
cohort <- cohort_csv_readr


# Exploring data ---------------------------------------------------------------

# Dimensions
nrow(cohort)
ncol(cohort)

# Variables
colnames(cohort)
head(cohort)
summary(cohort)

# Missingness
sum(is.na(cohort$duration))

# Categorical variable
unique(cohort$insulin)
length(unique(cohort$insulin))

# Using Tidyverse's dplyr package
library(dplyr)
count(cohort, sex)

# The pipe (%>%) operator below is a way to organize chains of code where one
# line's output serves as the next line's input
cohort %>%
  count(sex)


# Clean data -------------------------------------------------------------------

# There was some missingness in the outcome (HbA1c); we are going to use a
# complete case analysis, which means we drop observations with missing outcome
cohort_complete <- cohort %>%
  filter(!is.na(hba1c))
nrow(cohort_complete)

# Convert the birthdates to ages, accounting for leap years and only considering
# completed years
analysis_date <- Sys.Date()
library(lubridate)
cohort_complete <- cohort_complete %>%
  mutate(age = interval(birthdate, analysis_date) %/% years(1)) %>%
  relocate(age, .after = birthdate)

# Check your work; if you want to drop certain columns, use select()
cohort_complete <- cohort_complete %>%
  select(-id, -birthdate)

# To sort by descending outcome (HbA1c), use arrange()
cohort_complete <- cohort_complete %>%
  arrange(desc(hba1c))

# Explore the string variables using a function from another file. Note how a
# default argument works.
library(stringr)
source(here("src", "unique_drugs.R"))
find_unique_drugs
find_unique_drugs(cohort_complete, "non_insulin")
find_unique_drugs(cohort_complete, "insulin")

# Reformat the string variables into binary flags
contains_drug <- function(x, drug) {
  return(as.integer(str_detect(x, drug)))
}
cohort_complete <- cohort_complete %>%
  mutate(
    # Non-insulins
    empagliflozin = contains_drug(non_insulin, "empagliflozin"),
    metformin = contains_drug(non_insulin, "metformin"),
    semaglutide = contains_drug(non_insulin, "semaglutide"),
    # Insulins
    insulin_long = contains_drug(insulin, "long-acting"),
    insulin_rapid = contains_drug(insulin, "rapid-acting")
  )


# Explor data (again) ----------------------------------------------------------

# Plot histogram of the outcome (HbA1c) using base R
hist(
  cohort_complete$hba1c,
  breaks = seq(min(cohort_complete$hba1c), max(cohort_complete$hba1c) + 0.5, by = 0.5),
  col = "skyblue",
  border = "white",
  main = "Distribution of HbA1c",
  xlab = "HbA1c (%)"
)

# Plot histogram of the outcome (HbA1c) using Tidyverse's ggplot2 package
library(ggplot2)
ggplot(cohort_complete, aes(x = hba1c)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 4,
    fill = "skyblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of HbA1c",
    x = "HbA1c (%)",
    y = "Frequency"
  )


# Create Table 1 ---------------------------------------------------------------

# There are several popular packages for generating Table 1, including tableone,
# table1, and gtsummary. Let insulin_long be the main exposure variable.

# Prepare the variables for all three examples
table_variables <- c(
  "age", "sex", "bmi", "duration", "hba1c", "empagliflozin", "metformin",
  "semaglutide", "insulin_rapid"
)
binary_variables <- c(
  "empagliflozin", "metformin", "semaglutide", "insulin_rapid"
)
cohort_table1 <- cohort_complete %>%
  mutate(
    sex = factor(sex),
    across(
      all_of(binary_variables),
      ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
    ),
    insulin_long = factor(
      insulin_long,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# tableone package (plain output)
library(tableone)
tableone_result <- CreateTableOne(
  vars = table_variables,
  strata = "insulin_long",
  data = cohort_table1,
  factorVars = binary_variables,
  includeNA = TRUE
)
print(tableone_result, showAllLevels = TRUE, missing = TRUE, test = FALSE, smd = TRUE)


# table1 package (SMD is not supported)
library(table1)
table1_result <- table1(
  ~ age + sex + bmi + duration + hba1c + empagliflozin + metformin +
    semaglutide + insulin_rapid | insulin_long,
  data = cohort_table1,
  overall = FALSE,
  render.continuous = "Mean (SD)",
  render.missing = "FREQ (PCT%)"
)
table1_result


# gtsummary package (formulaic; note: does not need factors)
library(gtsummary)
gtsummary_result <- cohort_table1 %>%
  tbl_summary(
    by = insulin_long,
    include = all_of(table_variables),
    type = all_categorical() ~ "categorical",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany",
    missing_stat = "{N_miss} ({p_miss}%)",
    missing_text = "Missing"
  ) %>%
  add_difference(test = everything() ~ "smd") %>%
  modify_header(estimate ~ "**SMD**") %>%
  modify_column_hide(c(conf.low, conf.high)) %>%
  remove_abbreviation("CI = Confidence Interval")
gtsummary_result


# Save data --------------------------------------------------------------------

output_data_dir <- here("data", "processed")
if (!dir.exists(output_data_dir)) {
  dir.create(output_data_dir, recursive = TRUE)
}
write_csv(cohort_complete, file.path(output_data_dir, "cohort_complete.csv"))

output_image_dir <- here("output", "images")
if (!dir.exists(output_image_dir)) {
  dir.create(output_image_dir, recursive = TRUE)
}
library(gt)
gtsummary_result %>%
  as_gt() %>%
  gtsave(file.path(output_image_dir, "table1.png"))
