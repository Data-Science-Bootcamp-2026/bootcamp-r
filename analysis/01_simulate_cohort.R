# ------------------------------------------------------------------------------
# Simulates cohort of patients with type 2 diabetes
# ------------------------------------------------------------------------------

set.seed(42)
n <- 100
output_dir <- "./data/raw"

# Covariates
reference_date <- as.Date("2026-07-15")
birthdate <- reference_date - round(rnorm(n, mean = 60, sd = 10) * 365.25)
sex <- sample(c("Female", "Male"), n, replace = TRUE)
bmi <- round(rnorm(n, mean = 30, sd = 5), 1)
duration <- round(rnorm(n, mean = 15, sd = 3))
non_insulin <- sample(c("metformin",
                        "metformin, empagliflozin",
                        "metformin, semaglutide",
                        "metformin, empagliflozin, semaglutide"),
                      n,
                      replace = TRUE)
insulin <- sample(c("rapid-acting",
                    "long-acting",
                    "rapid-acting, long-acting"
                  ),
                  n,
                  replace = TRUE)

# Treatment effects on HbA1c (in %)
non_insulin_effect <- c(
  "metformin" = -0.4,
  "metformin, empagliflozin" = -0.7,
  "metformin, semaglutide" = -1.0,
  "metformin, empagliflozin, semaglutide" = -1.3
)[non_insulin]
insulin_effect <- c(
  "rapid-acting" = -0.6,
  "long-acting" = -0.8,
  "rapid-acting, long-acting" = -1.2
)[insulin]

# Outcome
hba1c <- round(
  6 +
  0.05 * as.numeric(reference_date - birthdate) / 365.25 +
  0.1 * bmi -
  0.15 * duration +
  non_insulin_effect +
  insulin_effect +
  rnorm(n, mean = 0, sd = 1),
  1
)

# Make 5% missing completely at random in each variable
duration[sample.int(n, size = round(0.05 * n))] <- NA
hba1c[sample.int(n, size = round(0.05 * n))] <- NA

# Cohort
cohort <- data.frame(
  id = seq_len(n),
  birthdate,
  sex,
  bmi,
  duration,
  non_insulin,
  insulin,
  hba1c
)

# Variable labels
variable_labels <- c(
  id = "patient identifier",
  birthdate = "date of birth",
  sex = "sex",
  bmi = "body mass index (kg/m²)",
  duration = "diabetes duration (years)",
  non_insulin = "non-insulin diabetes medication",
  insulin = "insulin medication",
  hba1c = "glycated hemoglobin (HbA1c, %)"
)
for (variable in names(variable_labels)) {
  attr(cohort[[variable]], "label") <- variable_labels[[variable]]
}

# Save data
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(cohort, file.path(output_dir, "cohort.csv"), row.names = FALSE)
writexl::write_xlsx(cohort, file.path(output_dir, "cohort.xlsx"))
haven::write_sas(cohort, file.path(output_dir, "cohort.sas7bdat"))
saveRDS(cohort, file.path(output_dir, "cohort.rds"))
