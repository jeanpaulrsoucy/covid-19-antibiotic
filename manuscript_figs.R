# Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis #
# Langford et al. #
# Script author: Jean-Paul R. Soucy #

# Reproduce manuscript figures #

# Note: This script assumes the working directory is set to the root directory of the project
# This is most easily achieved by using the provided covid-19-antibiotic.Rproj in RStudio

# How to export plots:
# Plots can be exported at any size in PDF or PNG format using RStudio

# prepare data and figures
source("global.R")

# Main result: Calculate pooled average proportion of patients prescribed antibiotics

## forest plot
forest_plot(forest_load("All"))

## alternative method (same results)
mod_antibiotic <- rma_mod(dat, "All")
extract_pooled_prop(mod_antibiotic)

# Figure 3. Antibiotic Prescribing in Patients with COVID-19 by Region
forest_plot(forest_load("Region"))

# Figure 4. Antibiotic Prescribing in Patients with COVID-19 by Age Group
forest_plot(forest_load("AgeGroup"))

# Figure 5. Antibiotic Prescribing in Patients with COVID-19 by Healthcare Setting
forest_plot(forest_load("Setting"))

# Figure 6. Antibiotic Prescribing in Patients with COVID-19 by Study Quartile of Proportion of Patients Requiring Mechanical Ventilation
forest_plot(forest_load("QuartileMechanicalVentilation"))

# Figure 7. Antibiotic Prescribing in Patients with COVID-19 by Study End Date
forest_plot(forest_load("Month"))

# Supplementary Figure 1. Forest Plot of Antibiotic Prescribing in COVID-19: All Studies Combined
forest_plot(forest_load("All"), show_ind_studies = TRUE)

# Supplementary Figure 2. Antibiotic Prescribing in COVID-19 for Studies Reporting and Not Reporting Antibiotic Classes
forest_plot(forest_load("ReportedClasses"))

# Supplementary Figure 3. Antibiotic Prescribing in COVID-19 for Studies Reporting Special (At-Risk) Populations
forest_plot(forest_load("SpecialPopulation"))

# Supplementary Figure 4. Antibiotic Prescribing in COVID-19 with and without Potentially Overlapping Study Populations
forest_plot(forest_load("OverlappingPopulation"))

# Supplementary Figure 5. Normal Quantile-Quantile Plots for the Residual Distributions of Each Univariable Meta-Regression Model

## plot histograms in a grid
par(mfrow = c(4, 4)) # 4 x 4 grid
for (i in 1:length(terms_plus_all)) {
  
  ### calculate RMA
  mod <- rma_mod(dat, terms_plus_all[i])
  
  ## extract residuals
  res <- residuals(mod)
  
  ### plot histogram
  qqnorm(res, main = terms_plus_all_names[i])
  qqline(res)
  
}
par(mfrow = c(1, 1)) # reset plotting

# Bonus Figure: Histograms for the Residual Distributions of Each Univariable Meta-Regression Model

## plot histograms in a grid
par(mfrow = c(4, 4)) # 4 x 4 grid
for (i in 1:length(terms_plus_all)) {
  
  ### calculate RMA
  mod <- rma_mod(dat, terms_plus_all[i])
  
  ### plot histogram
  hist(residuals(mod), xlab = "Residuals", main = terms_plus_all_names[i])
  
}
par(mfrow = c(1, 1)) # reset plotting

# Additional analysis: Calculate polled average proportion of patients w/ bacterial co-infection (in studies reporting this)

## subset of studies reporting proportion of patients w/ bacterial co-infection
dat_coinfection <- dat %>% filter(!is.na(coinfected_bacterial) & !is.na(coinfected_sample))

## forest plot
forest_plot(
  metaprop(
    event = coinfected_bacterial,
    n = coinfected_sample,
    studlab = study,
    data = dat_coinfection,
    method = "GLMM",
    sm = "PLOGIT"
  )
)

## alternative method (same results)
mod_coinfection <-
  rma.glmm(xi = coinfected_bacterial,
           ni = coinfected_sample,
           data = dat_coinfection,
           measure = "PLO",
           method = "ML"
  )
extract_pooled_prop(mod_coinfection)