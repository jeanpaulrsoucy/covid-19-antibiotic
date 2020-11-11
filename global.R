# load libraries

## meta-analysis and meta-regression
library(meta)
library(metafor)

## data processing
library(dplyr)
library(tidyr)
library(lubridate)
library(broom)
library(stringi)
library(boot)

## plotting
library(ggplot2)
library(plotly)

## dashboard
library(shiny)
library(shinydashboard)
library(metathis)

# load data
dat <- read.csv("data/antibiotic.csv", header = TRUE, stringsAsFactors = FALSE)

# process data

## create new variables / modify variables
dat <- dat %>%
  mutate(
    Region = factor(Region, levels = c("China", "Middle East", "East/Southeast Asia (ex-China)", "Europe", "North America", "Multiple")),
    Setting = factor(Setting, levels = c("Hospital", "Hospital ICU", "Hospital/Outpatient")),
    Month = factor(month(as.Date(Study_End, "%m/%d/%Y"), label = TRUE), ordered = FALSE),
    SpecialPopulation = factor(ifelse(Special.Population == "", "No", "Yes")),
    OverlappingPopulation = factor(ifelse(Overlap == "N", "No", "Yes")),
    ReportedClasses = factor(ifelse(Report_Classes == "No", "No", "Yes")),
    AgeGroup = factor(ifelse(Age.Group %in% c("Not reported", "not specfied", "not specified", "not stated"), "Not specified", Age.Group), levels = c("Adults", "Children", "All", "Not specified")),
    QuartileMechanicalVentilation = factor(cut(mech_vent, 
                                               breaks = quantile(mech_vent, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
                                               include.lowest = TRUE), labels = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile")),
    haq_score = as.integer(ifelse(haq_score == "#N/A", NA, haq_score)),
    Age = as.integer(ifelse(Age == "not reported", NA, Age))
  )
levels(dat$Month) <- c(levels(dat$Month), "Not specified")
levels(dat$QuartileMechanicalVentilation) <- c(levels(dat$QuartileMechanicalVentilation), "Not specified")

## replace NAs
dat <- dat %>%
  replace_na(
    list(Month = "Not specified",
         QuartileMechanicalVentilation = "Not specified")
  )

## convert proportion variables to %
dat <- dat %>%
  mutate(
    percent_female = X..female * 100,
    percent_vent = X.Mech_Vent * 100,
    percent_smoker = X.smokers * 100,
    percent_copd = X.COPD * 100,
    percent_cvd = X.CVD * 100,
    percent_diabetes = X.DM * 100,
    percent_deaths = X.Deaths * 100
  )

# define functions

## run rma.glmm model w/ or w/out moderator
rma_mod <- function(dat, term) {
  if (term == "All") {
    rma.glmm(
      xi = abx,
      ni = abx_sample,
      data = dat,
      measure = "PLO",
      method = "ML")
  } else {
    rma.glmm(
      xi = abx,
      ni = abx_sample,
      data = dat,
      measure = "PLO",
      method = "ML",
      mods = as.formula(paste0("~", term)))
  }
}

## extract pooled proportion and 95% CI from rma.glmm model
extract_pooled_prop <- function(mod) {
  
  ## extract values
  prop <- sprintf("%.1f", inv.logit(mod$b) * 100)
  prop_lb <- sprintf("%.1f", inv.logit(mod$ci.lb) * 100)
  prop_ub <- sprintf("%.1f", inv.logit(mod$ci.ub) * 100)
  
  ## print values
  paste0(prop, "% (", prop_lb, "% to ", prop_ub, "%)")
  
}

## calculate forest plots w/ or w/out subgroups
forest_calc <- function(dat, type) {
  
  if (type == "All") {
    metaprop(
      event = abx,
      n = abx_sample,
      studlab = study,
      data = dat,
      method = "GLMM",
      sm = "PLOGIT"
    )
  } else {
    metaprop(
      event = abx,
      n = abx_sample,
      studlab = study,
      data = dat,
      method = "GLMM",
      sm = "PLOGIT",
      byvar = dat[, type],
      bylab = type
    )
  }
}

## load calculated forest plot
forest_load <- function(type) {
  get(paste0("forest_", type))
}

## plot forest plot
forest_plot <- function(dat, show_ind_studies = FALSE) {
  
  ## determine subgroup variable
  type <- ifelse(is.null(dat$byvar), "All", dat$byvar)
  
  ## order subgroup results in decreasing order of prevalence (except for certain variables)
  if (!type %in% c("All", "Month", "QuartileMechanicalVentilation", "SpecialPopulation", "OverlappingPopulation", "ReportedClasses")) {
    o <- order(dat$TE.random.w, decreasing = FALSE)
    for (var in c("bylevs", grep("\\.w$", names(dat)[!names(dat) %in% c("df.hakn.w", "df.Q.w")], value = TRUE))) {
      dat[[var]] <- dat[[var]][o]
    }
  }
  
  # ## subgroup label
  # sgroup_lab <- case_when(
  #   type == "Region" ~ "Region",
  #   type == "Setting" ~ "Setting",
  #   type == "AgeGroup" ~ "Age Group",
  #   type == "Month" ~ "Month",
  #   type == "QuartileMechanicalVentilation" ~ "Quartile of Mechanical Ventilation",
  #   type == "SpecialPopulation" ~ "Special Population",
  #   type == "OverlappingPopulation" ~ "Overlapping Population",
  #   type == "ReportedClasses" ~ "Reported Classes"
  # )
  
  ## forest plot
  meta::forest(dat,
               xlim = c(0, 100),
               pscale = 100,
               rightcols = FALSE,
               leftcols = c("studlab", "n", "effect", "ci"),
               leftlabs = c("Subgroup",
                            "Total Patients",
                            "Prevalence",
                            "95% C.I."),
               xlab = "Prevalence", smlab = "",
               weight.study = "random", squaresize = 0.5, col.square = "navy",
               col.square.lines = "navy",
               col.diamond = "maroon", col.diamond.lines = "maroon",
               pooled.totals = TRUE,
               comb.fixed = FALSE,
               fs.hetstat = 10,
               print.tau2 = TRUE,
               print.Q = TRUE,
               print.pval.Q = TRUE,
               print.I2 = TRUE,
               digits = 1,
               # bylab = sgroup_lab,
               bylab = "",
               col.by = "black",
               study.results = ifelse(show_ind_studies == TRUE, TRUE, FALSE)) # show individual studies for "All"
  
}

# define model terms

## all terms
terms <- c("Region", "haq_score", "Month", "Setting", "AgeGroup", "Age", "percent_female", "percent_vent", "percent_smoker", "percent_copd", "percent_cvd", "percent_diabetes", "percent_deaths")

## term names plus "All" (intercept-only / no subgroups)
terms_plus_all <- c("All", terms)

## term names
terms_names <- c("Region", "HAQ score", "Study end month", "Setting/severity", "Age group", "Age", "Female (%)", "Mechanical ventilation (%)", "Smoker (%)", "COPD (%)", "CVD (%)", "Diabetes (%)", "Deaths (%)")

## term names plus "All" (intercept-only / no subgroups)
terms_plus_all_names <- c("Intercept-only", terms_names)

## terms displayed in units of 10
terms_10 <- c("Age", "haq_score", "percent_female", "percent_vent", "percent_smoker", "percent_copd", "percent_cvd", "percent_diabetes", "percent_deaths")

# define ggplot theme for figures
theme_antibiotic <- theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# calculate forest plots for dashboard and manuscript figures
forest_All <- forest_calc(dat, "All")
forest_Region <- forest_calc(dat, "Region")
forest_Setting <- forest_calc(dat, "Setting")
forest_AgeGroup <- forest_calc(dat, "AgeGroup")
forest_Month <- forest_calc(dat, "Month")
forest_QuartileMechanicalVentilation <- forest_calc(dat, "QuartileMechanicalVentilation")
forest_SpecialPopulation <- forest_calc(dat, "SpecialPopulation")
forest_OverlappingPopulation <- forest_calc(dat, "OverlappingPopulation")
forest_ReportedClasses <- forest_calc(dat, "ReportedClasses")