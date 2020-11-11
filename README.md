# Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis

## Purpose of this repository

This repository contains all code and data necessary to reproduce the analyses presented in the manuscript "Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis" by Langford et al. This includes every figure and supplementary figure except for figures 1 and 2. Table 1 is generated as part of the dashboard. Supplementary table 1 comprises a subset of the complete dataset found in the directory `data`.

This repository also contains the code necessary to reproduce the interactive [R Shiny](https://shiny.rstudio.com/) dashboard hosted at: [https://brxad.shinyapps.io/covid-19-antibiotic/](https://brxad.shinyapps.io/covid-19-antibiotic/).

For more details on the methodological approach used in this meta-analysis, please see the manuscript [Meta-analysis of Proportions Using Generalized Linear
Mixed Models](https://doi.org/10.1097/EDE.0000000000001232) by Lin & Chu (2020) and the sample code they provide.

## Requirements

All code is written in the programming language [R](https://www.r-project.org/). It is mostly easily run using the IDE [RStudio](https://rstudio.com/). An .Rproj file is included with this repository for your convenience.

All required R packages are listed at the top of `global.R`.

## Reproducing the figures

Run `manuscript_figs.R`.

## Running the dashboard

Run `app.R`.
