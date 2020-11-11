# Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis #
# Langford et al. #
# Script author: Jean-Paul R. Soucy #

# Launch interactive dashboard #
# https://brxad.shinyapps.io/covid-19-antibiotic/ #

# Note: This script assumes the working directory is set to the root directory of the project
# This is most easily achieved by using the provided covid-19-antibiotic.Rproj in RStudio

# load global.R
source("global.R")

# load ui and server files
source("ui.R")
source("server.R")

# run app
shinyApp(ui, server)