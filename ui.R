# Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis #
# Langford et al. #
# Script author: Jean-Paul R. Soucy #

# UI for interactive dashboard #

ui <- dashboardPage(
  
  # webpage title
  title = "Antimicrobial Prescribing in COVID-19",
  
  # no header
  header = dashboardHeader(disable = TRUE),
  
  # no sidebar
  sidebar = dashboardSidebar(disable = TRUE),
  
  # body
  body = dashboardBody(
    # define meta tags
    meta() %>%
      meta_social(
        title = "Antibiotic Prescribing in COVID-19",
        description = "Antibiotic Prescribing in Patients with COVID-19: A Rapid Review and Meta-Analysis",
        url = "https://brxad.shinyapps.io/covid-19-antibiotic/",
        image = "https://source.unsplash.com/VhjsGKMefkk/640x426",
        image_alt = "COVID-19"),
    h1("Antimicrobial Prescribing in COVID-19", align = "center"),
    fluidRow(
      column(
        p("The forest plot is a visual summary of the estimated proportion of antibiotic prescribing in each study included in the meta-analysis, along with the overall estimate.", align = "center"),
        p("The results may be stratified by region, setting/severity (hospital, hospital ICU, or hospital/outpatient), age group, end month of study, and quartile of mechanical ventilation.", align = "center"),
        p("On mobile, please scroll to see the entire forest plot.", align = "center"),
        width = 8,
        offset = 2
      )),
    fluidRow(column(
      width = 8,
      offset = 2,
      align = "center",
      radioButtons(
        "typeInput",
        NULL,
        choices = c(
          "by Region" = "Region",
          "by Study End Month" = "Month",
          "by Setting/Severity" = "Setting",
          "by Age Group" = "AgeGroup",
          "by Quartile of Mechanical Ventilation" = "QuartileMechanicalVentilation"
        ),
        selected = "Region",
        inline = TRUE
      ))),
    fluidRow(
      # must scale height to that of the tallest plot when adding new studies
      div(style='height:520px; width:100%; overflow-x: scroll',
          plotOutput("forest", height = "480px", width = "700px"),
          align = "center"
      )),
    fluidRow(
      column(
        p("Additional forest plots are available as sensitivity analyses.", align = "center"),
        width = 8,
        offset = 2
      )),
    fluidRow(column(
      width = 8,
      offset = 2,
      align = "center",
      radioButtons(
        "sensInput",
        NULL,
        choices = c(
          "Special Population (Y/N)" = "SpecialPopulation",
          "Overlapping Population (Y/N)" = "OverlappingPopulation",
          "Reported Classes (Y/N)" = "ReportedClasses"
        ),
        selected = "SpecialPopulation",
        inline = TRUE
      ))),
    fluidRow(
      # must scale height to that of the tallest plot when adding new studies
      div(style='height:300px; width:100%; overflow-x: scroll',
          plotOutput("forest_sens", height = "260px", width = "700px"),
          align = "center"
      )),
    # meta-regression
    fluidRow(
      column(
        p("Meta-regression can help us to understand which study characteristics are associated with a higher estimated prevalence of antimicrobial prescribing. Below are the results of univariable meta-regressions run on a series of variables in turn.", align = "center"),
        width = 8,
        offset = 2
      )
    ),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        box(width = 12,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Table",
                p(
                  "The following table shows the results of univariable meta-regressions using each individual variable in turn. Studies are excluded from univariable models if they did not provide information on the variable in question."
                ),
                tableOutput("meta_tab_uni"),
                p(
                  "All coefficients represent prevalence odds ratios (POR). Coefficient for HAQ score is for a 10-point difference; coefficient for age is for a 10-year difference; coefficients for percentages are for a 10% difference. Reference values for categorical variables are: China (region), January 2020 (end month), hospital (setting), and adults (age group). Unadjusted p-values for individual variables are presented alongside omnibus p-values for categorical variables."
                )
              ),
              tabPanel(
                "Scatter plot/box plot",
                selectInput(
                  "scatterInput",
                  "Select variable",
                  choices = c(
                    "Region" = "Region",
                    "Healthcare access/quality score" = "haq_score",
                    "Study end month" = "Month",
                    "Setting/severity" = "Setting",
                    "Age group" = "AgeGroup",
                    "Age" = "Age",
                    "Female (%)" = "percent_female",
                    "Mechanical ventilation (%)" = "percent_vent",
                    "Smoker (%)" = "percent_smoker",
                    "COPD (%)" = "percent_copd",
                    "CVD (%)" = "percent_cvd",
                    "Diabetes (%)" = "percent_diabetes",
                    "Deaths (%)" = "percent_deaths"
                  ),
                  selected = "Region",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
                ),
                plotlyOutput("meta_scatter"),
                p(
                  "Select a variable to show a scatter plot (or box plot for categorical variables) of prevalence by that variable for all studies with information for that variable. On scatter plots, dot sizes are proportional to the sample size of the study."
                )
              ),
              tabPanel(
                "QQ plot",
                selectInput(
                  "qqInput",
                  "Select variable",
                  choices = c(
                    "All (intercept-only)" = "All",
                    "Region" = "Region",
                    "Healthcare access/quality score" = "haq_score",
                    "Study end month" = "Month",
                    "Setting/severity" = "Setting",
                    "Age group" = "AgeGroup",
                    "Age" = "Age",
                    "Female (%)" = "percent_female",
                    "Mechanical ventilation (%)" = "percent_vent",
                    "Smoker (%)" = "percent_smoker",
                    "COPD (%)" = "percent_copd",
                    "CVD (%)" = "percent_cvd",
                    "Diabetes (%)" = "percent_diabetes",
                    "Deaths (%)" = "percent_deaths"
                  ),
                  selected = "All",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
                ),
                plotOutput("meta_qq"),
                p(
                  "Select a variable to show a QQ plot of the residuals for the univariable meta-regression using that variable. This plot is used to assess the normality of the model residuals."
                )
              ),
              tabPanel(
                "Residual histogram",
                selectInput(
                  "histInput",
                  "Select variable",
                  choices = c(
                    "All (intercept-only)" = "All",
                    "Region" = "Region",
                    "Healthcare access/quality score" = "haq_score",
                    "Study end month" = "Month",
                    "Setting/severity" = "Setting",
                    "Age group" = "AgeGroup",
                    "Age" = "Age",
                    "Female (%)" = "percent_female",
                    "Mechanical ventilation (%)" = "percent_vent",
                    "Smoker (%)" = "percent_smoker",
                    "COPD (%)" = "percent_copd",
                    "CVD (%)" = "percent_cvd",
                    "Diabetes (%)" = "percent_diabetes",
                    "Deaths (%)" = "percent_deaths"
                  ),
                  selected = "All",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
                ),
                plotOutput("meta_hist"),
                p(
                  "Select a variable to show a histogram of the residuals for the univariable meta-regression using that variable. This plot is used to assess the normality of the model residuals."
                )
              )
            ),))), 
  # footer
  p(HTML("Developed by <a href='https://jeanpaulsoucy.com' target='_blank'> Jean-Paul R. Soucy</a> for Antibiotic prescribing in patients with COVID-19: a rapid review and meta-analysis (Langford et al., 2020)."), align = "left"))
)