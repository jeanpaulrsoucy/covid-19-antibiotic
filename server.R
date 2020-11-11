# Antibiotic prescribing in patients with COVID-19: A rapid review and meta-analysis #
# Langford et al. #
# Script author: Jean-Paul R. Soucy #

# Server functions for interactive dashboard #

server <- function(input, output) {
  
  # load data for selected forest plot
  forest_data <- reactive({
    forest_load(input$typeInput)
  })
  
  # display primary forest plot
  output$forest <- renderPlot ({
    
    ## load data
    dat_for <- forest_data()
    
    ## forest plot
    forest_plot(dat_for)
    
  })
  
  # load data for selected forest plot - sensitivity analysis
  forest_data_sens <- reactive({
    forest_load(input$sensInput)
  })
  
  # display forest plot - sensitivity analysis
  output$forest_sens <- renderPlot ({
    
    ## load data
    dat_for <- forest_data_sens()
    
    ## forest plot
    forest_plot(dat_for)

  })
  
  # meta-regression
  
  ## report univariable results
  output$meta_tab_uni <- renderTable({
    
    ### create table
    tab <- tibble(
      Term = character(0),
      `Prevalence difference (%)` = character(0),
      `Confidence interval (%)` = character(0),
      `Studies included` = integer(0)
    )
    
    ### function: calculate p-values
    term_p <- function(mod) {
      p <- sprintf("%.4f", mod$pval[-1])
      ifelse(p == "0.0000", "<0.0001", p)
    }
    
    ### function: calculate omnibus p-value
    omni_p <- function(type) {
      mod <- rma_mod(dat, type)
      p <- sprintf("%.4f", mod$QMp)
      ifelse(p == "0.0000", "<0.0001", p)
    }
    
    ### calculate table
    for (i in terms) {
      
      ### calculate RMA
      mod <- rma_mod(dat, i)
      
      ### calculate values
      d <- tidy(mod, conf.int = TRUE, measure = "PLO", exponentiate = FALSE) %>%
        slice(-1, ) %>% # remove intercept
        mutate(
          term = case_when(
            term == "RegionMiddle East" ~ paste0(stri_dup(intToUtf8(160), 6), "Middle East"),
            term == "RegionEast/Southeast Asia (ex-China)" ~ paste0(stri_dup(intToUtf8(160), 6), "East/Southeast Asia excluding China"),
            term == "RegionEurope" ~ paste0(stri_dup(intToUtf8(160), 6), "Europe"),
            term == "RegionNorth America" ~ paste0(stri_dup(intToUtf8(160), 6), "North America"),
            term == "RegionMultiple" ~ paste0(stri_dup(intToUtf8(160), 6), "Multiple"),
            term == "SettingHospital ICU" ~ paste0(stri_dup(intToUtf8(160), 6), "Hospital ICU"),
            term == "SettingHospital/Outpatient" ~ paste0(stri_dup(intToUtf8(160), 6), "Hospital/outpatient"),
            term == "AgeGroupChildren" ~ paste0(stri_dup(intToUtf8(160), 6), "Children"),
            term == "AgeGroupAll" ~ paste0(stri_dup(intToUtf8(160), 6), "All"),
            term == "AgeGroupNot specified" ~ paste0(stri_dup(intToUtf8(160), 6), "Not specified"),
            term == "MonthFeb" ~ paste0(stri_dup(intToUtf8(160), 6), "February"),
            term == "MonthMar" ~ paste0(stri_dup(intToUtf8(160), 6), "March"),
            term == "MonthApr" ~ paste0(stri_dup(intToUtf8(160), 6), "April"),
            term == "MonthMay" ~ paste0(stri_dup(intToUtf8(160), 6), "May"),
            term == "MonthNot specified" ~ paste0(stri_dup(intToUtf8(160), 6), "Not specified"),
            term == "Age" ~ "Age (10-year change)",
            term == "haq_score" ~ "Healthcare access/quality score (10-point change)",
            term == "percent_female" ~ "Female (10% change)",
            term == "percent_vent" ~ "Ventilator (10% change)",
            term == "percent_smoker" ~ "Smoker (10% change)",
            term == "percent_copd" ~ "COPD (10% change)",
            term == "percent_cvd" ~ "CVD (10% change)",
            term == "percent_diabetes" ~ "Diabetes (10% change)",
            term == "percent_deaths" ~ "Deaths (10% change)"
          ),
          estimate = sprintf("%.2f", if (i %in% terms_10) {exp(estimate * 10)} else {exp(estimate)}),
          conf.low = sprintf("%.2f", if (i %in% terms_10) {exp(conf.low * 10)} else {exp(conf.low)}),
          conf.high = sprintf("%.2f", if (i %in% terms_10) {exp(conf.high * 10)} else {exp(conf.high)}),
          `Confidence interval` = paste0(conf.low, " to ", conf.high),
          `Studies included` = if (i %in% terms_10) {as.character(mod$k)} else {as.character(table(dat[ , i]))[-1]},
          p = term_p(mod)
        ) %>%
        select(term, estimate, `Confidence interval`, `Studies included`, p) %>%
        rename(
          Term = term,
          `POR` = estimate
        )
      tab <- rbind(tab, d)
    }
    tab
    
    ### add rows to break up table
    tab <- tab %>%
      add_row(
        Term = c("Region", paste0(stri_dup(intToUtf8(160), 6), "China")),
        `POR` = c("", "Baseline"),
        `Confidence interval` = c("", ""),
        `Studies included` = c(as.character(sum(!is.na(dat$Region))), as.character(table(dat[ , "Region"]))[1]),
        p = c(omni_p("Region"), ""),
        .before = 1) %>%
      add_row(
        Term = c("End month", paste0(stri_dup(intToUtf8(160), 6), "January")),
        `POR` = c("", "Baseline"),
        `Confidence interval` = c("", ""),
        `Studies included` = c(as.character(sum(!is.na(dat$Month))), as.character(table(dat[ , "Month"]))[1]),
        p = c(omni_p("Month"), ""),
        .before = 9) %>%
      add_row(
        Term = c("Setting", paste0(stri_dup(intToUtf8(160), 6), "Hospital")),
        `POR` = c("", "Baseline"),
        `Confidence interval` = c("", ""),
        `Studies included` = c(as.character(sum(!is.na(dat$Setting))), as.character(table(dat[ , "Setting"]))[1]),
        p = c(omni_p("Setting"), ""),
        .before = 16) %>%
      add_row(
        Term = c("Age group", paste0(stri_dup(intToUtf8(160), 6), "Adults")),
        `POR` = c("", "Baseline"),
        `Confidence interval` = c("", ""),
        `Studies included` = c(as.character(sum(!is.na(dat$AgeGroup))), as.character(table(dat[ , "AgeGroup"]))[1]),
        p = c(omni_p("AgeGroup"), ""),
        .before = 20)
    
    ### stylize p value table column name
    tab %>%
      rename(
        `<i><b>p</b></i>` = p
      )
    
  }, sanitize.text.function=function(x){x})
  
  ## model for scatter plots
  meta_model_scatter <- reactive({
    
    dat <- filter_at(dat, vars(input$scatterInput), all_vars(!is.na(.)))
    rma_mod(dat, input$scatterInput)
    
  })
  
  ## scatterplot
  output$meta_scatter <- renderPlotly({
    
    ### get model
    mod <- meta_model_scatter()
    
    ### make scatter plot or box plot
    y <- mod$yi * 100
    x <- filter_at(dat, vars(input$scatterInput), all_vars(!is.na(.))) %>% pull(input$scatterInput)
    z <- filter_at(dat, vars(input$scatterInput), all_vars(!is.na(.))) %>% pull(abx_sample)
    n <- filter_at(dat, vars(input$scatterInput), all_vars(!is.na(.))) %>% pull(study)
    if (input$scatterInput %in% c("Region", "Setting", "AgeGroup", "Month")) {
      ## categorical variable
      p <- ggplot(data = NULL, aes(x = x, y = y)) +
        geom_boxplot() +
        labs(
          x = case_when(
            input$scatterInput == "Region" ~ "Region",
            input$scatterInput == "Setting" ~ "Setting",
            input$scatterInput == "AgeGroup" ~ "Age Group",
            input$scatterInput == "Month" ~ "End month"
          ),
          y = "Percent prescribed antibiotics"
        ) +
        theme_antibiotic
      ggplotly(p, aspectratio = 1) %>%
        config(displaylogo = FALSE) %>%
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE))
    } else {
      ## continuous variable
      p <- ggplot(data = NULL, aes(x = x, y = y, size = 1/5 * z^(1/5), text =
                                     paste0(
                                       "Study: ",
                                       n,
                                       "<br>",
                                       "Sample size: ",
                                       formatC(z, format = "d", big.mark = ",")
                                     ))) +
        geom_point(alpha = 0.4) +
        labs(
          x = case_when(
            input$scatterInput == "Age" ~ "Age",
            input$scatterInput == "haq_score" ~ "Healthcare access/quality score",
            input$scatterInput == "percent_female" ~ "Female (%)",
            input$scatterInput == "percent_vent" ~ "Ventilator (%)",
            input$scatterInput == "percent_smoker" ~ "Smoker (%)",
            input$scatterInput == "percent_copd" ~ "COPD (%)",
            input$scatterInput == "percent_cvd" ~ "CVD (%)",
            input$scatterInput == "percent_diabetes" ~ "Diabetes (%)",
            input$scatterInput == "percent_deaths" ~ "Deaths (%)"
          ),
          y = "Percent prescribed antibiotics"
        ) +
        theme_antibiotic
      ggplotly(p, tooltip = "text", aspectratio = 1) %>%
        style(textposition = "top") %>%
        config(displaylogo = FALSE) %>%
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE))
    }
    
  })
  
  ## model for qq plots
  meta_model_qq <- reactive({
    
    if (input$qqInput != "All") {
      dat <- filter_at(dat, vars(input$qqInput), all_vars(!is.na(.))) 
    }
    rma_mod(dat, input$qqInput)
    
  })
  
  ## qq plot
  output$meta_qq <- renderPlot({
    
    ### get model
    mod <- meta_model_qq()
    
    ### qq plot
    qqnorm(residuals(mod), main = paste0("Normal Q-Q plot (", input$qqInput, ")"))
    qqline(residuals(mod))
    
  })
  
  ## model for histograms
  meta_model_hist <- reactive({
    
    if (input$histInput != "All") {
      dat <- filter_at(dat, vars(input$histInput), all_vars(!is.na(.))) 
    }
    rma_mod(dat, input$histInput)
    
  })
  
  ## residual histogram
  output$meta_hist <- renderPlot({
    
    ### get model
    mod <- meta_model_hist()
    
    ## hist
    p <- hist(residuals(mod), xlab = "Residuals", main = paste0("Histogram of residuals (", input$histInput, ")"))
    
  })
  
}