library(shiny)
library(tidyverse)
library(plotly)
library(httr)

# -------------------------------
# FUNCTION: Fetch data from KoBoToolbox (LIVE)
# -------------------------------

get_kobo_data <- function() {
  
  # --- INSERT YOUR CREDENTIALS ---
  username <- "vickman"
  api_token <- "86de4c11a3b1c2de54279896dfa70099558451eb"
  
  # API endpoint for data retrieval (KoBo REST API v2)
  url <- "https://kf.kobotoolbox.org/api/v2/assets/aytGmdAwwqG9ps42jffYPo/data/?format=json"
  
  
  # API request
  res <- GET(
    url,
    authenticate(username, api_token)
  )
  
  # Convert response to data frame
  content <- content(res, as = "parsed", simplifyVector = TRUE)
  
  df <- as.data.frame(content$results)
  
  return(df)
}

# -------------------------------
# SHINY APP
# -------------------------------

ui <- fluidPage(
  titlePanel("Real-Time KoBoToolbox Dashboard with Filters & Percentages"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("organization", "Organization:",
                  choices = NULL, multiple = TRUE),
      
      selectInput("gender", "Gender:",
                  choices = NULL, multiple = TRUE),
      
      selectInput("nationality", "Nationality:",
                  choices = NULL, multiple = TRUE),
      
      selectInput("age_group", "Age Group:",
                  choices = c("<18 years", "18–30 years", ">30 years"),
                  selected = c("<18 years","18–30 years",">30 years"),
                  multiple = TRUE)
    ),
    
    mainPanel(
      h3("Main Food Source Distribution by Organization"),
      plotlyOutput("org_plot"),
      
      h3("Main Food Source Distribution by Gender"),
      plotlyOutput("gender_plot"),
      
      h3("Main Food Source Distribution by Nationality"),
      plotlyOutput("nat_plot"),
      
      h3("Main Food Source Distribution by Age Group"),
      plotlyOutput("age_plot")
    )
  )
)


server <- function(input, output, session) {
  
  # -------------------------------
  # LIVE KOBO DATA (reactive)
  # -------------------------------
  raw_data <- reactive({
    tryCatch({
      get_kobo_data()
    }, error = function(e) {
      showNotification("Error fetching KoBo data. Check API token.", type = "error")
      return(NULL)
    })
  })
  
  # -------------------------------
  # CLEAN DATA REACTIVELY
  # -------------------------------
  df <- reactive({
    
    req(raw_data())
    
    data <- raw_data()
    
    # Rename your variables here based on actual KoBo field names
    data <- data %>%
      select(
        organization = `Name.of.the.Organization`,
        age = age30,
        gender = gender31,
        nationality = nationality32,
        main_food_source = main_food_source_pillar1
      ) %>%
      mutate(
        age_group = case_when(
          age < 18 ~ "<18 years",
          age >= 18 & age <= 30 ~ "18–30 years",
          age > 30 ~ ">30 years",
          TRUE ~ NA_character_
        )
      )
    
    return(data)
  })
  
  # Dynamically fill filter options after data loads
  observeEvent(df(), {
    updateSelectInput(session, "organization",
                      choices = unique(df()$organization),
                      selected = unique(df()$organization))
    
    updateSelectInput(session, "gender",
                      choices = unique(df()$gender),
                      selected = unique(df()$gender))
    
    updateSelectInput(session, "nationality",
                      choices = unique(df()$nationality),
                      selected = unique(df()$nationality))
  })
  
  
  # -------------------------------
  # APPLY FILTERS
  # -------------------------------
  filtered_data <- reactive({
    df() %>%
      filter(
        organization %in% input$organization,
        gender %in% input$gender,
        nationality %in% input$nationality,
        age_group %in% input$age_group
      )
  })
  
  # Helper function for % breakdown
  make_percentage_data <- function(data, group_var) {
    data %>%
      group_by({{ group_var }}) %>%
      summarise(
        avg_value = mean(main_food_source, na.rm = TRUE),
        n = n()
      ) %>%
      ungroup() %>%
      mutate(pct = n / sum(n) * 100)
  }
  
  
  # -------------------------------
  # PLOTS (Organization / Gender / Nationality / Age Group)
  # -------------------------------
  
  output$org_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), organization)
    
    plot_ly(
      dt, x = ~organization, y = ~avg_value, type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = "outside",
      marker = list(color = ~organization)
    )
  })
  
  output$gender_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), gender)
    
    plot_ly(
      dt, x = ~gender, y = ~avg_value, type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = "outside",
      marker = list(color = ~gender)
    )
  })
  
  output$nat_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), nationality)
    
    plot_ly(
      dt, x = ~nationality, y = ~avg_value, type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = "outside",
      marker = list(color = ~nationality)
    )
  })
  
  output$age_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), age_group)
    
    plot_ly(
      dt, x = ~age_group, y = ~avg_value, type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = "outside",
      marker = list(color = ~age_group)
    )
  })
  
}

shinyApp(ui, server)
