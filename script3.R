library(shiny)
library(tidyverse)
library(plotly)

# ---- SAMPLE DATA (Replace with your actual dataset if needed) ----
df <- tribble(
  ~organization, ~age30, ~gender31, ~nationality32, ~family_size, ~main_food_source_pillar1,
  "Akkar", 38.10, "female", "lebanese", NA, 49,
  "Akkar", 34.12, "female", "lebanese", NA, 71,
  "Akkar", 32.89, "male", "lebanese", NA, 71,
  "Akkar", 30.95, "male", "lebanese", NA, 75,
  "Akkar", 55.93, "male", "lebanese", NA, 76.5,
  "Akkar", 36.06, "male", "syrian", 7, 78,
  "Akkar", 54.56, "male", "syrian", NA, 83,
  "Akkar", 39.28, "male", "lebanese", NA, 86,
  "Akkar", 27, "female", "lebanese", NA, 43,
  "Akkar", 28.32, "male", "lebanese", NA, 35,
  "Akkar", 46.90, "male", "lebanese", NA, 52,
  "Akkar", 22.07, "female", "syrian", NA, 36,
  "Akkar", 32.73, "male", "lebanese", NA, 50,
  "Akkar", 37.95, "male", "syrian", NA, 39
)

# ---- PREPROCESSING ----
df <- df %>%
  rename(
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

# ---- UI ----
ui <- fluidPage(
  titlePanel("Interactive Dashboard with Reactive Filters & Percentages"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("organization", "Organization:",
                  choices = unique(df$organization),
                  selected = unique(df$organization),
                  multiple = TRUE),
      
      selectInput("gender", "Gender:",
                  choices = unique(df$gender),
                  selected = unique(df$gender),
                  multiple = TRUE),
      
      selectInput("nationality", "Nationality:",
                  choices = unique(df$nationality),
                  selected = unique(df$nationality),
                  multiple = TRUE),
      
      selectInput("age_group", "Age Group:",
                  choices = unique(df$age_group),
                  selected = unique(df$age_group),
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

# ---- SERVER ----
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df %>%
      filter(
        organization %in% input$organization,
        gender %in% input$gender,
        nationality %in% input$nationality,
        age_group %in% input$age_group
      )
  })
  
  
  # Helper function to compute % distribution
  make_percentage_data <- function(data, group_var) {
    
    data %>%
      group_by({{ group_var }}) %>%
      summarise(
        avg_value = mean(main_food_source, na.rm = TRUE),
        n = n()
      ) %>%
      ungroup() %>%
      mutate(
        pct = n / sum(n) * 100
      )
  }
  
  
  # ---- PLOTS ----
  
  output$org_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), organization)
    
    plot_ly(
      dt,
      x = ~organization,
      y = ~avg_value,
      type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = 'outside',
      marker = list(color = ~organization)
    ) %>%
      layout(
        yaxis = list(title = "Avg. Main Food Source"),
        xaxis = list(title = "Organization"),
        title = "Main Food Source by Organization (Percentages)"
      )
  })
  
  
  output$gender_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), gender)
    
    plot_ly(
      dt,
      x = ~gender,
      y = ~avg_value,
      type = 'bar',
      text = ~paste0(round(pct, 1), "%"),
      textposition = 'outside',
      marker = list(color = ~gender)
    ) %>%
      layout(
        yaxis = list(title = "Avg. Main Food Source"),
        xaxis = list(title = "Gender"),
        title = "Main Food Source by Gender (Percentages)"
      )
  })
  
  
  output$nat_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), nationality)
    
    plot_ly(
      dt,
      x = ~nationality,
      y = ~avg_value,
      type = "bar",
      text = ~paste0(round(pct, 1), "%"),
      textposition = 'outside',
      marker = list(color = ~nationality)
    ) %>%
      layout(
        yaxis = list(title = "Avg. Main Food Source"),
        xaxis = list(title = "Nationality"),
        title = "Main Food Source by Nationality (Percentages)"
      )
  })
  
  
  output$age_plot <- renderPlotly({
    dt <- make_percentage_data(filtered_data(), age_group)
    
    plot_ly(
      dt,
      x = ~age_group,
      y = ~avg_value,
      type = "bar",
      text = ~paste0(round(pct, 1), "%"),
      textposition = 'outside',
      marker = list(color = ~age_group)
    ) %>%
      layout(
        yaxis = list(title = "Avg. Main Food Source"),
        xaxis = list(title = "Age Group"),
        title = "Main Food Source by Age Group (Percentages)"
      )
  })
  
}

shinyApp(ui, server)
