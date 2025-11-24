# Install packages if needed
# install.packages(c("shiny", "tidyverse", "plotly"))

library(shiny)
library(tidyverse)
library(plotly)

# ---- SAMPLE DATA (replace with your real dataset or readxl import) ----
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

# Standardize column names
df <- df %>%
  rename(
    organization = organization,
    age = age30,
    gender = gender31,
    nationality = nationality32,
    main_food_source = main_food_source_pillar1
  )

# ---- SHINY DASHBOARD ----

ui <- fluidPage(
  titlePanel("Interactive Dashboard: Responsive Filters & Bar Charts"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("organization", "Organization:", 
                  choices = unique(df$organization), 
                  selected = unique(df$organization), multiple = TRUE),
      
      sliderInput("age", "Age Range:",
                  min = floor(min(df$age, na.rm = TRUE)),
                  max = ceiling(max(df$age, na.rm = TRUE)),
                  value = c(floor(min(df$age)), ceiling(max(df$age)))),
      
      selectInput("gender", "Gender:", 
                  choices = unique(df$gender),
                  selected = unique(df$gender), multiple = TRUE),
      
      selectInput("nationality", "Nationality:",
                  choices = unique(df$nationality),
                  selected = unique(df$nationality), multiple = TRUE),
      
      sliderInput("food", "Main Food Source (pillar1):",
                  min = min(df$main_food_source, na.rm = TRUE),
                  max = max(df$main_food_source, na.rm = TRUE),
                  value = c(min(df$main_food_source), max(df$main_food_source)))
    ),
    
    mainPanel(
      plotlyOutput("gender_plot"),
      plotlyOutput("nationality_plot"),
      plotlyOutput("food_plot")
    )
  )
)

server <- function(input, output) {
  
  # ---- Reactive filtered dataset ----
  filtered_data <- reactive({
    df %>%
      filter(
        organization %in% input$organization,
        age >= input$age[1],
        age <= input$age[2],
        gender %in% input$gender,
        nationality %in% input$nationality,
        main_food_source >= input$food[1],
        main_food_source <= input$food[2]
      )
  })
  
  # ---- Bar Chart: Gender distribution ----
  output$gender_plot <- renderPlotly({
    filtered_data() %>%
      count(gender) %>%
      plot_ly(x = ~gender, y = ~n, type = "bar") %>%
      layout(title = "Gender Distribution")
  })
  
  # ---- Bar Chart: Nationality distribution ----
  output$nationality_plot <- renderPlotly({
    filtered_data() %>%
      count(nationality) %>%
      plot_ly(x = ~nationality, y = ~n, type = "bar") %>%
      layout(title = "Nationality Distribution")
  })
  
  # ---- Bar Chart: Main Food Source ----
  output$food_plot <- renderPlotly({
    filtered_data() %>%
      plot_ly(x = ~main_food_source, type = "histogram") %>%
      layout(title = "Main Food Source (pillar1) Distribution")
  })
}

# Run the App
shinyApp(ui, server)
