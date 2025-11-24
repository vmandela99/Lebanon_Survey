library(shiny)
library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)

# ---------------------------------------------------
# 1. Function to Download Live Kobo Data
# ---------------------------------------------------
get_kobo_data <- function() {
  
  url <- "https://kc.kobotoolbox.org/api/v1/data/aytGmdAwwqG9ps42jffYPo"
  
  req <- GET(url, authenticate("86de4c11a3b1c2de54279896dfa70099558451eb", ""))
  raw <- content(req, as = "text", encoding = "UTF-8")
  data <- fromJSON(raw, flatten = TRUE)
  
  df <- as.data.frame(data)
  
  # ---------------------------------------------------
  # Rename Kobo columns to your desired names
  # ---------------------------------------------------
  df <- df %>% 
    rename(
      organization = starts_with("organization"),
      gender31 = starts_with("gender"),
      nationality32 = starts_with("nationality"),
      family_size = starts_with("family"),
      main_food_source_pillar1 = starts_with("main_food")
    )
  
  return(df)
}

# ---------------------------------------------------
# 2. UI
# ---------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Live Kobo Dashboard – Food Source Breakdown"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data"),
      
      selectInput("org_filter", "Organization", choices = NULL,
                  multiple = TRUE),
      selectInput("gender_filter", "Gender", choices = NULL,
                  multiple = TRUE),
      selectInput("nat_filter", "Nationality", choices = NULL,
                  multiple = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("org_plot"),
      plotlyOutput("gender_plot"),
      plotlyOutput("nat_plot")
    )
  )
)

# ---------------------------------------------------
# 3. SERVER
# ---------------------------------------------------
server <- function(input, output, session) {
  
  # Load data on start and on refresh
  kobo_data <- eventReactive(input$refresh, {
    get_kobo_data()
  }, ignoreNULL = FALSE)
  
  # Populate filter inputs dynamically
  observe({
    df <- kobo_data()
    
    updateSelectInput(session, "org_filter",
                      choices = sort(unique(df$organization)))
    
    updateSelectInput(session, "gender_filter",
                      choices = sort(unique(df$gender31)))
    
    updateSelectInput(session, "nat_filter",
                      choices = sort(unique(df$nationality32)))
  })
  
  # Filter data reactively
  filtered_data <- reactive({
    df <- kobo_data()
    
    if (!is.null(input$org_filter)) {
      df <- df %>% filter(organization %in% input$org_filter)
    }
    if (!is.null(input$gender_filter)) {
      df <- df %>% filter(gender31 %in% input$gender_filter)
    }
    if (!is.null(input$nat_filter)) {
      df <- df %>% filter(nationality32 %in% input$nat_filter)
    }
    
    df
  })
  
  # ---------------------------------------------------
  # Reusable plot function
  # ---------------------------------------------------
  make_plot <- function(df, group_var) {
    
    df_sum <- df %>%
      group_by(across(all_of(group_var)), main_food_source_pillar1) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(across(all_of(group_var))) %>%
      mutate(percent = round(n / sum(n) * 100, 1))
    
    plot_ly(df_sum,
            x = ~get(group_var),
            y = ~percent,
            type = "bar",
            color = ~main_food_source_pillar1,
            text = ~paste0(percent, "%"),
            textposition = "outside") %>%
      layout(
        yaxis = list(title = "Percentage (%)"),
        xaxis = list(title = group_var),
        barmode = "group"
      )
  }
  
  # ---------------------------------------------------
  # Render Plots
  # ---------------------------------------------------
  output$org_plot <- renderPlotly({
    make_plot(filtered_data(), "organization")
  })
  
  output$gender_plot <- renderPlotly({
    make_plot(filtered_data(), "gender31")
  })
  
  output$nat_plot <- renderPlotly({
    make_plot(filtered_data(), "nationality32")
  })
}

shinyApp(ui, server)
