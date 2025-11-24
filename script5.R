library(shiny)
library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)

# ------------------------------
# 1. FUNCTION: DOWNLOAD LIVE KOBO DATA
# ------------------------------
get_kobo_data <- function() {
  
  url <- "https://kc.kobotoolbox.org/api/v1/data/aytGmdAwwqG9ps42jffYPo"
  # https://kc.kobotoolbox.org/api/v2/assets/aytGmdAwwqG9ps42jffYPo
  
  req <- GET(url, authenticate("86de4c11a3b1c2de54279896dfa70099558451eb", ""))
  raw <- content(req, as = "text", encoding = "UTF-8")
  data <- fromJSON(raw, flatten = TRUE)
  
  df <- as.data.frame(data)
  
  # ------------------------------
  # RENAME COLUMNS TO YOUR SPECIFIED NAMES
  # ------------------------------
  df <- df %>% 
    rename(
      organization = starts_with("organization"),
      age30 = starts_with("age"),
      gender31 = starts_with("gender"),
      # nationality32 = starts_with("nationality"),
      family_size = starts_with("family"),
      main_food_source_pillar1 = starts_with("main_food")
    )
  
  # ------------------------------
  # CREATE AGE CATEGORIES
  # ------------------------------
  df <- df %>% 
    mutate(
      age_cat = case_when(
        age30 < 18 ~ "Below 18",
        age30 >= 18 & age30 <= 30 ~ "18–30",
        age30 > 30 ~ "Above 30",
        TRUE ~ "Unknown"
      )
    )
  
  return(df)
}

# ------------------------------
# 2. UI
# ------------------------------
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
                  multiple = TRUE),
      selectInput("age_filter", "Age Category", choices = NULL,
                  multiple = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("org_plot"),
      plotlyOutput("gender_plot"),
      plotlyOutput("nat_plot"),
      plotlyOutput("age_plot")
    )
  )
)

# ------------------------------
# 3. SERVER
# ------------------------------
server <- function(input, output, session) {
  
  # Load data on start + refresh button
  kobo_data <- eventReactive(input$refresh, {
    get_kobo_data()
  }, ignoreNULL = FALSE)
  
  # Update dropdown filters based on live data
  observe({
    df <- kobo_data()
    
    updateSelectInput(session, "org_filter",
                      choices = sort(unique(df$organization)))
    
    updateSelectInput(session, "gender_filter",
                      choices = sort(unique(df$gender31)))
    
    updateSelectInput(session, "nat_filter",
                      choices = sort(unique(df$nationality32)))
    
    updateSelectInput(session, "age_filter",
                      choices = c("Below 18","18–30","Above 30"))
  })
  
  # Filter data based on user selections
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
    if (!is.null(input$age_filter)) {
      df <- df %>% filter(age_cat %in% input$age_filter)
    }
    
    df
  })
  
  # ------------------------------
  # 4. PLOT FUNCTIONS (Reusable)
  # ------------------------------
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
  
  # ------------------------------
  # 5. OUTPUT PLOTS
  # ------------------------------
  output$org_plot <- renderPlotly({
    make_plot(filtered_data(), "organization")
  })
  
  output$gender_plot <- renderPlotly({
    make_plot(filtered_data(), "gender31")
  })
  
  output$nat_plot <- renderPlotly({
    make_plot(filtered_data(), "nationality32")
  })
  
  output$age_plot <- renderPlotly({
    make_plot(filtered_data(), "age_cat")
  })
}

shinyApp(ui, server)
