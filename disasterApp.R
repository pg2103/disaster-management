library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(googleAuthR)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)
library(randomForest)
library(scales)
library(httr)
library(jsonlite)

data_preparation <- function() {
  set.seed(123)
  years <- 2000:2024
  disaster_types <- c("Earthquake", "Flood", "Hurricane", "Drought", "Wildfire")
  regions <- c("North America", "South America", "Europe", "Asia", "Africa")
  n_records <- 1000
  
  full_data <- data.frame(
    Start.Year = sample(years, n_records, replace = TRUE),
    Disaster.Type = factor(sample(disaster_types, n_records, replace = TRUE), 
                           levels = disaster_types),
    Region = factor(sample(regions, n_records, replace = TRUE), 
                    levels = regions),
    Total.Deaths = round(rlnorm(n_records, meanlog = 3, sdlog = 1.5)),
    Total.Damage..Adjusted...000.US.. = round(rlnorm(n_records, meanlog = 8, sdlog = 2))
  )
  
  # Split data into training and prediction sets
  train_data <- full_data[1:800, ]
  pred_data <- full_data[801:1000, ]
  
  return(list(full_data = full_data, train_data = train_data, pred_data = pred_data))
}
train_rf_model <- function(train_data) {
  # Ensure predictors are factors where appropriate
  train_data$Disaster.Type <- factor(train_data$Disaster.Type)
  train_data$Region <- factor(train_data$Region)
  
  rf_model <- randomForest(
    Total.Deaths ~ Start.Year + Disaster.Type + Region + Total.Damage..Adjusted...000.US..,
    data = train_data,
    ntree = 100
  )
  return(rf_model)
}
options(googleAuthR.webapp.client_id = "86961742563-3e26eqi4oo7pvouf1vlul8bb0rdtmg0v.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "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")
options(googleAuthR.scopes.selected = c(
  "https://www.googleapis.com/auth/userinfo.profile",
  "https://www.googleapis.com/auth/userinfo.email"
))
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        font-family: 'Roboto', 'Helvetica Neue', Arial, sans-serif;
        background-color: #f4f7f6;
      }
      
      /* Login page styles */
      .login-container {
        background-image: linear-gradient(rgba(0,0,0,0.6), rgba(0,0,0,0.6)), 
                          url('https://static-ssl.businessinsider.com/image/560aeee19dd7cc16008bde7c-1416-1062/bi%20web_0014.jpg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
        min-height: 100vh;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
      .login-box {
        background: rgba(255, 255, 255, 0.1);
        backdrop-filter: blur(10px);
        padding: 3rem;
        border-radius: 20px;
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.2);
        text-align: center;
        max-width: 550px;
        width: 90%;
        color: white;
        border: 1px solid rgba(255, 255, 255, 0.2);
        transition: all 0.3s ease;
      }
      
      .login-box:hover {
        transform: scale(1.02);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.3);
      }
      
      .login-title {
        font-size: 2.8rem;
        margin-bottom: 1rem;
        font-weight: 700;
        letter-spacing: 1px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
      }
      
      .login-subtitle {
        font-size: 1.3rem;
        margin-bottom: 2rem;
        color: #e0e0e0;
        line-height: 1.6;
      }
      
      .google-btn {
        background-color: #4285F4;
        color: white;
        border: none;
        padding: 1rem 2rem;
        border-radius: 10px;
        font-size: 1.1rem;
        font-weight: bold;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
      }
      
      .google-btn:hover {
        background-color: #357ae8;
        transform: translateY(-3px);
        box-shadow: 0 6px 8px rgba(0,0,0,0.2);
      }

      /* Landing page styles */
      .landing-page {
        background: linear-gradient(45deg, #1a2a6c, #b21f1f, #fdbb2d);
        background-size: 400% 400%;
        animation: gradientShift 15s ease infinite;
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        color: white;
        text-align: center;
        padding: 0 20px;
      }

      @keyframes gradientShift {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }
      
      .landing-title {
        font-size: 4rem;
        margin-bottom: 30px;
        font-weight: 900;
        text-shadow: 3px 3px 6px rgba(0,0,0,0.5);
        letter-spacing: 3px;
        line-height: 1.2;
      }
      
      .landing-subtitle {
        font-size: 1.5rem;
        margin-bottom: 40px;
        max-width: 900px;
        line-height: 1.8;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        opacity: 0.9;
      }
      
      .enter-button {
        background-color: transparent;
        color: white;
        padding: 15px 50px;
        font-size: 1.2rem;
        border: 3px solid rgba(255,255,255,0.8);
        border-radius: 50px;
        cursor: pointer;
        transition: all 0.4s ease;
        backdrop-filter: blur(10px);
        font-weight: bold;
        letter-spacing: 1px;
      }
      
      .enter-button:hover {
        background-color: rgba(255,255,255,0.2);
        transform: scale(1.1);
        box-shadow: 0 0 30px rgba(255,255,255,0.4);
      }
      
      /* Dashboard content styles */
      #dashboard-content {
        background-color: #f0f4f8;
      }
      
      .title-panel {
        background: linear-gradient(135deg, #1a2a6c 0%, #b21f1f 100%);
        color: white;
        padding: 25px;
        font-size: 36px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 40px;
        border-radius: 15px;
        box-shadow: 0 8px 15px rgba(0,0,0,0.2);
        letter-spacing: 2px;
      }
      
      .stat-box {
        background: white;
        padding: 30px;
        border-radius: 20px;
        margin-bottom: 30px;
        box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        transition: all 0.4s ease;
        border-top: 6px solid #1a2a6c;
        position: relative;
        overflow: hidden;
      }
      
      .stat-box::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 6px;
        background: linear-gradient(to right, #1a2a6c, #b21f1f);
      }
      
      .stat-box:hover {
        transform: translateY(-10px);
        box-shadow: 0 15px 25px rgba(0,0,0,0.15);
      }
      
      .stat-box h4 {
        color: #1a2a6c;
        font-weight: 700;
        margin-bottom: 15px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .info-card {
        background-color: white;
        padding: 30px;
        border-radius: 20px;
        margin-bottom: 30px;
        box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        transition: all 0.4s ease;
        border-left: 6px solid #b21f1f;
      }
      
      .info-card:hover {
        transform: translateX(10px);
      }
      
      .info-card h3 {
        color: #b21f1f;
        font-weight: 700;
        margin-bottom: 20px;
        letter-spacing: 1px;
      }
      
      /* Navbar customization */
      .navbar {
        background: linear-gradient(135deg, #1a2a6c 0%, #b21f1f 100%);
        border: none;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .navbar-brand {
        color: white !important;
        font-weight: bold;
        letter-spacing: 1px;
      }
      
      .navbar-nav > li > a {
        color: rgba(255,255,255,0.8) !important;
        transition: all 0.3s ease;
      }
      
      .navbar-nav > li > a:hover {
        color: white !important;
        transform: scale(1.05);
      }
      
      .navbar-default .navbar-nav > .active > a, 
      .navbar-default .navbar-nav > .active > a:hover, 
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: rgba(255,255,255,0.2) !important;
        color: white !important;
      }
    "))
  ),
  
  # Login Page
  div(id = "login-page",
      class = "login-container",
      div(class = "login-box",
          h1("Disaster Analysis Dashboard", class = "login-title"),
          p("Explore comprehensive insights into global natural disasters", 
            class = "login-subtitle"),
          hr(style = "border-top: 1px solid #555; margin-bottom: 1.5rem;"),
          actionButton("login_btn", 
                       "Login with Google",
                       icon = icon("google"),
                       class = "google-btn w-100")
      )
  ),
  
  # Landing Page (initially hidden)
  hidden(
    div(id = "landing-page",
        class = "landing-page",
        h1(class = "landing-title", "Natural Disaster Analysis Platform"),
        p(class = "landing-subtitle", 
          "Explore comprehensive insights into global natural disasters through our interactive dashboard. Analyze patterns, predict impacts, and understand global trends with advanced analytics and visualization tools."),
        actionButton("enterDashboard", "Enter Dashboard", class = "enter-button")
    )
  ),
  
  # Dashboard Content (initially hidden)
  hidden(
    div(id = "dashboard-content",
        navbarPage(
          id = "mainNavbar",
          title = "Disaster Analysis Dashboard",
          tabPanel("Home",
                   fluidRow(
                     column(4, 
                            div(class = "stat-box",
                                h4("Total Disasters"), 
                                textOutput("totalDisasters"),
                                plotOutput("disasterTrendMini", height = "100px"))),
                     column(4, 
                            div(class = "stat-box",
                                h4("Total Deaths"), 
                                textOutput("totalDeaths"),
                                plotOutput("deathTrendMini", height = "100px"))),
                     column(4, 
                            div(class = "stat-box",
                                h4("Total Damage"), 
                                textOutput("totalDamage"),
                                plotOutput("damageTrendMini", height = "100px")))
                   ),
                   fluidRow(
                     column(6,
                            div(class = "info-card",
                                h3("Temporal Distribution"),
                                plotOutput("temporalTrends", height = "250px")
                            )
                     ),
                     column(6,
                            div(class = "info-card",
                                h3("Regional Impact"),
                                plotOutput("regionalImpact", height = "250px")
                            )
                     )
                   )
          ),
          navbarMenu("Visualizations",
                     tabPanel("Bar Plot",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("disasterType", "Disaster Type", 
                                              choices = NULL, selected = NULL, multiple = TRUE),
                                  sliderInput("yearRange", "Select Year Range",
                                              min = 2000, max = 2024, 
                                              value = c(2000, 2024), step = 1, sep = "")
                                ),
                                mainPanel(
                                  plotOutput("barPlot", height = "400px")
                                )
                              )
                     ),
                     tabPanel("Scatter Plot",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("scatterRegion", "Region",
                                              choices = NULL, selected = NULL, multiple = TRUE),
                                  sliderInput("yearRangeScatter", "Select Year Range",
                                              min = 2000, max = 2024,
                                              value = c(2000, 2024), step = 1, sep = "")
                                ),
                                mainPanel(
                                  plotOutput("scatterPlot", height = "400px")
                                )
                              )
                     )
          ),
          navbarMenu("Advanced Visualizations",
                     tabPanel("Correlation Heatmap",
                              plotOutput("correlationHeatmap", height = "500px")
                     ),
                     tabPanel("Disaster Type Boxplot",
                              plotOutput("disasterTypeBoxplot", height = "500px")
                     ),
                     tabPanel("Region Damage Violin Plot",
                              plotOutput("regionDamageViolin", height = "500px")
                     )
          ),
          tabPanel("Predict Death Toll",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("pred_disaster_type", "Disaster Type", choices = NULL),
                       selectInput("pred_region", "Region", choices = NULL),
                       numericInput("pred_damage", "Damage (000 US$)", value = 0),
                       numericInput("pred_year", "Year", value = 2024),
                       actionButton("predict", "Predict", 
                                    class = "btn btn-warning btn-block")
                     ),
                     mainPanel(
                       h4("Prediction Results"),
                       verbatimTextOutput("predictionOutput"),
                       plotOutput("featureImportance", height = "300px")
                     )
                   )
          )
        )
    )
  )
)

server <- function(input, output, session) {
  # Authentication states
  access_token <- reactiveVal(NULL)
  user_info <- reactiveVal(NULL)
  
  # Login handler
  observeEvent(input$login_btn, {
    tryCatch({
      # Initialize OAuth endpoint
      oauth <- oauth_endpoints("google")
      
      # Create OAuth app
      app <- oauth_app(
        "google",
        key = getOption("googleAuthR.webapp.client_id"),
        secret = getOption("googleAuthR.webapp.client_secret")
      )
      
      # Get OAuth token
      token <- oauth2.0_token(
        oauth,
        app,
        scope = getOption("googleAuthR.scopes.selected"),
        cache = FALSE
      )
      
      if (!is.null(token)) {
        access_token(token)
        
        # Get user info
        userinfo_url <- "https://www.googleapis.com/oauth2/v3/userinfo"
        user_response <- GET(
          url = userinfo_url,
          config = add_headers(
            Authorization = paste("Bearer", token$credentials$access_token)
          )
        )
        
        if (status_code(user_response) == 200) {
          user_data <- fromJSON(rawToChar(user_response$content))
          user_info(user_data)
          
          # Hide login, show landing
          hide("login-page")
          show("landing-page")
        } else {
          showNotification("Failed to get user info", type = "error")
        }
      }
    }, error = function(e) {
      showNotification(
        paste("Login failed:", e$message),
        type = "error"
      )
    })
  })
  
  # Landing page to dashboard transition
  observeEvent(input$enterDashboard, {
    hide("landing-page")
    show("dashboard-content")
  })
  
  # Logout handler
  observeEvent(input$logout, {
    auth_status(FALSE)
    user_info(NULL)
    # Reset to login page
    show("login-page")
    hide("landing-page")
    hide("dashboard-content")
  })
  
  data_lists <- data_preparation()
  full_data <- reactiveVal(data_lists$full_data)
  train_data <- data_lists$train_data
  
  # Train model
  rf_model <- reactive({
    train_rf_model(train_data)
  })
  
  observe({
    req(full_data())
    updateSelectInput(session, "disasterType", 
                      choices = unique(full_data()$Disaster.Type))
    updateSelectInput(session, "scatterRegion", 
                      choices = unique(full_data()$Region))
    updateSelectInput(session, "pred_disaster_type", 
                      choices = unique(full_data()$Disaster.Type))
    updateSelectInput(session, "pred_region", 
                      choices = unique(full_data()$Region))
  })
  filteredData <- reactive({
    req(full_data(), input$yearRange)
    df <- full_data() %>%
      filter(Start.Year >= input$yearRange[1] & 
               Start.Year <= input$yearRange[2])
    
    if (!is.null(input$disasterType) && length(input$disasterType) > 0) {
      df <- df %>% filter(Disaster.Type %in% input$disasterType)
    }
    df
  })
  output$totalDisasters <- renderText({
    format(nrow(filteredData()), big.mark = ",")
  })
  
  output$totalDeaths <- renderText({
    format(sum(filteredData()$Total.Deaths, na.rm = TRUE), big.mark = ",")
  })
  
  output$totalDamage <- renderText({
    paste0("$", format(sum(filteredData()$Total.Damage..Adjusted...000.US.., 
                           na.rm = TRUE), big.mark = ","), "K")
  })
  
  # Mini trends
  output$disasterTrendMini <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = Start.Year)) +
      geom_line(stat = "count", color = "#4285F4") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
  })
  
  output$deathTrendMini <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = Start.Year, y = Total.Deaths)) +
      geom_line(color = "#4285F4") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
  })
  
  output$damageTrendMini <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = Start.Year, 
                               y = Total.Damage..Adjusted...000.US..)) +
      geom_line(color = "#4285F4") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
  })
  output$correlationHeatmap <- renderPlot({
    req(full_data())
    numeric_data <- full_data() %>% 
      select(Start.Year, Total.Deaths, Total.Damage..Adjusted...000.US..)
    
    cor_matrix <- cor(numeric_data)
    
    # Melt the correlation matrix
    cor_melted <- as.data.frame(as.table(cor_matrix))
    names(cor_melted) <- c("Var1", "Var2", "value")
    
    ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab") +
      theme_minimal() +
      labs(title = "Correlation Heatmap of Numeric Variables",
           x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$disasterTypeBoxplot <- renderPlot({
    req(full_data())
    ggplot(full_data(), aes(x = Disaster.Type, y = Total.Deaths, fill = Disaster.Type)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Distribution of Deaths by Disaster Type",
           x = "Disaster Type", y = "Total Deaths") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d()
  })
  
  output$regionDamageViolin <- renderPlot({
    req(full_data())
    ggplot(full_data(), aes(x = Region, y = Total.Damage..Adjusted...000.US.., fill = Region)) +
      geom_violin(trim = FALSE) +
      theme_minimal() +
      labs(title = "Distribution of Damage by Region",
           x = "Region", y = "Total Damage (000 US$)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d() +
      scale_y_continuous(labels = comma)
  })
  
  # Prediction handler
  observeEvent(input$predict, {
    req(input$pred_disaster_type, input$pred_region, 
        input$pred_damage, input$pred_year)
    
    new_data <- data.frame(
      Start.Year = as.numeric(input$pred_year),
      Disaster.Type = input$pred_disaster_type,
      Region = input$pred_region,
      Total.Damage..Adjusted...000.US.. = as.numeric(input$pred_damage)
    )
    
    # Ensure new_data has same levels as training data
    new_data$Disaster.Type <- factor(new_data$Disaster.Type, 
                                     levels = levels(train_data$Disaster.Type))
    new_data$Region <- factor(new_data$Region, 
                              levels = levels(train_data$Region))
    
    # Predict
    prediction <- predict(rf_model(), newdata = new_data)
    
    output$predictionOutput <- renderText({
      paste0("Predicted Death Toll: ", 
             format(round(prediction), big.mark = ","))
    })
    
    # Feature Importance
    output$featureImportance <- renderPlot({
      importance_df <- data.frame(
        Feature = rownames(importance(rf_model())),
        Importance = importance(rf_model())[,1]
      ) %>% arrange(desc(Importance))
      
      ggplot(importance_df, aes(x = reorder(Feature, Importance), 
                                y = Importance)) +
        geom_bar(stat = "identity", fill = "#4285F4") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Feature Importance in Death Toll Prediction",
             x = "Features", y = "Importance Score")
    })
  })

  # Main plots
  output$temporalTrends <- renderPlot({
    req(filteredData())
    filteredData() %>%
      group_by(Start.Year, Disaster.Type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ggplot(aes(x = Start.Year, y = count, color = Disaster.Type)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Temporal Distribution of Disasters",
           x = "Year", y = "Number of Disasters") +
      theme(legend.position = "bottom") +
      scale_color_viridis_d()
  })
  
  output$regionalImpact <- renderPlot({
    req(filteredData())
    filteredData() %>%
      group_by(Region) %>%
      summarise(total_deaths = sum(Total.Deaths, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Region, total_deaths), y = total_deaths)) +
      geom_bar(stat = "identity", fill = "#4285F4") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Regional Impact",
           x = "Region", y = "Total Deaths") +
      scale_y_continuous(labels = comma)
  })
  
  # Visualization tab plots
  output$barPlot <- renderPlot({
    req(filteredData())
    ggplot(filteredData(), aes(x = Disaster.Type, fill = Disaster.Type)) +
      geom_bar() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Disaster Types",
           x = "Disaster Type", y = "Count") +
      scale_fill_viridis_d()
  })
  
  output$scatterPlot <- renderPlot({
    req(filteredData(), input$scatterRegion)
    scatter_data <- filteredData()
    if (length(input$scatterRegion) > 0) {
      scatter_data <- scatter_data %>% 
        filter(Region %in% input$scatterRegion)
    }
    
    ggplot(scatter_data, aes(x = Total.Damage..Adjusted...000.US.., 
                             y = Total.Deaths, color = Region)) +
      geom_point(alpha = 0.6) +
      scale_x_log10(labels = comma) +
      scale_y_log10(labels = comma) +
      theme_minimal() +
      labs(title = "Deaths vs Damage",
           x = "Damage (000 US$)", y = "Deaths")
  })
  
  # Death toll prediction
  model <- reactive({
    req(data())
    # Replace with your actual model training code
    train_data <- data() # Add proper data preparation here
    randomForest(Total.Deaths ~ Start.Year + Disaster.Type + Region + 
                   Total.Damage..Adjusted...000.US.., 
                 data = train_data)
  })
  
  observeEvent(input$predict, {
    req(input$pred_disaster_type, input$pred_region, 
        input$pred_damage, input$pred_year)
    
    new_data <- data.frame(
      Start.Year = as.numeric(input$pred_year),
      Disaster.Type = factor(input$pred_disaster_type, 
                             levels = levels(train_data$Disaster.Type)),
      Region = factor(input$pred_region, 
                      levels = levels(train_data$Region)),
      Total.Damage..Adjusted...000.US.. = as.numeric(input$pred_damage)
    )
    
    prediction <- predict(model(), newdata = new_data)
    
    output$predictionOutput <- renderText({
      paste0("Predicted Death Toll: ", 
             format(round(prediction), big.mark = ","))
    })
    
    output$featureImportance <- renderPlot({
      importance_df <- data.frame(
        Feature = rownames(importance(rf_model())),
        Importance = importance(rf_model())[,1]
      ) %>% arrange(desc(Importance))
      
      ggplot(importance_df, aes(x = reorder(Feature, Importance), 
                                y = Importance)) +
        geom_bar(stat = "identity", fill = "#4285F4") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Feature Importance in Death Toll Prediction",
             x = "Features", y = "Importance Score")
    })
  })
}
shinyApp(ui = ui, server = server)
