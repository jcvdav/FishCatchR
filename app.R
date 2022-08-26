#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(here)
library(ggimage)
library(DT)
library(tidyverse)
library(shiny)
library(shinydashboard)

source("functions.R")


################################################################################
# Model parameters
K <- 100                                                                        # population carrying capacity
iterations <- 50                                                                # number of iterations
gr <- 0.1                                                                       # 10 percent annual rate of increase
catprob <- 0.1                                                                  # probability of a catastrophic event
catmort <- 0.5                                                                  # catastrophic mortality

# Derivated parameters
N <- 0.75 * K                                                                   # Initial Conditions
set.seed(20)
ge <- rlnormcustom(iterations, m = 1, s = 0.1)                                  # vector of shocks
ce <- ifelse(runif(iterations) > catprob, 1, (1 - catmort))                     # vector of mortalities
df <- tibble(Nc = N, Np = 0, obsN = 0, h = 0, TotCatch = 0, ge = 0, ce = 0, t = 0)

## DEFINE UI ###################################################################
# Define APP header

header <- dashboardHeader(
  title = "Economic Games App (beta)"
  )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      tabName = "game",
      text = "Juego",
      icon = icon("fish")),
    menuItem(
      tabName = "results",
      text = "Resultados",
      icon = icon("table")
    ),
    menuItem(
      tabName = "settings",
      text = "Ajustes",
      icon = icon("gear")
      )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "game",
      h2("Updated August 26, 2022"),
      fluidRow(
        box(
          title = "Juego recreado",
          status = "primary",
          width = 12,
          column(
            width = 4,
            box(
              width = 12,
              uiOutput(
                outputId = "harvest_slider"
              ),
              actionButton(
                inputId = "button",
                label = "Go fish!"
              ),
            )
          ),
          column(
            width = 8,
            box(
              width = 12,
              plotOutput(
                outputId = "pop_space"
              ),
              plotOutput(
                outputId = "catch_others"
              ),
              # plotOutput(
              #   outputId = "pop_ts"
              # ) 
            )
          )
        )
      ),
    ),
    tabItem(
      tabName = "results",
      fluidRow(
        box(
          title = "Results",
          width = 12,
          dataTableOutput(
            outputId = "results"
          )
        )
      )
    ),
    tabItem(
      tabName = "settings",
      fluidRow(
        box(
          title = "Population parameters",
          status = "primary",
          width = 12,
          collapsible = T,
          column(
            width = 4,
            box(
              width = 12,
              numericInput(
                inputId = "iterations",
                label = "Iterations",
                min = 1,
                max = 50,
                value = iterations
              ),
              numericInput(
                inputId = "InitCond",
                label = "Initial conditions",
                min = 0,
                max = K,
                value = N
              ),
              numericInput(
                inputId = "gr",
                label = "Growht rate",
                min = 0,
                max = 1,
                value = gr
              ),
              numericInput(
                inputId = "catprob",
                label = "Probability of catastrophic event",
                min = 0,
                max = 1,
                value = catprob
              ),
              numericInput(
                inputId = "catmort",
                label = "Catastrophic mortality rate",
                min = 0,
                max = 1,
                value = catmort
              ),
              actionButton(
                inputId = "update_pars",
                label = "Update & restart"
              )
            )
          ),
          column(
            width = 8,
            box(
              width = 12,
              plotOutput(outputId = "pop_plot"),
              plotOutput(outputId = "ge_plot"),
              plotOutput(outputId = "ce_plot")
            )
          )
        )
      )
    )
  )
)

# Combine all elements
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# Define server logic
server <- function(input, output) {
  
  query_modal <- modalDialog(
    title = "Bienvenido",
    size = "l",
    fluidRow(
        box(
          width = 12,
          status = "primary",
          title = "Instrucciones",
          p("1) The school of fish reflects current population size."),
          p("2) The graph on the bottom will show you the total catch by other fishers in the previous round."),
          p("3) Use the slider on the left to indicate the number of fish you would like to harvest. The school of fish will be colored to reflect this."),
          p("4) When you are sure how many fish you want to harvest, click on the `Go Fish!` button."),
          p("5) Both graphs will update to show you catch by others in the last round, and the new population size.")),
        box(
          width = 12,
          status = "primary",
          title = "Other notes",
          p("Use the results tab to look at all the data"),
          p("Use the settings tab to reconfigure parameters"),
          p("Both of this can be password protected")
        )
    ),
    easyClose = F,
    footer = tagList(
      actionButton(
        inputId = "go",
        label = "Comenzar")
    )
  )
  
  showModal(query_modal)
  observeEvent(input$go, {
    removeModal()
  })
  
  values <- reactiveValues(
    i = 1,
    df = df,
    iterations = iterations,
    N = N,
    gr = gr,
    ge = ge,
    ce = ce
  )
  
  observeEvent(
    input$update_pars, {
      values$i <- 1
      values$iterations <- input$iterations
      values$N <- input$InitCond
      values$gr <- input$gr
      set.seed(20)
      values$ge <- rlnormcustom(values$iterations, m = 1, s = 0.1)                            
      values$ce <- ifelse(runif(values$iterations) > input$catprob, 1, (1 - input$catmort))               
      values$df <- tibble(Nc = values$N, Np = 0, obsN = 0, TotCatch = 0, ge = 0, ce = 0, t = 0)
  })
  
  output$test <- renderText(values$ce)
  
  observeEvent(
    input$button, {
      values$i <- values$i + 1
      values$df <- rbind(values$df,
                         get_N(harvest = input$harvest,
                               N = N,
                               K = K,
                               gr = values$gr,
                               ge = values$ge[values$i],
                               ce = values$ce[values$i],
                               FishermenType = c("A", "M", "C", "C"),
                               i = values$i
                         ))
      values$N <- tail(values$df$Nc, 1)
    })
  
  ## SYSTEM SETTINGS SIDE ######################################################

  output$ge_plot <- renderPlot({
    tibble(ge = values$ge,
           time = 1:length(ge)) %>% 
      ggplot(aes(x = time, y = ge)) + 
      geom_line() + 
      labs(x = "time",
           y = "Background Env. Variability") +
      theme_bw()
  })
  
  output$ce_plot <- renderPlot({
    tibble(ce = values$ce,
           time = 1:length(ce)) %>% 
      ggplot(aes(x = time, y = ce)) + 
      geom_col() + 
      lims(y = c(0, 1)) +
      labs(x = "time",
           y = "Survival from cat event") +
      theme_bw()
  })
  
  output$pop_plot <- renderPlot({
    n <- numeric(length = values$iterations)
    n[1] <- N  # set the initial condition
    
    for (i in (1:(values$iterations - 1))){  # run the simulation
      
      n[i+1] <- SRfun(n[i], K, values$gr, values$ge[i], values$ce[i])
      
    }
    
    tibble(N = n,
           time = 1:length(N)) %>% 
      ggplot(aes(x = time, y = N)) + 
      geom_line() +
      geom_hline(yintercept = K, linetype = "dashed") +
      geom_vline(xintercept = c(1+which(values$ce == catmort)), color = "red") +
      theme_bw()
  })
  
  #  GAME SIDE #################################################################
  
  output$harvest_slider <- renderUI({
    sliderInput(
      inputId = "harvest",
      label = "Harvest (N):",
      min = 0,
      max = floor(values$N),
      value = 0)
  })
  
  output$pop_space <- renderPlot({
    req(input$harvest)
    df <- tail(values$df, 1)
    
    set.seed(1)
    data <- tibble(x = runif(n = df$Nc, min = 0, max = 10),
                   y = runif(n = df$Nc, min = 0, max = 10),
                   to_catch = c(rep(T, times = input$harvest), rep(F, times = df$Nc - input$harvest)),
                   image = ifelse(to_catch,
                                  here::here("www", "caught_fish.png"),
                                  here::here("www", "fish.png")))
    
    
    ggplot(data = data,
                mapping = aes(x = x, y = y, image = image)) +
      geom_image(size = 0.05,
                 position = position_jitter(width = 0.5, height = 0.5)) +
      lims(x = c(-1, 11),
           y = c(-1, 11)) +
      coord_equal() +
      scale_color_manual(values = c("red", "black")) +
      theme_void() +
      ggtitle(label = paste0("There are ", floor(df$Nc), " fish"))
      
    
  })
  
  output$catch_others <- renderPlot({
    ggplot(values$df, aes(x = t, y = TotCatch - h)) +
      geom_col() +
      theme_bw() +
      lims(x = c(-1, values$iterations),
           y = c(0, K / 5)) +
      labs(x = "Ronda",
           y = "Captura de los demás") +
      theme_bw()
  })
  
  output$results <- renderDataTable({
    DT::datatable(data = values$df)
  })
  
  # output$pop_ts <- renderPlot({
  #   
  #   p <- ggplot(values$df, aes(x = t, y = Nc)) +
  #     geom_point() +
  #     theme_bw() +
  #     lims(x = c(0, values$iterations), y = c(0, 120)) +
  #     theme_bw()
  #   
  #   if(dim(values$df)[1] > 1){
  #     p <- p + geom_line()
  #   }
  #   p
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
