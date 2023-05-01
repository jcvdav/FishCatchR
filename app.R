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
library(googledrive)
library(googlesheets4)
library(DT)
library(tidyverse)
library(fresh)
library(shinydashboard)
library(shiny)

source("functions.R")

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

my_theme <- create_theme(
  adminlte_color(
    blue = "#175E54",
    gray_lte = "#175E54", 
    red = "#8C1515",
    light_blue = "#8C1515",
    aqua = "#175E54"
  ),
  adminlte_global(info_box_bg = "#175E54")
)


################################################################################
# Model parameters
K <- 100                                                                        # population carrying capacity
iterations <- 100                                                               # number of iterations
gr <- 0.1                                                                       # 10 percent annual rate of increase
catprob <- 0.1                                                                  # probability of a catastrophic event
catmort <- 0.5                                                                  # catastrophic mortality

# Derivated parameters
N <- K                                                                          # Initial Conditions
set.seed(20)
ce <- ifelse(runif(iterations) > catprob, 1, (1 - catmort))                     # vector of mortalities

df <- tibble(
  last_N = 0,
  H = 0,
  E = 0,
  Nt = N,
  h = 0,
  ce = 0,
  t = 0
)

## DEFINE UI ###################################################################
# Define APP header

header <- dashboardHeader(
  disable = T,
  title = "Economic Games App (beta)"
  )

sidebar <- dashboardSidebar(
  disable = T,
  sidebarMenu(
    menuItem(
      tabName = "game",
      text = "Juego",
      icon = icon("fish"))
  )
)

body <- dashboardBody(
  # use_theme(my_theme),
  tabItems(
    tabItem(
      tabName = "game",
      fluidRow(
        box(
          plotOutput(
            outputId = "pop_space",
            height = 300
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
            fluidRow(
              infoBoxOutput(
                outputId = "your_catch",
                width = 6
              ),
              infoBoxOutput(
                outputId = "others_total_catch",
                width = 6
              )
            ),
          wellPanel(
            fluidRow(
              column(
                width = 10,
                uiOutput(
                  outputId = "harvest_slider"
                )
                ),
              column(
                width = 2,
                actionButton(
                  inputId = "go_fish",
                  label = "Pescar",
                  icon = icon("fish")
                )
              )
            )
          )
        )
      )
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
        ),
        box(
          width = 12,
          plotOutput(
            outputId = "pop_ts"
          )
        )
      )
    )
  )
)

# Combine all elements
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    use_theme(my_theme))

# Define server logic
server <- function(input, output) {
  session_id <- openssl::md5(timestamp(quiet = T))
  
  info_modal <- modalDialog(
    title = "Paso 1 de 2",
    size = "l",
    fluidRow(
      box(
        title = "Ingresa tus datos:",
        width = 12,
        status = "primary",
        checkboxGroupInput(
          inputId = "age_bracket",
          label = "Rango de edad:",
          choices = c("menos de 20" = 10,
                      "20-30" = 25,
                      "30-40" = 35,
                      "40-50" = 45,
                      "50-60" = 55,
                      "más de 60" = 65,
                      "Ninguno" = 0),
          inline = T),
        checkboxGroupInput(
          inputId = "gender",
          label = "Género",
          choices = c("Femenino" = "F",
                      "Masculino" = "M",
                      "Otro" = "O",
                      "Ninguno" = "N"),
          inline = T),
        checkboxGroupInput(
          inputId = "region",
          label = "Región",
          choices = c("BC Pacifico" = 1,
                      "Golfo de California" = 2,
                      "Pacífico" = 3,
                      "Golfo de México" = 4,
                      "Caribe" = 5,
                      "Ninguno" = 0)
        )
      )
    ),
    easyClose = F,
    footer = tagList(
      actionButton(
        inputId = "next_step",
        label = "Siguiente")
    )
  )
  
  instructions_modal <- modalDialog(
    title = "Paso 2 de 2",
    fluidRow(
      box(
        width = 12,
        status = "primary",
        title = "Instrucciones",
        p("Your objective is to fish as much as possible, without collapsing the stock"),
        p("1) Observa el tamaño de la población"),
        p("2) Elige el tamaño de tu captura"),
        p("3) Usa el botón de `Pescar`")
      ) 
    ),
    easyClose = F,
    footer = tagList(
      actionButton(
        inputId = "done",
        label = "Comenzar")
    )
  )
  
  values <- reactiveValues(
    game = 1,
    i = 1,
    N = N,
    gr = gr,
    ce = ce,
    df = df
  )
  
  # Show modal windows
  showModal(info_modal)

  # Close and go to next one
  observeEvent(
    input$next_step, {
      removeModal()
      showModal(instructions_modal)
      
      file_id <- paste0(session_id, "_",
                        values$game, "_",
                        input$age_bracket, "_",
                        input$gender, "_",
                        input$region)
      
      values$gsheet_id <- make_table(file_id, data = df)
    })
  
  # Close and start game
  observeEvent(
    input$done, {
      removeModal()
    })
  
  observeEvent(
    input$restart, {
      removeModal()
      
      file_id <- paste0(session_id, "_",
                        values$game, "_",
                        input$age_bracket, "_",
                        input$gender, "_",
                        input$region)
      
      values$gsheet_id <- make_table(new_name = file_id,
                                     data = df)
      
      values$df <- tibble(
        last_N = 0,
        H = 0,
        E = 0,
        Nt = N,
        h = 0,
        ce = 0,
        t = 0
      )
      
      values$N <- N
      values$i <- 1
    })
  
  
  observeEvent(
    input$go_fish, {
      values_this_round <- get_N(
        harvest = input$harvest,
        N = values$N,
        K = K,
        gr = values$gr,
        ce = values$ce[values$i],
        t = values$i)
      
      values$i <- values$i + 1
      
      values$df <- rbind(values$df,
                         values_this_round)
      
      # Update data on Google Drive
      sheet_append(ss = values$gsheet_id,
      data = values_this_round)
      # Update population size
      values$N <- tail(values$df$Nt, 1)
      
      if(values$N == 0) {
        values$game <- values$game + 1
        showModal(
          modalDialog(
            title = "Colapsó la población",
            fluidRow(
              p("Pescaste demasiado."),
              p("Jugar de nuevo?")
            ),
            easyClose = F,
            footer = tagList(
              actionButton(
                inputId = "restart",
                label = "Reiniciar")
            )
          )
        )
      }
    })
  
   #  GAME SIDE #################################################################
  
  output$harvest_slider <- renderUI({
    df <- values$df
    
    sliderInput(
      inputId = "harvest",
      label = paste0("Elige tu captura para la ronda ", values$i, ":"),
      min = 0,
      step = 1,
      max = min(values$N, 5),
      value = 0)
  })
  
  output$pop_space <- renderPlot({
    req(input$harvest)
    df <- tail(values$df, 1)
    pop <- floor(df$Nt)
    
    set.seed(1)
    data <- tibble(x = runif(n = df$Nt, min = 0, max = 10),
                   y = runif(n = df$Nt, min = 0, max = 10),
                   to_catch = c(rep(T, times = input$harvest), rep(F, times = df$Nt - input$harvest)),
                   image = ifelse(to_catch,
                                  here::here("www", "caught_fish.png"),
                                  here::here("www", "fish.png")))
    
    
    ggplot(data = data,
                mapping = aes(x = x, y = y, image = image)) +
      geom_image(size = 0.075) +
      lims(x = c(-0.5, 10.5),
           y = c(-0.5, 10.5)) +
      coord_equal() +
      theme_void() +
      labs(caption = paste0("Población: ", pop)) +
      theme(plot.caption = element_text(
        size = 15,
        hjust = 0.5,
        colour = case_when((pop > (0.75 * K)) ~ "#175E54",
                           between(pop, 0.5 * K, 0.75 * K) ~ "darkorange1",
                           (pop < (0.5 * K)) ~ "#8C1515"),
        face = "bold"))
    
  })
  
  output$your_catch <- renderInfoBox({
    req(input$harvest)
    df <- values$df
    infoBox(title = "Tu ultima captura:",
            subtitle = paste0("Todas tus capturas:", sum(df$h)),
            value = tail(df$h, 1),
            icon = icon("user"))
  })
  
  output$others_total_catch <- renderInfoBox({
    req(input$harvest)
    df <- values$df
    infoBox(title = "Captura de todos",
            subtitle = paste0("Captura total de todos:", sum(df$H)),
            value = tail(df$H, 1),
            icon = icon("users"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
