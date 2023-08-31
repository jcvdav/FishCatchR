pacman::p_load(
  here,
  ggimage,
  googledrive,
  googlesheets4,
  tidyverse,
  shiny,
  shinyMobile
)

source("functions.R")

## DEFINE SOME BASICS ##########################################################
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# Model parameters
K <- 100                                                                        # population carrying capacity
rounds <- 15                                                                    # number of rounds
gr <- 0.1                                                                       # 10 percent annual rate of increase
mort_prob <- 0.1                                                                  # probability of a catastrophic event
mort <- 0.5                                                                  # catastrophic mortality

# Derivated parameters
N <- K                                                                          # Initial Conditions
# set.seed(20)
# vector of survivals
s <- sample(x = c(1, 1 - mort),
            size = rounds,
            replace = T,
            prob = c(1 - mort_prob, mort_prob))

df <- tibble(
  last_N = 0,
  H = 0,
  E = 0,
  Nt = N,
  h = 0,
  s = 0,
  t = 0
)

pop_space_master <- tibble(x = runif(n = K, min = -5, max = 5),
                           y = runif(n = K, min = -5, max = 5)) %>% 
  mutate(dist = sqrt(x ^ 2 + y ^ 2)) %>% 
  arrange(dist)

## DEFINE UI ###################################################################
body <- f7SingleLayout(
  navbar = f7Navbar(
    title = "Título del juego",
    hairline = TRUE,
    shadow = TRUE
  ),
  toolbar = f7Toolbar(
    position = "top",
    # uiOutput(outputId = "your_catch"),
    uiOutput(outputId = "your_hist_catch"),
    uiOutput(outputId = "everyones_catch")
  ),
  # main content
  intensity = 16,
  hover = TRUE,
  f7Card(
    plotOutput(
      outputId = "pop_space",
      height = 300
    ),
    f7Card(
      uiOutput(outputId = "pop"),
      f7Slider(
        inputId = "harvest",
        label = "Elige tu captura para la ronda:",
        min = 0,
        max = 5,
        value = 0,
        scale = TRUE
      ),
      footer = tagList(
        f7Button(
          inputId = "go_fish",
          label = "Pescar"
        )
      )
    )
  )
)

ui <- f7Page(
  options = list(dark = F),
  title = "Título del juego",
  body
)

# Define server logic
server <- function(input, output) {
  session_id <- openssl::md5(timestamp(quiet = T))
  
  values <- reactiveValues(
    game = 1,
    i = 1,
    N = N,
    gr = gr,
    s = s,
    df = df
  )
  
  # Show modal windows
  f7Popup(
    id = "next_step",
    title = "Información del jugador",
    closeButton = TRUE,
    closeByBackdropClick = FALSE,
    closeOnEscape = FALSE,
    swipeToClose = FALSE,
    animate = FALSE,
    f7Card(
      f7Picker(
        inputId = "age_bracket",
        label = "Indica tu rango de edad:",
        choices = c("Ninguno",
                    "0-20",
                    "20-30",
                    "30-40",
                    "40-50",
                    "50-60",
                    "60+")
      ),
      # f7checkBoxGroup(
      #   inputId = "gender",
      #   label = "Género",
      #   choices = c("Femenino" = "F",
      #               "Masculino" = "M",
      #               "Otro" = "O",
      #               "Ninguno" = "N"),
      # ),
      f7Picker(
        inputId = "region",
        label = "Indica tu región",
        choices = c("Ninguna",
                    "BC Pacifico",
                    "Golfo de California",
                    "Pacífico",
                    "Golfo de México",
                    "Caribe")
      ),
      footer = tagList(f7Block("Cierra este mensaje para ver las instrucciones"))
    )
  )
  
  # Ventana de instrucciones
  observeEvent(
    input$next_step, {
      req(input$age_bracket,
          # input$gender,
          input$region)
      
      if(!input$next_step) {
        f7Popup(
          id = "done",
          title = "Instrucciones",
          f7Block(
            h1("Reglas del juego:"),
            p("1) Vamos a jugar 15 rondas"),
            p("2) Por cada pez que captures recibirás un boleto para la rifa de X"),
            p("3) Puedes pescar entre 1 y 5 peces"),
            p("4) Usa el botón de `Pescar`"),
            p(""),
            p("Cierra este mensaje para empezar a jugar")
          )
        )
        
        # Encoede file name parameters
        encoded_age <- encode_age(input$age_bracket)
        encoded_region <- encode_region(input$region) 
        
        file_id <- paste0(session_id, "_",
                          values$game, "_",
                          encoded_age, "_",
                          # input$gender, "_",
                          encoded_region)
        
        
        values$gsheet_id <- make_table(file_id, data = df)
      }
    })
  
  # Close and start game
  # observeEvent(
  # input$done, {
  # removeModal()
  # })
  
  observeEvent(
    input$restart, {
      
      # Encoede file name parameters
      encoded_age <- encode_age(input$age_bracket)
      encoded_region <- encode_region(input$region) 
      
      file_id <- paste0(session_id, "_",
                        values$game, "_",
                        encoded_age, "_",
                        # input$gender, "_",
                        encoded_region)
      
      values$gsheet_id <- make_table(new_name = file_id,
                                     data = df)
      
      values$df <- tibble(
        last_N = 0,
        H = 0,
        E = 0,
        Nt = N,
        h = 0,
        s = 0,
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
        s = values$s[values$i],
        t = values$i)
      
      if(values$s[values$i] < 1) {
        f7Notif(
          text = "Oh no! Hubo mortalidad!",
          icon = f7Icon("fish"),
          title = paste("Notification"),
          subtitle = "A subtitle"
        )
      }
      
      values$i <- values$i + 1
      
      values$df <- rbind(values$df,
                         values_this_round)
      
      # Update data on Google Drive
      sheet_append(ss = values$gsheet_id,
                   data = values_this_round)
      # Update population size
      values$N <- tail(values$df$Nt, 1)
      
      if(values$N == 0 | values$i == 15) {
        values$game <- values$game + 1
        
        f7Dialog(
          id = "restart",
          type = "confirm",
          title = ifelse(values$i == 15, "Fin del juego", "Se acabó el recurso"),
          "Quieres volver a jugar?"
        )
      }
    })
  
  #  GAME SIDE #################################################################
  
  observeEvent(
    input$go_fish, {
      updateF7Slider(
        inputId = "harvest",
        min = 0,
        max = min(values$N, 5),
        value = 0,
        scale = TRUE,
        scaleSubSteps = 1
      )
    })
  
  output$pop_space <- renderPlot({
    df <- tail(values$df, 1)
    pop <- floor(df$Nt)
    
    
    data <- pop_space_master[1:df$Nt, ]
    
    ggplot(data = data,
           mapping = aes(x = x,
                         y = y,
                         image = here::here("www", "fish.png"))) +
      geom_image(size = 0.075) +
      lims(x = c(-5.5, 5.5),
           y = c(-5.5, 5.5)) +
      coord_equal() +
      theme_void()
    
  })
  
  # output$your_catch <- renderUI({
  #   # req(input$harvest)
  #   f7Chip(label = paste("Tu captura pasada:", tail(values$df$h, 1)),
  #          icon = f7Icon("person"),
  #          iconStatus = "blue")
  # })
  
  output$your_hist_catch <- renderUI({
    # req(input$harvest)
    f7Chip(label = paste("Tus capturas totales:", sum(values$df$h)),
           icon = f7Icon("person"),
           iconStatus = "blue")
  })
  
  output$everyones_catch <- renderUI({
    # req(input$harvest)
    f7Chip(label = paste("Captura del grupo:", tail(values$df$H, 1)),
           icon = f7Icon("person_3"),
           iconStatus = "blue")
  })
  
  output$pop <- renderUI({
    df <- tail(values$df, 1)
    pop <- floor(df$Nt)
    # req(input$harvest)
    f7Chip(label = paste("Tamaño de la pobación:", pop),
           # icon = f7Icon("person_3"),
           iconStatus = case_when((pop > (0.75 * K)) ~ "green",
                                  between(pop, 0.5 * K, 0.75 * K) ~ "orange",
                                  (pop < (0.5 * K)) ~ "red"))
  })
  
}

shinyApp(ui = ui, server = server)

