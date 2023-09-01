pacman::p_load(
  here,
  # ggimage,
  emojifont,
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

## DEFINE UI ###################################################################
body <- f7SingleLayout(
  navbar = f7Navbar(
    title = "Título del juego",
    hairline = TRUE,
    shadow = TRUE
  ),
  toolbar = f7Toolbar(
    position = "top",
    uiOutput(outputId = "your_hist_catch"),
    uiOutput(outputId = "everyones_catch")
  ),
  # main content
  f7Shadow(
    intensity = 16,
    hover = TRUE,
    f7Card(
      plotOutput(
        outputId = "pop_space",
        # width = "90%"
        height = 300
      ),
      f7Card(
        f7Row(uiOutput(outputId = "round"),
              uiOutput(outputId = "pop")),
        # f7Row(
          f7Slider(
            inputId = "harvest",
            label = "Elige tu captura para la ronda:",
            min = 0,
            max = 5,
            value = 0,
            scale = TRUE
          # )
        ),
        footer = tagList(
          f7Button(
            inputId = "go_fish",
            label = "PESCAR!"
          )
        )
      )
    )
  )
)

ui <- f7Page(
  options = list(theme = "ios",
                 filled = T,
                 color = "#08519B"),
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
    df = df,
    game_color = game_color,
  )
  
  # Show modal windows
  f7Popup(
    id = "next_step",
    title = "Ventana 1 / 2",
    closeButton = TRUE,
    closeByBackdropClick = FALSE,
    closeOnEscape = FALSE,
    swipeToClose = FALSE,
    animate = FALSE,
    f7Card(title = "Ingresa tus datos (opcional):",
      f7Picker(
        inputId = "age_bracket",
        label = "Rango de edad:",
        choices = c("Ninguno",
                    "0-20",
                    "21-30",
                    "31-40",
                    "41-50",
                    "51-60",
                    "61+")
      ),
      f7Picker(
        inputId = "region",
        label = "Región (opcional):",
        choices = c("Ninguna",
                    "BC Pacifico",
                    "Golfo de California",
                    "Pacífico",
                    "Golfo de México",
                    "Caribe")
      )
    ),
    f7Card(
      p("Si quieres participar en la rifa de X, ingresa tu número de celular
        para que podamos contactarte:"),
      f7Text(inputId = "phone",
             label = "10 dígitos de tu teléfono:",
             placeholder = "xxx-xxx-xx-xx"),
      footer = tagList(
        p("Cierra este mensaje para ver las reglas del juego")
      )
    )
  )
  
  # Ventana de instrucciones
  observeEvent(
    input$next_step, {
      req(input$age_bracket,
          input$region)
      
      if(!input$next_step) {
        f7Popup(
          id = "done",
          title = "Ventana 2 / 2",
          f7Block(
            h1("Reglas del juego:"),
            p("1) Cada juego tiene 15 rondas"),
            p("2) Puedes pescar entre 1 y 5 peces en cada ronda"),
            p("3) Cuando hayas escogido cuanto quieres pescar, usa el botón de 'PESCAR!', se ve así:"),
            f7Button(inputId = "NA",
                     label = "PESCAR!"),
            p("Por cada pez que hayas capturdo al final de cada juego recibirás un boleto para la rifa de X."),
            p("Cierra este mensaje para empezar a jugar")
          )
        )
        
        # Encode file name parameters
        print(input$phone)
        encoded_age <- encode_age(input$age_bracket)
        encoded_region <- encode_region(input$region)
        encoded_phone <- encode_phone(input$phone)
        
        file_id <- paste(session_id,
                         encoded_age,
                         encoded_region,
                         encoded_phone,
                         sep = "_")
        
        
        values$gsheet_id <- make_table(file_id,
                                       data = df)
      }
    })

  observeEvent(
    input$restart, {
      
      if(!input$restart) {
        f7Notif(title = "Gracias por jugar",
                subtitle = "Desconectando...",
                text = "Adios")
        Sys.sleep(3)
        stopApp()
      }
      
      # Update game counter
      values$game <- values$game + 1
      
      # Encoede file name parameters
      encoded_age <- encode_age(input$age_bracket)
      encoded_region <- encode_region(input$region) 
      
      file_id <- paste0(session_id, "_",
                        # values$game, "_",
                        encoded_age, "_",
                        # input$gender, "_",
                        encoded_region)
      
      values$df <- rbind(values$df,
                         df)
      
      values$N <- N
      values$i <- 1
      values$s <- get_s()
      values$game_color <- "#B1182B"
      
      f7Notif(
        title = paste("Alerta!"),
        subtitle = "Hay cambio climático",
        text = "En 1 de cada 10 rondas puede haber ondas de calor que matan a la mitad del recurso",
        icon = f7Icon("flame"),
      )
      
    })
  
  # This is the fishing module -------------------------------------------------
  observeEvent(
    input$go_fish, {
      values_this_round <- get_N(
        harvest = input$harvest,
        N = values$N,
        K = K,
        gr = values$gr,
        s = values$s[values$i],
        t = values$i,
        g = values$game)
      
      if(values$s[values$i] < 1) {
        f7Notif(
          title = paste("¡Alerta!"),
          subtitle = "¡Hubo mortalidad!",
          text = "Una onda de calor mató a la mitad del recurso",
          icon = f7Icon("flame"),
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
      
      if(values$N <= 0 | values$i == 15) {
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
    input$go_fish | input$restart, {
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
    bug_colors <- rev(c(rep(values$game_color, pop - input$harvest), rep("black", input$harvest)))
    
    base_plot +
      geom_text(data = head(pop_space_master, df$Nt),
                mapping = aes(x = x,
                              y = y,
                              label = emoji("crab")),
                family = "EmojiOne",
                fontface = "bold",
                size = 20,
                color = bug_colors)
    
  })
  
  output$your_hist_catch <- renderUI({
    # req(input$harvest)
    h <- max(0, 
             sum(values$df$h[values$df$g == values$game]),
             na.rm = T)
    f7Chip(label = paste("Tus capturas totales:", h),
           icon = f7Icon("person"),
           iconStatus = "blue",
           status = "gray")
  })
  
  output$everyones_catch <- renderUI({
    # req(input$harvest)
    H <- max(0,
             tail(values$df$H[values$df$g == values$game], 1),
             na.rm = T)
    f7Chip(label = paste("Captura del grupo:", H),
           icon = f7Icon("person_3"),
           iconStatus = "blue",
           status = "gray")
  })
  
  output$pop <- renderUI({
    
    pop <- floor(tail(values$df$Nt, 1))
    
    f7Chip(label = paste("Población:",
                         pop),
           status = case_when((pop > (0.75 * K)) ~ "green",
                              between(pop, 0.5 * K, 0.75 * K) ~ "orange",
                              (pop < (0.5 * K)) ~ "red"))
  })
  
  output$round <- renderUI({
    
    f7Chip(label = paste("Ronda:", values$i, "de 15"),
           icon = f7Icon("clock"),
           iconStatus = "blue")
  })
  
}

shinyApp(ui = ui, server = server)

