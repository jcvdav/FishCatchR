pacman::p_load(
  here,
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
    title = "La Pesca Cambiante",
    hairline = TRUE,
    shadow = TRUE
  ),
  # toolbar = f7Toolbar(
  #   position = "top",
  #   f7Row()
  # ),
  # main content
  f7Shadow(
    intensity = 24,
    hover = TRUE,
    f7Card(
      plotOutput(
        outputId = "pop_space",
        height = 300
      ),
      f7Card(
        f7Row(uiOutput(outputId = "everyones_catch"),
              uiOutput(outputId = "your_catch")),
        f7Row(uiOutput(outputId = "pop"),
              uiOutput(outputId = "round")),
        f7Slider(
          inputId = "harvest",
          label = "Elige tu captura para este viaje:",
          min = 0,
          max = 5,
          value = 0,
          scale = TRUE
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
  title = "PescaCambiante",
  body
)

## Define server logic #########################################################
server <- function(input, output) {
  bug <- sample(x = c("crab", "fish", "shrimp"),
                size = 1)
  session_id <- str_replace_all(str_squish(date()), " |\\:", "_")
  
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
    f7Block(
      h1("Ingresa tus datos")
    ),
    f7Picker(
      inputId = "fisher",
      label = "¿Eres pescador?",
      choices = c("No especificar",
                  "Sí",
                  "No"),
      value = "Seleccionar...", 
      openIn = "sheet",
      toolbarCloseText = "Listo!",
      rotateEffect = F,
      scrollToInput = F
    ),
    f7Picker(
      inputId = "age_bracket",
      label = "Rango de edad (opcional):",
      choices = c("No especificar",
                  "0-20",
                  "21-30",
                  "31-40",
                  "41-50",
                  "51-60",
                  "61+"),
      value = "Seleccionar...", 
      openIn = "sheet",
      toolbarCloseText = "Listo!",
      rotateEffect = F,
      scrollToInput = F
    ),
    f7Picker(
      inputId = "sexo",
      label = "Género",
      choices = c("No especificar",
                  "Hombre",
                  "Mujer"),
      value = "Selecionar...",
      openIn = "sheet",
      toolbarCloseText = "Listo!",
      rotateEffect = F,
      scrollToInput = F
    ),
    f7Picker(
      inputId = "region",
      label = "Región (opcional):",
      choices = c("No especificar",
                  "BC Pacifico",
                  "Golfo de California",
                  "Pacífico Sur",
                  "Golfo de México",
                  "Caribe"),
      value = "Seleccionar...",
      openIn = "sheet",
      toolbarCloseText = "Listo!",
      rotateEffect = F,
      scrollToInput = F
    ),
    # f7Block(
      f7Text(inputId = "phone",
             label = "10 dígitos de tu teléfono:",
             placeholder = "xxx-xxx-xx-xx")
    )
  # )
  
  
  # Ventana de instrucciones
  observeEvent(
    input$next_step, {
      req(input$age_bracket,
          input$region)
      
      if(!input$next_step) {
        f7Popup(
          id = "done",
          title = "Ventana 2 / 2",
          button_text = "Jugar",
          f7Block(
            h1("Reglas del juego:"),
            p("1) El juego tiene dos mundos. El primero es azul y el segundo rojo"),
            p("2) Vas a jugar contra otros 4 pescadores"),
            p("3) Cada juego tiene 15 viajes de pesca"),
            p("4) Puedes pescar entre 0 y 5 peces en cada viaje"),
            h1("Cómo jugar:"),
            p("Tu objetivo es pescar lo más que puedas, pero sin acabarte el recurso."),
            p("En la siguiente pantalla, selecciona el número de peces que quieres pescar, y después usa el botón de 'PESCAR!'")
          )
        )
        
        # Encode file name parameters
        encoded_bug <- encode_bug(bug)
        encoded_fisher <- encode_fisher(input$fisher)
        encoded_age <- encode_age(input$age_bracket)
        encoded_sex <- encode_sex(input$sexo)
        encoded_region <- encode_region(input$region)
        encoded_phone <- encode_phone(input$phone)
        
        file_id <- paste(session_id,
                         encoded_bug,
                         encoded_fisher,
                         encoded_age,
                         encoded_sex,
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
      
      # Re set game df
      values$df <- df
      # Reset populaito size
      values$N <- N
      # Rest round counter
      values$i <- 1
      # Get neww vector ot survivals
      values$s <- get_s()
      # Update color of bugs
      values$game_color <- "#B1182B"
      
      f7Notif(
        title = paste("Alerta!"),
        subtitle = "Hay cambio climático",
        text = "En uno de cada diez viajes puede haber ondas de calor que matan a la mitad del recurso",
        icon = f7Icon("flame"),
        closeTimeout = 10000
      )
      
    })
  
  # This is the fishing module -------------------------------------------------
  observeEvent(
    input$go_fish, {
      insertUI(selector = "#go_fish",
               where = "afterEnd",
               # beep.wav should be in /www of the shiny app
               ui = tags$audio(src = "facebook.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")
      )
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
          closeTimeout = 10000
        )
      }
      
      # Append the results of this round to the main table
      values$df <- rbind(values$df,
                         values_this_round)
      

      # Update population size
      values$N <- tail(values$df$Nt, 1)
      
      if(values$N <= 0 | values$i == 15) {
        
        # Update data on Google Drive
        sheet_append(ss = values$gsheet_id,
                     data = values$df %>% 
                       filter(!g == 0))
        
        f7Dialog(
          id = "restart",
          type = "confirm",
          title = ifelse(values$i == 15,
                         "¡Terminaste el juego!",
                         "¡Se acabó el recurso!"),
          "¿Quieres volver a jugar?"
        )
      }
      
      values$i <- values$i + 1
    })
  
  #  GAME SIDE #################################################################
  
  observeEvent(
    input$go_fish | input$restart, {
      updateF7Slider(
        inputId = "harvest",
        min = 0,
        max = min(values$N, 5),
        scaleSteps = min(values$N, 5),
        value = 0,
        scale = TRUE,
        scaleSubSteps = 0
      )
    })
  
  output$pop_space <- renderPlot({
    df <- tail(values$df, 1)
    
    base_plot +
      geom_text(data = head(pop_space_master,
                            df$Nt),
                mapping = aes(x = x,
                              y = y,
                              label = emoji(bug)),
                family = "EmojiOne",
                fontface = "bold",
                size = 20,
                color = values$game_color)
    
  })
  
  # Everyone's catch
  output$everyones_catch <- renderUI({
    # req(input$harvest)
    H <- max(0,
             tail(values$df$H[values$df$g == values$game], 1),
             na.rm = T)
    
    f7Chip(label = paste("Entre todos capturaron:", H),
           icon = f7Icon("person_3"),
           status = "blue")
  })
  
  # Your catch last time
  output$your_catch <- renderUI({
    # req(input$harvest)
    h <- max(0,
             tail(values$df$h[values$df$g == values$game], 1),
             na.rm = T)
    
    f7Chip(label = paste("Tu capturaste:", h),
           icon = f7Icon("person_3"),
           status = "blue")
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
    
    pop <- floor(tail(values$df$Nt, 1))
    f7Chip(label = paste("Viaje:", values$i, "de 15"),
           icon = f7Icon("clock"),
           status = case_when((pop > (0.75 * K)) ~ "green",
                              between(pop, 0.5 * K, 0.75 * K) ~ "orange",
                              (pop < (0.5 * K)) ~ "red"))
  })
  
}

shinyApp(ui = ui, server = server)

