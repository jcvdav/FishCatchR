################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

get_N <- function(harvest, N, K, gr, s, t, g) {
  # Escapement after player's harvest
  E <- max((N - harvest), 0)
  
  # Total harvests
  H <- 0
  if(E > 0) {
    n_fishers <- 4
    # Formula is 5 * (intercept + slope on round)
    # this is then multiplied times 5 (max possible catch) because the
    # coefficients are "Dependent variable: Average group catch as fraction of
    # maximum possible catch" (Table 2 from Finkbeiner et al)
    lambda <- ((0.519) + (-0.012 * t)) * 5
    if(g > 1) {
      # In the presence of a shock, the other four robots play as if they communicate
      lambda <- ((0.519 - 0.106) + (-0.012 * t)) * 5
    }
    
    H_others <- min(sum(rpois(n = n_fishers, lambda = lambda)), E)
    E <- max(E - H_others, 0)
    H <- H_others + harvest
  }
  
  # Next N
  Ntp1 <- min(round((1 + gr) * E * s), K)
  
  results <- tibble(last_N = N,
                    H = H,
                    E = E,
                    Nt = Ntp1,
                    h = harvest,
                    s,
                    t = t,
                    g = g)
  
  return(results)
}

get_s <- function(mort = 0.5, rounds = 15, mort_prob = 0.1) {
  sample(x = c(1, 1 - mort),
         size = rounds,
         replace = T,
         prob = c(1 - mort_prob, mort_prob))
}


make_table <- function(new_name, data) {
  
  ss <- gs4_create(name = new_name,
                   sheets = list(data = data))
  
  print(new_name)
  
  a <- drive_mv(file = ss,
                path = "~/FishCatchR_data_2025/",
                overwrite = F)
  
  return(a)
}

encode_bug <- function(bug) {
  case_when(bug == "crab" ~ 1,
            bug == "shrimp" ~ 2,
            bug == "fish" ~ 3)
}

encode_fisher <- function(fisher) {
  case_when(
    fisher %in% c("No especificar", "Seleccionar...") ~ 0,
    fisher == "Sí" ~ 1,
    fisher == "No" ~ 2,
    T ~ 0
    )
}

encode_age <- function(age) {
  case_when(
    age %in% c("No especificar", "Seleccionar...") ~ 0,
    age == "0-20" ~ 1,
    age == "21-30" ~ 2,
    age == "31-40" ~ 3,
    age == "41-50" ~ 4,
    age == "51-60" ~ 5,
    age == "61+" ~ 6,
    T ~ 0
    )
}

encode_region <- function(region) {
  case_when(
    region %in% c("No especificar", "Seleccionar...") ~ 0,
    region == "BC Pacifico" ~ 1,
    region == "Golfo de California" ~ 2,
    region == "Pacífico Sur" ~ 3,
    region == "Golfo de México" ~ 4,
    region == "Caribe" ~ 5,
    T ~ 0
    )
}

encode_sex <- function(sexo) {
  case_when(
    sexo %in% c("No especificar", "Seleccionar...") ~ 0,
    sexo == "Hombre" ~ 1,
    sexo == "Mujer" ~ 2,
    T ~ 0
    )
}

encode_phone <- function(phone) {
  clean <- str_remove_all(phone, "[:alpha:]|[:punct:]")
  if(!str_length(clean) == 10){
    return("0000000000")
  } else {
    return(clean)
  }
}


#########################
# Model parameters
K <- 100                                                                        # population carrying capacity
rounds <- 15                                                                    # number of rounds
gr <- 0.1                                                                       # 10 percent annual rate of increase
mort_prob <- 0.1                                                                # probability of a catastrophic event
mort <- 0.5                                                                     # catastrophic mortality

# Derivated parameters
N <- K                                                                          # Initial Conditions
# set.seed(20)
# vector of survivals
s <-rep(1, 15)

df <- tibble(
  last_N = 0,
  H = 0,
  E = 0,
  Nt = N,
  h = 0,
  s = 0,
  t = 0,
  g = 0
)

pop_space_master <- tibble(x = runif(n = K, min = -5, max = 5),
                           y = runif(n = K, min = -5, max = 5)) %>% 
  mutate(dist = sqrt(x ^ 2 + y ^ 2)) %>% 
  arrange(dist)

base_plot <- ggplot() +
  lims(x = c(-5.5, 5.5),
       y = c(-6, 5)) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", # "#EEF2FE", 
                                        color = "transparent"))

game_color <- "#053061"

f7Popup <- function(..., id, title = NULL,
                    backdrop = TRUE,
                    closeByBackdropClick = TRUE,
                    closeOnEscape = FALSE,
                    animate = TRUE,
                    swipeToClose = FALSE,
                    fullsize = FALSE,
                    closeButton = TRUE,
                    button_text = "Siguiente",
                    survey = FALSE,
                    session = shiny::getDefaultReactiveDomain()) {
  
  message <- shiny:::dropNulls(
    list(
      id = session$ns(id),
      backdrop = backdrop,
      closeByBackdropClick = closeByBackdropClick,
      closeOnEscape = closeOnEscape,
      animate = animate,
      swipeToClose = swipeToClose
    )
  )
  
  content <- shiny::tags$div(
    class = "block",
    if (!is.null(title)) shiny::tags$div(class = "block-title", title),
    ...
  )
  
  if (closeButton) {
    content <- htmltools::tagAppendChild(
      content,
      shiny::tags$a(
        class = "link popup-close",
        style = "position: absolute; bottom: -50px; right: 50px;",
        href = "#",
        button_text,
        f7Icon("arrowtriangle_right_fill")
      )
    )
  }
  
  if(survey) {
    content <- htmltools::tagAppendChild(
      content,
      shiny::tags$a(
        class = "link popup-close",
        style = "position: absolute; top: -15px; right: 10px;",
        href = "#",
        "Saltar encuesta",
        f7Icon("arrowtriangle_right_fill")
      )
    )
  }
  
  popup_tag <- shiny::tags$div(
    class = paste0("popup", if (fullsize) "popup-tablet-fullscreen"),
    content
  )
  
  message$content <- as.character(popup_tag)
  
  # see my-app.js function
  session$sendCustomMessage(
    type = "popup",
    message = jsonlite::toJSON(
      message,
      auto_unbox = TRUE,
      json_verbatim = TRUE
    )
  )
}
