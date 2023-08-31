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
  # Total harvests
  n_fishers <- 4
  H <- sum(sample(x = 0:5, size = n_fishers)) + harvest
  
  # Escapement
  E <- max((N - H), 0)
  
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
                path = "~/FishCatchR_data/",
                overwrite = F)
  
  return(a)
}

encode_age <- function(age) {
  case_when(
    age == "Ninguno" ~ 0,
    age == "0-20" ~ 1,
    age == "21-30" ~ 2,
    age == "31-40" ~ 3,
    age == "41-50" ~ 4,
    age == "51-60" ~ 5,
    age == "61+" ~ 6)
}

encode_region <- function(region) {
  case_when(
    region == "Ninguna" ~ 0,
    region == "BC Pacifico" ~ 1,
    region == "Golfo de California" ~ 2,
    region == "Pacífico" ~ 3,
    region == "Golfo de México" ~ 4,
    region == "Caribe" ~ 5)
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
s <- get_s(mort = 0.5, rounds = 15, mort_prob = 0.1)

s <- c(1, 1, 0.5, 1, 1)

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
       y = c(-5.5, 5.5)) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#EEF2FE", 
                                        color = "transparent"))