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

get_N <- function(harvest, N, K, gr, s, t) {
  # Total harvests
  n_fishers <- 4
  lambda <- ifelse(s == 1, 1, 2)
  H <- sum(rpois(n = n_fishers, lambda = lambda)) + harvest
  
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
                    t = t)
  
  return(results)
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
    age == "10-20" ~ 1,
    age == "20-30" ~ 2,
    age == "30-40" ~ 3,
    age == "40-50" ~ 4,
    age == "50-60" ~ 5,
    age == "60+" ~ 6)
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