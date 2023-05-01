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

get_N <- function(harvest, N, K, gr, ce, t) {
  # Total harvests
  n_fishers <- 3
  H <- sum(sample(1:5, size = n_fishers)) + harvest
  
  # Escapement
  E <- max((N - H), 0)
  
  # Next N
  Ntp1 <- min(round((1 + gr) * E * ce), K)
  
  results <- tibble(last_N = N,
                    H = H,
                    E = E,
                    Nt = Ntp1,
                    h = harvest,
                    ce,
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