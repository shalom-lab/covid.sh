pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  epicontacts,  # Analysing transmission networks
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)
future::plan(multisession)


# Import Data -------------------------------------------------------------

load('./data.rda')
linelist<- readRDS("D:/Github/covid.sh/local/linelist_cleaned.rds")

incubation_period_lit <- list(
  mean = log(9.1),
  mean_sd = log(0.1),
  sd = log(7.3),
  sd_sd = log(0.1),
  max = 30
)

## estimate incubation period
incubation_period <- bootstrapped_dist_fit(
  linelist$date_onset - linelist$date_infection,
  dist = "lognormal",
  max_value = 100,
  bootstraps = 1
)

## generate contacts
contacts <- linelist %>%
  transmute(
    from = infector,
    to = case_id
  ) %>%
  drop_na()

## generate epicontacts object
epic <- make_epicontacts(
  linelist = linelist,
  contacts = contacts,
  directed = TRUE
)

## estimate gamma generation time
generation_time <- bootstrapped_dist_fit(
  get_pairwise(epic, "date_infection"),
  dist = "gamma",
  max_value = 20,
  bootstraps = 1
)

## get incidence from onset dates
cases <- linelist %>%
  group_by(date = date_onset) %>%
  summarise(confirm = n())


## run epinow
epinow_res <- epinow(
  reported_cases = cases,
  generation_time = generation_time,
  delays = delay_opts(incubation_period),
  return_output = TRUE,
  verbose = TRUE,
  horizon = 21,
  stan = stan_opts(samples = 750, chains = 4)
)

## plot summary figure
plot(epinow_res)
