# NOT RUN {
## load data on pandemic flu in a school in 2009
data("Flu2009")

## estimate the reproduction number (method "non_parametric_si")
## when not specifying t_start and t_end in config, they are set to estimate
## the reproduction number on sliding weekly windows
res <- estimate_R(incid = Flu2009$incidence,
                  method = "non_parametric_si",
                  config = make_config(list(si_distr = Flu2009$si_distr)))
plot(res)

## the second plot produced shows, at each each day,
## the estimate of the reproduction number over the 7-day window
## finishing on that day.

## to specify t_start and t_end in config, e.g. to have biweekly sliding
## windows
t_start <- seq(2, nrow(Flu2009$incidence)-13)
t_end <- t_start + 13
res <- estimate_R(incid = Flu2009$incidence,
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = Flu2009$si_distr,
                    t_start = t_start,
                    t_end = t_end)))
plot(res)

## the second plot produced shows, at each each day,
## the estimate of the reproduction number over the 14-day window
## finishing on that day.

## example with an incidence object

## create fake data
library(incidence)
data <- c(0,1,1,2,1,3,4,5,5,5,5,4,4,26,6,7,9)
location <- sample(c("local","imported"), length(data), replace=TRUE)
location[1] <- "imported" # forcing the first case to be imported

## get incidence per group (location)
incid <- incidence(data, groups = location)

## Estimate R with assumptions on serial interval
res <- estimate_R(incid, method = "parametric_si",
                  config = make_config(list(
                    mean_si = 2.6, std_si = 1.5)))
plot(res)
## the second plot produced shows, at each each day,
## the estimate of the reproduction number over the 7-day window
## finishing on that day.

## estimate the reproduction number (method "parametric_si")
res <- estimate_R(Flu2009$incidence, method = "parametric_si",
                  config = make_config(list(mean_si = 2.6, std_si = 1.5)))
plot(res)
## the second plot produced shows, at each each day,
## the estimate of the reproduction number over the 7-day window
## finishing on that day.

## estimate the reproduction number (method "uncertain_si")
res <- estimate_R(Flu2009$incidence, method = "uncertain_si",
                  config = make_config(list(
                    mean_si = 2.6, std_mean_si = 1,
                    min_mean_si = 1, max_mean_si = 4.2,
                    std_si = 1.5, std_std_si = 0.5,
                    min_std_si = 0.5, max_std_si = 2.5,
                    n1 = 100, n2 = 100)))
plot(res)
## the bottom left plot produced shows, at each each day,
## the estimate of the reproduction number over the 7-day window
## finishing on that day.

# }
# NOT RUN {
## Note the following examples use an MCMC routine
## to estimate the serial interval distribution from data,
## so they may take a few minutes to run

## load data on rotavirus
data("MockRotavirus")

## estimate the reproduction number (method "si_from_data")
MCMC_seed <- 1
overall_seed <- 2
R_si_from_data <- estimate_R(MockRotavirus$incidence,
                             method = "si_from_data",
                             si_data = MockRotavirus$si_data,
                             config = make_config(list(si_parametric_distr = "G",
                                                       mcmc_control = make_mcmc_control(list(burnin = 1000,
                                                                                             thin = 10, seed = MCMC_seed),
                                                                                        n1 = 500, n2 = 50,
                                                                                        seed = overall_seed))))

## compare with version with no uncertainty
R_Parametric <- estimate_R(MockRotavirus$incidence,
                           method = "parametric_si",
                           config = make_config(list(
                             mean_si = mean(R_si_from_data$SI.Moments$Mean),
                             std_si = mean(R_si_from_data$SI.Moments$Std))))
## generate plots
p_uncertainty <- plot(R_si_from_data, "R", options_R=list(ylim=c(0, 1.5)))
p_no_uncertainty <- plot(R_Parametric, "R", options_R=list(ylim=c(0, 1.5)))
gridExtra::grid.arrange(p_uncertainty, p_no_uncertainty,ncol=2)

## the left hand side graph is with uncertainty in the SI distribution, the
## right hand side without.
## The credible intervals are wider when accounting for uncertainty in the SI
## distribution.

## estimate the reproduction number (method "si_from_sample")
MCMC_seed <- 1
overall_seed <- 2
SI.fit <- coarseDataTools::dic.fit.mcmc(dat = MockRotavirus$si_data,
                                        dist = "G",
                                        init.pars = init_mcmc_params(MockRotavirus$si_data, "G"),
                                        burnin = 1000,
                                        n.samples = 5000,
                                        seed = MCMC_seed)
si_sample <- coarse2estim(SI.fit, thin = 10)$si_sample
R_si_from_sample <- estimate_R(MockRotavirus$incidence,
                               method = "si_from_sample",
                               si_sample = si_sample,
                               config = make_config(list(n2 = 50,
                                                         seed = overall_seed)))
plot(R_si_from_sample)

## check that R_si_from_sample is the same as R_si_from_data
## since they were generated using the same MCMC algorithm to generate the SI
## sample (either internally to EpiEstim or externally)
all(R_si_from_sample$R$`Mean(R)` == R_si_from_data$R$`Mean(R)`)
# }
# NOT RUN {
# }
