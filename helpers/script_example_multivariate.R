# a script to show the workflow on an example data set:
# obs and ensemble forecasts at 4 stations for summer 2021

################# SETUP
# load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gamlss)
library(scoringRules)
library(depPPR)

####################################################################
################# GLOBAL VARIABLES:
####################################################################

main_dir <- getwd()

# ensemble information:
station_names <- c("De Bilt", "Schiphol", "Cabauw mast", "Maastricht")
lead_times <- sprintf("%02d", 1:48)
keps_members <- paste0("EM", sprintf("%03d", 0:10))
init_hours <- c("00")

# methods:
quantile_methods <- c("R", "Q", "QJ", "QH")
reshuffling_methods <- c("ecc", "sshw", "simssh")

####################################################################
################# IMPORT DATA:
####################################################################
# import forecast data: kepsobs_predictions
# this is a data.frame containing all hourly observations for all stations in station_names
# name: station name
# hlon/hlat: lon/lat from Harmonie
# lon/lat: lon/lat from the stations
# init_time: initialisation time of the forecast in the format "2019-02-10 UTC"
# leadtime: leadtime of the forecast in the format "01"
# valid_time: datetime in the format "2019-02-10 01:00:00"
# T: Observed 2m temperature
# EM000-EM010: Ensemble member forecasts
# EnsM / EnsS = ensemble mean and sd
# mu = predicted mu
# sigma = predicted sigma
# y = T2m

# which init_times have the complete data:
kepsobs_complete_inits <- kepsobs_predictions %>%
  count(init_time) %>%
  filter(n == length(station_names) * length(lead_times)) %>%
  pull(init_time)

kepsobs_predictions <- kepsobs_predictions %>% filter(init_time %in% kepsobs_complete_inits)



####################################################################
################# IMPORT DATA:
####################################################################
# make new ensemble members by drawing from the distributions:
# draw members with each type of 'quantile_methods'
quants <- mapply(FUN = get_quantiles,
                 method = quantile_methods,
                 MoreArgs = list(n_members = length(keps_members),
                                 n_reps = nrow(kepsobs_predictions)),
                 SIMPLIFY = FALSE)


# a function to draw samples from a normal distribution:
sample_dist <- function(params, quantiles, newname){
  sampout <- lapply(seq_along(params$mu), function(nr){
    qNO(mu = params$mu[nr], sigma = params$sigma[nr], p = as.numeric(quantiles[nr,])) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
  }) %>% bind_rows()
  sampout %>%
    magrittr::set_colnames(paste0(newname, 1:ncol(quantiles)))

}


# draw from the forecast distribution using the methods in quantile_methods
kepsobs_predictions_samples <- mapply(FUN = sample_dist,
                                      newname = quantile_methods,
                                      quantiles = quants,
                                      MoreArgs = list(params = list(mu = kepsobs_predictions$mu,
                                                                    sigma = kepsobs_predictions$sigma)),
                                      SIMPLIFY = FALSE) %>% bind_cols()



# combine raw and post-processed forecasts together:
kepsobs_predictions_samples <- cbind(kepsobs_predictions, kepsobs_predictions_samples) %>%
  arrange(init_time, name, leadtime)

####################################################################
# calculate univariate scores:
####################################################################
# crps
raw_quantile_methods <- c("EM", quantile_methods)

scores_crps <- lapply(raw_quantile_methods, function(qm){
  fc_dat <- kepsobs_predictions_samples %>%
    dplyr::select(matches(paste0("^", qm, "[0-9]")))
  crps_sample(y = kepsobs_predictions_samples$T,
              dat = fc_dat %>% as.matrix())


}) %>% bind_cols() %>%
  magrittr::set_colnames(paste0("crps_", raw_quantile_methods)) %>%
  cbind(kepsobs_predictions_samples %>% dplyr::select(name, init_time, leadtime, valid_time), .)


colMeans(scores_crps %>% dplyr::select(starts_with("crps")))


# plot example
plotdate = "2020-08-30"

ggplot(kepsobs_predictions_samples %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(starts_with(raw_quantile_methods)), names_to = "member", values_to = "T2m") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = T2m, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name, ncol = length(station_names)) +
  theme_bw() +
  ggtitle(plotdate)



# crps plot
ggplot(scores_crps %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(starts_with("crps")), names_to = "sample_type", values_to = "CRPS")) +
  geom_line(aes(x = leadtime, y = CRPS, group = sample_type, color = sample_type)) +
  facet_wrap(~name) +
  theme_bw() +
  ggtitle(plotdate) +
  theme(legend.position = "bottom")


####################################################################
############# Restore dependencies
####################################################################

# function to get the date/times for each leadtime
make_lt_templates <- function(start_date, leadtimes){
  as.POSIXct(start_date, tz = "UTC") + as.numeric(leadtimes) * 60 * 60
}


# --- not needed now that the data is in the package
# import historical observations:
# define file
# obs_rds <- "/Users/kiriwhan/surfdrive/KNMI_WFHDocs/StudentsCollabs/Kate/depPPR/data//OBS_T_1950-2021.rds"

# import obs_data
# this is a data.frame containing all hourly observations for all stations in station_names
# IT_DATETIME: datetime in the format "19510102_010000_000000"
# datetime: datetime in the format "19510102 010000"
# valid_time: datetime in the format "1951-01-02 01:00:00"
# DS_CODE: station number
# name: station name
# lat/lon
# T: 2m temperature
# obs_data <- readRDS(obs_rds) %>%
#   mutate(valid_date = as.Date(valid_time),
#          valid_hour = hour(valid_time)) %>%
#   na.omit() %>%
#   unique()
#
# #usethis::use_data(obs_data)
# --- not needed now that the data is in the package



####################################################################
######### SSh - window
####################################################################
# application specific (eg. 4 stations all need the same suitable dates)
obs_datetime = obs_data %>%
  count(valid_time) %>%
  filter(n == 4) %>%
  pull(valid_time)

potential_schaake_datetimes <- get_schaake_shuffle_dates(obs_datetime,
                                                         window = days(2),
                                                         init_times = init_hours,
                                                         tz = "UTC")

# make a template from observations
# what dates do we need to reshuffle: all the dates in the test set
kepsobs_predictions_dates <- kepsobs_predictions_samples %>% pull(init_time) %>% unique() %>% as.Date()
# get the templates:
window_templates_initday <- mapply(FUN = schaake_template_window,
                                   date_val = kepsobs_predictions_dates,
                                   MoreArgs = list(n_members = 11, window = 5, historical_dates = as.Date(potential_schaake_datetimes)),
                                   SIMPLIFY = FALSE)

# returns a list where length = number of days in test set, and
#   where each item has nrows = length(lead_times) and ncols = n_members
window_templates_leadtimes <- lapply(seq_along(window_templates_initday), function(wt){
  mapply(FUN = make_lt_templates,
         start_date = paste0(window_templates_initday[[wt]], " 00:00:00"),
         MoreArgs = list(leadtimes = lead_times),
         SIMPLIFY = FALSE) %>% as.data.frame(.)
})

sshw_template <- lapply(seq_along(kepsobs_predictions_dates), function(pd){
  lapply(1:ncol(window_templates_leadtimes[[pd]]), function(sd){
    colname_tmp <- paste0("sshw", sd)
    obs_data %>% filter(valid_time %in% window_templates_leadtimes[[pd]][,sd]) %>% arrange(name, valid_time) %>%
      dplyr::select(T) %>% rename(.,"{colname_tmp}" := T)
  }) %>% bind_cols()
}) %>% bind_rows()


# sshw for each quantile_method
kepsobs_predictions_reshuff <- lapply(seq_along(quantile_methods), function(qm){
  reshuffle_members(forecast = kepsobs_predictions_samples %>%
                      dplyr::select(matches(paste0(quantile_methods[qm], "[0-9]"))) %>% as.matrix(),
                    template = sshw_template %>% as.matrix()) %>%
    magrittr::set_colnames(paste0(quantile_methods[qm], "_sshw_", 1:ncol(.)))
}) %>% bind_cols() %>%
  cbind(kepsobs_predictions_samples, .)


####################################################################
######### SIMSSh
####################################################################
simssh_template <- lapply(seq_along(preds_dates), function(pl){
  fc <- kepsobs_predictions_sshw %>% filter(as.character(init_time) == preds_dates[pl]) %>%
    arrange(init_time, valid_time, leadtime, name)

  fcall_list <- train_data %>%
    arrange(init_time, valid_time, leadtime, name) %>%
    group_split(init_time)
  fc_list <- lapply(fcall_list, function(l) l %>% dplyr::select(matches("EM[0-9]")) )
  ob_list <- lapply(fcall_list, function(l) l %>% dplyr::select(T))
  simt <- get_sim_schaake_template(forecast = fc %>% dplyr::select(matches("EM[0-9]")),
                                   forecast_list = fc_list,
                                   obs_list = ob_list)
  return(simt %>% as.data.frame())
}) %>%
  bind_rows() %>%
  magrittr::set_colnames(paste0("simssh", 1:ncol(.)))

# simssh for each quantile_method
kepsobs_predictions_reshuff <- lapply(seq_along(quantile_methods), function(qm){
  reshuffle_members(forecast = kepsobs_predictions_samples %>%
                      dplyr::select(matches(paste0(quantile_methods[qm], "[0-9]"))) %>% as.matrix(),
                    template = simssh_template %>% as.matrix()) %>%
    magrittr::set_colnames(paste0(quantile_methods[qm], "_simssh_", 1:ncol(.)))
}) %>% bind_cols() %>%
  cbind(kepsobs_predictions_reshuff, .)


####################################################################
######### ECC
####################################################################
# ecc for each quantile_method
kepsobs_predictions_reshuff <- lapply(seq_along(quantile_methods), function(qm){
  reshuffle_members(forecast = kepsobs_predictions_samples %>%
                      dplyr::select(matches(paste0(quantile_methods[qm], "[0-9]"))) %>% as.matrix(),
                    template = kepsobs_predictions_samples %>%
                      dplyr::select(matches("EM[0-9]")) %>% as.matrix()) %>%
    magrittr::set_colnames(paste0(quantile_methods[qm], "_ecc_", 1:ncol(.)))
}) %>% bind_cols() %>%
  cbind(kepsobs_predictions_reshuff, .)



####################################################################
######### SCORING
####################################################################
# Energy Score:
es_df <- lapply(seq_along(preds_dates), function(pd){
  daydat <- kepsobs_predictions_reshuff %>% filter(as.character(init_time) == preds_dates[pd])
  yy <- daydat %>% pull(T)
  data.frame(date = preds_dates[pd],
             RAW = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("EM[0-9]")) %>% as.matrix()),
             EMOS = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q[0-9]")) %>% as.matrix()),
             ECCQ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_ecc_[0-9]")) %>% as.matrix()),
             ECCQJ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_ecc_[0-9]")) %>% as.matrix()),
             ECCQH = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_ecc_[0-9]")) %>% as.matrix()),
             SSHWQ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_sshw_[0-9]")) %>% as.matrix()),
             SSHWQJ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_sshw_[0-9]")) %>% as.matrix()),
             SSHWQH = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_sshw_[0-9]")) %>% as.matrix()),
             SIMSSHQ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_simssh_[0-9]")) %>% as.matrix()),
             SIMSSHQJ = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_simssh_[0-9]")) %>% as.matrix()),
             SIMSSHQH = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_simssh_[0-9]")) %>% as.matrix()))
}) %>% bind_rows()


# Variogram Score:
vs_df <- lapply(seq_along(preds_dates), function(pd){
  daydat <- kepsobs_predictions_reshuff %>% filter(as.character(init_time) == preds_dates[pd])
  yy <- daydat %>% pull(T)
  data.frame(date = preds_dates[pd],
             RAW = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("EM[0-9]")) %>% as.matrix()),
             EMOS = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q[0-9]")) %>% as.matrix()),
             ECCQ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_ecc_[0-9]")) %>% as.matrix()),
             ECCQJ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_ecc_[0-9]")) %>% as.matrix()),
             ECCQH = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_ecc_[0-9]")) %>% as.matrix()),
             SSHWQ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_sshw_[0-9]")) %>% as.matrix()),
             SSHWQJ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_sshw_[0-9]")) %>% as.matrix()),
             SSHWQH = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_sshw_[0-9]")) %>% as.matrix()),
             SIMSSHQ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("Q_simssh_[0-9]")) %>% as.matrix()),
             SIMSSHQJ = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QJ_simssh_[0-9]")) %>% as.matrix()),
             SIMSSHQH = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("QH_simssh_[0-9]")) %>% as.matrix()))
}) %>% bind_rows()


colMeans(es_df[,-1])
colMeans(vs_df[,-1])

# ggplot(es_df %>% pivot_longer(., RAW:SIMSSHQH, names_to = "method", values_to = "ES")) + geom_line(aes(x = method, y = ES, group = date))
# ggplot(vs_df %>% pivot_longer(., RAW:SIMSSHQH, names_to = "method", values_to = "VS")) + geom_line(aes(x = method, y = VS, group = date))
