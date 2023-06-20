# a script to show the workflow on an example data set:
# obs and ensemble forecasts at 4 stations for summer 2021

################# SETUP
# load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(data.table)
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
quantile_methods <- c("random", "equally_spaced", "equally_spaced_shift")
reshuffling_methods <- c("ecc", "ssh", "simssh")

####################################################################
################# IMPORT ALL DATA AND POST-PROCESS:
####################################################################
# define file
kepsobs_rds <- list.files(
  path =  paste0(main_dir, "/data/"),
  pattern = "KEPSOBS_T_2019-2021",
  full.names = TRUE)

# import forecast data:
# this is a data.frame containing all hourly observations for all stations in station_names
# name: station name
# hlon/hlat: lon/lat from Harmonie
# lon/lat: lon/lat from the stations
# init_time: initialisation time of the forecast in the format "2019-02-10 UTC"
# leadtime: leadtime of the forecast in the format "01"
# valid_time: datetime in the format "2019-02-10 01:00:00"
# T: Observed 2m temperature
# EM000-EM010: Ensemble member forecasts
kepsobs_data <- readRDS(kepsobs_rds)

# make predictors for EMOS: ensemble mean and SD
kepsobs_data$EnsM <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, mean)
kepsobs_data$EnsS <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, sd)

# which init_times have the complete data:
kepsobs_complete_inits <- kepsobs_data %>%
  count(init_time) %>%
  filter(n == length(station_names) * length(lead_times)) %>%
  pull(init_time)

kepsobs_data <- kepsobs_data %>% filter(init_time %in% kepsobs_complete_inits)



####################################################################
# local PP
# train on June-July 2020, test on August 2020
####################################################################

train <- kepsobs_data %>%
  filter(year(init_time) == 2020 & month(init_time) %in% c(6,7))
test <- kepsobs_data %>%
  filter(year(init_time) == 2020 & month(init_time) %in% c(8)) # 31 days

# fit a model per station and leadtime:
# loop over stations:
fits <- list()
preds_list <- list()
for(st in seq_along(station_names)){
  # loop over leadtimes
  fits_lt <- list()
  preds_lt <- list()
  for(lt in seq_along(lead_times)){
    # define train/test for each station/leadtime
    train_stlt <- train %>% filter(name == station_names[st] & leadtime == lead_times[lt])
    test_stlt <- test %>% filter(name == station_names[st] & leadtime == lead_times[lt])

    # fit the model:
    #   a normal distribution where we predict mu from EnsM and sigma from EnsS
    fits_lt[[lt]] <- gamlss(T~EnsM, sigma.formula = ~EnsS,
                            data = as.data.frame(train_stlt),
                            family = NO())

    # predict the params and combine with test set
    preds_lt[[lt]] <- predictAll(fits_lt[[lt]], newdata = test_stlt) %>%
      as.data.frame() %>%
      cbind(test_stlt, .)
  }
  fits[[st]] <- fits_lt
  preds_list[[st]] <- rbindlist(preds_lt)

}

preds <- rbindlist(preds_list)


# make new ensemble members by drawing from the distributions:
# draw members: random or equally spaced
quants <- mapply(FUN = get_quantiles, method = quantile_methods[-1], MoreArgs = list(n_members = length(keps_members)), SIMPLIFY = FALSE)

# for random quants we need a new random draw for each forecast time
quants$random = lapply(seq_len(nrow(preds)), function(nr){
  mapply(FUN = get_quantiles, method = quantile_methods[1], MoreArgs = list(n_members = length(keps_members)), SIMPLIFY = TRUE) %>%
    t() %>%
    as.data.frame()
}) %>%
  bind_rows() %>%
  magrittr::set_colnames(paste0("RQ", 1:length(keps_members)))


sample_dist <- function(params, family, quantiles, newname){
  if(newname == "random"){
    sampout <- lapply(seq_along(params$mu), function(nr){
      qNO(mu = params$mu[nr], sigma = params$sigma[nr], p = as.numeric(quantiles[nr,])) %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame()
    }) %>% bind_rows()
  } else{
    sampout <- mapply(qNO, mu = params$mu, sigma = params$sigma, USE.NAMES = TRUE, MoreArgs = list(p = quantiles)) %>%
      t() %>%
      as.data.frame()
  }
  sampout %>%
    magrittr::set_colnames(paste0(newname, 1:length(quantiles)))

}

# draw from the forecast distribution using the methods in quantile_methods
ppfcsts <- lapply(seq_along(quantile_methods), function(qm){
  sample_dist(params = list(mu = preds$mu, sigma = preds$sigma),
              quantiles = quants[quantile_methods[qm]][[1]],
              newname = quantile_methods[qm])
}) %>% bind_cols()


# combine raw and post-processed forecasts together:
preds <- cbind(preds, ppfcsts)

####################################################################
# calculate univariate scores:
####################################################################
# crps
preds$crpsRAW <- crps_sample(y = preds$T,
                             dat = as.matrix(preds %>% dplyr::select(matches(keps_members))))


preds$crpsE <- crps_sample(y = preds$T,
                           dat = as.matrix(preds %>% dplyr::select(matches(paste0("^equally_spaced[0-9]")))))

preds$crpsES <- crps_sample(y = preds$T,
                            dat = as.matrix(preds %>% dplyr::select(matches(paste0("^equally_spaced_shift[0-9]")))))

preds$crpsR <- crps_sample(y = preds$T,
                           dat = as.matrix(preds %>% dplyr::select(matches(paste0("^random[0-9]")))))

# mean crps improves
mean(preds$crpsRAW)
mean(preds$crpsE)
mean(preds$crpsES)
mean(preds$crpsR)


# plot example
plotdate = "2020-08-30"
ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(starts_with("EM")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)

ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(matches("equally_spaced[0-9]")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)


ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(matches("equally_spaced_shift[0-9]")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)


ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(matches("random")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)


# crps plot
ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(crpsRAW, crpsE, crpsES), names_to = "member", values_to = "KEPS")) +
  geom_line(aes(x = leadtime, y = KEPS, group = member, color = member)) +
  facet_wrap(~name) +
  theme_bw() +
  ggtitle(plotdate)


####################################################################
############# Restore dependencies
####################################################################

# function to get the date/times for each leadtime
make_lt_templates <- function(start_date, leadtimes){
  as.POSIXct(start_date, tz = "UTC") + as.numeric(leadtimes) * 60 * 60
}


# import historical observations:
# define file
obs_rds <- list.files(
  path =  paste0(main_dir, "/data/"),
  pattern = "OBS_T_1950-2021",
  full.names = TRUE)

# import obs_data
# this is a data.frame containing all hourly observations for all stations in station_names
# IT_DATETIME: datetime in the format "19510102_010000_000000"
# datetime: datetime in the format "19510102 010000"
# valid_time: datetime in the format "1951-01-02 01:00:00"
# DS_CODE: station number
# name: station name
# lat/lon
# T: 2m temperature
obs_data <- readRDS(obs_rds) %>%
  mutate(valid_date = as.Date(valid_time),
         valid_hour = hour(valid_time)) %>%
  na.omit() %>%
  unique()


# application specific (eg. 4 stations all need the same suitable dates)
obs_datetime = obs_data %>%
  count(valid_time) %>%
  filter(n == 4) %>%
  pull(valid_time)

good_schaake_datetimes <- get_schaake_shuffle_dates(obs_datetime,
                                                    window = days(2),
                                                    init_times = init_hours,
                                                    tz = "UTC")

# SSh - window
# make a template from observations
# what dates do we need to reshuffle: all the dates in the test set
preds_dates <- test %>% pull(init_time) %>% unique() %>% as.Date()
# get the templates:
wind_templates <- mapply(FUN = schaake_template_window,
                         date_val = preds_dates,
                         MoreArgs = list(n_members = 11, window = 5, historical_dates = as.Date(good_schaake_datetimes)),
                         SIMPLIFY = FALSE)



# returns a list where length = number of days in test set, and
#   where each item has nrows = length(lead_times) and ncols = n_members
wind_templates_lts <- lapply(seq_along(wind_templates), function(wt){
  mapply(FUN = make_lt_templates,
         start_date = paste0(wind_templates[[wt]], " 00:00:00"),
         MoreArgs = list(leadtimes = lead_times),
         SIMPLIFY = FALSE) %>% as.data.frame(.)
})

# Shuffle all stations and leadtimes at once:
apply_rst_ssh <- function(pl, var){
  # get the forecasts in a matrix where nrows = length(station_names) and ncols = n_members
  fc_dat <- preds %>%
    filter(init_time == preds_dates[pl]) %>% arrange(init_time, valid_time, leadtime, name)
  # some forecasts are missing (e.g. 2020-08-17 07:00:00 and +31 hours)
  if(nrow(fc_dat) == 192){
    # get the observations in a matrix where nrows = length(station_names) and ncols = n_members/length(template)
    ob_dat <- lapply(1:ncol(wind_templates_lts[[pl]]), function(aa){
      tmp <- obs_data %>%
        filter(valid_time %in% wind_templates_lts[[pl]][,aa]) %>%
        arrange(valid_time, name) %>%
        dplyr::select(T)
      names(tmp) <- names(wind_templates_lts[[pl]])[aa]
      return(tmp)
    }) %>% bind_cols()

    # shuffle
    run_shuffle_template(forecast = fc_dat %>%
                           dplyr::select(matches(paste0(var, "[0-9]"))) %>% as.matrix(),
                         template = ob_dat %>% as.matrix()) %>%
      magrittr::set_colnames(paste0(var, "_ssh_", 1:length(quants[[var]]))) %>%
      cbind(fc_dat, .) %>%
      mutate(pdd = pl)
  }
}

# shuffle predictions for each day in the test set (all leadtimes and stations at once):

preds_ssh <- lapply(seq_along(quantile_methods), function(qm){
  lapply(seq_along(preds_dates), function(pl){
    apply_rst_ssh(pl, var = quantile_methods[qm])
  }) %>% bind_rows()
})

preds_ssh <- bind_cols(list(preds_ssh[[1]],
                            preds_ssh[[2]] %>% dplyr::select(starts_with("equally_spaced_shift_ssh")),
                            preds_ssh[[3]] %>% dplyr::select(starts_with("equally_spaced_shift_ssh"))))

# scoring:
es_df <- lapply(seq_along(preds_dates), function(pd){
  daydat <- preds_ssh %>% filter(as.character(init_time) == preds_dates[pd])
  yy <- daydat %>% pull(T)
  data.frame(date = preds_dates[pd],
             RAW = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("EM[0-9]")) %>% as.matrix()),
             EMOS = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced[0-9]")) %>% as.matrix()),
             SShE = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_ssh_[0-9]")) %>% as.matrix()),
             SShES = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_shift_ssh_[0-9]")) %>% as.matrix()))
}) %>% bind_rows()

colMeans(es_df[,-1])

# sim ssh

preds_simsshE <- lapply(seq_along(preds_dates), function(pl){
  fc <- preds %>% filter(as.character(init_time) == preds_dates[pl]) %>%
    arrange(init_time, valid_time, leadtime, name)
  if(nrow(fc) == 192){
  fcall_list <- train %>%
    arrange(init_time, valid_time, leadtime, name) %>%
    group_split(init_time)

  fc_list <- lapply(fcall_list, function(l) l %>% dplyr::select(matches("EM[0-9]")) )
  ob_list <- lapply(fcall_list, function(l) l %>% dplyr::select(T))
  simt <- get_sim_schaake_template(forecast = fc %>% dplyr::select(matches("EM[0-9]")),
                                   forecast_list = fc_list,
                                   obs_list = ob_list)
  var = "equally_spaced"
  run_shuffle_template(forecast = fc %>%
                         dplyr::select(matches(paste0(var, "[0-9]"))) %>% as.matrix(),
                       template = simt) %>%
    magrittr::set_colnames(paste0(var, "_simssh_", 1:length(quants[[var]]))) %>%
    cbind(fc, .) %>%
    mutate(pdd = pl)
  }
}) %>% bind_rows()

preds_simsshES <- lapply(seq_along(preds_dates), function(pl){
  fc <- preds %>% filter(as.character(init_time) == preds_dates[pl]) %>%
    arrange(init_time, valid_time, leadtime, name)
  if(nrow(fc) == 192){
  fcall_list <- train %>%
    arrange(init_time, valid_time, leadtime, name) %>%
    group_split(init_time)

  fc_list <- lapply(fcall_list, function(l) l %>% dplyr::select(matches("EM[0-9]")) )
  ob_list <- lapply(fcall_list, function(l) l %>% dplyr::select(T))
  simt <- get_sim_schaake_template(forecast = fc %>% dplyr::select(matches("EM[0-9]")),
                                   forecast_list = fc_list,
                                   obs_list = ob_list)
  var = "equally_spaced_shift"
  run_shuffle_template(forecast = fc %>%
                         dplyr::select(matches(paste0(var, "[0-9]"))) %>% as.matrix(),
                       template = simt) %>%
    magrittr::set_colnames(paste0(var, "_simssh_", 1:length(quants[[var]]))) %>%
    cbind(fc, .) %>%
    mutate(pdd = pl)
  }
}) %>% bind_rows()


preds_simssh <- bind_cols(list(preds_simsshE,
                            preds_simsshES %>% dplyr::select(starts_with("equally_spaced_shift_simssh"))))


preds_eccE <- lapply(seq_along(preds_dates), function(pl){
  fc <- preds %>% filter(as.character(init_time) == preds_dates[pl]) %>%
    arrange(init_time, valid_time, leadtime, name)
  if(nrow(fc) == 192){
    var = "equally_spaced"
    run_shuffle_template(forecast = fc %>%
                           dplyr::select(matches(paste0(var, "[0-9]"))) %>% as.matrix(),
                         template = fc %>%
                           dplyr::select(matches(paste0("EM", "[0-9]"))) %>% as.matrix()) %>%
      magrittr::set_colnames(paste0(var, "_ecc_", 1:length(quants[[var]]))) %>%
      cbind(fc, .) %>%
      mutate(pdd = pl)
  }
}) %>% bind_rows()

preds_eccES <- lapply(seq_along(preds_dates), function(pl){
  fc <- preds %>% filter(as.character(init_time) == preds_dates[pl]) %>%
    arrange(init_time, valid_time, leadtime, name)
  if(nrow(fc) == 192){
    var = "equally_spaced_shift"
    run_shuffle_template(forecast = fc %>%
                           dplyr::select(matches(paste0(var, "[0-9]"))) %>% as.matrix(),
                         template = fc %>%
                           dplyr::select(matches(paste0("EM", "[0-9]"))) %>% as.matrix()) %>%
      magrittr::set_colnames(paste0(var, "_ecc_", 1:length(quants[[var]]))) %>%
      cbind(fc, .) %>%
      mutate(pdd = pl)
  }
}) %>% bind_rows()

preds_ecc <- bind_cols(list(preds_eccE,
                            preds_eccES %>% dplyr::select(starts_with("equally_spaced_shift_ecc"))))

## scoring:
es_df <- lapply(seq_along(preds_dates), function(pd){
  daydat <- preds_ssh %>% filter(as.character(init_time) == preds_dates[pd])
  simdaydat <- preds_simssh %>% filter(as.character(init_time) == preds_dates[pd])
  eccdaydat <- preds_ecc %>% filter(as.character(init_time) == preds_dates[pd])
  yy <- daydat %>% pull(T)
  data.frame(date = preds_dates[pd],
             RAW = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("EM[0-9]")) %>% as.matrix()),
             EMOS = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced[0-9]")) %>% as.matrix()),
             ECCE = es_sample(y = yy, dat = eccdaydat %>% dplyr::select(matches("equally_spaced_ecc_[0-9]")) %>% as.matrix()),
             ECCES = es_sample(y = yy, dat = eccdaydat %>% dplyr::select(matches("equally_spaced_shift_ecc_[0-9]")) %>% as.matrix()),
             SShE = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_ssh_[0-9]")) %>% as.matrix()),
             SShES = es_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_shift_ssh_[0-9]")) %>% as.matrix()),
             simSShE = es_sample(y = yy, dat = simdaydat %>% dplyr::select(matches("equally_spaced_simssh_[0-9]")) %>% as.matrix()),
             simSShES = es_sample(y = yy, dat = simdaydat %>% dplyr::select(matches("equally_spaced_shift_simssh_[0-9]")) %>% as.matrix()))
}) %>% bind_rows()

colMeans(es_df[,-1])

vs_df <- lapply(seq_along(preds_dates), function(pd){
  daydat <- preds_ssh %>% filter(as.character(init_time) == preds_dates[pd])
  simdaydat <- preds_simssh %>% filter(as.character(init_time) == preds_dates[pd])
  eccdaydat <- preds_ecc %>% filter(as.character(init_time) == preds_dates[pd])
  yy <- daydat %>% pull(T)
  data.frame(date = preds_dates[pd],
             RAW = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("EM[0-9]")) %>% as.matrix()),
             EMOS = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced[0-9]")) %>% as.matrix()),
             ECCE = vs_sample(y = yy, dat = eccdaydat %>% dplyr::select(matches("equally_spaced_ecc_[0-9]")) %>% as.matrix()),
             ECCES = vs_sample(y = yy, dat = eccdaydat %>% dplyr::select(matches("equally_spaced_shift_ecc_[0-9]")) %>% as.matrix()),
             SShE = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_ssh_[0-9]")) %>% as.matrix()),
             SShES = vs_sample(y = yy, dat = daydat %>% dplyr::select(matches("equally_spaced_shift_ssh_[0-9]")) %>% as.matrix()),
             simSShE = vs_sample(y = yy, dat = simdaydat %>% dplyr::select(matches("equally_spaced_simssh_[0-9]")) %>% as.matrix()),
             simSShES = vs_sample(y = yy, dat = simdaydat %>% dplyr::select(matches("equally_spaced_shift_simssh_[0-9]")) %>% as.matrix()))
}) %>% bind_rows()

colMeans(vs_df[,-1])/100

