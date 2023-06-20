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

################# GLOBAL VARIABLES:
main_dir <- getwd()

################# IMPORT ALL DATA AND POST-PROCESS:
## import all data and make df:

# define file
kepsobs_rds <- list.files(
  path =  paste0(main_dir, "/data/"),
  pattern = "KEPSOBS_T_2019-2021",
  full.names = TRUE)

# import data
kepsobs_data <- readRDS(kepsobs_rds)
station_names <- unique(kepsobs_data$name)
lead_times <- unique(kepsobs_data$leadtime)
keps_members <- kepsobs_data %>% dplyr::select(starts_with("EM")) %>% names()

# make predictors for EMOS: ensemble mean and SD
kepsobs_data$EnsM <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, mean)
kepsobs_data$EnsS <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, sd)




# local PP
# train on June-July 2020, test on August 2020
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

quantile_methods <- c("random", "equally_spaced")
quants <- mapply(FUN = get_quantiles, method = quantile_methods, MoreArgs = list(n_members = 11), SIMPLIFY = FALSE)

sample_dist <- function(params, family, quantiles, newname){
  mapply(qNO, mu = params$mu, sigma = params$sigma, USE.NAMES = TRUE, MoreArgs = list(p = quantiles)) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(paste0(newname, 1:length(quantiles)))

}

ppfcsts <- mapply(FUN = sample_dist, quantiles = quants, newname = quantile_methods,
                  MoreArgs = list(params = list(mu = preds$mu, sigma = preds$sigma)), SIMPLIFY = FALSE)


# combine raw and post-processed forecasts together:
preds <- cbind(preds, ppfcsts[[1]]) %>% cbind(., ppfcsts[[2]])

# calculate scores:

# crps
preds$crpsRAW <- crps_sample(
  y = preds$T,
  dat = as.matrix(preds %>%
                    dplyr::select(matches(keps_members)))
)


preds$crpsE <- crps_sample(
  y = preds$T,
  dat = as.matrix(preds %>%
                    dplyr::select(matches(paste0("^equally_spaced"))))
)

preds$crpsR <- crps_sample(
  y = preds$T,
  dat = as.matrix(preds %>%
                    dplyr::select(matches(paste0("^random"))))
)

# mean crps improves
mean(preds$crpsRAW)
mean(preds$crpsE)
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
         pivot_longer(., c(starts_with("equally_spaced")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)


ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(starts_with("random")), names_to = "member", values_to = "KEPS") %>%
         mutate(model = gsub("[0-9]", "", member))) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T, group = name), col = 'black') +
  facet_wrap(~model + name) +
  theme_bw() +
  ggtitle(plotdate)


# crps plot
ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(crpsRAW, crpsE), names_to = "member", values_to = "KEPS")) +
  geom_line(aes(x = leadtime, y = KEPS, group = member, color = member)) +
  facet_wrap(~name) +
  theme_bw() +
  ggtitle(plotdate)



############# Restore dependencies


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





# potential init_times - keep valid_hour == 0 as all our forecasts are initilised at 00 UTC
hist_init_times <- obs_data %>%
  filter(valid_hour == 0) %>%
  pull(valid_time) %>%
  unique()

# for each potential init_time, make a vector of all the leadtimes that we would need:
hist_init_lt <- mapply(FUN = make_lt_templates,
                       start_date = hist_init_times,
                       MoreArgs = list(leadtimes = lead_times),
                       SIMPLIFY = FALSE)


# check that all leadtimes are available for each potential init_time:
all_valid_times <- obs_data$valid_time


# this is a slow function:
# for each of the potential init times, count how many observations there are
# there should be length(lead_times) * length(station_names) = 48 * 4 = 192
n.cores = 8
clust <- makeCluster(n.cores)
clusterExport(clust, c("hist_init_lt", "all_valid_times"))
hist_init_lt_keep <- parLapply(clust, seq_along(hist_init_lt), function(hil){
  sum(all_valid_times %in% hist_init_lt[[hil]])
}) %>% unlist()
stopCluster(clust)


# check that we have length(lead_times) * length(station_names)
hist_init_lt_keep_df <- data.frame(hist_init_lt = hist_init_lt_keep,
                                potential_init = hist_init_times)

# keep only the potential_init times that have the right number of observations:
hist_init_notmissing <- hist_init_lt_keep_df %>%
  filter(hist_init_lt == length(lead_times) * length(station_names)) %>%
  pull(potential_init)

#
hist_initslt_notmissing <- hist_init_lt[which(hist_init_times %in% hist_init_notmissing)] %>% do.call("c", .) %>% unique()

# filter obs_data
obs_data <- obs_data %>% filter(valid_time %in% hist_initslt_notmissing)

# SSh - window
# make a template from observations

# what dates do we need to reshuffle: all the dates in the test set
preds_dates <- test %>% pull(init_time) %>% unique() %>% as.Date()
# vector of historical dates:
#hist_dates <- obs_data %>% filter(name == station_names[1] &  hour(valid_time) == 1) %>% pull(valid_time) %>% as.Date()
hist_dates <- hist_init_times[which(hist_init_times %in% hist_init_notmissing)]
# get the templates:
wind_templates <- mapply(FUN = schaake_template_window,
                         date_val = preds_dates,
                         MoreArgs = list(n_members = 11, window = 5, historical_dates = as.Date(hist_dates)),
                         SIMPLIFY = FALSE)



# returns a list where length = number of days in test set, and
#   where each item has nrows = length(lead_times) and ncols = n_members
wind_templates_lts <- lapply(seq_along(wind_templates), function(wt){
  mapply(FUN = make_lt_templates,
         start_date = paste0(wind_templates[[wt]], " 00:00:00"),
         MoreArgs = list(leadtimes = lead_times),
         SIMPLIFY = FALSE) %>% as.data.frame(.)
})

# a function to apply run_shuffle_template to a leadtime and station combination:
apply_rst <- function(pl){
  pl = as.numeric(pl)
  # get the forecasts in a matrix where nrows = length(station_names) and ncols = n_members
  fc_dat <- preds %>%
    filter(init_time == preds_dates[pl[1]] & leadtime == lead_times[pl[2]])
  tp_dates <- wind_templates_lts[[pl[1]]][pl[2],]
  # get the observations in a matrix where nrows = length(station_names) and ncols = n_members/length(template)
  ob_dat <- obs_data %>%
    filter(valid_time %in% tp_dates) %>%
    dplyr::select(valid_time, T, name) %>%
    pivot_wider(., values_from = "T", names_from = "name") %>%
    dplyr::select(-valid_time) %>% as.matrix() %>% t()

  # shuffle
  run_shuffle_template(forecast = fc_dat %>%
                         dplyr::select(starts_with("equally_spaced")) %>% as.matrix(),
                       template = ob_dat) %>%
    magrittr::set_colnames(paste0("equally_spaced_ssh_", 1:length(quants$equally_spaced))) %>%
    cbind(fc_dat, .) %>%
    mutate(pdd = pl[1], ltt = pl[2])
}

# shuffle predictions for each day in the test set and each leadtime:
preds_ssh <- lapply(1:nrow(expand.grid(seq_along(preds_dates), seq_along(lead_times))), function(nr){
  apply_rst(pl = expand.grid(seq_along(preds_dates), seq_along(lead_times))[nr,])
})

# preds_ssh <- mapply(FUN = apply_rst,
#                     pl = expand.grid(seq_along(preds_dates), seq_along(lead_times)),
#                     SIMPLIFY = FALSE)
#
# for(ltt in seq_along(lead_times)){
#   for(pdd in seq_along(preds_dates)){
#     print(paste0("LTT == ", ltt, " & PDD == ", pdd))
#     apply_rst(pl = expand.grid(seq_along(preds_dates), seq_along(lead_times))[1,])
#   }
# }




# sim ssh
get_sim_schaake_template(forecast = preds[1,] %>% dplyr::select(starts_with("EM")),
                         forecast_list = preds[5000:5900,] %>% dplyr::select(starts_with("EM")),
                         obs_list = preds[5000:5900,] %>% dplyr::select(T))
