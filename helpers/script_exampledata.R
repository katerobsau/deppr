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
  as.character(as.POSIXct(start_date, tz = "UTC") + as.numeric(leadtimes) * 60 * 60)
}


# import historical observations:
# define file
obs_rds <- list.files(
  path =  paste0(main_dir, "/data/"),
  pattern = "OBS_T_1950-2021",
  full.names = TRUE)

# import data
obs_data <- readRDS(obs_rds) %>%
  mutate(valid_date = as.Date(valid_time))


# function to make sure we only provide dates that have all dates/times available:
check_avail_datestimes <- function(obsdf, stations, leadtimes){

}

# only keep cases where there are no missing values:
countsmissing <- obs_data %>%
  group_by(valid_time) %>%
  summarise(ncase = length(name)) %>%
  mutate(hour = hour(valid_time))


missing_dates <- countsmissing %>% filter(ncase != 4)
notmissing_dates <- countsmissing %>%
  filter(ncase == 4) %>%
  mutate(min2 = valid_time - 2 * 60 * 60 * 24,
         plu2 = valid_time + 2 * 60 * 60 * 24)




notmissing_lt_list <- mapply(FUN = make_lt_templates, start_date = countsmissing %>% filter(hour == 0) %>% pull(valid_time),
       MoreArgs = list(leadtimes = lead_times), SIMPLIFY = FALSE)


lapply(notmissing_lt_list, function(x) {
  sum(as.POSIXct(notmissing_lt_list[[1]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %in% notmissing$valid_time)
  }) %>% unlist() %>% sum()

obs_data <- obs_data %>% filter(valid_time %in% notmissing)

# SSh - window
# make a template from observations

# what dates do we need to reshuffle: all the dates in the test set
preds_dates <- test %>% pull(init_time) %>% unique() %>% as.Date()
# vector of historical dates:
hist_dates <- obs_data %>% filter(name == station_names[1] &  hour(valid_time) == 1) %>% pull(valid_time) %>% as.Date()
# get the templates:
wind_templates <- mapply(FUN = schaake_template_window,
       date_val = preds_dates,
       MoreArgs = list(n_members = 11, window = 5, historical_dates = hist_dates),
       SIMPLIFY = FALSE)



# returns a list where length = number of days in test set, and
#   where each item has nrows = length(lead_times) and ncols = n_members
wind_templates_lts <- lapply(seq_along(wind_templates), function(wt){
  mapply(FUN = make_lt_templates, start_date = paste0(wind_templates[[wt]], " 00:00:00"), MoreArgs = list(leadtimes = lead_times))
})

# a function to apply run_shuffle_template to all leadtimes and stations at once:
apply_rst <- function(ltt, pdd){
  fc_dat <- preds %>%
    filter(init_time == preds_dates[pdd] & leadtime == lead_times[ltt])
  tp_dates <- wind_templates_lts[[pdd]][ltt,] %>%
    as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  ob_dat <- obs_data %>%
    filter(valid_time %in% tp_dates) %>%
    dplyr::select(valid_time, T, name) %>%
    pivot_wider(., values_from = "T", names_from = "name") %>%
    dplyr::select(-valid_time) %>% as.matrix() %>% t()

  run_shuffle_template(forecast = fc_dat %>%
                         dplyr::select(starts_with("equally_spaced")) %>% as.matrix(),
                       template = ob_dat) %>%
    magrittr::set_colnames(paste0("equally_spaced_ssh_", 1:length(quants$equally_spaced))) %>%
    cbind(fc_dat, .)
}

# loop over each day in the test set:
preds_ssh <- lapply(seq_along(preds_dates), function(pd){
  mapply(FUN = apply_rst, lt = seq_along(lead_times), SIMPLIFY = FALSE) %>% bind_rows()
}) %>% bind_rows()



mapply(FUN = apply_rst, lt = seq_along(lead_times), pdd = seq_along(preds_dates), SIMPLIFY = FALSE)


# sim ssh
get_sim_schaake_template(forecast = preds[1,] %>% dplyr::select(starts_with("EM")),
                         forecast_list = preds[5000:5900,] %>% dplyr::select(starts_with("EM")),
                         obs_list = preds[5000:5900,] %>% dplyr::select(T))
