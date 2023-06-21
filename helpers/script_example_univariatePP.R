# a script to show the workflow on an example data set:
# univariate post-processing
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

####################################################################
################# IMPORT ALL DATA AND POST-PROCESS:
####################################################################
# define file
# --- not needed now that the data is in the package
# kepsobs_rds <- list.files(
#   path =  paste0(main_dir, "/data/"),
#   pattern = "KEPSOBS_T_2019-2021",
#   full.names = TRUE)
#
# # import forecast data:
# # this is a data.frame containing all hourly observations for all stations in station_names
# # name: station name
# # hlon/hlat: lon/lat from Harmonie
# # lon/lat: lon/lat from the stations
# # init_time: initialisation time of the forecast in the format "2019-02-10 UTC"
# # leadtime: leadtime of the forecast in the format "01"
# # valid_time: datetime in the format "2019-02-10 01:00:00"
# # T: Observed 2m temperature
# # EM000-EM010: Ensemble member forecasts
# kepsobs_data <- readRDS(kepsobs_rds)
#
# # make predictors for EMOS: ensemble mean and SD
# kepsobs_data$EnsM <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, mean)
# kepsobs_data$EnsS <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, sd)
#
# # which init_times have the complete data:
# kepsobs_complete_inits <- kepsobs_data %>%
#   count(init_time) %>%
#   filter(n == length(station_names) * length(lead_times)) %>%
#   pull(init_time)
#
# kepsobs_data <- kepsobs_data %>% filter(init_time %in% kepsobs_complete_inits)

#usethis::use_data(kepsobs_data)
# --- not needed now that the data is in the package


####################################################################
# local PP
# train on June-July 2020, test on August 2020
####################################################################

train_data <- kepsobs_data %>%
  filter(year(init_time) == 2020 & month(init_time) %in% c(6,7))
test_data <- kepsobs_data %>%
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
    train_stlt <- train_data %>% filter(name == station_names[st] & leadtime == lead_times[lt])
    test_stlt <- test_data %>% filter(name == station_names[st] & leadtime == lead_times[lt])

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
  preds_list[[st]] <- bind_rows(preds_lt)

}

kepsobs_predictions <- bind_rows(preds_list)

# usethis::use_data(kepsobs_predictions)

