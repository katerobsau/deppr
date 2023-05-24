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
    pattern = "KEPS_KNMI_lts_membs_",
    full.names = TRUE)

# import data
kepsobs_data <- readRDS(kepsobs_rds)
station_names <- unique(kepsobs_data$station)
lead_times <- unique(kepsobs_data$leadtime)
keps_members <- kepsobs_data %>% dplyr::select(starts_with("EM")) %>% names()

# make predictors for EMOS: ensemble mean and SD
kepsobs_data$EnsM <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, mean)
kepsobs_data$EnsS <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, sd)



# local PP
# train on June-July, test on August
train_dates <- unique(kepsobs_data$init_time)[grep("-06-|-07-", unique(kepsobs_data$init_time))]
test_dates <- unique(kepsobs_data$init_time)[grep("-08-", unique(kepsobs_data$init_time))]

train <- kepsobs_data %>% filter(init_time %in% train_dates)
test <- kepsobs_data %>% filter(init_time %in% test_dates)

# fit a model per station and leadtime:
# loop over stations:
fits <- list()
preds <- list()
for(st in seq_along(station_names)){
    # loop over leadtimes
    fits_lt <- list()
    preds_lt <- list()
    for(lt in seq_along(lead_times)){
        # define train/test for each station/leadtime
        train_stlt <- train %>% filter(station == station_names[st] & leadtime == lead_times[lt])
        test_stlt <- test %>% filter(station == station_names[st] & leadtime == lead_times[lt])

        # fit the model:
        #   a normal distribution where we predict mu from EnsM and sigma from EnsS
        fits_lt[[lt]] <- gamlss(T2m~EnsM, sigma.formula = ~EnsS,
            data = as.data.frame(train_stlt),
            family = NO())

        # predict the params and combine with test set
        preds_lt[[lt]] <- predictAll(fits_lt[[lt]], newdata = test_stlt) %>%
        as.data.frame() %>%
        cbind(test_stlt, .)
    }
    fits[[st]] <- fits_lt
    preds[[st]] <- rbindlist(preds_lt)

}

preds <- rbindlist(preds)


# make new ensemble members by drawing from the distributions:
# draw random members:
ppfcstsR <- mapply(rNO, mu = preds$mu, sigma = preds$sigma, USE.NAMES = TRUE, MoreArgs = list(n = 11)) %>% t()
colnames(ppfcstsR) <- paste0("EMOSR", 1:11)

# draw equally spaced quantiles:
ppfcstsQ <- mapply(qNO, mu = preds$mu, sigma = preds$sigma, USE.NAMES = TRUE, MoreArgs = list(p = (1:11)/12)) %>% t()
colnames(ppfcstsQ) <- paste0("EMOSQ", 1:11)

# combine raw and post-processed forecasts together:
preds <- cbind(preds, ppfcstsR) %>% cbind(., ppfcstsQ)

# calculate scores:

# crps
preds$crpsRAW <- crps_sample(
        y = preds$T2m,
        dat = as.matrix(preds %>%
          dplyr::select(matches(keps_members)))
      )


preds$crpsEMOSQ <- crps_sample(
        y = preds$T2m,
        dat = as.matrix(preds %>%
          dplyr::select(matches(paste0("^EMOSQ", 1:11, "$"))))
      )

# mean crps improves
mean(preds$crpsRAW)
mean(preds$crpsEMOSQ)


# plot example
plotdate = "2021-08-02"
ggplot(preds %>%
    filter(init_time == as.Date(plotdate)) %>%
    pivot_longer(., EM000:EM010, names_to = "member", values_to = "KEPS")) +
geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
geom_line(aes(x = leadtime, y = T2m, group = station), col = 'black') +
facet_wrap(~station) +
theme_bw() +
ggtitle(plotdate)


# plot example
ggplot(preds %>%
    filter(init_time == as.Date(plotdate)) %>%
    pivot_longer(., EMOSR1:EMOSR11, names_to = "member", values_to = "KEPS")) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T2m, group = station), col = 'black') +
  facet_wrap(~station) +
  theme_bw()+
  labs(title = plotdate, subtitle = "Observation = black,\npost-processed members with random quantiles = green")


ggplot(preds %>%
    filter(init_time == as.Date(plotdate)) %>%
    pivot_longer(., EMOSQ1:EMOSQ11, names_to = "member", values_to = "KEPS")) +
  geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
  geom_line(aes(x = leadtime, y = T2m, group = station), col = 'black') +
  facet_wrap(~station) +
  theme_bw()+
  labs(title = plotdate, subtitle = "Observation = black,\npost-processed members with equally spaced quantiles = green")



############# Restore dependencies


