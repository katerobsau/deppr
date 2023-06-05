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
  filter(year(init_time) == 2020 & month(init_time) %in% c(8))

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
    preds[[st]] <- rbindlist(preds_lt)

}

preds <- rbindlist(preds)


# make new ensemble members by drawing from the distributions:
# draw random members:

sample_dist_all <- function(){

}

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
        y = preds$T,
        dat = as.matrix(preds %>%
          dplyr::select(matches(keps_members)))
      )


preds$crpsEMOSQ <- crps_sample(
        y = preds$T,
        dat = as.matrix(preds %>%
          dplyr::select(matches(paste0("^EMOSQ", 1:11, "$"))))
      )

# mean crps improves
mean(preds$crpsRAW)
mean(preds$crpsEMOSQ)


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


# crps plot
ggplot(preds %>%
         filter(init_time == as.Date(plotdate)) %>%
         pivot_longer(., c(crpsRAW, crpsEMOSQ), names_to = "member", values_to = "KEPS")) +
  geom_line(aes(x = leadtime, y = KEPS, group = member, color = member)) +
  facet_wrap(~name) +
  theme_bw() +
  ggtitle(plotdate)



############# Restore dependencies



# SSh
# make a template from observations



# compare ECC and SSh
# same function for any method. Just give different template.
ECC = run_shuffle_template(forecast = as.matrix(preds[1,] %>% dplyr::select(starts_with("EMOSR"))),
                     template = as.matrix(preds[1,] %>% dplyr::select(matches("EM[0-9]"))))


SSh = run_shuffle_template(forecast = as.matrix(preds[1,] %>% dplyr::select(starts_with("EMOSR"))),
                           template = as.matrix(preds[1,] %>% dplyr::select(matches("EM[0-9]"))))

# scoring using scoringRules

