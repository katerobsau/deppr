# a script to make an example data set:
# obs and ensemble forecasts at 4 stations for summer 2021

################# SETUP
# load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(terra)
library(sf)
library(data.table)
library(gamlss)

################# GLOBAL VARS
# define directories
knmionly_dir <- "/net/pc200268//nobackup/users/baar/community_climatology/2023_03_12_egu_vienna_2023_statistics/output_raw_data/"
harm_dir <- "/net/pc200276/nobackup/users/stat/arch/HA40_ens/"
powdr_dir <- "/nobackup_1/users/whan/R/POWDR/"

# define station information
station_coords = data.frame(
    lon = c(5.1810, 4.7683, 4.8979, 5.7712),
    lat = c(52.1093, 52.3105, 51.9653, 50.9123),
    name = c("DeBilt", "Schiphol", "Cabauw", "Maastricht"))
station_coords_sf <- st_as_sf(station_coords[, c("lon", "lat")], coords = c("lon", "lat"))
st_crs(station_coords_sf) <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# setup some info for the KNMI data
KNMIformat <- "_AirTemperature_Celsius_1pdOnly_Yi.csv"
KNMIlon <- read_csv("/net/pc200268//nobackup/users/baar/community_climatology/2023_03_12_egu_vienna_2023_statistics/output_raw_data/LON.csv",
  col_names = FALSE, show_col_types = FALSE
)
KNMIlat <- read_csv("/net/pc200268//nobackup/users/baar/community_climatology/2023_03_12_egu_vienna_2023_statistics/output_raw_data/LAT.csv",
  col_names = FALSE, show_col_types = FALSE
)
KNMImask <- read_csv("/net/pc200268//nobackup/users/baar/community_climatology/2023_03_12_egu_vienna_2023_statistics/output_raw_data/MASK.csv",
  col_names = FALSE, na = c("0"), show_col_types = FALSE
)

# find the closest grid point in the KNMI data for each station
eg_knmi <- read_csv("/net/pc200268//nobackup/users/baar/community_climatology/2023_03_12_egu_vienna_2023_statistics/output_raw_data/2021_06_01_01h00_AirTemperature_Celsius_1pdOnly_Yi.csv",
              col_names = FALSE,
              show_col_types = FALSE
            ) * KNMImask
eg_knmi <- data.frame(
              lon = unlist(KNMIlon),
              lat = unlist(KNMIlat),
              T2m = unlist(eg_knmi)
              ) %>% na.omit()
eg_knmi_sf <- st_as_sf(eg_knmi, coords = c("lon", "lat"))
st_crs(eg_knmi_sf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
station_closest_points <- st_nearest_feature(x = station_coords_sf, y = eg_knmi_sf)


# define summer dates:
allinits <- seq(
  as.Date("2021-06-01"),
  as.Date("2021-08-31"),
  by = "days"
)
lead_times <- sprintf("%02d", 1:48)
keps_members <- sprintf("%03d", 0:10)


################# IMPORT AND MATCH FORECASTS AND OBS:

# loop over each day, to make a daily by lt file with all membs
for (ii in seq_along(allinits)) {
  print(paste0("date == ", allinits[ii], " at ", Sys.time()))
  iidate <- gsub("-", "", allinits[ii])
  iioutfile <- paste0(powdr_dir, "/data/", paste0(
    "KEPS_KNMI_lts_membs_",
    allinits[ii], ".rds"
  ))
  if (!file.exists(iioutfile)) {
  ltdat <- lapply(seq_along(lead_times), function(lt) {
      print(paste0("lead time == ", lead_times[lt]))
      nmdat <- lapply(seq_along(keps_members), function(nm) {
        print(paste0("member == ", keps_members[nm]))
        memb_dir <- paste0(harm_dir, "mbr", keps_members[nm], "/")
        iiltnm_file <- paste0(
          memb_dir, "HA40_N25_", keps_members[nm],
          "_", iidate, "0000_0", lead_times[lt], "00_GB"
        )
        if (file.exists(iiltnm_file)) {
          # import HA
          tmprast <- rast(iiltnm_file, lyrs = 4)
          hyear <- substr(gsub(
            paste0("HA40_N25_", keps_members[nm], "_"),
            "", basename(iiltnm_file)
          ), 1, 4)
          hmon <- substr(gsub(
            paste0("HA40_N25_", keps_members[nm], "_"),
            "", basename(iiltnm_file)
          ), 5, 6)
          hday <- substr(gsub(
            paste0("HA40_N25_", keps_members[nm], "_"),
            "", basename(iiltnm_file)
          ), 7, 8)
          hhr <- substr(gsub(
            paste0("HA40_N25_", keps_members[nm], "_"),
            "", basename(iiltnm_file)
          ), 9, 10)
          itdate <- as.POSIXct(paste0(hyear, hmon, hday, hhr),
            format = "%Y%m%d%H", tz = "UTC"
          )
          tmpdf_ha <- extract(tmprast, station_coords[,c("lon", "lat")], xy = TRUE)
          names(tmpdf_ha) <- c("ID",  paste0("EM", keps_members[nm]), "x", "y")
          tmpdf_ha <- tmpdf_ha %>%
            mutate(
                hlon = x,
                hlat = y,
                station = station_coords$name,
                stlon = station_coords$lon,
                stlat = station_coords$lat,
                init_time = itdate,
                leadtime = lead_times[lt],
                valid_time = time(tmprast)
                ) %>% dplyr::select(-ID, -x, -y)
   
          # if mem == 1, then add acpc and obs
          if (nm == 1) {
            # obs times look like this: estimate_mean_day=18_hour=24_month=6
            # to get "2021-06-18 UTC" we need to take this estimate_mean_day=17_hour=24_month=6
            hatime <- tmpdf_ha$valid_time[1]
            if (hour(hatime) == 0) {
              obstime <- tmpdf_ha$valid_time[1] - 1 * 60 * 60 * 24
            } else {
              obstime <- tmpdf_ha$valid_time[1]
            }
            obshour <- hour(obstime)
            obsday <- day(obstime)
            obsmonth <- month(obstime)
            # create tmpdf_obs: x, y, lon, lat, valid_time, Obs
            obshour <- ifelse(obshour == 0, 24, obshour)
            whichhday <- times_obs_rast$day == obsday # as.numeric(hday)
            whichhmon <- times_obs_rast$month == obsmonth # as.numeric(hmon)
            whichhhr <- times_obs_rast$hour == obshour # obs_hr_240
            vtchr <- ifelse(lt %in% c(24),
              format(
                round(tmpdf_ha$valid_time[1], units = "day"),
                "%Y-%m-%d %M:%H:%S"
              ), as.character(tmpdf_ha$valid_time[1])
            )
            # KNMI only obs:
            knmionlyfile <- paste0(
              knmionly_dir,
              gsub("-", "_", as.Date(obstime)),
              "_", str_pad(obshour, 2, pad = "0"), "h00", KNMIformat
            )
            tmpdf_knmi <- read_csv(knmionlyfile,
              col_names = FALSE,
              show_col_types = FALSE
            ) * KNMImask
            tmpdf_knmi <- data.frame(
              olon = unlist(KNMIlon),
              olat = unlist(KNMIlat),
              T2m = unlist(tmpdf_knmi)
            ) %>% na.omit()
            tmpdf_knmi <- tmpdf_knmi[station_closest_points,]
            tmpdf_haobs <- cbind(tmpdf_ha, tmpdf_knmi) %>%
            dplyr::select(init_time, leadtime, valid_time,
                hlon, hlat, stlon, stlat, olon, olat, station,
                T2m, starts_with("EM"))

            return(tmpdf_haobs)
          } else {
            return(tmpdf_ha %>% dplyr::select(starts_with("EM")))
          }
        } # end if file exists
      }) # end membs
      # combine members into 1 df
      if (!any(unlist(lapply(nmdat, is.null)))) {
        nmdat <- do.call("cbind", nmdat) %>%
          dplyr::select(
           init_time, leadtime, valid_time,
                hlon, hlat, stlon, stlat, olon, olat, station,
                T2m, starts_with("EM")
          )
        return(nmdat)
      }
    }) # end lts
    ltdat <- data.table::rbindlist(ltdat)
    saveRDS(ltdat, iioutfile)
    } # if file doens't exist
} # end ii loop


# plot example
ggplot(ltdat %>% pivot_longer(., EM000:EM010, names_to = "member", values_to = "KEPS")) + 
geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
geom_line(aes(x = leadtime, y = T2m, group = station), col = 'black') +
facet_wrap(~station) +
theme_bw()


################# IMPORT ALL DATA AND POST-PROCESS:
## import all data and make df:

# define files
kepsobs_rds <- list.files(
    path =  paste0(powdr_dir, "/data/"),
    pattern = "KEPS_KNMI_lts_membs_",
    full.names = TRUE)

# import data
kepsobs_data <- lapply(seq_along(kepsobs_rds), function(ff){
    readRDS(kepsobs_rds[ff])
}) %>%
rbindlist()

# make predictors
kepsobs_data$EnsM <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, mean)
kepsobs_data$EnsS <- apply(kepsobs_data %>% dplyr::select(starts_with("EM")), 1, sd)

# save final dataset:
saveRDS(kepsobs_data, file = "/nobackup_1/users/whan/R/POWDR//data//KEPS_KNMI_lts_membs_20210601-20210831.rds")

# local PP
# train on June-July, test on August
train_dates <- unique(kepsobs_data$init_time)[grep("-06-|-07-", unique(kepsobs_data$init_time))]
test_dates <- unique(kepsobs_data$init_time)[grep("-08-", unique(kepsobs_data$init_time))]

train <- kepsobs_data %>% filter(init_time %in% train_dates)
test <- kepsobs_data %>% filter(init_time %in% test_dates)

# loop over stations:
fits <- list()
preds <- list()
for(st in seq_along(station_closest_points)){
    # loop over leadtimes
    fits_lt <- list()
    preds_lt <- list()
    for(lt in seq_along(lead_times)){
        # define train/test for each station/leadtime
        train_stlt <- train %>% filter(station == station_coords$name[st] & leadtime == lead_times[lt])
        test_stlt <- test %>% filter(station == station_coords$name[st] & leadtime == lead_times[lt])
        
        # fit the model
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
          dplyr::select(matches(paste0("^EM", keps_members, "$"))))
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
ggtitle(plotdate)

ggplot(preds %>%
    filter(init_time == as.Date(plotdate)) %>%
    pivot_longer(., EMOSQ1:EMOSQ11, names_to = "member", values_to = "KEPS")) + 
geom_line(aes(x = leadtime, y = KEPS, group = member), col = 'forestgreen') +
geom_line(aes(x = leadtime, y = T2m, group = station), col = 'black') +
facet_wrap(~station) +
theme_bw()+
ggtitle(plotdate)



############# Restore dependencies