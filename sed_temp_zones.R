# exploring sediment relationship at FCR, sunapee, lake alex
################################################################################
pacman::p_load(ggplot2, ggrepel, patchwork)

################################################################################
# FCR
################################################################################
meteo <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b")

meteo_cleaned <- meteo |>
  dplyr::filter(meteo$DateTime > "2018-07-05") |>
  dplyr::select(DateTime, AirTemp_C_Average, Flag_AirTemp_C_Average, Note_AirTemp_C_Average) |>
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::group_by(DateTime) |>
  dplyr::summarize(AirTemp_C_Average = mean(AirTemp_C_Average))

meteo_cleaned$year <- format(meteo_cleaned$DateTime, "%y")
meteo_cleaned$doy <- yday(meteo_cleaned$DateTime)

max_airtemp <- meteo_cleaned |>
  dplyr::group_by(year) |>
  dplyr::filter(AirTemp_C_Average == max(AirTemp_C_Average, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::select(DateTime, amplitude = AirTemp_C_Average)

mean(max_airtemp$amplitude)
mean(yday(max_airtemp$DateTime))

# get daily average over the years
avg_airtemp <- meteo_cleaned |>
  dplyr::group_by(doy) |>
  dplyr::summarize(Temp = mean(AirTemp_C_Average, na.rm = T))

water_temp <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136")

water_temp_cleaned <- water_temp |>
  dplyr::select(DateTime, ThermistorTemp_C_surface, ThermistorTemp_C_1,
                ThermistorTemp_C_2, ThermistorTemp_C_3, ThermistorTemp_C_4, ThermistorTemp_C_5,
                ThermistorTemp_C_6, ThermistorTemp_C_7, ThermistorTemp_C_8) |>
  tidyr::pivot_longer(!DateTime, names_to = "Depth", values_to = "Temp") |>
  dplyr::mutate(DateTime = as.Date(DateTime)) |>
  dplyr::group_by(DateTime, Depth) |>
  dplyr::summarize(Temp = mean(Temp)) |>
  dplyr::mutate(Depth = gsub("[^0-9.]", "", Depth)) |>
  dplyr::mutate(Depth = replace(Depth, Depth == "", 0))

water_temp_cleaned$year <- format(water_temp_cleaned$DateTime, "%y")
water_temp_cleaned$doy <- yday(water_temp_cleaned$DateTime)
# get average each day
max_sedtemp <- water_temp_cleaned |>
  dplyr::group_by(year, Depth) |>
  dplyr::filter(Temp == max(Temp, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::select(DateTime, Depth, amplitude = Temp)

mean(max_sedtemp$amplitude[as.numeric(max_sedtemp$Depth) > 4])
mean(yday(max_sedtemp$DateTime[as.numeric(max_sedtemp$Depth) > 4]))

# get daily average over the years
avg_sedtemp <- water_temp_cleaned |>
  dplyr::group_by(doy, Depth) |>
  dplyr::summarize(Temp = mean(Temp, na.rm = T))

# compare!
fcre <- ggplot() +
  geom_line(data = avg_airtemp, aes(x = doy, y = Temp), color = 'darkblue') +
  geom_line(data = avg_sedtemp, aes(x = doy, y = Temp, color = Depth)) +
  theme_classic() +
  #geom_label_repel(data = avg_sedtemp |> group_by(Depth) |> filter(Temp == max(Temp, na.rm = TRUE)),
             #aes(x = doy, y = Temp, label = paste0("   peak doy = ", doy, "\nmax temp = ", round(Temp, 2))),
             #vjust = 0.5, hjust = -0.2, alpha = 0.7) +
  labs(title = "FCR", x = "Day of Year")
fcre


mean(avg_airtemp$Temp, na.rm = T)
mean(avg_sedtemp[avg_sedtemp$Depth > 5,]$Temp, na.rm = T)
airtemp_amp <- (max(avg_airtemp$Temp) - min(avg_airtemp$Temp)) / 2


# estimate peak doy at depth
airtemp_peakdoy <- which.max(avg_airtemp$Temp)
# adding air temp peak doy to 2*depth and days of year/2pi (converting radians to days)
watertemp_peakdoy <- airtemp_peakdoy + 2*8 + (365/(2*pi))

(272 + 198) / 1.8

airtemp_peakdoy + (2*1) + 1/8*(365/(2*pi))

# estimate amplitude at depth (-8 is the depth of the lake, 8 is the sed depth)
watertemp_amp <- airtemp_amp * exp(-9/8)


(max(avg_sedtemp[avg_sedtemp$Depth > 7,]$Temp) - min(avg_sedtemp[avg_sedtemp$Depth > 7,]$Temp)) / 2
#### OR PEAK IS NEAR SECOND MEAN 284
mean_temp <- mean(avg_airtemp$Temp, na.rm = T)

# smooth data
avg_airtemp$smoothed <- stats::filter(
  avg_airtemp$Temp,
  rep(1/14, 14),   # 15-day moving average
  sides = 2
)
mean_temp <- mean(avg_airtemp$smoothed, na.rm = T)
crossings <- which(diff(avg_airtemp$smoothed > mean_temp) != 0)


################################################################################
# Sunapee
################################################################################
wq_sunapee <- read_csv("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/sunp/sunp-targets-insitu.csv")

wq_sunapee_cleaned <- wq_sunapee |>
  dplyr::select(-c(variable, site_id))
wq_sunapee_cleaned$year <- format(wq_sunapee_cleaned$datetime, "%y")
wq_sunapee_cleaned$doy <- yday(wq_sunapee_cleaned$datetime)

# get daily average over the years
avg_sedtemp_sunp <- wq_sunapee_cleaned |>
  dplyr::group_by(doy, depth) |>
  dplyr::summarize(Temp = mean(observation, na.rm = T))

# met
sunp_files <- list.files(path = "/Users/mollystroud/Desktop/postdoc/sunapee/edi.234.7", 
                         pattern = '.csv', full.names = T)
sunp_met <- data.frame()
for(file in sunp_files[-19]){
  csv <- read_csv(file)
  csv <- csv |>
    dplyr::select(datetime, airTemperature_degC)
  sunp_met <- rbind(sunp_met, csv)
}

sunp_met_cleaned <- sunp_met |>
  dplyr::mutate(datetime = as.Date(datetime)) |>
  dplyr::group_by(datetime) |>
  dplyr::summarize(Temp = mean(airTemperature_degC))

sunp_met_cleaned$doy <- yday(sunp_met_cleaned$datetime)

# get daily average over the years
avg_airtemp_sunp <- sunp_met_cleaned |>
  dplyr::group_by(doy) |>
  dplyr::summarize(Temp = mean(Temp, na.rm = T))



mean(avg_airtemp_sunp$Temp, na.rm = T)
mean(avg_sedtemp_sunp[avg_sedtemp_sunp$depth > 15,]$Temp, na.rm = T)


sunp <- ggplot() +
  geom_line(data = avg_airtemp_sunp, aes(x = doy, y = Temp), color = 'darkblue') +
  geom_line(data = avg_sedtemp_sunp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  geom_label(data = avg_sedtemp_sunp |> group_by(depth) |> filter(Temp == max(Temp, na.rm = TRUE)),
             aes(x = doy, y = Temp, 
                 label = paste0("   peak doy = ", doy, "\nmax temp = ", round(Temp, 2))),
             vjust = 0.5, hjust = -0.2, alpha = 0.7) +
  labs(title = "Sunapee", x = "Day of Year", color = "Depth")
sunp

################################################################################
# Alex
################################################################################
alex_watertemp_df <- read_csv('https://amnh1.osn.mghpcc.org/bio230121-bucket01/flare/targets/ALEX/ALEX-targets-insitu.csv') |> 
  filter(variable == 'temperature')

temp_alex <- alex_watertemp_df |>
  dplyr::select(-c(variable, site_id))
temp_alex$doy <- yday(temp_alex$datetime)

# get daily average over the years
avg_watertemp_alex <- temp_alex |>
  dplyr::group_by(doy) |>
  dplyr::summarize(Temp = mean(observation, na.rm = T))


# met data
devtools::install_github("FLARE-forecast/ropenmeteo", force = T)
library(ropenmeteo)

alex_era5_download <- get_historical_weather(latitude = -35.435564,
                                             longitude = 139.170332,
                                             site_id = NULL,
                                             start_date = "2020-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))

alex_cleaned <- alex_era5_download |>
  dplyr::select(-c(site_id, model_id, variable, unit)) |>
  dplyr::mutate(datetime = as.Date(datetime)) |>
  dplyr::group_by(datetime) |>
  dplyr::summarize(Temp = mean(prediction))

alex_cleaned$year <- format(alex_cleaned$datetime, "%y")
alex_cleaned$doy <- yday(alex_cleaned$datetime)

# get daily average over the years
avg_airtemp_alex <- alex_cleaned |>
  dplyr::group_by(doy) |>
  dplyr::summarize(Temp = mean(Temp, na.rm = T))


alex <- ggplot() +
  geom_line(data = avg_airtemp_alex, aes(x = doy, y = Temp, color = 'Air Temp')) +
  geom_line(data = avg_watertemp_alex, aes(x = doy, y = Temp, color = '0.5m')) +
  geom_label(data = avg_watertemp_alex %>% filter(Temp == max(Temp, na.rm = TRUE)),
            aes(x = doy, y = Temp, 
                label = paste0("   peak doy = ", doy, "\nmax temp = ", round(Temp, 2))),
            vjust = 0.5, hjust = -0.2, alpha = 0.7) +
  theme_classic() +
  scale_color_manual(labels = c("Air Temp", "0.5m"), values = c("darkblue", "green")) +
  labs(title = "Lake Alex", x = "Day of Year", color = element_blank())
alex



mean(avg_airtemp_alex$Temp, na.rm = T)
mean(avg_watertemp_alex$Temp, na.rm = T)

################################################################################
# NEON
################################################################################
neon_temps <- read_csv("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/NEON_insitu_columntemp.csv")
neon_temps$doy <- yday(neon_temps$datetime)

# functions
watertemp_clean <- function(temps, site){
  avg_temp <- temps |>
    dplyr::filter(site_id == site) |>
    dplyr::group_by(doy, depth) |>
    dplyr::summarize(Temp = mean(observation, na.rm = T))
}

airtemp_clean <- function(airtemps){
  temps <- airtemps |>
    dplyr::select(-c(site_id, model_id, variable, unit)) |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime) |>
    dplyr::summarize(Temp = mean(prediction))
  temps$doy <- yday(temps$datetime)
  # get daily average over the years
  avg_temps <- temps |>
    dplyr::group_by(doy) |>
    dplyr::summarize(Temp = mean(Temp, na.rm = T))
}

# SUGG
sugg_watertemp <- watertemp_clean(neon_temps, "SUGG")
sugg_era5_download <- get_historical_weather(latitude = 29.6880,
                                             longitude = -82.0179,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
sugg_airtemp <- airtemp_clean(sugg_era5_download)
sugg_plot <- ggplot() +
  geom_line(data = sugg_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = sugg_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Lake Suggs", x = "Day of Year", color = element_blank())
sugg_plot


mean(sugg_watertemp$Temp, na.rm = T)
mean(sugg_airtemp$Temp, na.rm = T)

# BARC
barc_watertemp <- watertemp_clean(neon_temps, "BARC")
barc_era5_download <- get_historical_weather(latitude = 29.6761,
                                             longitude = -82.0086,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
barc_airtemp <- airtemp_clean(barc_era5_download)
barc_plot <- ggplot() +
  geom_line(data = barc_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = barc_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Lake Barco", x = "Day of Year", color = element_blank())
barc_plot

mean(barc_watertemp$Temp, na.rm = T)
mean(barc_airtemp$Temp, na.rm = T)

# CRAM
cram_watertemp <- watertemp_clean(neon_temps, "CRAM")
cram_era5_download <- get_historical_weather(latitude = 46.2097,
                                             longitude = -89.4730,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
cram_airtemp <- airtemp_clean(cram_era5_download)
cram_plot <- ggplot() +
  geom_line(data = cram_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = cram_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Crampton Lake", x = "Day of Year", color = element_blank())
cram_plot

mean(cram_watertemp$Temp, na.rm = T)
mean(cram_airtemp$Temp, na.rm = T)

airtemp_amp_cram <- (max(cram_airtemp$Temp) - min(cram_airtemp$Temp)) / 2
# estimate amplitude at depth (-10 is the depth of the lake, 10 is the sed depth)
watertemp_amp_cram <- airtemp_amp_cram * exp(-1/10)^2 # not sure about ^2


airtemp_amp_cram * 10^(-5/10) #


185 + 2*10.3 + (365/(2*pi))

(185 + 263) / 1.8



# PRLA
prla_watertemp <- watertemp_clean(neon_temps, "PRLA")
prla_era5_download <- get_historical_weather(latitude = 47.1598,
                                             longitude = -99.1184,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
prla_airtemp <- airtemp_clean(prla_era5_download)
prla_plot <- ggplot() +
  geom_line(data = prla_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = prla_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Prairie Lake", x = "Day of Year", color = element_blank())
prla_plot

mean(prla_airtemp$Temp, na.rm = T)
mean(prla_watertemp$Temp, na.rm = T)


# LIRO
liro_watertemp <- watertemp_clean(neon_temps, "LIRO")
liro_era5_download <- get_historical_weather(latitude = 45.9958,
                                             longitude = -89.7020,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
liro_airtemp <- airtemp_clean(liro_era5_download)
liro_plot <- ggplot() +
  geom_line(data = liro_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = liro_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Little Rock Lake", x = "Day of Year", color = element_blank())
liro_plot

mean(liro_airtemp$Temp, na.rm = T)
mean(liro_watertemp$Temp, na.rm = T)

# PRPO
prpo_watertemp <- watertemp_clean(neon_temps, "PRPO")
prpo_era5_download <- get_historical_weather(latitude = 47.1301,
                                             longitude = -99.2527,
                                             site_id = NULL,
                                             start_date = "2017-06-01",
                                             end_date = "2025-06-01",
                                             variables = c("temperature_2m"))
prpo_airtemp <- airtemp_clean(prpo_era5_download)
prpo_plot <- ggplot() +
  geom_line(data = prpo_airtemp, aes(x = doy, y = Temp,), color = 'darkblue') +
  geom_line(data = prpo_watertemp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Prairie Pothole", x = "Day of Year", color = element_blank())
prpo_plot

mean(prpo_airtemp$Temp, na.rm = T)
mean(prpo_watertemp$Temp, na.rm = T)
(max(prpo_airtemp$Temp) - min(prpo_airtemp$Temp)) / 2



### PLOT
(sugg_plot + barc_plot + cram_plot + prla_plot + liro_plot + prpo_plot) + 
  plot_layout(ncol = 3, axis_titles = "collect")







