# exploring sediment relationship at FCR, sunapee, lake alex
################################################################################
# FCR
################################################################################
# get amplitude and doy of air temp each year
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


################################################################################
# get amplitude and doy of sed temp each year
################################################################################
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
ggplot() +
  geom_line(data = avg_airtemp, aes(x = doy, y = Temp), color = 'darkblue') +
  geom_line(data = avg_sedtemp, aes(x = doy, y = Temp, color = Depth)) +
  theme_classic() +
  labs(title = "FCR", x = "Day of Year")



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




ggplot() +
  geom_line(data = avg_airtemp_sunp, aes(x = doy, y = Temp), color = 'darkblue') +
  geom_line(data = avg_sedtemp_sunp, aes(x = doy, y = Temp, color = as.factor(depth))) +
  theme_classic() +
  labs(title = "Sunapee", x = "Day of Year", color = "Depth")


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



ggplot() +
  geom_line(data = avg_airtemp_alex, aes(x = doy, y = Temp, color = 'Air Temp')) +
  geom_line(data = avg_watertemp_alex, aes(x = doy, y = Temp, color = '0.5m')) +
  theme_classic() +
  scale_color_manual(labels = c("Air Temp", "0.5m"), values = c("darkblue", "green")) +
  labs(title = "Lake Alex", x = "Day of Year", color = element_blank())


