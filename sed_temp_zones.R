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

