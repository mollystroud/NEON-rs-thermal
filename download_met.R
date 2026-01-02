################################################################################
# Code started by Molly Stroud on 12/16/25
################################################################################
require(pacman)
p_load('tidyverse', 'zarr', 'Rarr', 'reticulate', 'sf')

################################################################################
# Use Python env where we've called os.environ["SSL_CERT_FILE"] = certifi.where()
################################################################################
use_virtualenv("~/Desktop/postdoc/.venv", required = TRUE)
py_config()

# python libraries
certifi <- import("certifi")
os <- import("os")
xr <- import("xarray")
builtins <- import("builtins", convert = FALSE)
# make sure http can be accessed
os$environ["SSL_CERT_FILE"] <- certifi$where()


# open the zarr from dynamical.org
ds <- xr$open_zarr(
  "https://data.dynamical.org/noaa/gefs/forecast-35-day/latest.zarr?email=optional@email.com",
  #"https://data.dynamical.org/noaa/gefs/analysis/latest.zarr",
  consolidated = TRUE,
  decode_timedelta = TRUE,
  chunks = "auto"
)
#py_to_r(ds$init_time$values)

# use already defined bboxes
source("NEON_bboxes.R")
# variables of interest
vars <- c(
  'temperature_2m',
  'relative_humidity_2m',
  'pressure_surface',
  'wind_u_10m',
  'wind_v_10m',
  'downward_long_wave_radiation_flux_surface',
  'downward_short_wave_radiation_flux_surface',
  'precipitation_surface'
)
# function
get_temp_gefs <- function(site_id, start_time) {
  lat_min = get(paste0(site_id, '_bbox'))[[2]]
  lat_max = get(paste0(site_id, '_bbox'))[[4]]
  lon_min = get(paste0(site_id, '_bbox'))[[1]]
  lon_max = get(paste0(site_id, '_bbox'))[[3]]
  mean_lat <- mean(c(lat_min, lat_max))
  mean_lon <- mean(c(lon_min, lon_max))
  temp <- ds[r_to_py(vars)]$sel(
    init_time = as.character(start_time),
    latitude = mean_lat,
    longitude = mean_lon,
    method = "nearest"
  )
  temp_r <- temp$assign_coords(
    lead_hours = temp$lead_time$astype("timedelta64[h]")$astype("int"),
    member_id  = temp$ensemble_member,
    init_time = temp$init_time
  )
  temp_df <- temp_r$to_dataframe() |>
    py_to_r() |>
    tibble::as_tibble() |>
    select(-c(expected_forecast_length,
              ingested_forecast_length,
              latitude,
              longitude,
              spatial_ref,
              lead_hours)) |>
    pivot_longer(cols = all_of(vars),
                 names_to = 'variable',
                 values_to = 'prediction') |>
    mutate(family = 'ensemble', site_id = site_id, 
           init_time = as.Date(init_time), tz = '') |>
    dplyr::rename(
      'reference_datetime' = 'init_time',
      'datetime' = 'valid_time',
      'parameter' = 'member_id'
    )
  # change variable names
  temp_df$variable[temp_df$variable == "temperature_2m"] <- "air_temperature"
  temp_df$variable[temp_df$variable == "relative_humidity_2m"] <- "relative_humidity"
  temp_df$variable[temp_df$variable == "pressure_surface"] <- "air_pressure"
  temp_df$variable[temp_df$variable == "wind_u_10m"] <- "eastward_wind"
  temp_df$variable[temp_df$variable == "wind_v_10m"] <- "northward_wind"
  temp_df$variable[temp_df$variable == "downward_long_wave_radiation_flux_surface"] <- "surface_downwelling_longwave_flux_in_air"
  temp_df$variable[temp_df$variable == "downward_short_wave_radiation_flux_surface"] <- "surface_downwelling_shortwave_flux_in_air"
  temp_df$variable[temp_df$variable == "precipitation_surface"] <- "precipitation_flux"
  # change to UTC
  attr(temp_df$datetime, "tzone") <- "UTC"
  return(temp_df)
}



df <- get_temp_gefs(site_id = 'fcre', start_time = "2025-02-10")


parameters <- unique(df$parameter)
datetime <- seq(min(df$datetime), max(df$datetime), by = "1 hour")
variables <- unique(df$variable)
sites <- unique(df$site_id)

parameter_maxtime <- df |>
  dplyr::group_by(site_id, family, parameter) |>
  dplyr::summarise(max_time = max(datetime), .groups = "drop")

full_time <- expand.grid(sites, parameters, datetime, variables) |>
  dplyr::rename(site_id = Var1,
                parameter = Var2,
                datetime = Var3,
                variable = Var4) |>
  dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
  dplyr::arrange(site_id, parameter, variable, datetime) |>
  dplyr::left_join(parameter_maxtime, by = c("site_id","parameter")) |>
  dplyr::filter(datetime <= max_time) |>
  dplyr::select(-c("max_time")) |>
  dplyr::distinct()

states <- df |>
  dplyr::select(site_id, family, parameter, datetime, variable, prediction) |>
  dplyr::group_by(site_id, parameter, variable) |>
  dplyr::right_join(full_time, by = c("site_id", "parameter", "datetime", "family", "variable")) |>
  dplyr::filter(variable %in% c("air_pressure", "relative_humidity",
                                "air_temperature", "eastward_wind", "northward_wind")) |>
  dplyr::arrange(site_id, parameter, datetime) |>
  dplyr::mutate(prediction =  imputeTS::na_interpolation(prediction, option = "linear")) |>
  dplyr::mutate(prediction = ifelse(variable == "air_temperature", prediction + 273, prediction)) |>
  dplyr::mutate(prediction = ifelse(variable == "RH", prediction/100, prediction)) |>
  dplyr::ungroup()


fluxes <- df |>
  dplyr::select(site_id, family, parameter, datetime, variable, prediction) |>
  dplyr::group_by(site_id, family, parameter, variable) |>
  dplyr::right_join(full_time, by = c("site_id", "parameter", "datetime", "family", "variable")) |>
  dplyr::filter(variable %in% c("precipitation_flux","surface_downwelling_longwave_flux_in_air","surface_downwelling_shortwave_flux_in_air")) |>
  dplyr::arrange(site_id, family, parameter, datetime) |>
  tidyr::fill(prediction, .direction = "up") |>
  dplyr::mutate(prediction = ifelse(variable == "precipitation_flux", prediction / (6 * 60 * 60), prediction)) |>
  dplyr::ungroup()

hourly_df <- dplyr::bind_rows(states, fluxes) |>
  dplyr::arrange(site_id, family, variable, datetime)
