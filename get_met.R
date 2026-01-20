################################################################################
# Code started by Molly Stroud on 12/16/25
# Download data from dynamical.org and put into correct formatting for FLARE
################################################################################
pacman::p_load('tidyverse', 'zarr', 'Rarr', 'sf', 'reticulate')

################################################################################
# Use Python env
################################################################################
reticulate::use_virtualenv("~/Desktop/postdoc/.venv/", required = TRUE)
reticulate::py_config()

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
source("to_hourly.R")
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

# function: get met data from dynamical.org
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
  var_order <- names(temp_df)
  # set as UTC
  temp_df$datetime <- as_datetime(temp_df$datetime)
  attr(temp_df$datetime, "tzone") <- "UTC"
  # call hourly function
  df <- get_hourly(temp_df, mean_lon, mean_lat)
  return(df)
}

# can't be before 2020-10-01
metdata <- get_temp_gefs(site_id = 'BARC', start_time = "2021-09-27")


# FUNCTIONIZE
# run
start_date <- as.Date("2020-10-01")
end_date <- as.Date("2021-01-01")
dates <- seq(start_date, end_date, by = "1 day")

allmetdata <- data.frame()
for(date in dates){
  print(as.Date(date))
  metdata <- get_temp_gefs(site_id = 'fcre', start_time = as.character(as.Date(date))) # try as character
  metdata$reference_datetime <- as.Date(date)
  allmetdata <- rbind(allmetdata, metdata)
}


# save out (can't use arrow here due to earlier loading of python)
allmetdata |>
  group_by(reference_datetime, site_id) |>
  group_walk(~ {
    dir <- file.path(
      "drivers/met/test/stage2",
      paste0("reference_datetime=", .y$reference_datetime),
      paste0("site_id=", .y$site_id)
    )
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(
      .x,
      file.path(dir, "part-0.parquet")
    )
  })


# test
stage2 <- arrow::read_parquet('/Users/mollystroud/Desktop/postdoc/NEON-rs-thermal/drivers/met/gefs-v12/stage2/reference_datetime=2020-10-01/site_id=fcre/part-0.parquet')
stage2_dynamical <- arrow::read_parquet('/Users/mollystroud/Desktop/postdoc/NEON-rs-thermal/drivers/met/test/stage2/reference_datetime=2020-10-01/site_id=fcre/part-0.parquet')


