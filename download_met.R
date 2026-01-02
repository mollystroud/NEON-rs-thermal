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
vars <- c('temperature_2m', 'relative_humidity_2m', 'pressure_surface', 
          'wind_u_10m', 'wind_v_10m', 'downward_long_wave_radiation_flux_surface',
          'downward_short_wave_radiation_flux_surface', 'precipitation_surface')
# function
get_temp_gefs <- function(site_id, start_time) {
  lat_min = get(paste0(site_id,'_bbox'))[[2]]
  lat_max = get(paste0(site_id,'_bbox'))[[4]]
  lon_min = get(paste0(site_id,'_bbox'))[[1]]
  lon_max = get(paste0(site_id,'_bbox'))[[3]]
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
    select(-c(expected_forecast_length, ingested_forecast_length, 
              latitude, longitude, spatial_ref, lead_hours)) |> 
    pivot_longer(cols = all_of(vars), 
                 names_to = 'variable', 
                 values_to = 'prediction') |>
    mutate(family = 'ensemble',
           site_id = site_id) |>
    dplyr::rename('reference_datetime' = 'init_time', 
                  'datetime' = 'valid_time',
                  'parameter' = 'member_id')
    return(temp_df)
}



df <- get_temp_gefs(site_id = 'fcre',
                    start_time = "2025-02-10")






