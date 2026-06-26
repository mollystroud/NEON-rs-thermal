library(tidyverse)
secchi <- read_csv('https://pasta.lternet.edu/package/data/eml/edi/198/14/547f9a388a26711b4b10e4c7ad7e1a4e')

# get secchi depths
ccr <- secchi |>
  filter(Reservoir == "CCR") |>
  summarize(mean = mean(Secchi_m))


sunp <- read_csv("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/sunp/LSPALMP_1986-2022_v2023-01-22.csv") |>
  filter(parameter == "secchiDepth_m") |>
  summarize(mean = mean(value, na.rm = T))


# get depth and volume
bathy <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184")

library(terra)
sunp_bathy <- rast('/Users/mollystroud/Downloads/original rasters/sun_ras_z_m/prj.adf')
plot(sunp_bathy)
sunp_reproj <- terra::project(sunp_bathy, "EPSG:26919")

# calc cell area 
cell_area <- 1.300119 * 1.300119  # m²
# get vol
vol_raster <- sunp_reproj * cell_area
# sum
total_volume <- global(vol_raster, sum, na.rm = TRUE)

# watershed size

# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1254.1
library(sf)
ccr <- read_sf('/Users/mollystroud/Downloads/Watersheds/CCR watershed/layers/globalwatershed.shp')
plot(ccr)
st_area(ccr)

bvr <- read_sf('/Users/mollystroud/Downloads/Watersheds/FCR and BVR watersheds/BEAVERDAM_watershed.shp')
st_area(bvr)

fcr <- read_sf('/Users/mollystroud/Downloads/Watersheds/FCR and BVR watersheds/FCR_watershed.shp')
st_area(fcr)




# mean annual air temperature
devtools::install_github("FLARE-forecast/ropenmeteo", force = T, upgrade = "never")
library(ropenmeteo)
library(zoo)


era5_download <- get_historical_weather(latitude = 37.38547,
                                        longitude = -79.950403,
                                        start_date = Sys.Date() - 3000, # get a long enough date range
                                        end_date = Sys.Date(),
                                        variables = c("temperature_2m")) # precipitation

mean(era5_download$prediction)

# mean annual precip
annual_precip <- era5_download |>
  mutate(year = format(as.Date(datetime, format="%d/%m/%Y"),"%Y")) |>
  group_by(year) |>
  summarize(meanprecip = sum(prediction))
mean(annual_precip$meanprecip)



# mean 10-day air temp sd
airtempsd <- era5_download |>
  mutate(date = as.Date(datetime)) |>
  filter(date >= as_date("2021-05-18") & date <= as_date("2021-10-31")) |>
  group_by(date) |>
  summarize(air_temp_day = mean(prediction, na.rm = TRUE), .groups = "drop") |>
  mutate(roll_sd = rollapply(air_temp_day, width = 10, FUN = sd, fill = NA)) |>
  summarise(mean = mean(roll_sd, na.rm = TRUE))
print(airtempsd)



# n remote sensing images
rs <- read_csv('/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/fcre/fcre-targets-rs.csv') |>
  filter(datetime >= "2020-11-08" & datetime < "2023-01-01")



# wind speed
# for NEON sites
product <- "DP1.20059.001" # wind speed
sites <- c("PRLA", "PRPO", "SUGG", "LIRO", "CRAM", "BARC") # all sites (takes longer)

# download available files
neonstore::neon_download(product = product, site = sites, 
                         start_date = "2021-01-01",
                         end_date = "2023-01-01")
wind <- neonUtilities::stackFromStore(filepaths=tools::R_user_dir("neonstore"),
                               dpID="DP1.20059.001",
                               package="basic",
                               site=sites)
wind_30min <- wind$WSDBuoy_30min |>
  group_by(siteID) |>
  summarize(mean_wind = mean(buoyWindSpeedMean, na.rm = T))
# for wvwa sites
carvins <- read_csv('https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18') |>
  filter(DateTime >= "2021-01-01") |>
  filter(DateTime <= "2023-01-01") |>
  summarize(mean_wind = mean(WindSpeed_Average_m_s, na.rm = T))

fcre_bvre <- read_csv('https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b') |>
  filter(DateTime >= "2021-01-01") |>
  filter(DateTime <= "2023-01-01") |>
  summarize(mean_wind = mean(WindSpeed_Average_m_s, na.rm = T))
# for sunp
# mean annual air temperature
devtools::install_github("FLARE-forecast/ropenmeteo", force = T, upgrade = "never")
library(ropenmeteo)
library(zoo)


era5_download <- get_historical_weather(latitude = 43.383904,
                                        longitude = -72.054173,
                                        start_date = "2021-01-01",
                                        end_date = "2023-01-01",
                                        variables = c("wind_speed_10m")) # precipitation

mean(era5_download$prediction)




# stratification
# for each site, get 2nd shallowest and deepest depths for each day of data,
# calculate strat on each day and count days

library(rLakeAnalyzer)

sitenames <- list.files('targets')
for(site in sitenames){
  data <- read_csv(paste0('targets/', site, '/', site, '-targets-insitu.csv'), show_col_types = F) |>
    filter(datetime >= "2020-12-31") |>
    filter(datetime <= "2023-01-01") |>
    filter(variable == 'temperature') |>
    filter(!is.na(observation)) |>
    select(-c(variable, site_id))
  mindepth <- sort(unique(data$depth))[2]
  #maxdepth <- tail(sort(unique(data$depth)), n = 2)[1]
  maxdepth <- max(sort(unique(data$depth)))
  data_wide <- data |>
    filter(depth %in% c(mindepth, maxdepth)) |>
    pivot_wider(names_from = depth,
                values_from = observation) |>
    rename_with(~c('datetime', 'temp_shallow', 'temp_deep')) |>
    drop_na() |>
    mutate(diff_c = temp_shallow - temp_deep,
           diff_dens = water.density(temp_deep) - water.density(temp_shallow),
           strat_temp = ifelse(diff_c < 1, "Not_Strat", "Strat"),
           strat_dens = ifelse(diff_dens > 0.1, "Strat", "Not_Strat"))
  days_strat <- sum(data_wide$strat_dens == "Strat")
  print(paste0(site, ' ', days_strat/2)) # AND NORMALIZE BY n DAYS
  
}



# make in situ as frequent as RS data
names <- list.files("targets/")
for(name in names){
  insitu <- read_csv(paste0("targets/", name, "/", name, "-targets-insitu.csv"))
  rs <- read_csv(paste0("targets/", name, "/", name, "-targets-rs-old.csv"))
  matches <- left_join(rs, insitu, by = c("datetime", "site_id", "variable")) |>
    select(datetime, site_id, variable, depth.y, observation.y) |>
    rename(depth = depth.y, observation = observation.y)
  write_csv(matches, paste0("targets/", name, "/", name, "-targets-insitu-spaced.csv"))
  
}


