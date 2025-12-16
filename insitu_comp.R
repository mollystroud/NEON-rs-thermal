################################################################################
# Code started by Molly Stroud on 11/24/25
################################################################################
# Compare LS thermal to in situ NEON temp data
library(neonstore)
library(neonUtilities)
library(tidyverse)
################################################################################
# Get NEON data
################################################################################
product <- "DP1.20054.001" # surface water temp, "TOSW_30_min-basic"
product <- "DP1.20264.001" # temp at depths
site <- "PRLA" # one site
sites <- c("PRLA", "PRPO", "SUGG", "LIRO", "CRAM", "BARC") # all sites (takes longer)

# download available files
neonstore::neon_download(product = product, site = sites)
# temp at depths
data <- neon_read("TSD_30_min-basic")
data <- data[!is.na(data$tsdWaterTempMean),] # remove NAs

# clean up and get mean surface temp every day
data_cleaned <- data |>
  mutate(datetime = as.Date(startDateTime)) |>
  group_by(datetime, siteID, thermistorDepth) |>
  summarize(mean_temp = mean(tsdWaterTempMean))
# format for targets
data_cleaned$variable <- "temperature"
colnames(data_cleaned) <- c("datetime", "site_id", "depth", "observation", "variable")
write_csv(data_cleaned, "NEON_insitu_columntemp.csv")

################################################################################
# compare RS and in situ data
################################################################################
insitutemps <- read_csv("NEON_insitu_columntemp.csv")
# save out for one place, proper targets format
sugg <- insitutemps[insitutemps$site_id == "SUGG",]
sugg$datetime <- paste0(sugg$datetime, "T00:00:00Z")
# now bin depths as per the configure file
yml_config <- yaml::read_yaml("configuration/analysis/configure_flare_SUGG.yml")
test <- cut(sugg$depth, breaks = yml_config$model_settings$modeled_depths,
                       right = FALSE, dig.lab = 4)
pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"

sugg$depth <- as.numeric(gsub(pattern, "\\2", test))
write_csv(sugg, "targets/SUGG/SUGG-targets-insitu.csv")

# comp
rs_temp <- read_csv("thermal_rs_prairiepothole.csv") # or your site of choice

insitutemps$datetime <- as.Date(insitutemps$datetime) # convert to date
temp_all <- left_join(rs_temp, insitutemps[insitutemps$siteID == "PRPO",], # join together, pick abbreviation
                      by = c("time" = "datetime"))
temp_all <- na.omit(temp_all) # remove NAs
colnames(temp_all)[3] <- "rs_temp"

# plot
tempcomp <- ggplot(temp_all, aes(x = rs_temp, y = mean_temp, color = time)) +
  geom_point() +
  scale_color_viridis(trans = 'date') +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Remotely sensed temperature (C)", y = "In situ temperature (C)",
       color = element_blank())
tempcomp

# get residuals
ggplot(temp_all, aes(x = time, y = rs_temp - mean_temp)) +
  geom_point() +
  theme_classic() +
  geom_abline(slope = 0, intercept = 0)

resids <- temp_all$rs_temp - temp_all$mean_temp
sd(resids)



################################################################################
# Get Sunapee data
################################################################################
# Download here: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.499.4
sunp_insitu <- read_csv("targets/sunp/LSPALMP_1986-2022_v2023-01-22.csv")
sunp_insitu <- sunp_insitu |>
  filter(parameter == "waterTemperature_degC" & station == 210)|>
  mutate(date = as.Date(date)) |>
  select(date, depth_m, parameter, value) |>
  mutate(site_id = 'sunp')
colnames(sunp_insitu) <- c("datetime", "depth", "variable", "observation", "site_id")
sunp_insitu$datetime <- paste0(sunp_insitu$datetime, "T00:00:00Z")

# correct depths
yml_config <- yaml::read_yaml("configuration/analysis/configure_flare_sunp.yml")
depths <- cut(sunp_insitu$depth, breaks = yml_config$model_settings$modeled_depths,
            right = FALSE, dig.lab = 4)
pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"

sunp_insitu$depth <- as.numeric(gsub(pattern, "\\2", depths))
write_csv(sunp_insitu, "targets/sunp/sunp-targets-insitu.csv")



