################################################################################
# Code started by Molly Stroud on 11/24/25
################################################################################
# Compare LS thermal to in situ NEON temp data
library(neonstore)
library(neonUtilities)
################################################################################
# Get NEON data
################################################################################
insitutemps <- neonstore::neon_download(product = "DP1.20264.001", 
                                            start_date = "2013-04-01",
                                        end_date = "2025-12-01",
                                        type = "basic",
                                      site = c("PRLA", "PRPO", "SUGG",
                                               "LIRO", "CRAM", "BARC"))
product <- "DP1.20054.001" # surface water temp
site <- "PRLA" # one site
sites <- c("PRLA", "PRPO", "SUGG", "LIRO", "CRAM", "BARC") # all sites (takes longer)

# download available files
neonstore::neon_download(product = product, site = sites)
#test <- neon_store() # to get variable name
data <- neon_read("TOSW_30_min-basic") # read all 30min together
data <- data[!is.na(data$surfacewaterTempMean),] # remove NAs
# clean up and get mean surface temp every day
data_cleaned <- data |>
  mutate(datetime = as.Date(startDateTime)) |>
  group_by(datetime, siteID) |>
  summarize(mean_temp = mean(surfacewaterTempMean))
#write_csv(data_cleaned, "NEON_insitu_surfacetemp.csv")

################################################################################
# compare RS and in situ data
################################################################################
insitutemps <- read_csv("NEON_insitu_surfacetemp.csv")
rs_temp <- read_csv("thermal_rs_prairiepothole.csv") # or your site of choice

insitutemps$datetime <- as.Date(insitutemps$datetime) # convert to date
temp_all <- left_join(rs_temp, insitutemps[insitutemps$siteID == "PRPO",], # join together, pick abbreviation
                      by = c("time" = "datetime"))
temp_all <- na.omit(temp_all) # remove NAs

# plot
tempcomp <- ggplot(temp_all, aes(x = thermal_C, y = mean_temp, color = time)) +
  geom_point() +
  scale_color_viridis(trans = 'date') +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Remotely sensed temperature (C)", y = "In situ temperature (C)",
       color = element_blank())
tempcomp

# get residuals
ggplot(temp_all, aes(x = time, y = thermal_C - mean_temp)) +
  geom_point() +
  theme_classic() +
  geom_abline(slope = 0, intercept = 0)

resids <- temp_all$mean_thermal_C - temp_all$mean_insitu_temp
sd(resids)
