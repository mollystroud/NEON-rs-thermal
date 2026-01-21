# ################################################################################
# Code started by Molly Stroud on 1/20/26
# Get Kw value from LAGOS
# Download dataset here: https://doi.org/10.6073/pasta/128700feb3bbc3ffe5800e7b232bd81f
# And lake IDs here: https://doi.org/10.6073/pasta/e5c2fb8d77467d3f03de4667ac2173ca
################################################################################

# Can search by NHD code, name, or lat long
# just change filter variable
lakeinfo <- read_csv("LAGOS_lake_information.csv")
mylake <- lakeinfo %>%
  filter(str_detect(lake_namegnis, 'Sunapee'))
# SELECT YOUR LAKE OF INTEREST!

#lagos <- read_csv("LAGOS_US_LANDSAT_matchups.csv")

lagos_qual <- read_csv("LAGOS_US_LANDSAT_Predictions_v1_QAQC.csv")
mylake_secchi <- lagos_qual %>%
  filter(QAQC_recommend == TRUE) %>%
  filter(lagoslakeid == mylake$lagoslakeid) %>%
  select(SENSING_TIME, lagoslakeid, Secchi_predicted)

meansecchi <- mean(mylake_secchi$Secchi_predicted, na.rm = T)
Kw <- 1.7 / meansecchi 
