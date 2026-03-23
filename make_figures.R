# FIGURE MAKING!!!!!!
pacman::p_load(dplyr, ggplot2, patchwork)
# in situ targets
#insitu <- readr::read_csv("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/bvre/bvre-targets-insitu.csv")
# scores
scores <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon < 0)
# just one site
scores <- arrow::open_dataset("scores/parquet/site_id=fcre") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0)

#BARC_obssd_1_perturb_0_005
scores <- arrow::open_dataset("scores/parquet/site_id=BARC") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0)


# using Freya's code
# aggregated forecast skill over horizon
ccre_025 <- scores |>  
  #filter(site_id == "PRPO_obssd_0_25") |>
  #filter(depth < 3.5) |>
  reframe(.by = c(depth, horizon, variable, model_id), 
          crps = mean(crps, na.rm = T),
          rmse = sqrt(mean((observation - mean)^2, na.rm = T))) |> 
  ggplot(aes(x = horizon, y = crps, colour = model_id)) +
  geom_line() +
  facet_wrap(~depth) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  labs(y= 'CRPS (degree C)', x= 'Horizon (days ahead)') +
  theme_bw()# +
  #ylim(0, 3) 
ccre_025



# GROUPED FIGURE OF ALL AVERAGE CRPS VALUES FOR EACH LAKE
scores_all <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  #filter(depth <= 1) |>
  group_by(site_id, model_id, horizon) |>
  summarize(median_crps = median(crps, na.rm = T),
            mean = mean(crps, na.rm = T))

ggplot(data = scores_all[scores_all$model_id == "analysis_run_with_rs",], 
       aes(x = horizon, y = mean, colour = site_id)) +
  geom_line() +
  geom_line(data = scores_all[scores_all$model_id == "analysis_run_no_da",], 
            aes(x = horizon, y = mean, colour = site_id), linetype = 'dashed', alpha = 0.5) +
  facet_wrap(~site_id) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  labs(y= 'CRPS (degree C)', x= 'Horizon (days ahead)', title = 'Average accuracy across depth') +
  theme_bw()



# get scores of ccre with 3 obs_sd values
# looking into the negative horizon over all dates
library(tidyverse)
rs <- read_csv('/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/LIRO/LIRO-targets-rs.csv') |>
  filter(datetime > "2021-01-01" & datetime < "2023-01-01")


# OPEN DATA OF MEAN/OBSERVATION
scores <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(grepl("LIRO", site_id)) |>
  group_by(datetime, site_id, model_id, depth) |>
  summarise_at(vars(mean, observation, quantile97.5, quantile02.5), mean, na.rm = TRUE) #|>
  filter(depth < 2)
# PLOT
ggplot(data = scores[scores$depth<1,], 
       aes(x = datetime, y = mean, color = model_id)) +
  geom_line() +
  geom_ribbon(aes(ymax= quantile97.5,
                  ymin = quantile02.5,
                  fill = model_id), alpha = 0.5) +
  geom_point(data = scores, aes(x = datetime, y = observation), 
             color = 'black', size = 0.5, alpha = 0.5) +
  geom_point(data = rs, aes(x = datetime, y = observation), color = 'black') +
  #facet_wrap(~depth) +
  theme_classic()





# OPEN DATA OF LW FACTOR
#BARC_obssd_1_perturb_0_005
lw <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "lw_factor") |>
  #filter(horizon < 0) |>
  filter(grepl("BARC", site_id))

# PLOT
ggplot(data = lw[lw$site_id == "BARC",], 
       aes(x = datetime, y = mean, color = model_id)) +
  geom_line() +
  geom_ribbon(aes(ymax= quantile97.5,
                  ymin = quantile02.5,
                  fill = model_id), alpha = 0.5) +
  #facet_wrap(~depth) +
  theme_classic()



