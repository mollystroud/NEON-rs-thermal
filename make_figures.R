# FIGURE MAKING!!!!!!
pacman::p_load(dplyr, ggplot2, patchwork, tidyverse, lubridate, viridis)
# in situ targets
#insitu <- readr::read_csv("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/bvre/bvre-targets-insitu.csv")
# scores

################################################################################
# GROUPED FIGURE OF ALL AVERAGE CRPS VALUES FOR EACH LAKE
################################################################################
scores_all <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  #filter(site_id == "CRAM") |>
  #filter(depth >= 1) |>
  group_by(model_id, horizon, depth) |>
  summarize(median_crps = median(crps, na.rm = T),
            mean_crps = mean(crps, na.rm = T))

ggplot(data = scores_all, aes(x = horizon, y = mean_crps, colour = model_id)) +
  geom_line() +
  #facet_wrap(~site_id, nrow = 2) +
  facet_wrap(~depth) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  labs(y = 'Mean CRPS (degree C)', x = 'Horizon (days ahead)', 
       title = 'Accuracy across depth (ccre)', color = "") +
  theme_bw() +
  theme(legend.position = 'bottom')


################################################################################
# looking into the negative horizon over all dates
################################################################################
library(tidyverse)
rs <- read_csv('/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets/PRLA/PRLA-targets-rs.csv') |>
  filter(datetime > "2021-01-01" & datetime < "2023-01-01")


# OPEN DATA OF MEAN/OBSERVATION
scores <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  #filter(grepl("PRLA", site_id)) |>
  group_by(datetime, site_id, model_id, depth) |>
  summarise_at(vars(mean, observation, quantile97.5, quantile02.5), mean, na.rm = TRUE) |>
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




################################################################################
# OPEN DATA OF LW FACTOR
################################################################################
lw <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "lw_factor")
  #filter(horizon < 0)

# PLOT
ggplot(data = lw, 
       aes(x = datetime, y = mean, color = model_id)) +
  geom_line() +
  geom_ribbon(aes(ymax= quantile97.5,
                  ymin = quantile02.5,
                  fill = model_id), alpha = 0.5) +
  facet_wrap(~site_id) +
  theme_classic()


################################################################################
# SEASONAL ANALYSIS
################################################################################
scores_seasonal <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  #filter((month(datetime) >= 5 ) & (month(datetime) <= 9)) |> # SUMMER
  #filter((month(datetime) >= 10 ) | (month(datetime) <= 4)) |> # WINTER
  mutate(month = month(datetime), 
         season = case_when(
           month %in% c(12, 1, 2)  ~ "Winter",
           month %in% c(3, 4, 5)   ~ "Spring",
           month %in% c(6, 7, 8)   ~ "Summer",
           month %in% c(9, 10, 11) ~ "Fall")) |>
  filter(horizon > 0) |>
  group_by(site_id, model_id, horizon, season) |>
  summarize(median_crps = median(crps, na.rm = T),
            mean_crps = mean(crps, na.rm = T))

ggplot(data = scores_seasonal[scores_seasonal$model_id == "analysis_run_with_rs",], 
       aes(x = horizon, y = mean_crps, color = site_id)) +
  geom_line() +
  facet_wrap(~season) +
  scale_colour_viridis_d() +
  labs(y = 'CRPS (degree C)', x = 'Horizon (days ahead)', color = "") +
  theme_bw() +
  theme(legend.position = 'bottom')



################################################################################
# accuracy vs. variable
################################################################################
stats_orig <- read_csv("LakeStats.csv")

accuracy <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  filter(model_id == "analysis_run_with_rs") |>
  group_by(site_id, model_id,) |>
  summarize(median_crps = median(crps, na.rm = T))

stats <- left_join(stats_orig, accuracy, by = c("siteID" = "site_id"))

stats_long <- stats |>
  pivot_longer(cols = -c(median_crps, lake_name, siteID, state, mixing_regime, catchment_cover, website, model_id),
    names_to = "variable",
    values_to = "value")


ggplot(stats_long, aes(x = value, y = median_crps)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_x") +
  geom_smooth(aes(color = r), method = "lm", se = FALSE) +
  scale_color_gradient2(
    low = "#f94144", mid = "white", high = "#277da1",
    midpoint = 0,
    name = "Correlation (r)"
  ) +
  theme_bw()


cor_vals <- stats_long %>%
  group_by(variable) %>%
  summarise(
    r = cor(value, median_crps, use = "complete.obs"),
    .groups = "drop"
  )
stats_long <- stats_long %>%
  left_join(cor_vals, by = "variable")




################################################################################
# spearmans correlation 
################################################################################
stats_orig <- read_csv("LakeStats.csv")

accuracy_1day <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon == 1) |>
  filter(model_id == "analysis_run_with_rs") |>
  group_by(site_id) |>
  summarize(crps_1 = median(crps, na.rm = T))

accuracy_14day <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon == 14) |>
  filter(model_id == "analysis_run_with_rs") |>
  group_by(site_id) |>
  summarize(crps_14 = median(crps, na.rm = T))

stats_spearman <- left_join(stats_orig, accuracy_1day, by = c("siteID" = "site_id"))
stats_spearman <- left_join(stats_spearman, accuracy_14day, by = c("siteID" = "site_id"))


stats_spearman_long <- stats_spearman |>
  pivot_longer(cols = -c(crps_1, crps_14, lake_name, siteID, state, mixing_regime, catchment_cover, website),
               names_to = "variable",
               values_to = "value")





spearman_corr_1 <- stats_spearman_long %>%
  group_by(variable) %>%
  summarise(
    r = cor(value, crps_1, method = "spearman"),
    .groups = "drop"
  )

spearman_corr_14 <- stats_spearman_long %>%
  group_by(variable) %>%
  summarise(
    r = cor(value, crps_14, method = "spearman"),
    .groups = "drop"
  )


ggplot() +
  geom_point(data = spearman_corr_1, aes(x = r, y = variable, color = "1 day"), size = 2) +
  geom_point(data = spearman_corr_14, aes(x = r, y = variable, color = "14 days"), size = 2) +
  theme_bw() +
  xlim(-1, 1) +
  geom_vline(xintercept = -0.5, linetype = 'dashed') +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  labs(x = "Spearman's correlation coefficient", y = element_blank(), color = element_blank()) +
  scale_color_manual(values = c("1 day" = '#277da1', "14 days" = '#f94144'))


#############################################################################
# calculate accuracy of RS data in each location
#############################################################################
folders <- list.files("/Users/mollystroud/Desktop/postdoc/flare-rs-thermal/targets", full.names = T)
library(stringr)
alldata <- data.frame()
for(folder in folders){
  name <- str_sub(folder, start = -4)
  insitu <- read_csv(paste0(folder, '/', name, '-targets-insitu.csv')) |>
    filter(depth == 0) |>
    rename(insitu = observation)
  rs <- read_csv(paste0(folder, '/', name, '-targets-rs.csv'))
  data <- left_join(rs, insitu, by = c('datetime', 'site_id'))
  alldata <- rbind(data, alldata)
}
alldata <- na.omit(alldata)

library(viridis)
ggplot(alldata[alldata$datetime > "2020-10-01" & alldata$datetime < "2023-01-01",], aes(x = insitu, y = observation)) +
  geom_point(aes(color = as.Date(datetime)), alpha = 0.8, size = 2) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(se = F, color = 'maroon', method = 'lm') +
  facet_wrap(~site_id) +
  theme_classic() +
  scale_color_viridis_c(trans = 'date') +
  labs(x = 'insitu', y = 'RS', color = element_blank())

#############################################################################
# STDEV of RESIDS
#############################################################################
scores_sd <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  group_by(model_id, horizon, site_id) |>
  summarize(median_crps = median(crps, na.rm = T),
            mean_crps = mean(crps, na.rm = T),
            sd_resid = sd((observation - mean), na.rm = T))



ggplot(data = scores_sd, aes(x = horizon, y = median_crps)) +
  geom_ribbon(aes(ymax = median_crps + sd_resid, ymin = median_crps - sd_resid, fill = model_id), alpha = 0.5) +
  geom_line(aes(x = horizon, y = median_crps, colour = model_id)) +
  facet_wrap(~site_id, nrow = 2) +
  #facet_wrap(~depth) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(y = 'Median CRPS (degree C)', x = 'Horizon (days ahead)', 
       title = 'Accuracy across depth with stdev of resids', color = "", fill = "") +
  theme_bw() +
  theme(legend.position = 'bottom')



#############################################################################
# reliability plot
#############################################################################
scores_all <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  filter(!is.na(observation)) |>
  pivot_longer(cols = starts_with("quantile") | median,
               names_to = "quantile_name", values_to = "q_value") |>
  mutate(quant = case_when(
      quantile_name == "quantile02.5" ~ 0.025,
      quantile_name == "quantile10"   ~ 0.10,
      quantile_name == "median"       ~ 0.50,
      quantile_name == "quantile90"   ~ 0.90,
      quantile_name == "quantile97.5" ~ 0.975),
    below = as.numeric(observation <= q_value)) |>
  group_by(model_id, site_id, variable, quant) |>
  summarise(prop = mean(below, na.rm = TRUE), n = n(), .groups = "drop")


ggplot(scores_all, aes(x = quant, y = prop, colour = model_id)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~site_id) +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "Forecast conf quantile", y = "Proportion of obs in forecast interval",
       title = "Reliability plot (aggregated depths)") +
  theme_bw() +
  theme(legend.position = 'bottom')

# PLOT #2
scores_all <- arrow::open_dataset("scores/parquet/") |>
  dplyr::collect() |>
  filter(datetime > "2021-01-01") |>
  filter(variable == "temperature") |>
  filter(horizon > 0) |>
  filter(!is.na(observation)) |>
      mutate(is_in = between(observation, quantile10, quantile90)) |> 
      group_by(horizon, is_in, model_id, site_id) |> 
      summarise(n = n()) |> 
      pivot_wider(names_from = is_in, names_prefix = 'within_', values_from = n, values_fill = 0) |> 
      mutate(perc = within_TRUE/(within_FALSE + within_TRUE)*100)

  
  ggplot(scores_all[scores_all$model_id == "analysis_run_with_rs",], aes(x=horizon, y=perc, colour = site_id)) +
      geom_hline(yintercept = 90, colour = 'grey3', linetype = 'dashed') +
      geom_line(alpha = 0.8) +
      labs(y = 'Percentage of observations within\n95% confidence intervals', x='Horizon (days)') +
      annotate('text', x = 10, y = 100, label = 'underconfident', size = 5, hjust = 1) +
      annotate('text', x = 10, y = 40, label = 'overconfident', size = 5, hjust = 1) +
    theme_bw() +
    ylim(0, 100)




#############################################################################
# DEPTHS
#############################################################################
library(wesanderson)
scores_depth <- arrow::open_dataset("scores/parquet/") |>
    dplyr::collect() |>
    filter(datetime > "2021-01-01") |>
    filter(variable == "temperature") |>
    filter(horizon == 1) |>
    group_by(model_id, horizon, site_id, depth) |>
    summarize(median_crps = median(crps, na.rm = T),
              mean_crps = mean(crps, na.rm = T))
  
scores_depth <- scores_depth |>
  arrange(site_id, depth) |>
  na.omit()

ggplot(scores_depth[scores_depth$model_id == "analysis_run_with_rs",], 
         aes(x = mean_crps, y = depth, color = site_id, group = site_id)) +
    geom_line(orientation = "y") +
  geom_point() +
    theme_bw() +
    scale_color_manual(values = wes_palette(10, name = "Zissou1", type = 'continuous')) +
    #xlim(0, 2.5) + 
    scale_y_continuous(trans = c("reverse"))







