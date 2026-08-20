require(pacman)
pacman::p_load(tidyverse, lubridate)
#remotes::install_github("FLARE-forecast/FLAREr")
#remotes::install_github("rqthomas/GLM3r")
set.seed(200)

site <- "fcre" # or, set to your chosen site

# This need to be set to run each experiment
run_name <- "run"
config_flare_file <- paste0("configure_flare_", site, ".yml")
starting_index <- 1
redownload_remote_sensing <- FALSE 
#remote sensing is already downloaded.  Setting to redownload_remote_sensing <- FALSE 
# will require a slow download of the imagery.  

# Pick which DA experiment(s) will be run. More detailed descriptions of each
# scenario may be seen in Table 1 of the manuscript.

# This runs both the 'no data assimilation' forecast experiment (no_da) and the
# experiment with remote sensing data assimilation. To run remote sensing data 
# assimilation with in situ uncertainty, change obs_sd in 
# configuration/observations_config.csv.
experiments <- c("no_da", "with_rs")
# This runs the experiment with in situ (all depths)
#experiments <- c("with_insitu")

# This runs the experiment with in situ data assimilation using only surface 
# data
#experiments <- c("with_insitu_surface")

# This runs the experiment with in situ temporally spaced to remote sensing
# frequency
# experiments <- c("with_insitu_spaced")

# These don't need to be changed
config_set_name <- "analysis"
configure_run_file <- paste0("configure_run_", site,".yml")
use_s3 <- FALSE

lake_directory <- here::here()

# set this to your own GLM path
#Need to run 
#remotes::install_github("flare-forecast/GLMAEDr")
#GLMAEDr::glm_install()
Sys.setenv('GLM_PATH'= GLMAEDr::glm_path())

options(future.globals.maxSize = 891289600)

walk(list.files(file.path(lake_directory, "R"), full.names = TRUE), source)

### Set up simulation start and end dates
num_forecasts <- 104
days_between_forecasts <- 7
forecast_horizon <- 14
starting_date <- as_date("2020-11-08") 
second_date <- as_date("2021-01-01") - days(days_between_forecasts)

all_dates <- seq.Date(starting_date,second_date + days(days_between_forecasts * num_forecasts), by = 1)


# update this with your current DA experiment

potential_date_list <- list(no_da = all_dates, with_rs = all_dates)
#potential_date_list <- list(with_insitu = all_dates)

date_list <- potential_date_list[which(names(potential_date_list) %in% experiments)]

# Download RS data if assimilating RS data
# If the forecast period is long (>a few months), this will take a very long
# time to run
if("with_rs" %in% experiments & redownload_remote_sensing){
  source("R/get_LST.R")
  source("R/bboxes.R")
  rsdata <- get_LST(bbox = get(paste0(site, "_bbox")), 
                    start_date = min(date_list[[1]]), 
                    end_date = max(date_list[[1]]))
  vals <- get_vals(get(paste0(site, "_points")), rsdata)
  vals <- clean_data(vals)
  write_csv(vals, paste0("targets/", site, "/", site, "-targets-rs.csv"))
}

# set up
models <- names(date_list)

start_dates <- as_date(rep(NA, num_forecasts + 1))
end_dates <- as_date(rep(NA, num_forecasts + 1))
start_dates[1] <- starting_date
end_dates[1] <- second_date
for(i in 2:(num_forecasts+1)){
  start_dates[i] <- as_date(end_dates[i-1])
  end_dates[i] <- start_dates[i] + days(days_between_forecasts)
}

sims <- expand.grid(paste0(start_dates,"_",end_dates,"_", forecast_horizon), models)

names(sims) <- c("date","model")

sims$start_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,1]
sims$end_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,2]
sims$horizon <- stringr::str_split_fixed(sims$date, "_", 3)[,3]

sims <- sims |>
  mutate(model = as.character(model)) |>
  dplyr::select(-date) |>
  distinct_all() |>
  arrange(start_dates)

sims$horizon[1:length(models)] <- 0



# now run setup and forecasts
for(i in starting_index:nrow(sims)){
  message(paste0("index: ", i))
  message(paste0("     Running model: ", sims$model[i], " "))

  model <- sims$model[i]
  sim_names <- paste0(config_set_name, "_", run_name , "_" ,model)

  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = TRUE)

  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  yml$sim_name <- sim_names
  yml$configure_flare <- config_flare_file


  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, configure_run_file))

  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, config_flare_file))

  if(model == "no_da"){
    yml$da_setup$use_obs_constraint <- FALSE
  }else{
    yml$da_setup$use_obs_constraint <- TRUE
  }

  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, config_flare_file))

  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$configure_flare <- config_flare_file
  run_config$sim_name <- sim_names
  run_config$start_datetime <- as.character(paste0(sims$start_dates[i], " 00:00:00"))
  run_config$forecast_start_datetime <- as.character(paste0(sims$end_dates[i], " 00:00:00"))
  run_config$forecast_horizon <- as.numeric(sims$horizon[i])
  run_config$configure_flare <- config_flare_file
  if(i <= length(models)){
    config$run_config$restart_file <- NA
  }else{
    run_config$restart_file <- paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")
    if(!file.exists(file.path(config$file_path$restart_directory, paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")) )){
      warning(paste0("restart file: ", run_config$restart_file, " doesn't exist"))
    }
  }



  yaml::write_yaml(run_config, file = file.path(lake_directory, "restart", site, sim_names, configure_run_file))

  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = FALSE)

  config <- FLAREr:::get_restart_file(config, lake_directory)

  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  # Inflows
  #source(file.path(lake_directory, "workflows", config_set_name, "make_flow_drivers.R"))
  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

  met_out <- FLAREr:::create_met_files(config, lake_directory = lake_directory, met_forecast_start_datetime, met_start_datetime)


  inflow_outflow_files <- FLAREr:::create_inflow_outflow_files(config, config_set_name, lake_directory)

  # Create observation matrix
  # NOTE: obs are assimilated from the "-targets-insitu.csv" file here.
  # To rerun the "with_rs" or other experiments, update this file path accordingly.
  obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                    obs_config = obs_config,
                                    config)
  # obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
  #                                   obs_config = obs_config,
  #                                   config)
  # and update this file path too
  obs_non_vertical <- FLAREr:::create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                       obs_config,
                                                       start_datetime = config$run_config$start_datetime,
                                                       end_datetime = config$run_config$end_datetime,
                                                       forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                       forecast_horizon =  config$run_config$forecast_horizon)
  states_non_vertical <- NULL
  states_non_vertical$depth_sd <- 0

  states_config <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- FLAREr:::initiate_model_error(config, states_config)

  init <- FLAREr:::generate_initial_conditions(states_config,
                                               obs_config,
                                               pars_config,
                                               obs,
                                               config,
                                               obs_non_vertical = obs_non_vertical)
  da_forecast_output <- FLAREr:::run_da_forecast(states_init = init$states,
                                                 pars_init = init$pars,
                                                 aux_states_init = init$aux_states_init,
                                                 obs = obs,
                                                 obs_sd = obs_config$obs_sd,
                                                 model_sd = model_sd,
                                                 working_directory = config$file_path$execute_directory,
                                                 met_file_names = met_out$filenames,
                                                 inflow_file_names = inflow_outflow_files$inflow_file_names,
                                                 outflow_file_names = inflow_outflow_files$outflow_file_names,
                                                 config = config,
                                                 pars_config = pars_config,
                                                 states_config = states_config,
                                                 obs_config = obs_config,
                                                 da_method = config$da_setup$da_method,
                                                 par_fit_method = config$da_setup$par_fit_method,
                                                 obs_non_vertical = obs_non_vertical)

  # Save forecast

  saved_file <- FLAREr:::write_restart(da_forecast_output = da_forecast_output,
                                       forecast_output_directory = config$file_path$restart_directory,
                                       use_short_filename = TRUE)

  forecast_df <- FLAREr:::write_forecast(da_forecast_output = da_forecast_output,
                                         use_s3 = use_s3,
                                         bucket = config$s3$forecasts_parquet$bucket,
                                         endpoint = config$s3$forecasts_parquet$endpoint,
                                         local_directory = file.path(lake_directory, "forecasts/parquet"))

  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)
  #targets_df <- targets_df[targets_df$depth != 0,]
  #targets_0depth <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-rs.csv")),show_col_types = FALSE)
  #targets_df <- data.frame(rbind(targets_df, targets_0depth))


  targets_df <- obs_config |>
    rename(variable = target_variable) |>
    dplyr::select(variable, obs_sd) |>
    right_join(targets_df, by = "variable") |>
    mutate(up95 = observation + 1.96 * obs_sd,
           low95 = observation - 1.96 * obs_sd,
           low95 = ifelse(variable != "temperature" & low95 < 0, 0, low95))

  
  #THESE PLOTS WILL ONLY HAVE THE INSITU DATA RATHER THAN THE REMOTE SENSING DATA BECAUSE TARGET_DF IS THE INSITU DATA
  FLAREr:::plotting_general(forecast_df, targets_df, file_name = paste0(tools::file_path_sans_ext(basename(saved_file)),".pdf") , plots_directory = config$file_path$plots_directory)
  
  #THESE SCORE WILL ONLY HAVE THE INSITU DATA RATHER THAN THE REMOTE SENSING DATA BECAUSE TARGET_DF IS THE INSITU DATA
  generate_forecast_score_arrow(targets_df = targets_df,
                                forecast_df = forecast_df,
                                use_s3 = FALSE,
                                bucket = NULL,
                                endpoint = NULL,
                                local_directory = file.path(lake_directory, "scores/parquet"),
                                variable_types = c("state","parameter","diagnostic"))

}

