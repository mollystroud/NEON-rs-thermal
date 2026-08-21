lake_directory <- here::here()

site_ids <- c("fcre", "bvre", "ccre", "sunp", "BARC", "SUGG", "CRAM", "LIRO", "PRLA", "PRPO", "TOOK")
## Stage met data
s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3", endpoint_override = "amnh1.osn.mghpcc.org", anonymous = TRUE)
arrow::open_dataset(s3) |>
  dplyr::filter(site_id %in% site_ids) |> 
  arrow::write_dataset(path = file.path(lake_directory, "drivers/met/gefs-v12/stage3"), partitioning = c("site_id"))


dates <- seq.Date(lubridate::as_date("2020-12-25"), lubridate::as_date("2022-12-23"), by = 7)

s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2", endpoint_override = "amnh1.osn.mghpcc.org", anonymous = TRUE)
arrow::open_dataset(s3) |>
    dplyr::filter(site_id %in% site_ids,
                  reference_datetime %in% dates) |> 
    arrow::write_dataset(path = file.path(lake_directory, "drivers/met/gefs-v12/stage2"), partitioning = c("reference_datetime", "site_id"))
