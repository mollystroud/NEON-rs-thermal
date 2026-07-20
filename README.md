# FLARE-RS analysis

The code used in this paper may be found in the R folder and workflows folder. To run
the FLARE forecasts, you must first download drivers (workflows/analysis/download_drivers.R)
and then the Landsat imagery over the lake (R/get_LST.R, called from
workflows/analysis/combined_run.R). Then you may run FLARE to get forecasts
(workflows/analysis/combined_run.R).

For those just interested in the visualizations and analysis plots, the forecast scores
are included in this repo and the visualizations may be created using workflows/Make_Figures.qmd.

## Dependencies

You will need a compiled **GLM** (General Lake Model) executable. A macOS binary is
bundled at `binary/macos-tahoe26/glm`; `workflows/analysis/combined_run.R` currently points
`GLM_PATH` at a machine-specific location and must be edited to point at your own GLM
build, e.g.:

```r
Sys.setenv('GLM_PATH' = file.path(here::here(), "binary/macos-tahoe26/glm"))
```

Downloading remote-sensing imagery (`R/get_LST.R`) requires network access to the
Microsoft Planetary Computer STAC API, and driver staging (`download_drivers.R`) requires
network access to the FLARE OSN S3 bucket.

## Repository structure

Note: `flare_tempdir/`, `forecasts/`, and `restart/` are generated run artifacts
(model working directories, raw forecast output, and restart files) and are **not**
included in the repository — they are excluded below and via `.gitignore`.

```
flare-rs-thermal/
├── R/                            # Core helper functions, sourced by the workflows
│   ├── bboxes.R                  # Lake and reservoir site bounding boxes + sample points
│   ├── get_LST.R                 # Landsat thermal (LST) download + processing functions
│   ├── interpolate_targets.R     # Linear/other interpolation of target time series
│   └── score_forecasts.R         # CRPS/log scoring of forecasts
│
├── workflows/
│   ├── Make_Figures.qmd          # Generates all paper figures (reads scores/, targets/)
│   └── analysis/
│       ├── download_drivers.R    # Stages met drivers from the FLARE OSN S3 bucket
│       └── combined_run.R        # Main driver: runs FLARE forecasts for a site/experiment
│
├── configuration/
│   └── analysis/
│       ├── configure_flare_<site>.yml   # Per-site FLARE configuration
│       ├── configure_run_<site>.yml     # Per-site run/date configuration
│       ├── glm3_<site>.nml               # Per-site GLM namelist
│       ├── glm3_base.nml                 # Shared/base GLM namelist
│       ├── observations_config.csv       # Observation error/uncertainty settings
│       ├── parameter_calibration_config.csv
│       └── states_config.csv             # Modeled state variable configuration
│
├── binary/
│   └── macos-tahoe26/glm         # Bundled macOS GLM executable (see Dependencies)
│
├── drivers/                      # Staged met/inflow/outflow driver data (per site)
│   ├── met/gefs-v12/{stage2,stage3}
│   ├── inflow/{historical,future}
│   └── outflow/{historical,future}
│
├── targets/<site>/                # Observation ("target") files per site
│   ├── <site>-targets-insitu.csv         # Full-resolution in situ observations
│   ├── <site>-targets-insitu-spaced.csv  # In situ obs subsampled to RS revisit frequency
│   ├── <site>-targets-insitu-surface.csv # Surface-only in situ observations
│   └── <site>-targets-rs.csv             # Remote-sensing (Landsat LST) observations
│
├── scores/parquet/site_id=<site>/         # Forecast scoring output (CRPS, log score, etc.)
├── plots/<site>/                          # Per-forecast diagnostic PDF plots
├── paper_figs/                            # Final rendered figures used in the paper
│   └── rs-images/                         # Supporting bathymetry/imagery files
│
├── LakeStats.csv                  # Per-lake morphometry/climate summary stats (Figure 8)
├── insitu_comp.R                  # Exploratory: compares NEON in situ vs. RS temperatures
├── extra_scripts.R                # Exploratory: bathymetry processing, dependency listing
│
├── flare_tempdir/                  # [excluded] FLARE model working directories
├── forecasts/                      # [excluded] Raw forecast output (parquet)
├── restart/                        # [excluded] Model restart files per site/run
│
├── renv/                          # Partial renv setup (no committed lockfile)
├── flare-rs-thermal.Rproj
└── .gitignore
```
