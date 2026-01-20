# Thermal Remote Sensing FLARE analysis

This code requires an input bounding box and date range of a lake or reservoir of interest.

This repo contains scripts to download the following inputs:
- Landsat 8/9 thermal remote sensing data
- GEFS meteorological data
- Bathymetry from GLOBathy
- Kw derived from LAGOS-US Landsat

After downloading these inputs using the Get_Inputs.R script, users may run the combined_run.R script to run FLARE.
