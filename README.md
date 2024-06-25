Orderly task descriptions and the order you should run them in. Tasks 2 - 7 are district specific, only looking at rural areas => I exclude Kampala:
1. **demography** - SSA demography as a placeholder until the demography is fixed in foresite.
2. **seasonality_parameters** - fit the Fourier coefficients to CHIRPS rainfall data. Run merge_rasters locally before running the orderly task in order to have a copy of the necessary resource (or adjust accordingly to run on the cluster)
3. **site_file** - generate the new site file using an emsemble of information from the old site file and new information overlaid over the shape file. At this point, the site file does not have the calibrate EIR from MAP prevelance estimates -- that is the next step.
4. **calibrate_eir** - calibrates the EIR for the site to the MAP prevalence estimate using the cali package. Outputs a calibrated site file which is used when **calibrated == TRUE** in the orderly parameters
5. **run_counterfactual** - runs the model without SMC for that specific site. If calibrated == TRUE it uses output from calibrate_eir; else it is the old site file. runs with multiple repetitions with a default of 20, editable by the orderly parameter **repetitions**
6. **run_smc** - as above but in this case, with SMC. I use the parameters from our clinical trial fitting of SMC in Uganda in the Phase II trial as the drug protection profile. I run the model for a year without SMC to determine the optimal timing of delivery, and then run the model for 3 years, year 1 - no SMC; years 2 & 3 - 5 cycles of optimally timed SMC with SP+AQ
7. **analyse_impact** - combines the model runs with and without SMC to determine the impact of SMC implementation in that district using proportional and absolute metrics
8. **country impact** - assess the country level impact of the intervention 
9. **map_impact** - maps the impact of the intervention across all districts. This is the first point where we have to cycle over all districts in the country when reading in the dependencies which improves computational time vs doing it earlier
10. **figures** - generates generic figures for exploration. Specific R scripts are created for manuscript figures to exactly reproduce this code.


## for integration with the cluster
Follow the instructions for installation of hipercow on the website vignette: https://mrc-ide.github.io/hipercow/articles/hipercow.html
Set up the hipercow infrastructure on your personal drive - these tasks will later be pushed to a folder in the malaria drive so we can collaborate easily.
Clone the github repository into your personal drive. Move the hipercow folder into this directory. hipercow is in the .gitignore so that none of these outputs will be added to git or anywhere else.
Reinitialise orderly2 to use your personal drive now instead.

## install packages on the cluster for use
hipercow_provision(method = "pkgdepends") # ensure you have installed "pkgdepends" locally 

Now you can use the specific launch scripts for integration with hipercow.
NOTE: edits to the code base need to be made on a branch and merged into main before running on the cluster