# Overview
This repository contains the model inputs and study-specific code used for the
paper "Real-Time Pricing and the Cost of Clean Power" by Imelda, Matthias Fripp
and Michael J. Roberts (2023).

See the main study repository at https://doi.org/10.5281/zenodo.7228323 for an
overview of the study design and repositories and instructions for replicating the
results.

See the data warehouse repository at https://github.com/switch-hawaii/price_response_data
for the data sources and assumptions used in this study.


# Using this Repository

To use this repository, first install Switch (see https://github.com/switch-hawaii/switch/tree/price_response_study)
and clone this repository on your computer. Then open a terminal or command window
and run `conda activate price_response` to setup your switch environment.

If you have rebuilt the data warehouse and would like to regenerate the model
inputs in this repository, run `python get_scenario_data.py`. This extracts data
from the data warehouse for each scenario in the study and use that to create
input files for Switch. This includes downsampling the load and renewable
production profiles and projecting them to 2045, among other steps. The output
of this process (input data for Switch) is in the “inputs” folder in this
repository. The “get_scenario_data.py” script also creates scenarios.txt, which
contains the command-line arguments needed to run each of the 1008 scenarios
used for this study. You can also skip this step and instead use the input data
already stored in the `inputs` directory.

Next, you can run the base scenario by running `switch solve`. Or you can solve
all the scenarios by running `switch solve-scenarios`.

The `switch solve-scenarios` command can be run simultaneously or repeatedly on
multiple computers and will automatically work through the jobs defined in
`scenarios.txt` until they are all completed. The only requirement is that all
the computers have access to the model directory via a shared file system. For
the price response study, we ran all scenarios using the University of Hawai'i
(UH) high-performance computing (HPC) cluster (detail specifications of the
hardware: https://datascience.hawaii.edu/about-mana/). The
`solve_scenarios.slurm` script is useful for running these parallel jobs.

There are several scripts for monitoring job progress:

- `report_completion.py` creates `completion_status.csv`, showing how many
  iterations have been completed for each scenario, based on the output files
  created for each scenario
- `report_error_causes.py` reads through log files to identify the cause of
  crashes and records those in `error_status.csv`.
- `summarize_status_reports.py` aggregates information saved by
  `report_completion.py` and `report_error_causes.py` to summarize the status of
  all the scenarios.
- `list_running_scenarios.py` shows the scenarios that are currently running on
  the HPC system.
- `clear_unfinished_scenarios.py` prints a series of commands you can copy and
  paste to re-queue any scenarios that haven't been finished yet (useful when
  errors have been resolved and you want to restart scenarios that were
  previously marked as run). This works by removing the markers for these
  scenarios from the scenario queue (`sq` directory) in the file system, so they
  will be identified as un-run next time Switch looks for a scenario to run.
- `report_scenarios.py` identifies where and when each scenario was defined
  (useful if you create multiple versions of scenarios.txt to expand the pool
  of scenarios or re-run some)
- `remote_debug.py` can be added to your `modules.txt` list (as `remote_debug`)
  to enable remote debugging of code running on the cluster; when Switch
  encounters an error, this will place a message in the log with instructions on
  how to connect via `telnet` or `nc` to inspect and debug the job.

After the scenarios are completed, outputs will be saved in the `outputs*`
directories. The repository at https://doi.org/10.5281/zenodo.7228323 has the
outputs from running Switch for the price response study, as well as the scripts
used to generate tables and figures for the paper from these.


# Description of Inputs Used by Switch

All input files for Switch are included in this repository and are also
available at https://doi.org/10.5281/zenodo.7228323. The table below describes
each file used in the model.

<table>
  <tr>
   <td><strong>Input files</strong>
   </td>
   <td><strong>Description</strong>
   </td>
  </tr>
  <tr>
   <td>ev_bau_load.csv
   </td>
   <td>Electric vehicle hourly charging demand (MW) if a business-as-usual (non-optimized) charging schedule is used, assuming 50% penetration of electric vehicles
   </td>
  </tr>
  <tr>
   <td>ev_bau_load.2016_ev.csv
   </td>
   <td>Electric vehicle hourly charging demand if a business-as-usual charging schedule is used, assuming 2016 penetration rate (0.5%)
   </td>
  </tr>
  <tr>
   <td>ev_bau_load.full_ev.csv
   </td>
   <td>Electric vehicle hourly charging demand if a business-as-usual charging schedule is used, assuming 100% penetration rate 
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info.2016_ev.csv
   </td>
   <td>Electric vehicle share for 2016 assuming current penetration rate (0.5%)
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info.csv
   </td>
   <td>Electric vehicle share assuming 50% penetration rate
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info.full_ev.csv
   </td>
   <td>Electric vehicle share assuming 100% penetration rate 
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info_advanced.2016_ev.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info_advanced.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>ev_fleet_info_advanced.full_ev.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>ev_share.2016_ev.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>ev_share.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>ev_share.full_ev.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>financials.csv
   </td>
   <td>Interest rate and discount rate used for the study. The optimization minimizes costs on an NPV basis, using 0 discount rate, subject to meeting the targets described in the Renewable Portfolio Standard defined in rps_targets files. Capital costs are assumed to be financed with an annual payment over the life of the asset that is constant in real dollars, i.e., escalating with inflation. The cost of capital for this amortization is assumed to be 6% real.
   </td>
  </tr>
  <tr>
   <td>calc-flexshares.xlsx
   </td>
   <td>Calculate hourly flexible shares. Assumptions come from Navigant Consulting, Inc., 2015 and details on the assumptions and computations are described in Section 3.2 in the paper. The output of this calculation is flexshares.csv
   </td>
  </tr>
  <tr>
   <td>flexshares.csv
   </td>
   <td>Hourly flexible and non flexible shares from calc-flexshares.xlsx
   </td>
  </tr>
  <tr>
   <td>fuel_supply_curves.csv
   </td>
   <td>Unit cost of fuels using forecasted cost
   </td>
  </tr>
  <tr>
   <td>fuel_supply_curves.current_cost.csv
   </td>
   <td>Unit cost of fuels if costs stay constant at current levels
   </td>
  </tr>
  <tr>
   <td>fuels.csv
   </td>
   <td>List of fuels available in the study plus data on their CO2 intensity and RPS eligibility
   </td>
  </tr>
  <tr>
   <td>fuels.current_cost.csv
   </td>
   <td>Version of fuels.csv used for current-fuel-cost scenarios (same as regular fuels.csv)
   </td>
  </tr>
  <tr>
   <td>gen_build_costs.csv
   </td>
   <td>Capital and fixed O&M costs for each power generation project, using forecasted cost
   </td>
  </tr>
  <tr>
   <td>gen_build_costs.current_cost.csv
   </td>
   <td>Version of gen_build_costs.csv used for scenarios with costs constant at current levels
   </td>
  </tr>
  <tr>
   <td>gen_build_costs_10x.csv
   </td>
   <td>Version of gen_build_costs.csv used for scenarios with very high costs
   </td>
  </tr>
  <tr>
   <td>gen_build_predetermined.csv
   </td>
   <td>Dates and quantities when existing generation projects were placed in service
   </td>
  </tr>
  <tr>
   <td>gen_build_predetermined.current_cost.csv
   </td>
   <td>Same as gen_build_predetermined.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>gen_inc_heat_rates.csv
   </td>
   <td>Heat rate (fuel consumption) curves for fossil fuel plants
   </td>
  </tr>
  <tr>
   <td>gen_inc_heat_rates.current_cost.csv
   </td>
   <td>Same as gen_inc_heat_rates.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>gen_multiple_fuels.csv
   </td>
   <td>Allowed fuels for fossil fuel plants that use multiple fuels
   </td>
  </tr>
  <tr>
   <td>gen_multiple_fuels.current_cost.csv
   </td>
   <td>Same as gen_multiple_fuels.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>gen_timepoint_commit_bounds.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>gen_timepoint_commit_bounds.current_cost.csv
   </td>
   <td>(not used in this study)
   </td>
  </tr>
  <tr>
   <td>generation_projects_info.csv
   </td>
   <td>General parameters for each generation project (technology category, interconnection cost, limits on available resources if any, incremental size, outage rates, type of fuel used, variable operation & maintenance cost, minimum up- and down-time if any, storage efficiency and cycling limits)
   </td>
  </tr>
  <tr>
   <td>generation_projects_info.current_cost.csv
   </td>
   <td>Same as generation_projects_info.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>hydrogen.csv
   </td>
   <td>Cost assumptions for hydrogen project components, using projected 2045 costs
   </td>
  </tr>
  <tr>
   <td>hydrogen.current_cost.csv
   </td>
   <td>Cost assumptions for hydrogen project components, using 2016 costs
   </td>
  </tr>
  <tr>
   <td>load_zones.csv
   </td>
   <td>List of interconnected load zones with adequate internal transmission and no connection to neighbor islands; for this study, there is a single zone, Oahu
   </td>
  </tr>
  <tr>
   <td>loads.2007_load.csv
   </td>
   <td>Hourly load profile, using 2007 load
   </td>
  </tr>
  <tr>
   <td>loads.csv
   </td>
   <td>Hourly load profile, using projected load for 2045.
   </td>
  </tr>
  <tr>
   <td>non_fuel_energy_sources.csv
   </td>
   <td>List of non-fuel energy sources such as wind or solar power. These have no cost or emissions and are eligible for the RPS
   </td>
  </tr>
  <tr>
   <td>non_fuel_energy_sources.current_cost.csv
   </td>
   <td>Same as non_fuel_energy_sources.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>periods.csv
   </td>
   <td>List periods when new resources can be constructed; timeseries and timepoints (see below) fall within these; for this study, there was a single period, 2045
   </td>
  </tr>
  <tr>
   <td>pumped_hydro.csv
   </td>
   <td>(not used for this study)
   </td>
  </tr>
  <tr>
   <td>regional_fuel_markets.csv
   </td>
   <td>List of “regional fuel markets” for which fuel supply curves are provided; a regional fuel market is defined for each collection of load zones for which each fuel is available; for this study, there is one “Hawaii” market for each fuel.
   </td>
  </tr>
  <tr>
   <td>regional_fuel_markets.current_cost.csv
   </td>
   <td>Same as regional_fuel_markets.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>rps_targets.csv
   </td>
   <td>Annual RPS targets; for this study, this file shows 100% by 2045
   </td>
  </tr>
  <tr>
   <td>rps_targets.rps_000.csv – rps_targets.rps_098.csv
   </td>
   <td>Alternative versions of rps_targets.csv used for scenarios with 2045 RPS target ranging from 0% (no RPS) to 98%; 2045 target percentage is indicated in the file name
   </td>
  </tr>
  <tr>
   <td>switch_inputs_version.txt
   </td>
   <td>Identifies version of Switch that these input files are compatible with (2.0.6)
   </td>
  </tr>
  <tr>
   <td>timepoints.csv
   </td>
   <td>Identifiers for each timepoint (time step) in the study; for this work there were 24 in each timeseries
   </td>
  </tr>
  <tr>
   <td>timeseries.csv
   </td>
   <td>Identifiers and weighting factors for each timeseries in the study; for this study, timeseries were defined for 13 sample days in 2045, selected to span the range of wind, solar and load conditions observed in 2007–08. Weights were assigned to each timeseries based on how many historical days were most similar to that sample
   </td>
  </tr>
  <tr>
   <td>variable_capacity_factors.csv
   </td>
   <td>Production capability for each renewable energy project during each timepoint, based on weather that occurred on the corresponding historical day
   </td>
  </tr>
  <tr>
   <td>variable_capacity_factors.current_cost.csv
   </td>
   <td>Same as variable_capacity_factors.csv; used for current-cost scenarios
   </td>
  </tr>
  <tr>
   <td>zone_to_regional_fuel_market.csv
   </td>
   <td>List of load zones in each regional fuel market; for this study, each market contained only Oahu
   </td>
  </tr>
  <tr>
   <td>zone_to_regional_fuel_market.current_cost.csv
   </td>
   <td>Same as zone_to_regional_fuel_market.csv; used for current-cost scenarios
   </td>
  </tr>
</table>
