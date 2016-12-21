#!/usr/bin/env python

import sys, os, argparse
from textwrap import dedent

import switch_mod.hawaii.scenario_data as scenario_data

###########################
# Scenario Definitions

# definitions of standard scenarios (may also specify inputs_subdir to read in alternative data)
# TODO: find a way to define the base scenario here, then apply the others as changes to it
# Maybe allow each to start with --inherit-scenario <parent>? (to one level) 
# (--scenario does this already)

# note: we've abandoned total-cost pricing because it's theoretically messy.
# If a fixed lump has to be spread across all days of the year, then the
# retail price (and demand bid) on each day will change depending on quantities
# sold on other days of that year. Adding a fixed amount to the marginal cost
# or multiplying it by a fixed scalar would be much more tractable, but their
# effect would have to be backed out before reporting WTP to the supply side
# (otherwise it will think that it's worth doing a change that costs $1 and 
# results in $1 of welfare improvement, but due to the adder or multiplier,
# the apparent demand curve will be rotated or scaled, and we won't get the full
# $1 of welfare improvement.) Fixed adjustments may be theoretically interesting
# (to investigate the effect of "taxes" to recover stranded costs), but they 
# don't really address a question we're interested in right now, certainly not
# in a simple way.
marginal_pricing_options = ['marginal']  # + ['total']
tech_clusters = [["2045_fossil", "2045_rps"]]  # +[["2007", "2045_rps_ev"]]
elasticity_scenarios = [4]  # + [3, 2, 1]
dynamic_pricing_options = ["flat", "dynamic"]

scenario_list = []
for marginal in marginal_pricing_options:
    for tech_cluster in tech_clusters:
        for elasticity_scen in elasticity_scenarios:
            for tech in tech_cluster:
                for flat in dynamic_pricing_options:
                    # print flat, marginal, elasticity_scen, tech
                    s = ""
                    s += " --scenario-name " +  "_".join([tech, flat, "scen"+str(elasticity_scen)])
                    s += " --dr-elasticity-scenario " + str(elasticity_scen)
                    if flat == "flat":
                        s += " --dr-flat-pricing"
                    if marginal == "total":
                        s += " --dr-total-cost-pricing"
                    if tech == "2007":
                        s += " --inputs-dir inputs_2007_15"
                        s += " --exclude-modules switch_mod.hawaii.rps"
                    elif tech == "2045_fossil":
                        s += " --rps-deactivate"
                    elif tech == "2045_rps":
                        s += ""
                    elif tech == "2045_rps_ev":
                        s += " --include-module switch_mod.hawaii.ev"
                        if flat == "flat":
                            s += " --ev-timing bau"
                        else:
                            s += " --ev-timing optimal"
                    else:
                        print "WARNING: unrecognized technology option {}.".format(tech)
                    scenario_list.append(s)

with open('scenarios.txt', 'w') as f:
    f.writelines(s + '\n' for s in scenario_list)

scenario_list = []
for flat in dynamic_pricing_options:
    for marginal in marginal_pricing_options:
        for elasticity_scen in elasticity_scenarios:
            for tech in ["tiny"]:
                # print flat, marginal, elasticity_scen, tech
                s = ""
                s += " --scenario-name " + "_".join([tech, flat, "scen"+str(elasticity_scen)])
                s += " --dr-elasticity-scenario " + str(elasticity_scen)
                if flat == "flat":
                    s += " --dr-flat-pricing"
                if marginal == "total":
                    s += " --dr-total-cost-pricing"
                if tech == "2007":
                    s += " --inputs-dir inputs_2007_15"
                    s += " --exclude-modules rps"
                elif tech == "2045_fossil":
                    s += " --inputs-dir inputs_2045_15 --include-module no_renewables --exclude-module rps"
                elif tech == "2045_rps":
                    s += " --inputs-dir inputs_2045_15"
                elif tech == "2045_rps_ev":
                    s += " --inputs-dir inputs_2045_15 --include-module ev"
                    if flat == "flat":
                        s += " --ev-flat"
                elif tech == "tiny":
                    s += " --inputs-dir inputs_tiny"
                else:
                    print "WARNING: unrecognized technology option {}.".format(tech)
                scenario_list.append(s)

with open('scenarios_tiny.txt', 'w') as f:
    f.writelines(s + '\n' for s in scenario_list)

parser = argparse.ArgumentParser()
parser.add_argument('--skip-cf', action='store_true', default=False,
    help='Skip writing variable capacity factors file (for faster execution)')
parser.add_argument('--time-sample', default="2045_15")
parser.add_argument('--inputs-dir', default='inputs_2045_15')

cmd_line_args = parser.parse_args()

# particular settings chosen for this case
# (these will be passed as arguments when the queries are run)
args = dict(
    # directory to store data in
    inputs_dir = cmd_line_args.inputs_dir,
    # skip writing capacity factors file if specified (for speed)
    skip_cf = cmd_line_args.skip_cf,    
    # use heat rate curves for all thermal plants
    use_incremental_heat_rates=True,
    # could be 'tiny', 'rps', 'rps_mini' or possibly '2007', '2016test', 'rps_test_45', or 'main'
    # '2020_2025' is two 5-year periods, with 24 days per period, starting in 2020 and 2025
    time_sample = cmd_line_args.time_sample,
    # subset of load zones to model
    load_zones = ('Oahu',),       
    # "hist"=pseudo-historical, "med"="Moved by Passion", "flat"=2015 levels, "PSIP_2016_04"=PSIP 4/16
    load_scen_id = "PSIP_2016_04", 
    # '1'=low, '2'=high, '3'=reference, 'EIA_ref'=EIA-derived reference level, 'hedged'=2020-2030 prices from Hawaii Gas
    fuel_scen_id='PSIP_2016_09',
    # Blazing a Bold Frontier, Stuck in the Middle, No Burning Desire, Full Adoption, 
    # Business as Usual, (omitted or None=none)
    ev_scenario = 'Blazing a Bold Frontier',   
    # should the must_run flag be converted to set minimum commitment for existing plants?
    enable_must_run = 0,     
    # list of technologies to exclude (currently CentralFixedPV, because we don't have the logic
    # in place yet to choose between CentralFixedPV and CentralTrackingPV at each site)
    exclude_technologies = ('CentralFixedPV',),     
    base_financial_year = 2016,
    interest_rate = 0.06,
    discount_rate = 0.03,
    # used to convert nominal costs in the tables to real costs
    inflation_rate = 0.025,  
)

# battery data from 2016-04-01 PSIP report (pp. J-82 - J-83)
# this was used for main model runs from 2016-05-01 onward
# TODO: store this in the back-end database
psip_nominal_battery_cost_per_kwh = [
    530, 493, 454, 421, 
    391, 371, 353, 339, 326, 316, 306, 298, 291, 285, 
    280, 275, 271, 268, 264, 262, 259, 257, 255, 253, 
    252, 250, 249, 248, 247, 246
]
psip_battery_years = range(2016, 2045+1)
psip_battery_cost_per_mwh = [
    1000.0 * nom_cost * 1.018**(args["base_financial_year"] - year)
        for year, nom_cost in zip(psip_battery_years, psip_nominal_battery_cost_per_kwh)
]
# TODO: retire and replace with cheaper model after 15 years
args.update(
    BATTERY_CAPITAL_COST_YEARS = psip_battery_years,
    battery_capital_cost_per_mwh_capacity_by_year = psip_battery_cost_per_mwh,
    battery_n_years=15,
    # battery_n_cycles=365*15,
    battery_max_discharge=1.0,
    battery_min_discharge_time=6,
    battery_efficiency=0.88,
)

# electrolyzer data from centralized current electrolyzer scenario version 3.1 in 
# http://www.hydrogen.energy.gov/h2a_prod_studies.html -> 01D_Current_Central_Hydrogen_Production_from_PEM_Electrolysis_version_3.1.xlsm
# (cited by 46719.pdf)
# note: we neglect land costs because they are small and can be recovered later
# TODO: move electrolyzer refurbishment costs from fixed to variable

# liquifier and tank data from http://www.nrel.gov/docs/fy99osti/25106.pdf

# fuel cell data from http://www.nrel.gov/docs/fy10osti/46719.pdf

inflate_1995 = (1.0+args["inflation_rate"])**(args["base_financial_year"]-1995)
inflate_2007 = (1.0+args["inflation_rate"])**(args["base_financial_year"]-2007)
inflate_2008 = (1.0+args["inflation_rate"])**(args["base_financial_year"]-2008)
h2_lhv_mj_per_kg = 120.21   # from http://hydrogen.pnl.gov/tools/lower-and-higher-heating-values-fuels
h2_mwh_per_kg = h2_lhv_mj_per_kg / 3600     # (3600 MJ/MWh)
electrolyzer_kg_per_mwh = 1000.0/54.3    # (1000 kWh/1 MWh)(1kg/54.3 kWh)   # TMP_Usage cell
electrolyzer_mw = 50000.0 * (1.0/electrolyzer_kg_per_mwh) * (1.0/24.0)   # (kg/day) * (MWh/kg) * (day/h)    # design_cap cell

args.update(
    hydrogen_electrolyzer_capital_cost_per_mw=inflate_2007*144641663.0/electrolyzer_mw,        # depr_cap cell
    hydrogen_electrolyzer_fixed_cost_per_mw_year=inflate_2007*7134560.0/electrolyzer_mw,         # fixed cell
    hydrogen_electrolyzer_variable_cost_per_kg=0.0,       # they only count electricity as variable cost
    hydrogen_electrolyzer_kg_per_mwh=electrolyzer_kg_per_mwh,
    hydrogen_electrolyzer_life_years=40,                      # plant_life cell

    hydrogen_liquifier_capital_cost_per_kg_per_hour=inflate_1995*25600,       # 25106.pdf p. 18, for 1500 kg/h plant, approx. 100 MW
    hydrogen_liquifier_fixed_cost_per_kg_hour_year=0.0,   # unknown, assumed low
    hydrogen_liquifier_variable_cost_per_kg=0.0,      # 25106.pdf p. 23 counts tank, equipment and electricity, but those are covered elsewhere
    hydrogen_liquifier_mwh_per_kg=10.0/1000.0,        # middle of 8-12 range from 25106.pdf p. 23
    hydrogen_liquifier_life_years=30,             # unknown, assumed long

    liquid_hydrogen_tank_capital_cost_per_kg=inflate_1995*18,         # 25106.pdf p. 20, for 300000 kg vessel
    liquid_hydrogen_tank_life_years=40,                       # unknown, assumed long

    hydrogen_fuel_cell_capital_cost_per_mw=813000*inflate_2008,   # 46719.pdf
    hydrogen_fuel_cell_fixed_cost_per_mw_year=27000*inflate_2008,   # 46719.pdf
    hydrogen_fuel_cell_variable_cost_per_mwh=0.0, # not listed in 46719.pdf; we should estimate a wear-and-tear factor
    hydrogen_fuel_cell_mwh_per_kg=0.53*h2_mwh_per_kg,   # efficiency from 46719.pdf
    hydrogen_fuel_cell_life_years=15,   # 46719.pdf
)

args.update(
    pumped_hydro_headers=[
        'ph_project_id', 'ph_load_zone', 'ph_capital_cost_per_mw', 'ph_project_life', 'ph_fixed_om_percent',
        'ph_efficiency', 'ph_inflow_mw', 'ph_max_capacity_mw'],
    pumped_hydro_projects=[
        ['Lake_Wilson', 'Oahu', 2800*1000+35e6/150, 50, 0.015, 0.77, 10, 150],
    ]
)

args.update(
    rps_targets = {2015: 0.15, 2020: 0.30, 2030: 0.40, 2040: 0.70, 2045: 1.00}
)

# data definitions for alternative scenarios
alt_args = [
    dict(),         # base scenario
    # dict(inputs_dir='inputs_2045_15_22', time_sample='2045_15_22'),   # short usable scenario
    # dict(inputs_dir='inputs_tiny', time_sample='tiny_24'),   # tiny version of 2045
    dict(
        inputs_dir='inputs_2007_15', time_sample='2007_15', 
        load_scen_id='hist', ev_scenario=None,
        enable_must_run=1, fuel_scen_id='3',
        use_simple_fuel_costs=True
    ),         # 2007 scenario

    # make a copy of base data, for use in progressive hedging; 
    # use the HECO ref forecast as a starting point (it'll get changed later) 
    # to avoid having two kinds of LNG
    # dict(inputs_subdir='pha'), #, fuel_scen_id = '3'),

    # dict(inputs_subdir='high_oil_price', fuel_scen_id='EIA_high'),
    # dict(inputs_subdir='low_oil_price', fuel_scen_id='EIA_low'),
    # dict(inputs_subdir='lng_oil_peg', fuel_scen_id='EIA_lng_oil_peg'),
    # dict(inputs_subdir='high_lng_oil_peg', fuel_scen_id='EIA_high_lng_oil_peg'),
    # dict(inputs_subdir='re_cost_trend',
    #     wind_capital_cost_escalator=0.011,
    #     pv_capital_cost_escalator=-0.064),
    # dict(inputs_subdir='triple_ph',
    #     pumped_hydro_projects=[
    #         args["pumped_hydro_projects"][0],   # standard Lake Wilson project
    #         ['Project_2_(1.2x)', 'Oahu', 1.2*2800*1000+35e6/150, 50, 0.015, 0.77, 0, 100],
    #         ['Project_3_(1.3x)', 'Oahu', 1.3*2800*1000+35e6/150, 50, 0.015, 0.77, 0, 100],
    #     ]
    # ),
    # dict(
    #     inputs_subdir='rps_2030',
    #     time_sample = "rps_fast_mini",
    #     rps_targets = {2020: 0.4, 2025: 0.7, 2030: 1.0, 2035: 1.0},
    # ),
]


for a in alt_args:
    # clone the arguments dictionary and update it with settings from the alt_args entry, if any
    active_args = dict(args.items() + a.items())
    scenario_data.write_tables(**active_args)
    

