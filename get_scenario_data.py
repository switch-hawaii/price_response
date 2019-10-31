#!/usr/bin/env python
from __future__ import division

import sys, os, argparse
from textwrap import dedent
from itertools import chain

import switch_model.hawaii.scenario_data as scenario_data

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
# marginal_pricing_options = ['marginal']  # + ['total']
# tech_clusters = [["2045_fossil", "2045_free", "2045_rps"]]  # +[["2007", "2045_rps_ev"]]

load_scenarios = [('2007_load', 'flat_2007'), ('2045_load', 'PSIP_2016_12')]
ev_scenarios = [
    ('half_ev', 'Half Adoption'),
    ('full_ev', 'Full Adoption'),
    ('2016_ev', 'Flat 2016')
]
elasticity_scenarios = [1, 2, 3]
dynamic_pricing_options = ["flat", "dynamic"]
price_scenarios = ['current_cost', 'future_cost']
rps_levels = ['rps', 'free', 'fossil']
rps_fractions = [
    ('rps_{:03d}'.format(f), 0.01*f)
    for f in chain(range(0, 80, 10), range(80, 101, 2))
]

scenario_list = []
for load_scenario, load_scen_id in load_scenarios:
    for ev_scenario, ev_scenario_long in ev_scenarios:
        if load_scenario != '2045_load' and ev_scenario != 'half_ev':
            # don't do corner cases
            continue
        for elasticity_scen in elasticity_scenarios:
            for price_scenario in price_scenarios:
                for rps_level in rps_levels:
                    for flat in dynamic_pricing_options:
                        if rps_level == 'rps':
                            fractions = rps_fractions
                        else:
                            fractions = [('base', 1.0)]
                        for rps_frac_scen, rps_fraction in fractions:

                            # print flat, marginal, elasticity_scen, tech
                            s = "--scenario-name " +  \
                                "_".join([
                                    rps_frac_scen if rps_level == 'rps' else rps_level,
                                    price_scenario, flat,
                                    load_scenario, ev_scenario,
                                    "scen"+str(elasticity_scen)
                                ])

                            s += " --dr-elasticity-scenario " + str(elasticity_scen)

                            if rps_level == 'free':
                                s += ' --rps-deactivate'
                            elif rps_level == 'fossil':
                                s += ' --rps-no-new-renewables'

                            data_tags = [price_scenario, load_scenario, ev_scenario]
                            if rps_frac_scen != 'base':
                                data_tags.append(rps_frac_scen)

                            if flat == "flat":
                                s += " --dr-flat-pricing --ev-timing bau"

                            scenario_list.append((s, data_tags))


parser = argparse.ArgumentParser()
parser.add_argument('--skip-cf', action='store_true', default=False,
    help='Skip writing variable capacity factors file (for faster execution)')
# use k_means set with 12 days plus an extra "tough" day, 1-hour spacing
parser.add_argument('--time-sample', default="k_means_2045_12+")
parser.add_argument('--inputs-dir', default='inputs')

cmd_line_args = parser.parse_args()

with open('modules.txt') as f:
    modules = set(m.strip() for m in f if not m.strip().startswith('#'))

# particular settings chosen for this case
# (these will be passed as arguments when the queries are run)
args = dict(
    # directory to store data in
    inputs_dir=cmd_line_args.inputs_dir,
    # skip writing capacity factors file if specified (for speed)
    skip_cf=cmd_line_args.skip_cf,
    # skip writing EV bids if not using the ev_advanced module
    skip_ev_bids=('switch_model.hawaii.ev_advanced' not in modules),
    # use heat rate curves for all thermal plants
    use_incremental_heat_rates=True,
    # autoselect whether to use fuel markets or simple fuel costs
    use_simple_fuel_costs=('switch_model.energy_sources.fuel_costs.simple' in modules),
    # could be 'tiny', 'rps', 'rps_mini' or possibly '2007', '2016test', 'rps_test_45', or 'main'
    # '2020_2025' is two 5-year periods, with 24 days per period, starting in 2020 and 2025
    time_sample = cmd_line_args.time_sample,
    # subset of load zones to model
    load_zones = ('Oahu',),
    # "hist"=pseudo-historical, "med"="Moved by Passion", "flat"=2015 levels, "PSIP_2016_04"=PSIP 4/16
    load_scen_id = "PSIP_2016_12",
    # '1'=low, '2'=high, '3'=reference, 'EIA_ref'=EIA-derived reference level, 'hedged'=2020-2030 prices from Hawaii Gas
    fuel_scen_id='AEO_2019_Reference',
    # Blazing a Bold Frontier, Stuck in the Middle, No Burning Desire, Full Adoption,
    # Business as Usual, (omitted or None=none)
    tech_scen_id='ATB_2019_mid',
    ev_scenario = 'Half Adoption',
    # should the must_run flag be converted to set minimum commitment for existing plants?
    enable_must_run = 0,
    # Lake_Wilson is excluded because we don't have the custom code yet to prevent
    # zero-crossing reserve provision
    exclude_technologies = ('CentralFixedPV','Lake_Wilson'),
    # maximum type of reserves that can be provided by each technology (if restricted);
    # should be a list of tuples of (technology, reserve_type); if not specified, we assume
    # each technology can provide all types of reserves; reserve_type should be "none",
    # "contingency" or "reserve"
    # max_reserve_capability = [('Battery_Conting', 'contingency')],
    base_financial_year = 2020,
    interest_rate = 0.06,
    discount_rate = 0.00,
    # used to convert nominal costs in the tables to real costs
    inflation_rate = 0.025,
)

# electrolyzer data from centralized current electrolyzer scenario version 3.1 in
# http://www.hydrogen.energy.gov/h2a_prod_studies.html ->
# "Current Central Hydrogen Production from PEM Electrolysis version 3.101.xlsm"
# and
# "Future Central Hydrogen Production from PEM Electrolysis version 3.101.xlsm" (2025)
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

current_electrolyzer_kg_per_mwh=1000.0/54.3    # (1000 kWh/1 MWh)(1kg/54.3 kWh)   # TMP_Usage
current_electrolyzer_mw = 50000.0 * (1.0/current_electrolyzer_kg_per_mwh) * (1.0/24.0)   # (kg/day) * (MWh/kg) * (day/h)    # design_cap cell
future_electrolyzer_kg_per_mwh=1000.0/50.2    # TMP_Usage cell
future_electrolyzer_mw = 50000.0 * (1.0/future_electrolyzer_kg_per_mwh) * (1.0/24.0)   # (kg/day) * (MWh/kg) * (day/h)    # design_cap cell

current_hydrogen_args = dict(
    hydrogen_electrolyzer_capital_cost_per_mw=144641663*inflate_2007/current_electrolyzer_mw,        # depr_cap cell
    hydrogen_electrolyzer_fixed_cost_per_mw_year=7134560.0*inflate_2007/current_electrolyzer_mw,         # fixed cell
    hydrogen_electrolyzer_variable_cost_per_kg=0.0,       # they only count electricity as variable cost
    hydrogen_electrolyzer_kg_per_mwh=current_electrolyzer_kg_per_mwh,
    hydrogen_electrolyzer_life_years=40,                      # plant_life cell

    hydrogen_fuel_cell_capital_cost_per_mw=813000*inflate_2008,   # 46719.pdf
    hydrogen_fuel_cell_fixed_cost_per_mw_year=27000*inflate_2008,   # 46719.pdf
    hydrogen_fuel_cell_variable_cost_per_mwh=0.0, # not listed in 46719.pdf; we should estimate a wear-and-tear factor
    hydrogen_fuel_cell_mwh_per_kg=0.53*h2_mwh_per_kg,   # efficiency from 46719.pdf
    hydrogen_fuel_cell_life_years=15,   # 46719.pdf
)

args.update(
    hydrogen_electrolyzer_capital_cost_per_mw=58369966*inflate_2007/future_electrolyzer_mw,        # depr_cap cell
    hydrogen_electrolyzer_fixed_cost_per_mw_year=3560447*inflate_2007/future_electrolyzer_mw,         # fixed cell
    hydrogen_electrolyzer_variable_cost_per_kg=0.0,       # they only count electricity as variable cost
    hydrogen_electrolyzer_kg_per_mwh=future_electrolyzer_kg_per_mwh,
    hydrogen_electrolyzer_life_years=40,                      # plant_life cell

    hydrogen_liquifier_capital_cost_per_kg_per_hour=inflate_1995*25600,       # 25106.pdf p. 18, for 1500 kg/h plant, approx. 100 MW
    hydrogen_liquifier_fixed_cost_per_kg_hour_year=0.0,   # unknown, assumed low
    hydrogen_liquifier_variable_cost_per_kg=0.0,      # 25106.pdf p. 23 counts tank, equipment and electricity, but those are covered elsewhere
    hydrogen_liquifier_mwh_per_kg=10.0/1000.0,        # middle of 8-12 range from 25106.pdf p. 23
    hydrogen_liquifier_life_years=30,             # unknown, assumed long

    liquid_hydrogen_tank_capital_cost_per_kg=inflate_1995*18,         # 25106.pdf p. 20, for 300000 kg vessel
    liquid_hydrogen_tank_life_years=40,                       # unknown, assumed long


    # table 5, p. 13 of 46719.pdf, low-cost
    # ('The value of $434/kW for the low-cost case is consistent with projected values for stationary fuel cells')
    hydrogen_fuel_cell_capital_cost_per_mw=434000*inflate_2008,
    hydrogen_fuel_cell_fixed_cost_per_mw_year=20000*inflate_2008,
    hydrogen_fuel_cell_variable_cost_per_mwh=0.0, # not listed in 46719.pdf; we should estimate a wear-and-tear factor
    hydrogen_fuel_cell_mwh_per_kg=0.58*h2_mwh_per_kg,
    hydrogen_fuel_cell_life_years=26,
)

args.update(
    pumped_hydro_headers=[
        'ph_project_id', 'ph_load_zone', 'ph_capital_cost_per_mw', 'ph_project_life', 'ph_fixed_om_percent',
        'ph_efficiency', 'ph_inflow_mw', 'ph_max_capacity_mw'],
    pumped_hydro_projects=[
        # ['Lake_Wilson', 'Oahu', 2800*1000+35e6/150, 50, 0.015, 0.77, 10, 150],
    ]
)

args['rps_targets'] = {2015: 0.15, 2020: 0.30, 2030: 0.40, 2040: 0.70, 2045: 1.00}

flat_args = dict(
    tech_scen_id='ATB_2018_flat',
    fuel_scen_id='flat_2016',
)
flat_args.update(current_hydrogen_args)
all_price_scenarios = {
    'future_cost': dict(),
    'current_cost': flat_args
}

# We assume each data tag is unique, even across data types, so we can use the
# tag as a unique ID for any adjustment to the data. It would be possible to
# have different tag "name spaces" for each type of data (load, ev, prices, rps
# fraction) by making alt_args into a dict, with keys for each data type, but
# then we would need to maintain a list of data types somewhere and iterate
# through that. (That may eventually be a good idea, if it allows collapsing
# all the code into simpler loops.)
alt_args = []
alt_args.extend(
    dict(tag=load_name, load_scen_id=load_scen_id)
    for load_name, load_scen_id in load_scenarios
)
alt_args.extend(
    dict(tag=ev_name, ev_scenario=ev_scenario)
    for ev_name, ev_scenario in ev_scenarios
)
alt_args.extend(
    dict(tag=price_name, **all_price_scenarios[price_name])
    for price_name in price_scenarios
)
# note: the 100% target ends up using the base rps_targets file since it is identical
alt_args.extend(
    dict(
        tag=t,
        rps_targets={k: f * v for k, v in args['rps_targets'].items()}
    )
    for t, f in rps_fractions
)

scenario_data.write_tables(args, alt_args, scenario_list)
