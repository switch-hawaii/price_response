import os
import pandas as pd
summary = pd.DataFrame.from_csv('demand_response_summary.tsv', sep='\t', index_col=None)
last_iter = summary.groupby('tag')['iteration'].max().reset_index()
dual_files = 'dual_costs_'+last_iter.tag+'_'+last_iter.iteration.astype(str)+'.tsv'
all_surplus = pd.DataFrame()
for _, tag, i in last_iter[['tag', 'iteration']].itertuples():
    f = 'dual_costs_' + tag + '_' + str(i) + '.tsv'
    if os.path.exists(f):
        print "processing file {}...".format(f)
        duals = pd.DataFrame.from_csv(f, sep='\t', index_col=None)
        duals['component'] = duals['constraint'].apply(lambda c: c[:c.index('[')])
        surplus = -duals.groupby('component')['total_cost'].sum()
        surplus['tag'] = tag
        all_surplus = all_surplus.append(surplus)
    else:
        print "WARNING: file not found: {}".format(f)
all_surplus = all_surplus.set_index('tag').astype(float).fillna(0.0).sort_index()
all_surplus
groups = {
    'Consumer Surplus vs Baseline': ['DR_Convex_Bid_Weight'],
    'EV Charging Cost': ['ChargeEVs_bau', 'ChargeEVs_min'],
    'Existing Plant Rents': ['BuildProj'],
    'Unit Commitment & Reserves': [
        'integer: CommitProjectFlag', 'integer: CommitUnits',
        'integer: RunKalaeloaUnitFull', 'Run_Kalaeloa_Unit_Full_Enforce'
    ],
    'Lumpy Renewable Fuel Conversion':
        ['Enforce_DispatchRenewableFlag', 'integer: DispatchRenewableFlag'],
    'Limited/Lumpy LNG': ['integer: RFMBuildSupplyTier', 'FuelConsumptionByTier']
}
known_cols = [c for col_list in groups.values() for c in col_list]
cols = {g: all_surplus[cols].sum(axis=1) for g, cols in groups.items()}
extra_cols = {c: all_surplus[c] for c in all_surplus.columns if c not in known_cols} 
cols.update(extra_cols)
if extra_cols:
    print "Added unknown columns: {}".format(
        ', '.join('"{}"'.format(k) for k in extra_cols.keys())
    )
results = pd.DataFrame(cols)
results.to_csv('surplus_summary.tsv', sep='\t')
print "wrote surplus_summary.tsv"
