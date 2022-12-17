import glob, sys
import pandas as pd

for theta in ['theta01', 'theta05', 'theta2']:
    dirs = [
        'backup_2021_08_04/outputs_{}'.format(theta),
        'outputs_{}'.format(theta)
    ]
    rows = {d: [] for d in dirs}
    
    for d in dirs:
        files = glob.glob('{}/demand_response_summary_final*.csv'.format(d))
        print('reading {} files in {}.'.format(len(files), d))
        for file in files:
            rows[d].append(pd.read_csv(file))
    
    for col in ['iteration', 'total_cost']:
        df = pd.DataFrame()
        for d, r in rows.items():
            df[d] = pd.concat(r).set_index('tag')[col]

        plot_file = 'old_new_comparison_{}_{}.pdf'.format(col, theta)
        df.plot.scatter(x=dirs[0], y=dirs[1]).get_figure().savefig(plot_file)
        df.to_csv(plot_file[:-4] + '.csv')

        print('saved comparison in {}'.format(plot_file))
        diff = df.iloc[:,0] - df.iloc[:,1]
        print('mean absolute difference: {} (n={})'.format(diff.abs().mean(), diff.count()))

    print()

