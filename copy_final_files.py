import os, glob, shutil
import pandas as pd
summary = pd.DataFrame.from_csv('demand_response_summary.tsv', sep='\t', index_col=None)
last_iter = summary.groupby('tag')['iteration'].max().reset_index()

found_some = False
for _, tag, i in last_iter[['tag', 'iteration']].itertuples():
    # go through all matching files
    file_pattern = '*_' + tag + '_' + str(i) + '.tsv'
    files = glob.glob(file_pattern)
    if files:
        for f in files:
            found_some = True
            f_new = f.replace('_' + str(i) + '.tsv', '_final.tsv')
            print 'copying {} -> {}'.format(f, f_new)
            shutil.copy2(f, f_new)
    else:
        print 'no files found matching "{}"'.format(file_pattern) 
