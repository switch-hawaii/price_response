#!/usr/bin/env python
from __future__ import print_function

import glob, os, datetime

print("finding scenarios that have been started")
started = glob.glob('outputs_theta*/energy_sources*_0.csv')
all_complete = True
with open('incomplete_status.csv', 'w') as csv:
    csv.write('outputs_dir,scenario,start_time,iterations_run')
    for i, f in enumerate(started):
        if i % 100 == 0:
            print("checking scenarios {}-{} of {}".format(i+1, min(i+100, len(started)), len(started)))
        outdir, file = f.split('/')
        scen = file[len('energy_sources_'):]
        file_base = f[:-6]  # everything except the '_0.csv' at the end
        if not os.path.exists(file_base + '_.csv'):  # final file
            all_complete = False
            iters = len(glob.glob(file_base + '_*.csv'))
            start_time = datetime.datetime.utcfromtimestamp(os.path.getmtime(f)).strftime('%Y-%m-%d %H:%M:%S')
            print('incomplete ({} iters, started {}): {}/{}'.format(iters, start_time, outdir, scen))
            csv.write(','.join(str(x) for x in [outdir, scen, start_time, iters]))

if all_complete:
    print('no incomplete scenarios')

