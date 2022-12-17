#!/usr/bin/env python
from __future__ import print_function

import glob, os, datetime

started = glob.glob('energy_sources*_0.csv')
all_complete = True
for f in started:
    scen = f[len('energy_sources_'):-len('_0.csv')]
    if not os.path.exists('energy_sources_' + scen + '_.csv'):
        all_complete = False
        iters = glob.glob('energy_sources_' + scen + '_*.csv')
        start_time = datetime.datetime.utcfromtimestamp(os.path.getmtime(f)).strftime('%Y-%m-%d %H:%M:%S')
        print('incomplete ({} iters, started {}): {}'.format(len(iters), start_time, scen))

if all_complete:
    print('no incomplete scenarios')

