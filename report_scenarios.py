#!/usr/bin/env python
from __future__ import print_function

import glob, os, datetime, shlex, argparse, csv

parser = argparse.ArgumentParser()
parser.add_argument('--scenario-name')
parser.add_argument('--dr-r-script', default='nestedcespy.R')

# directories used for each elasticity scenario
output_dirs = {
    'nestedcespy.R': 'outputs_theta01',
    'nestedcespy-theta2.R': 'outputs_theta2',
    'nestedcespy-theta0.5.R': 'outputs_theta05',
    'nestedcespy-theta0.9.R': 'outputs_theta09',
}

scenario_files = sorted(glob.glob('scenarios*.txt'))
scenario_files = [
    s 
    for s in scenario_files 
    if s.startswith('scenarios-theta') or s == 'scenarios.txt'
]
scenarios = []
for i, file_name in enumerate(scenario_files):
    defined_time = datetime.datetime.utcfromtimestamp(
        os.path.getmtime(file_name)).strftime('%Y-%m-%d %H:%M:%S'
    )
    print('reading {}'.format(file_name))
    with open(file_name) as f:
        for row in f:
            args_raw = shlex.split(row, comments=True)
            args = parser.parse_known_args(args=args_raw)[0]
            out_dir = output_dirs[args.dr_r_script]
            if file_name.startswith('scenarios-'):
                defined_file = file_name[len('scenarios-'):-len('.txt')]
            else:
                defined_file = file_name
            scenarios.append([out_dir, args.scenario_name, defined_file, defined_time])

with open('scenario_status.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(['outputs_dir', 'scenario', 'defined_file', 'defined_time'])
    writer.writerows(scenarios)

print(
    'saved scenario info to scenario_status.csv ({} scenarios, {} unique)'
    .format(len(scenarios), len({s[1] for s in scenarios}))
)

