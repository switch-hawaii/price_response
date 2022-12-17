from __future__ import print_function
import subprocess, os, re, sys

status = subprocess.Popen("squeue --me --name demand_scenarios --states R --format %i", shell=True, stdout=subprocess.PIPE).stdout.readlines()
jobs = [line.decode().strip() for line in status[1:]]

scens = []
print("Gathering currently running scenarios.", end="")
for job in jobs:
    with open(os.path.join('logs', job + '.out')) as f:
        out = f.read()
    last_scen, log = re.split('(?m)^running scenario (.*)$', out)[-2:]
    # find outputs dir (could probably use re.search instead of re.findall)
    all_args = re.findall('(?m)^.*scenario_name=.*$', log)
    if all_args:
        # use first arguments line in this part of the .out file
        args_dict = eval('dict({})'.format(all_args[0]))
    else:
        args_dict = {}
    scens.append((args_dict.get('outputs_dir', 'unknown'), last_scen, job))
    print(".", end="")
    sys.stdout.flush()

print()
print()
print("Currently running scenarios:")

for out_dir, scen, job in sorted(scens):
    print('{:13} {:18} {}'.format(job, out_dir, scen))
