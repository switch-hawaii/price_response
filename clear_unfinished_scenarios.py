from __future__ import print_function
import subprocess, os, re, sys, glob

status = subprocess.Popen("squeue --me --name demand_scenarios --states R --format %i", shell=True, stdout=subprocess.PIPE).stdout.readlines()
jobs = [line.decode().strip() for line in status[1:]]
running = set()
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
    
    running.add((args_dict.get('outputs_dir', 'unknown'), last_scen))
    print(".", end="")
    sys.stdout.flush()
print()

for theta in ['theta01', 'theta05', 'theta2']:
    results_dir = os.path.join('outputs_{}'.format(theta))
    # we could in principle get a list of finished scenarios from demand_response_summary_final.csv
    # but that tends to get clobbered since it has no locking
    results_pattern = os.path.join(results_dir, 'energy_sources_*scen?.csv')
    result_files = glob.glob(results_pattern)
    finished_scens = {f[len(results_dir)+len('energy_sources_')+1 : -4] for f in result_files}

    sq_dir = os.path.join('sq', theta)
    if os.path.exists(sq_dir):
        started_scens = set(next(os.walk(sq_dir))[1]) # all dirs in sq_dir
    else:
        print("Scenario queue {} does not exist; skipping.".format(sq_dir))
        continue

    # sanity check, to avoid restarting everything accidentally if they are
    # not writing results with the right filenames or something
    if not any(s in finished_scens for s in started_scens):
        if not finished_scens:
            print("No finished scenario files found matching pattern {}".format(results_pattern))
        else:
            print("Some scenarios have finished but they are not marked as being started.")
            print("This probably indicates an error in identifying finished scenarios.")
            print("Finished scenarios:")
            print(', '.join(finished_scens))

        print("Skipping {}.".format(theta))
        print("(You can restart all scenarios by executing rm -rf {} .)".format(os.path.join(sq_dir, '*')))
        continue

    for s in sorted(started_scens):
        if s not in finished_scens:
            if (results_dir, s) in running:
                print('# skipping scenario {}/{} because it is currently running.'.format(theta, s))
                continue

            try:
                # os.rmdir(os.path.join(sq_dir, s))
                # print('restarting scenario {}/{}'.format(theta, s))
                print('rmdir sq/{}/{}'.format(theta, s))
            except:
                print('WARNING: unable to remove {}; skipping.'.format(os.path.join(sq_dir, s)))

    for s in sorted(finished_scens):
        if s not in started_scens:
            # TODO: check only scenarios in the scenario list?
            print("WARNING: scenario {}/{} has already finished but may run again.".format(theta, s))


