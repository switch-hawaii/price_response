from collections import namedtuple
import glob, re, os, datetime

# go through *.err
# match pattern to identify error
# scan backwards in matching .out file to find which scenario it is
# check against partial scenarios?
# check against scenarios.txt files?

Sig = namedtuple('Sig', ['error', 'err_sig', 'out_sig'])

# note: (?s) flag is needed at the start of a regex to make .* include newlines
signatures = [
    Sig(
        'integer solution found in second pass but not first (within time limit)',
        r'RuntimeError: No dual values have been calculated\.', 
        r'(?s)MIP - Time limit exceeded, no integer solution\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*Incumbent solution written to file'
    ),
    Sig(
        'no integer solution found within time limit',
        r'return self.solutions\[index\]\nIndexError: list index out of range',
        r'(?s)MIP - Time limit exceeded, no integer solution\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*MIP - Time limit exceeded, no integer solution\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo file written.'
    ),
    Sig(
        'no solution found in fixed_milp stage within time limit',
        r'return self.solutions\[index\]\nIndexError: list index out of range',
        r'Time limit exceeded:  Scaled infeas ='
    ),
    Sig(
        'no solution found in fixed_milp stage within time limit',
        r'return self.solutions\[index\]\nIndexError: list index out of range',
        r'Time limit exceeded:  Objective ='
    ),
    Sig(
        'infeasible or unbounded when smoothing variables',
        r'(?s)smooth_dispatch.py'
            r'.*return self.solutions\[index\]\nIndexError: list index out of range',
        r'(?s)MIP - Integer infeasible or unbounded\.'
            r'.*Current MIP best bound is infinite.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*MIP - Integer infeasible or unbounded\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo file written.'
    ),
    Sig(
        'infeasible in MIP stage',
        r'return self.solutions\[index\]\nIndexError: list index out of range',
        r'(?s)MIP - Integer infeasible\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*MIP - Integer infeasible\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo file written.'
    ),
    Sig(
        'infeasible in MIP stage',
        r'RuntimeError: Infeasible model',
        r'(?s)MIP - Integer infeasible\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*MIP - Integer infeasible\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo file written.'
    ),
    Sig(
        'infeasible or unbounded in MIP stage',
        r'return self.solutions\[index\]\nIndexError: list index out of range',
        r'(?s)MIP - Integer infeasible or unbounded\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo changes made\.'
            r'.*MIP - Integer infeasible or unbounded\.'
            r'.*CPLEX Error  1217: No solution exists\.\nNo file written.'
    ),
    Sig(
        'infeasible in fixed_milp stage (integrality problem)',
        r'RuntimeError: Infeasible model',
        r'MILP problem relaxed to LP with fixed integer variables using'
            r'\nincumbent solution\.'
            r'\nCPLEX> Infeasibility row'
    ),
    Sig(
        'infeasible in fixed_milp stage (variable integrality)',
        r'RuntimeError: Infeasible model',
        r'MILP problem relaxed to LP with fixed integer variables using'
            r'\nincumbent solution\.'
            r'\nCPLEX> Infeasible column'
    ),
    Sig(
        'infeasible in fixed_milp stage (variable integrality)',
        r'RuntimeError: Infeasible model',
        r'(?s)MILP problem relaxed to LP with fixed integer variables using'
            r'\nincumbent solution\.'
            r'.*Infeasible variable'
    ),
    Sig(
        "invalid weights for some bids",
        "ValueError: Some bids have invalid weights",
        ""
    ),
    Sig(
        "malformed inputs-alias argument in scenario definition",
        "AttributeError: 'DataPortal' object has no attribute 'file_aliases'",
        ".csv--scenario-name"
    ),
    Sig(
        "missing quote in scenario definition",
        "ValueError: No closing quotation",
        ""
    ),
    Sig(
        "wrong name for scenarios list",
        "with open(scenario_list_file, 'r') as f:\nFileNotFoundError: [Errno 2] No such file or directory:",
        ""
    ),
    Sig(
        "rounding error on dual value (small but should be zero)",
        "ValueError: .* has no .* bound but has a non-zero dual value .*",
        ""
    ),
    Sig(
        "missing flexshares.csv",
        "cannot open file 'flexshares.csv': No such file or directory",
        ""
    ),
    Sig(
        "slurm communication failure",
        "Application launch failed: Socket timed out on send/recv operation",
        ""
    ),
    Sig(
        "job exceeded time limit",
        "JOB .* CANCELLED .* DUE TO TIME LIMIT",
        "",
    ),
    Sig(
        "job ran out of memory",
        "(?s)Out Of Memory.*Terminating",
        "",
    ),
    Sig(
        "job preempted (may have been requeued or continued)",
        r"\*\*\* JOB .* CANCELLED AT \S* DUE TO PREEMPTION \*\*\*",
        "",
    ),
    Sig(
        "job cancelled",
        r"\*\*\* JOB .* CANCELLED AT \S* \*\*\*",
        "",
    ),
    Sig( # report this late, because sometimes it's caused by one of the ones above
        "file system error in CPLEX",
        "",
        "CPLEX Error  1426",  # low-level file system error; exact message differs by version
    ),
]

# file = 'logs/8910232_5.err'
results = []
print("finding all logs/*.err files")
err_files = glob.glob('logs/*.err')
for i, file in enumerate(err_files):
    # if file not in [
    #     "logs/4870180_17.err",
    #     "logs/4870180_22.err",
    #     "logs/5070073_139.err",
    #     "logs/5070073_141.err",
    # ]:
    #     continue
    if i % 100 == 0:
        print("processing files {}-{} of {}".format(i+1, min(i+100, len(err_files)), len(err_files)))
    with open(file) as f:
        err = f.read()
    if not err:
        continue
    with open(file[:-4] + '.out') as f:
        out = f.read()
    for error, err_sig, out_sig in signatures:
        # error, err_sig, out_sig = signatures[1]
        if re.search(err_sig, err):
            if re.search(out_sig, out):
                scenario_error = error
                break
    else:
        scenario_error = 'unknown error'
        # import pdb; pdb.set_trace()


    # get end time (time when .err file was closed)
    end_time = datetime.datetime.utcfromtimestamp(os.path.getmtime(file)).strftime('%Y-%m-%d %H:%M:%S')

    # get log for last scenario that was started (may have crashed before showing argument list)
    last_scen, log = re.split('(?m)^running scenario (.*)$', out)[-2:]

    # find outputs dir (could probably use re.search instead of re.findall)
    all_args = re.findall('(?m)^.*scenario_name=.*$', log)
    if all_args:
        # use first arguments line in this part of the .out file
        args_dict = eval('dict({})'.format(all_args[0]))
    else:
        args_dict = {}

    results.append((
        file, 
        scenario_error, 
        args_dict.get('outputs_dir', 'unknown'),
        last_scen, 
        end_time,
    ))

results.sort(key=lambda r: (r[1], r[0]))

with open('error_status.csv', 'w') as f:
    f.write('file,error,outputs_dir,scenario,end_time\n')
    for row in results:
        f.write(','.join(row) + '\n')

# err_sig = 'return self.solutions\[index\]\\nIndexError: list index out of range'
# err_sig = 'return self.solutions[index]'
# err = """Traceback (most recent call last):
#   File "/home/imelda9/.conda/envs/switch/bin/switch", line 11, in <module>
#     load_entry_point('switch-model', 'console_scripts', 'switch')()
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/main.py", line 39, in main
#     main()
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/solve_scenarios.py", line 158, in main
#     solve.main(args=args)
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/solve.py", line 159, in main
#     iterate(instance, iterate_modules)
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/solve.py", line 398, in iterate
#     iterate(m, iterate_modules, depth=depth+1)
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/solve.py", line 365, in iterate
#     solve(m)
#   File "/mnt/group/nfs_fs01/spl_group/switch/switch/switch_model/solve.py", line 780, in solve
#     if len(model.solutions[-1]._entry['variable']) == 0:
#   File "/home/imelda9/.conda/envs/switch/lib/python3.7/site-packages/pyomo/core/base/PyomoModel.py", line 206, in __getitem__
#     return self.solutions[index]
# IndexError: list index out of range
# srun: error: node-0151: task 0: Exited with exit code 1
# srun: Terminating job step 8910277.0
# """
# print(re.search(err_sig, err))