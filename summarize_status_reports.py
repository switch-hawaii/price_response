import pandas as pd

status = {}

def add_status(out_dir, scenario, time, event):
    if 'theta' in out_dir:
        theta = out_dir[len('outputs_'):]
        status.setdefault((theta, scenario), []).append(str(time) + ' ' + event)

df = pd.read_csv('scenario_status.csv')
for i, r in df.iterrows():
    event = 'defined in ' + r['defined_file']
    add_status(r['outputs_dir'], r['scenario'], r['defined_time'], event) 

df = pd.read_csv('completion_status.csv')
for i, r in df.iterrows():
    if r['completed']:
        event = 'ran to completion'
    else:
        event = 'stopped after {} iterations'.format(r['iterations_run'])
    add_status(r['outputs_dir'], r['scenario'], r['start_time'], event) 

df = pd.read_csv('error_status.csv')
df['event'] = 'error: ' + df['error']
for i, row in df.iterrows():
    add_status(*row[['outputs_dir', 'scenario', 'end_time', 'event']]) 

status_records = []
for (theta, scen), ev in status.items():
    events = sorted(ev)
    final = events[-1]

    # create a high-level summary description
    event_msgs = [e[20:] for e in events] # message, not date
    msg = final[20:]  
    if all(e.startswith('defined') for e in event_msgs):
        summary = ', '.join(event_msgs) + ' but not run'
    elif msg == 'ran to completion':
        summary = msg
    elif msg.startswith('stopped after'):
        summary = ', '.join(event_msgs[:-1]) + ', stopped early (no log)'
    elif msg.startswith('error'):
        summary = msg
    else:
        summary = ', '.join(event_msgs)
    
    
    status_records.append({
        'scenario': theta + ' / ' + scen,
        'history': '\n'.join(events),
        'final_status': final,
        'summary': summary,
    })

status_df = pd.DataFrame.from_records(status_records).sort_values('scenario')
status_df.to_csv('status_summary.csv', index=False)

overall_summary = status_df.groupby('summary').size()
overall_summary.to_csv('status_summary_overall.csv')

print("overall results:")
print(overall_summary)
