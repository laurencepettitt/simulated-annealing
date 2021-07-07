from os import error
import sys
import json
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="darkgrid")

numArgs = len(sys.argv)

# output from Main.hs
if numArgs == 1:
    raw_data = input()
    hists = json.loads(raw_data)
elif numArgs == 2:
    with open(sys.argv[1], 'r') as f:
        hists = json.load(f)
else:
    print("Wrong number of arguments.", file=sys.stderr)
    print("Usage: plot [FILE]")
    print("Prints energy timeseries of data in FILE.")
    print("If no FILE given, input is standard input.")
    exit(1)

# list of lists (inner list is history of state through simulation)
hist_data = hists

ets = pd.DataFrame(hist_data)

sns_plot = sns.relplot(x="time", y="energy",
            kind="line",
            ci="sd",
            data=ets)


plt.savefig("output.png")
