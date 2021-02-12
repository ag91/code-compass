#!/bin/env python

import pandas as pd
import matplotlib.pyplot as plt
import argparse

parser = argparse.ArgumentParser("simple_example")
parser.add_argument("csv", help="The csv file containing 2 columns.")
args = parser.parse_args()
csv_file=args.csv
data = pd.read_csv(csv_file)
Authors = data["author"]
Lines = data["+&-lines"]
x=[]
y=[]
x=list(Lines)
y=list(Authors)
plt.title('Ownership Fragmentation')
plt.pie(x,labels=y,autopct='%.2f%%')
plt.show()
