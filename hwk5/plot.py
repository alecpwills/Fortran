import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import os
#%%
# Use explicit one if in interactive shell, like in Spyder's IPython
#dir_path = '/home/alec/Documents/Fortran/hwk5/
# This path if running the file from shell, i.e. python plot.py
dir_path = os.path.dirname(os.path.realpath(__file__))
print('Your file directory is:', dir_path)
print('Reading in output.dat from the relaxation program...')
df = pd.read_table(os.path.join(dir_path, 'output.dat'), header=None, delim_whitespace=True)
print('Data read complete.')
#%%
print('Generating colored graph of potential solution...')
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
cols = np.linspace(min(df.iloc[:, 2]), max(df.iloc[:, 2]), num=len(df.iloc[:, 2]))
length = len(df.iloc[:, 2])
if length < 1000:
    size = 10
elif length < 10000:
    size = 1
elif length < 50000:
    size = 0.1
else:
    size = 0.01
cax = ax.scatter(xs=df.iloc[:, 0], ys=df.iloc[:, 1], zs=df.iloc[:, 2], marker='+', s=size, c=cols, cmap='inferno')
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
ax.set_zlabel('$\phi(x,y)$ (V)')
ax.set_autoscalez_on(True)
cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
plt.tight_layout()
plt.savefig(os.path.join(dir_path, 'outputgraph.pdf'), dpi=800)
print('Graph drawn. See outputgraph.pdf.')
#%%
print('Generating heatmap of solution...')
f = plt.figure()
ax = f.add_subplot(111)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
cax = ax.hexbin(x=df.iloc[:, 0], y=df.iloc[:, 1], C=df.iloc[:, 2], cmap='inferno')
cb = f.colorbar(cax)
cb.set_label('$\phi(x,y)$ (V)')
plt.savefig(os.path.join(dir_path, 'heatmap.pdf'), dpi=800)
print('Heatmap drawn. See heatmap.pdf.')
