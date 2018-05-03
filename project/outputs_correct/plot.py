import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D
import imageio
import os

#%%
def e_pot(q, pts, ref):
    q = q*1.602e-19
    pts = np.array(pts)
    ref = np.array(ref)
    eo = 8.854187817e-12
    return q/ (np.linalg.norm(pts-ref)*4*np.pi*eo)

def rho_strength(V, e=True):
    eo = 8.854187817e-12
    qe = 1.602e-19
    if e==True:
        return V*eo/qe
    else:
        return V*eo
#%%
# Use explicit one if in interactive shell, like in Spyder's IPython
#dir_path = '/home/alec/Documents/Fortran/hwk5/
# This path if running the file from shell, i.e. python plot.py
if __name__ == '__main__':
    dir_path = os.path.dirname(os.path.realpath(__file__))
    print('Your file directory is:', dir_path)
    #filename = input("Please specify filename to plot: ")
    print('Reading in {} from the relaxation program...'.format('p1_source.dat'))
    df = pd.read_table(os.path.join(dir_path, 'p1_source.dat'), header=None, delim_whitespace=True)
    print('Data read complete.')
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
#%%
LX = 40.0
LY = 15.0
DX = DY = 0.1
NX = int(LX/DX)
NY = int(LY/DY)
#xs = np.linspace(LX/4, 3*LX/4, NX)
#ys = np.linspace(LY/4, 3*LY/4, NY)
xs = np.linspace(0, LX, NX)
ys = np.linspace(0, LY, NY)
plt.plot(xs[1:], 1/xs[1:])
plt.zlim(0, 200)
#%%
lsts =[]
for i in xs:
    for j in ys:
        lsts.append([i, j, rho_strength(-100)/np.linalg.norm(np.array([i-LX/2, j-LY/2]))])
df = pd.DataFrame(lsts, columns=['x', 'y', 'z'])

#%%
LX = 40.0
LY = 15.0
DX = DY = 0.1
NX = int(LX/DX)
NY = int(LY/DY)
xs = np.linspace(LX/4, 3*LX/4, NX)
ys = np.linspace(LY/4, 3*LY/4, NY)
#xs = np.linspace(0, LX, NX)
#ys = np.linspace(0, LY, NY)
plt.plot(xs[1:], 1/xs[1:])
lsts =[]
for i in xs:
    for j in ys:
        lsts.append([i, j, 100/(4*np.pi*np.linalg.norm(np.array([i-LX/2, j-LY/2])))])
df = pd.DataFrame(lsts, columns=['x', 'y', 'z'])
f = plt.figure()
ax = f.add_subplot(111)
#h=plt.contour(xs, ys, z)
cax = ax.hexbin(x=df.iloc[:, 0], y=df.iloc[:, 1], C=df.iloc[:, 2], cmap='inferno')
cb = f.colorbar(cax)
cb.set_label('$\phi(x,y)$ (V)')

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
#cax = ax.scatter(xs=df.iloc[:, 0], ys=df.iloc[:, 1], zs=df.iloc[:, 2], marker='+', s=1, c=cols, cmap='inferno')
cax = ax.plot_trisurf(df.iloc[:, 0], df.iloc[:, 1], df.iloc[:, 2], cmap=cm.inferno,
                      antialiased=False, linewidth=0)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
ax.set_zlabel('$\phi(x,y)$ (V)')
#ax.set_autoscalez_on(True)
ax.set_zlim(0, 100)
cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
plt.tight_layout()
    
#%%
file = "four_bc"
dir_path = '/home/alec/Documents/Fortran/project'
df = pd.read_table(os.path.join(dir_path, 'outputs_correct/{}.dat'.format(file)), header=None, delim_whitespace=True)
f = plt.figure()
ax = f.add_subplot(111)
#h=plt.contour(xs, ys, z)
cax = ax.hexbin(x=df.iloc[:, 0], y=df.iloc[:, 1], C=df.iloc[:, 2], cmap='inferno')
cb = f.colorbar(cax)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
cb.set_label('$\phi(x,y)$ (V)')
plt.tight_layout()
plt.savefig(os.path.join(dir_path, 'report/{}_heatmap.pdf'.format(file)), dpi=800)
plt.show()
plt.close()

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
print(size)
cax = ax.plot_trisurf(df.iloc[:, 0], df.iloc[:, 1], df.iloc[:, 2], cmap='inferno',
                      antialiased=False, linewidth=1)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
ax.set_zlabel('$\phi(x,y)$ (V)')
#ax.set_autoscalez_on(True)
ax.set_zlim(-100, 100)
ax.set_ylim(15, 0)
cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
plt.tight_layout()
plt.savefig(os.path.join(dir_path, 'report/{}_graph.pdf'.format(file)), dpi=800)
plt.show()
plt.close()
#%%
LX = 40.0
LY = 15.0
DX = DY = 0.1
NX = int(LX/DX)
NY = int(LY/DY)
gifdir = os.path.join(dir_path, 'outputs_correct/gifruns.dat')
df = pd.read_table(gifdir, header=None, delim_whitespace=True, chunksize=(NX+2)*(NY+2))
#%%
#i=0
#for chunk in df:
#    f = plt.figure()
#    ax = f.add_subplot(111)
#    cax = ax.hexbin(x=chunk.iloc[:, 0], y=chunk.iloc[:, 1], C=chunk.iloc[:, 2], cmap='inferno')
#    cb = f.colorbar(cax)
#    cb.set_label('$\phi(x,y)$ (V)')
#    plt.tight_layout()
#    plt.savefig(os.path.join(dir_path, 'outputs_correct/pngs/heatmap_{:04d}'.format(i)))
#    plt.close()
#    i+=1

#%%
i=-1
for chunk in df:
    i+=1
    if i%50 != 0:
        continue
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    cols = np.linspace(min(chunk.iloc[:, 2]), max(chunk.iloc[:, 2]), num=len(chunk.iloc[:, 2]))
    length = len(chunk.iloc[:, 2])
    if length < 1000:
        size = 10
    elif length < 10000:
        size = 1
    elif length < 50000:
        size = 0.1
    else:
        size = 0.01
    cax = ax.plot_trisurf(chunk.iloc[:, 0], chunk.iloc[:, 1], chunk.iloc[:, 2], cmap=cm.inferno,
                      antialiased=False, linewidth=0)
    ax.set_ylabel('$y$ (cm)')
    ax.set_xlabel('$x$ (cm)')
    ax.set_zlabel('$\phi(x,y)$ (V)')
    ax.set_autoscalez_on(True)

    cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
    plt.tight_layout()
    plt.savefig(os.path.join(dir_path, 'outputs_correct/graphpngs/graph_{:04d}'.format(i)))
    plt.close()
#%%
#pngpath = os.path.join(dir_path, 'outputs_correct/graphpngs/')
#filenames = sorted(os.listdir(os.path.join(dir_path, 'outputs_correct/graphpngs/')))
#images = []
#for file in filenames[:-1]:
#    images.append(imageio.imread(os.path.join(pngpath, file)))
#imageio.mimsave(os.path.join(dir_path, 'graph.gif'), images, duration=1)
##%%
#pngpath = os.path.join(dir_path, 'outputs_correct/pngs/')
#filenames = sorted(os.listdir(os.path.join(dir_path, 'outputs_correct/pngs/')))
#images = []
#for file in filenames[:-1]:
#    images.append(imageio.imread(os.path.join(pngpath, file)))
#imageio.mimsave(os.path.join(dir_path, 'heatmap.gif'), images, duration=0.03)


#%%
dir_path = '/home/alec/Documents/Fortran/project'
df = pd.read_table(os.path.join(dir_path, 'diffmat.dat'), header=None, delim_whitespace=True)
f = plt.figure()
ax = f.add_subplot(111)
cax = ax.hexbin(x=df.iloc[:, 0], y=df.iloc[:, 1], C=df.iloc[:, 2], cmap='inferno')
cb = f.colorbar(cax)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
cb.set_label('$\Delta\phi(x,y)$ (V)')
plt.tight_layout()
plt.savefig('diffmat_heatmap.png'.format(file), dpi=800)
plt.show()
plt.close()

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
print(size)
cax = ax.plot_trisurf(df.iloc[:, 0], df.iloc[:, 1], df.iloc[:, 2], cmap='inferno',
                      antialiased=False, linewidth=1)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
ax.set_zlabel('$\Delta\phi(x,y)$ (V)')
#ax.set_autoscalez_on(True)
ax.set_zlim(-200, 200)
ax.set_ylim(15, 0)
cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
plt.tight_layout()
plt.savefig('diffmat_graph.png'.format(file), dpi=800)
plt.show()
plt.close()
#%%
dir_path = '/home/alec/Documents/Fortran/project'
diffmat = pd.read_table(os.path.join(dir_path, 'diffmat.dat'), header=None, delim_whitespace=True)
sol = pd.read_table(os.path.join(dir_path, 'outputs_correct/three_bc.dat'), header=None, delim_whitespace=True)
solsub = sol[(sol.iloc[:, 0] <= 40.0) & (sol.iloc[:, 1] <=15.0)]
abc = pd.read_table(os.path.join(dir_path, 'analyticalmat.dat'), header=None, delim_whitespace=True)
solsub = sol[(sol.iloc[:, 0] <= 40.0) & (sol.iloc[:, 1] <=15.0)]
abcsub = abc[(abc.iloc[:, 0] <= 40.0) & (abc.iloc[:, 1] <=15.0)].iloc[:-1]

LX = 40.1
LY = 15.1
DX = DY = 0.1
NX = int(LX/DX)
NY = int(LY/DY)
xs = np.linspace(0, LX, NX+1)
ys = np.linspace(0, LY, NY+1)
#xs = np.linspace(0, LX, NX)
#ys = np.linspace(0, LY, NY)
lsts =[]
for i in xs:
    for j in ys:
        if i == LX/2 or j == LY/2:
            lsts.append([i, j, 100/(4*np.pi*np.linalg.norm(np.array([i-(LX+0.005)/2, j-(LY+0.005)/2])))])
        else:
            lsts.append([i, j, 100/(4*np.pi*np.linalg.norm(np.array([i-LX/2, j-LY/2])))])
pt = pd.DataFrame(lsts, columns=['x', 'y', 'z'])
#%%
diffs = np.array(abc.iloc[:, 2]) + np.array(pt.iloc[:, 2])-np.array(sol.iloc[:, 2])
f = plt.figure()
ax = f.add_subplot(111)
cax = ax.hexbin(x=diffmat.iloc[:, 0], y=diffmat.iloc[:, 1], C=diffmat.iloc[:, 2], cmap='inferno')
cb = f.colorbar(cax)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
cb.set_label('$\Delta\phi(x,y)$ (V)')
plt.tight_layout()
#plt.savefig('ptdiff_heat.png', dpi=800)
plt.show()
plt.close()

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
cols = np.linspace(min(diffs), max(diffs), num=len(diffs))
length = len(pt.iloc[:, 2])
if length < 1000:
    size = 10
elif length < 10000:
    size = 1
elif length < 50000:
    size = 0.1
else:
    size = 0.01
print(size)
cax = ax.plot_trisurf(diffmat.iloc[:, 0], diffmat.iloc[:, 1], diffmat.iloc[:, 2], cmap='inferno',
                      antialiased=False, linewidth=1)
ax.set_ylabel('$y$ (cm)')
ax.set_xlabel('$x$ (cm)')
ax.set_zlabel('$\Delta\phi(x,y)$ (V)')
#ax.set_autoscalez_on(True)
ax.set_zlim(-200, 200)
ax.set_ylim(15, 0)
cb = fig.colorbar(cax, orientation='horizontal', fraction=0.05)
plt.tight_layout()
plt.savefig(os.path.join(dir_path, 'report/diffmat_graph.pdf'), dpi=800)
plt.show()
plt.close()