#!/usr/bin/python3

import sys,os
import numpy as np
from scipy import stats
from scipy.stats import kde
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


matplotlib.rcParams.update({'font.size': 15})



readfile=sys.argv[1]

Xlabel=sys.argv[2]
Ylabel=sys.argv[3]
Title=sys.argv[4]

Data = np.loadtxt(readfile,skiprows=0)
m1=Data[:,0]
m2=Data[:,1]

print(Data.shape)
print(np.min(Data),np.max(Data))
if (len(sys.argv) == 8):
    SUPPLIEDLIM=1
    INFLIM=float(sys.argv[5])
    SUPLIM=float(sys.argv[6])
    xmin=INFLIM
    xmax=SUPLIM
    ymin=INFLIM
    ymax=SUPLIM
    ticksize=float(sys.argv[7])
else:
    xmin=np.min(Data)
    xmax=np.max(Data)
    ymin=np.min(Data)
    ymax=np.max(Data)


#print(np.max(m1),np.max(m2))

maskm1=np.ones(len(m1), dtype=bool)
for i in range(len(m1)):
    if (m1[i] > xmax):
        maskm1[i]=False
m1=m1[maskm1]
m2=m2[maskm1]
maskm2=np.ones(len(m1), dtype=bool)
for i in range(len(m2)):
    if (m2[i] > ymax):
        maskm2[i]=False
m1=m1[maskm2]
m2=m2[maskm2]

nbins=100
xi, yi = np.mgrid[m1.min():m1.max():nbins*1j, m2.min():m2.max():nbins*1j]

#print(np.max(m1),np.max(m2))

X, Y = np.mgrid[xmin:xmax:100j, ymin:ymax:100j]
positions = np.vstack([X.ravel(), Y.ravel()])
values = np.vstack([m1, m2])
kernel = stats.gaussian_kde(values)
Z = np.reshape(kernel(positions).T, X.shape)

zi = kernel(np.vstack([xi.flatten(), yi.flatten()]))

fig, ax = plt.subplots()
# Show density
#ax.pcolormesh(xi, yi, zi.reshape(xi.shape))
ca=ax.imshow(np.rot90(Z), cmap=plt.cm.gist_stern_r,extent=[xmin, xmax, ymin, ymax])
#ca=ax.imshow(np.rot90(Z), cmap=plt.cm.gist_earth_r,extent=[xmin, xmax, ymin, ymax])
#ca=ax.imshow(np.rot90(Z), cmap=plt.cm.PuBu_r,extent=[xmin, xmax, ymin, ymax])
#ca=ax.imshow(np.rot90(Z), cmap=plt.cm.plasma,extent=[xmin, xmax, ymin, ymax])

# Add contour lines
#print("***",np.min(Data),np.min(Z),np.max(Data),np.max(Z))
#levels = np.arange(0.3, np.max(Z), np.max(Z)/6.)
levels = np.arange(0.6, np.max(Z), np.max(Z)/6.)
cs=ax.contour(X, Y, Z, levels, colors='k',linestyles='-',linewidths=0.5)
#cs=ax.contour(X, Y, Z,cmap=plt.cm.gist_stern_r,extent=[xmin, xmax, ymin, ymax])
#cs=ax.contour(X, Y, Z,cmap=plt.cm.gist_earth_r,extent=[xmin, xmax, ymin, ymax])
#cs=ax.contour(X, Y, Z,cmap=plt.cm.PuBu_r,extent=[xmin, xmax, ymin, ymax])
#cs=ax.contour(X, Y, Z,cmap=plt.cm.plasma,extent=[xmin, xmax, ymin, ymax])
ax.plot([xmin,xmax],[ymin,ymax],linestyle='dashed',color='k')

#plt.scatter(m1, m2, marker='.', s=1)

#ax.plot(m1, m2, 'k.', markersize=2)
ax.set_xlim([xmin, xmax])
ax.set_ylim([ymin, ymax])
#cbar = fig.colorbar(ca)
ax.set_xlabel(Xlabel)
ax.set_ylabel(Ylabel)
ax.set_title(Title)
ax.minorticks_on()
if (SUPPLIEDLIM == 1):
    ax.set_xticks(np.arange(xmin, xmax+ticksize, ticksize))
    ax.set_yticks(np.arange(xmin, xmax+ticksize, ticksize))

#for item in ([ax.title, ax.xaxis.label, ax.yaxis.label] + ax.get_xticklabels() + ax.get_yticklabels()):
#    item.set_fontsize(18)

base=os.path.basename(readfile)
basename=os.path.splitext(base)[0]

fileout=basename+".png"
plt.savefig(fileout,dpi=300)
print("saved file "+fileout)
#plt.show()

