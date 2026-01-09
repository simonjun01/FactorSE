
import subprocess
import sys

def install_package(package_name):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package_name])

required_packages = ['numpy', 'pandas', 'matplotlib', 'seaborn']

for package in required_packages:
    try:
        __import__(package)
        print(f"{package} is already installed.")
    except ImportError:
        print(f"{package} is not installed. Installing now...")
        install_package(package)


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import seaborn as sns


GRS_FSE = pd.read_csv('Figure1/Figure1_GRS_FSE.csv')
GRS_FSE = GRS_FSE.drop(GRS_FSE.columns[0], axis=1)
SR_FSE = pd.read_csv('Figure1/Figure1_SR_FSE.csv')
SR_FSE = SR_FSE.drop(SR_FSE.columns[0], axis=1)
model_FSE = pd.read_csv('Figure1/Figure1_model_FSE.csv')
model_FSE = model_FSE.drop(model_FSE.columns[0], axis=1)
HDA_FSE = pd.read_csv('Figure1/Figure1_HDA_FSE.csv')
HDA_FSE = HDA_FSE.drop(HDA_FSE.columns[0], axis=1)


data = GRS_FSE.T
plt.figure(figsize=(10, 5))  

cmap_coolwarm = plt.get_cmap('coolwarm')
#cmap_coolwarm = plt.cm.get_cmap('coolwarm')

start_color = cmap_coolwarm(0.0)
end_color = cmap_coolwarm(0.5) 

norm = mcolors.Normalize(vmin=1, vmax=6)
cmap_blue = mcolors.LinearSegmentedColormap.from_list('blue_part', [end_color,start_color])
ax = sns.heatmap(data, cmap=cmap_coolwarm, norm = norm,linewidths=0.5) 

colorbar = ax.collections[0].colorbar

plt.xlabel('FSE Steps')
plt.ylabel('Models')
plt.yticks(np.arange(7)+0.5, ('CAPM$^F$','FF3$^F$', 'FF5$^F$', 'FF6$^F$','Q5$^F$', 'DHS3$^F$','BS6$^F$'), rotation=0)


for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        value = SR_FSE.T.iloc[i, j]
        text = model_FSE.T.iloc[i, j] 
        star = HDA_FSE.T.iloc[i,j]
        if not pd.isnull(value):  
            text = ax.text(j + 0.5, i + 0.5, f"{text}\n{value:.2f}\n{star}", ha='center', va='center')


ax.spines['top'].set_visible(True)
ax.spines['bottom'].set_visible(True)
ax.spines['left'].set_visible(True)
ax.spines['right'].set_visible(True)

plt.savefig('Figure1/Figure1.pdf', dpi=600, bbox_inches='tight')



