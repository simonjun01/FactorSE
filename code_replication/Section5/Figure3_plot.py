
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import seaborn as sns


GRS_BSE = pd.read_csv('Figure3/Figure3_GRS_BSE.csv')
GRS_BSE = GRS_BSE.drop(GRS_BSE.columns[0], axis=1)
SR_BSE = pd.read_csv('Figure3/Figure3_SR_BSE.csv')
SR_BSE = SR_BSE.drop(SR_BSE.columns[0], axis=1)
model_BSE = pd.read_csv('Figure3/Figure3_model_BSE.csv')
model_BSE = model_BSE.drop(model_BSE.columns[0], axis=1)
PY_BSE = pd.read_csv('Figure3/Figure3_HDA_BSE.csv')
PY_BSE = PY_BSE.drop(PY_BSE.columns[0], axis=1)


data = GRS_BSE.T
plt.figure(figsize=(10, 5))  
cmap_coolwarm = plt.get_cmap('coolwarm')
start_color = cmap_coolwarm(0.0)  
end_color = cmap_coolwarm(0.5)  
norm = mcolors.Normalize(vmin=1, vmax=3)
cmap_blue = mcolors.LinearSegmentedColormap.from_list('blue_part', [end_color,start_color])
ax = sns.heatmap(data, cmap=cmap_coolwarm, norm = norm,linewidths=0.5) 


colorbar = ax.collections[0].colorbar

plt.xlabel('BSE Steps')
plt.ylabel('Models')
plt.yticks(np.arange(7)+0.5, ('CAPM$^{F+B}$','FF3$^{F+B}$', 'FF5$^{F+B}$', 'FF6$^{F+B}$','Q5$^{F+B}$', 'DHS3$^{F+B}$','BS6$^{F+B}$'), rotation=0)


for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        value = SR_BSE.T.iloc[i, j]
        text = model_BSE.T.iloc[i, j]  
        star = PY_BSE.T.iloc[i,j]
        if not pd.isnull(value): 
            text = ax.text(j + 0.5, i + 0.5, f"{text}\n{value:.2f}\n{star}", ha='center', va='center')


ax.spines['top'].set_visible(True)
ax.spines['bottom'].set_visible(True)
ax.spines['left'].set_visible(True)
ax.spines['right'].set_visible(True)

plt.savefig('Figure3/Figure3.pdf', dpi=600, bbox_inches='tight')