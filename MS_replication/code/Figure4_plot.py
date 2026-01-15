############################################################################
# Figure 4: Reduced Factor Models (FF3^F Example)
# Note: This file generates Figure 4 using the intermediate results produced by running Figure4.R. 
############################################################################

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use("Agg")

plot_data1 = pd.read_csv('../output/Figure4/Figure4.csv')

model_fin1 = plot_data1.iloc[:, 0]
labels = [str(i) + ' \n ' + model_fin1[i] for i in range(0, len(model_fin1))]

labels.append('11 \n REG')
fig, ax  = plt.subplots(figsize = (12,8))
ax.grid(False)
line1, = ax.plot(plot_data1['m'], plot_data1['GRS'], color='red', linestyle='solid', linewidth=3, label='GRS')
line2, = ax.plot(plot_data1['m'], plot_data1['Ann.SR'] * (6/4), color='blue', linestyle='dashed', linewidth=3, label='Ann.SR')

ax.set_ylim(0, 6)
ax.set_yticks(range(0, 7, 1))

ax_sec = ax.twinx()
ax_sec.set_ylim(0, 4)
ax_sec.set_yticks(range(0,5,1))
ax_sec.set_ylabel('Ann.SR', fontsize  =15)

line3 = ax.axvline(x=3.5, linestyle='--', linewidth=2, color='black', label='HDA')

ax.tick_params(axis='x', labelsize=13, length=5,direction = 'out')
ax.tick_params(axis='y', labelsize=13, length = 5, direction = 'out')
ax_sec.tick_params(axis='y', labelsize=12, length = 5, direction = 'out')

ax.set_xticks(range(0, len(labels)))
ax.set_xticklabels(labels)
ax.set_xlim(-0.3, len(labels)-0.3)
legend1 = ax.legend([line1, line2], ['GRS', 'Ann.SR'], bbox_to_anchor=(0.035,  0.99), 
                     ncol=2, loc='upper left', fontsize  =12, title='Metrics')
legend1.set_title('Metrics', prop={'size': 15})
for line in legend1.get_lines():
    line.set_linewidth(2)  
ax.set_xlabel('')
ax.set_ylabel('GRS', color='red', fontsize  =15)  
ax_sec.yaxis.label.set_color('blue')  


ax.grid(False)


fig.tight_layout()


plt.savefig('../output/Figure4/Figure4.pdf', dpi=600, bbox_inches='tight')