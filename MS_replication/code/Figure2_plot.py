############################################################################
# Figure 2: Expanded Factor Models (FF3 Example)
# Note: This file generates Figure 2 using the intermediate results produced by running Figure2.R. 
############################################################################

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use("Agg")


plot_data0 = pd.read_csv('../output/Figure2/Figure2.csv')

model_fin1 = plot_data0.iloc[:, 0]

fig, (ax1, ax3) = plt.subplots(1, 2, figsize=(12,8), sharey=True, gridspec_kw={'width_ratios': [5, 1]})


plt.subplots_adjust(wspace=0.05)  

# Plotting
df_low = plot_data0[plot_data0['m'] <= 18]
df_high = plot_data0[plot_data0['m'] >= 80]
model_fin_low = df_low.iloc[:, 0]
model_fin_low1 =  ['\n'.join(list(label)) for label in model_fin_low]
labels1 = [str(i) + ' \n ' + model_fin_low[i] for i in range(0, len(model_fin_low))]
labels1[0] = '0 \n MKT \n SMB \n HML'



line1, = ax1.plot(df_low['m'], df_low['GRS'], color='red', linestyle='solid', linewidth=3, label='GRS')
line2, = ax1.plot(df_low['m'], df_low['Ann.SR'] * (6/5), color='blue', linestyle='dashed', linewidth=3, label='Ann.SR')

line3, = ax3.plot(df_high['m'], df_high['GRS'], color='red', linestyle='solid', linewidth=3, label='GRS')
line4, = ax3.plot(df_high['m'], df_high['Ann.SR'] * (6/5), color='blue', linestyle='dashed', linewidth=3, label='Ann.SR')

 # Setting x and y axes limits and ticks
ax1.set_ylim(0, 6)
ax1.set_yticks(range(0, 7, 1))
ax1.set_ylabel('GRS', fontsize  =15)
ax1.tick_params(axis='y', labelsize=12, length = 5, direction = 'out')

ax_sec = ax3.twinx()
ax_sec.set_ylim(0, 5)
ax_sec.set_yticks(range(0,6,1))
ax_sec.set_ylabel('Ann.SR', fontsize =15)
ax_sec.tick_params(axis='y', labelsize=12, length=5, direction='out')


ax1.set_xticks(range(0, len(labels1)))
ax1.set_xticklabels(labels1, rotation=0, ha='center')
ax1.set_xlim(-0.3, len(labels1)-0.3)
ax1.tick_params(axis='x', labelsize=10, length=5,direction = 'out')
ax3.tick_params(axis='x', labelsize=10, length=5,direction = 'out')
line4 = ax1.axvline(x=8.5, linestyle='--', linewidth=2, color='black', label='HDA')


def add_diag_line(ax, pos, top):
    d = 0.015
    kwargs = dict(transform=ax.transAxes, color='k', clip_on=False)
    if top:
        ax.plot((pos-d, pos+d), (1-d, 1+d), **kwargs)  # Top diagonal line
    else:
        ax.plot((pos-d, pos+d), (-d, +d), **kwargs)  # Bottom diagonal line

    # Add 45 degree diagonal line
    x = np.linspace(pos-d, pos+d, 100)
    y = x
    ax.plot(x, y, **kwargs)

add_diag_line(ax1, 1, top=True)  # Add diagonal line with 45 degree slope to ax1
add_diag_line(ax1, 1, top=False)  # Add diagonal line with -45 degree slope to ax1
add_diag_line(ax3, 0, top=True)  # Add diagonal line with 45 degree slope to ax3
add_diag_line(ax3, 0, top=False)  # Add diagonal line with -45 degree slope to ax3


ax1.tick_params(axis='y', which='both', length=5, direction='out')  # Adding y-axis ticks back to the left panel
ax3.tick_params(axis='y', which='both', length=0)  # Ensuring right panel has no y-axis ticks


legend1 = ax1.legend([line1, line2], ['GRS', 'Ann.SR'], 
                     bbox_to_anchor=(0.05,  0.99), 
                     ncol=2,
                     loc='upper left', fontsize  =12, title='Metrics')
legend1.set_title('Metrics', prop={'size': 15})
for line in legend1.get_lines():
    line.set_linewidth(2)  

ax1.set_xlabel('')
ax1.set_ylabel('GRS', color='red', fontsize  =15)  
ax_sec.yaxis.label.set_color('blue') 



plt.savefig('../output/Figure2/Figure2.pdf', dpi=600, bbox_inches='tight')