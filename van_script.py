# -*- coding: utf-8 -*-
"""
Created on Tue Jan 24 12:19:55 2023

@author: Lucja Doradzinska <l.doradzinska@nencki.edu.pl>
"""

from collections import OrderedDict
import os

import erp_exp_ld as my

# path to ff_lab_mn module
os.chdir('E:\\ff_prep_scripts\\')
import ff_lab_mn

# path to experiments folder
path = 'D:\\ff_experiment\\'

# data structure
eeg_folder = 'Results\\EEG\\'
eeg_log_folder = eeg_folder
epochs_clean_folder = eeg_folder + 'epochs_clean\\'
evokeds_folder = eeg_folder + 'evokeds\\'
erps_folder = eeg_folder + 'ERPs\\'
van_folder = erps_folder + 'VAN\\'
comps_long_folder = eeg_folder + 'comps_long\\'

beh_folder = 'Results\\behavior\\'
raw_beh_df_folder = beh_folder + 'raw_beh_df\\'
beh_log_folder = beh_folder + 'beh_logs\\'
# %% VARIABLES

subjects = ['009', '010', '011', '012', '013', '014', '015', '016', '017', '018',
            '019', '020', '021', '022', '023', '024', '025', '026', '027', '028',
            '029', '030', '031', '032', '033', '034', '035', '036', '037', '038',
            '039', '040', '041', '042', '043', '044', '045', '046', '047', '048',
            '049']

# EEG MARKERS
eeg_stim_coding = OrderedDict({'task': {'DP': 1, 'ID': 65},
                               'masking': {'masked': 0, 'unmasked': 32},
                               'faces': {'F-F': 0, 'F-N': 8, 'N-F': 16, 'N-N': 24},
                               'gender': {'fem': 0, 'male': 4}})
reduce_factors = ['gender']
markers = ff_lab_mn.Eeg_markers_manager(eeg_stim_coding, reduce_factors)
quest_markers = [150, 151, 152, 153]
resp_markers = [201, 211, 202, 212]

# ERP CONDITIONS
symm_contrast = {'task': ['DP', 'ID'], 'masking': [
    'masked', 'unmasked'], 'faces': ['F-F', 'N-N']}
lat_contrast = {'task': ['DP', 'ID'], 'masking': [
    'masked', 'unmasked'], 'faces': ['F-N', 'N-F']}




#%% BEHAVIORAL ANALYSIS

beh_analizer = my.beh.Beh_analizer(subjects, path, raw_beh_df_folder, beh_log_folder)
beh_long_df = beh_analizer.get_beh_raw_long ('raw_beh_long', save_csv = True)

neutral_combine = {'task':['ID'],
                   'masking':['masked', 'unmasked'],
                   'contrast':['incongr', 'neutr-symm']}
neutral_compare = ff_lab_mn.beh_combine_conds(neutral_combine)
neutr_rates_log = beh_analizer.calculate_rates('neutr_ID_rates_log', neutral_compare, ['incorrect'], save_csv = True)

fearful_combine = {'task':['ID'],
                   'masking':['masked', 'unmasked'],
                   'contrast':['congr', 'fear-symm']}
fearful_compare = ff_lab_mn.beh_combine_conds(fearful_combine)
fear_rates_log = beh_analizer.calculate_rates('fear_ID_rates_log', fearful_compare, ['correct'], save_csv = True)

d_combine = {'task':['ID'],
             'masking':['masked', 'unmasked'],
             'emo_congr':['congr', 'incongr'] }
d_compare = ff_lab_mn.beh_combine_conds(d_combine)
signal = {'target':['fearful'], }
noise = {'target':['neutral']}
d_log, c_log, hit_fa_log = beh_analizer.calculate_sdt_params('ID_emo_congr', d_compare, signal, noise, d_corr = True, save_csv = True)

d_combine = {'task':['ID'],
             'masking':['masked', 'unmasked'],
             'dist_face':['fearful', 'neutral'] }
d_compare = ff_lab_mn.beh_combine_conds(d_combine)
signal = {'target':['fearful'], }
noise = {'target':['neutral']}
d_log, c_log, hit_fa_log = beh_analizer.calculate_sdt_params('ID_dist_face', d_compare, signal, noise, d_corr = True, save_csv = True)


# %% ERP CALCULATING AND PLOTTING

evokeds_labels = markers.evoked_labels()[0]

erp = my.erps.Erp_waveform(subjects, path, evokeds_folder, evokeds_labels)

plot_folder = van_folder + 'plots\\'

erp_labels = markers.erp_labels()

erp_name = 'all'
erp.mean_topo(plot_folder, erp_name, erp_labels, 0.07, 0.38, time_step=0.05, volt_min=-6, volt_max=6,
              cmap='magma', save_format='svg')

centr_rois = {'temporal':['P7', 'P8', 'PO7', 'PO8', 'P9', 'P10'],
              'occipital':['PO3', 'POz', 'PO4', 'O1', 'Oz', 'O2']}
left_roi =  ['P7', 'P9', 'PO7']
right_roi = ['P8', 'P10', 'PO8']
parietal = ['CP1', 'CPz', 'CP2', 'P1', 'Pz', 'P2']
time_windows = {'early': [140, 200], 'late': [200, 350]}
P3b_tw = [350, 650]
xlim = [-50, 700]
x_ticks = [0, 100, 200, 300, 400, 500, 600, 700]
fig_size = [9,6]
for roi in centr_rois:
    for tw in time_windows:
        erp.comp_plot(plot_folder, 'avg_' + roi + '_' + tw, erp_labels, centr_rois[roi], plot_time_wind = True, 
                      time_wind = time_windows[tw], colors=['#a9327dff'], legend=[], fig_size = fig_size, xlim = xlim, 
                      x_ticks = x_ticks, save_format='svg')
 
erp.comp_plot(plot_folder, 'avg_P3b', erp_labels, parietal, plot_time_wind = True, 
                      time_wind = P3b_tw, colors=['#a9327dff'], legend=[], fig_size = fig_size, xlim = xlim, 
                      x_ticks = x_ticks, save_format='png')

#VAN calc and plotting
contrast = {'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
           'task': {'irrelevant': ['DP'], 'relevant': ['ID']},
           'faces': {'neutral': ['N-N'], 'fearful': ['F-F']}}

contrast_left = {'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
                 'task': {'irrelevant': ['DP'], 'relevant': ['ID']},
                 'faces': {'': ['F-N']},}

contrast_right = {'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
                  'task': {'irrelevant': ['DP'], 'relevant': ['ID']},
                 'faces': {'': ['N-F']}}

erp_labels = markers.erp_labels(contrast = contrast)
erp_labels_left = markers.erp_labels(contrast = contrast_left)
erp_labels_right = markers.erp_labels(contrast = contrast_right)

for roi in centr_rois:
    for tw in time_windows:
        erp.calc_central_erp_amp(van_folder, 'VAN_' + roi + '_' + tw, erp_labels, centr_rois[roi], time_windows[tw])


for tw in time_windows: 
    erp.calc_lateral_erp_amp(van_folder, 'VAN_lat_' + tw, erp_labels_left, erp_labels_right, 
                             left_roi, right_roi, time_windows[tw])


legend = ['']
legend_loc = (0.5, 1.1)
colors_centr = ["#f9cbaaff", "#fb8961ff","#9066a0f0", "#762181ff","#f9cbaaff", "#fb8961ff","#9066a0f0", "#762181ff"]
colors_lat = ['#db87abff','#c33b75ff','#2b1f76ff', '#120d31ff', '#db87abff','#c33b75ff','#2b1f76ff', '#120d31ff']
styles = ['--', '--' ,'--', '--' ,'-', '-', '-', '-']


for roi in centr_rois:
    for tw in time_windows:
        erp.comp_plot(plot_folder, 'VAN_' + roi + '_' + tw, erp_labels, centr_rois[roi], plot_time_wind=True, 
                      time_wind = time_windows[tw], colors = colors_centr, styles = styles, 
                      legend = legend, legend_loc = legend_loc, fig_size = fig_size, xlim = xlim, 
                      x_ticks = x_ticks, save_format = 'png')

for tw in time_windows: 
    erp.lat_comp_plot(plot_folder, 'VAN_lat_' + tw, erp_labels_left, erp_labels_right, 
                      left_roi, right_roi, plot_time_wind = True, time_wind = time_windows[tw], 
                      colors = colors_lat, styles = styles, legend = legend, legend_loc = legend_loc, 
                      fig_size = fig_size, xlim = xlim,x_ticks = x_ticks, save_format = 'png')
    
erp.comp_plot(plot_folder, 'P3b', erp_labels, parietal, plot_time_wind = True, 
                      time_wind = P3b_tw, colors = colors_centr, styles = styles, 
                      legend = legend, legend_loc = legend_loc, fig_size = fig_size, xlim = xlim, 
                      x_ticks = x_ticks, save_format = 'png')


#differential plot
comp_by = {'task':{'irrelevant':'DP','relevant':'ID'}, 'faces':{'neutral':'N-N', 'fearful':'F-F'}}

comp_by_left = {'task':{'irrelevant':'DP','relevant':'ID'}, 'faces':{'':'F-N'}}

comp_by_right = {'task':{'irrelevant':'DP','relevant':'ID'}, 'faces':{'':'N-F'}}

contrast = {'masking':{'masked':['masked'], 'unmasked':['unmasked']}}

erp_labels = markers.erp_labels(comp_by, contrast) 
erp_labels_left = markers.erp_labels(comp_by_left, contrast)
erp_labels_right = markers.erp_labels(comp_by_right, contrast)


colors_centr = ["#aeaeaeff", "#4f4f4fff"]
colors_lat = ["#aeaeaeff", "#4f4f4fff"]
ylim = [-5, 4.5]
y_ticks = [-4.5, -3, -1.5, 0, 1.5, 3, 4.5]
styles = ['--', '--' ,'-', '-' ]


for roi in centr_rois:
    for tw in time_windows:
        erp.diff_plot(plot_folder, 'VAN_diff_' + roi + '_'+ tw , erp_labels, centr_rois[roi], plot_time_wind=True,
                      time_wind = time_windows[tw], colors = colors_centr,styles = styles,  legend=[''], legend_loc = legend_loc, 
                      fig_size = fig_size, ylim = ylim, y_ticks = y_ticks, xlim = xlim, x_ticks = x_ticks, save_format = 'png')
        
for tw in time_windows:       
    erp.lat_diff_plot (plot_folder, 'VAN_diff_lat_' + tw, erp_labels_left, erp_labels_right, left_roi, right_roi, 
                   plot_time_wind = True, time_wind = time_windows[tw], colors = colors_lat, styles = styles, legend=[''], 
                   legend_loc=legend_loc, fig_size = fig_size, ylim = ylim, y_ticks = y_ticks,  xlim = xlim, x_ticks = x_ticks, 
                   save_format = 'png')
    
    
erp.diff_plot(plot_folder, 'P3b_diff', erp_labels, parietal, plot_time_wind = True, 
                      time_wind = P3b_tw, colors = colors_centr, styles = styles, legend=[''], legend_loc = legend_loc, 
                      fig_size = fig_size, ylim = ylim, y_ticks = y_ticks, xlim = xlim, x_ticks = x_ticks, save_format = 'png')    






    
# differential topoplots
contrast = {'masking':{'unmasked':['unmasked'], 'masked':['masked']}}
erp_labels = markers.erp_labels(contrast = contrast)
erp.diff_topo(plot_folder, 'diff_masking', erp_labels, 0.07, 0.38, time_step=0.05, volt_min =-6, volt_max = 6, 
              cmap='magma', save_format='svg')

contrast = {'task':{ 'relevant':['ID'], 'irrelevant':['DP'],}}
erp_labels = markers.erp_labels(contrast = contrast)
erp.diff_topo(plot_folder, 'diff_task', erp_labels, 0.07, 0.38, time_step=0.05, volt_min = -4, volt_max = 4, 
              cmap='magma', save_format='svg')

contrast = {'faces': {'fearful': ['F-F'], 'neutral': ['N-N']}}
contrast_left = {'faces': {'': ['F-N'],}}
contrast_right = {'faces': {'': ['N-F'],}}
comp_by = {'masking':{'unmasked':'unmasked'}}
erp_labels = markers.erp_labels(comp_by, contrast)
erp_labels_left = markers.erp_labels(comp_by, contrast_left)
erp_labels_right = markers.erp_labels(comp_by, contrast_right)
erp_labels_left = markers.erp_labels(contrast = contrast_left)
erp_labels_right = markers.erp_labels(contrast = contrast_right)
erp.diff_topo(plot_folder, 'diff_stim', erp_labels, 0.07, 0.38, time_step=0.05, volt_min = -1.5, volt_max = 1.5, 
              cmap='magma', save_format='svg')
#erp.lat_diff_topo(plot_folder, 'diff_lat_stim ', erp_labels_left, erp_labels_right, 0.07, 0.38, time_step=0.05, volt_min = 0, volt_max = 1, 
#                  cmap='Purples', save_format='png') #?????




# %% EXTRACT COMPONENTS TRIAL BY TRIAL

reduce_markers = markers.reduce_markers()
evoked_labels = markers.evoked_labels()
epochs_data = my.comps_long.Trials_data(subjects, path, epochs_clean_folder, reduce_markers, evoked_labels=evoked_labels)

centr_rois = {'temporal':['P7', 'P8', 'PO7', 'PO8', 'P9', 'P10'],
              'occipital':['PO3', 'POz', 'PO4', 'O1', 'Oz', 'O2']}
left_roi =  ['P7', 'P9', 'PO7']
right_roi = ['P8', 'P10', 'PO8']

time_windows = {'early': [140, 200], 'late': [200, 350]}
conditions = {'task': {'ID': ['ID'], 'DP': ['DP']}, 'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
              'faces': {'neutral': ['N-N'], 'fearful': ['F-F']}}
long_labels = markers.comp_long_labels(conditions)

for roi in centr_rois:
    for tw in time_windows:
        epochs_data.get_central_comp(comps_long_folder, 'VAN_' + roi + '_' + tw, long_labels, centr_rois[roi], time_windows[tw], 
                                     integrate_beh = True,  beh_folder = raw_beh_df_folder, beh_conds = {'response':['correct', 'incorrect']})
    


conditions_left = {'task': {'ID': ['ID'], 'DP': ['DP']}, 'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
                   'faces': {'left': ['F-N']}}
conditions_right = {'task': {'ID': ['ID'], 'DP': ['DP']}, 'masking': {'masked': ['masked'], 'unmasked': ['unmasked']},
                    'faces': {'right': ['N-F']}}
long_labels_left = markers.comp_long_labels(conditions_left)
long_labels_right = markers.comp_long_labels(conditions_right)

for tw in ['early', 'late']:
    epochs_data.get_lateral_comp(comps_long_folder, 'VAN_lat_' + tw, long_labels_left, long_labels_right, left_roi, right_roi, time_windows[tw],
                                 integrate_beh = True,  beh_folder = raw_beh_df_folder, beh_conds = {'response':['correct', 'incorrect']})


