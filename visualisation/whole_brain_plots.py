## visualise results of whole brain analyses
## if you are looking for efficient and flexible ways to visualise results similar to these, I would suggest looking into other packages - e.g. without issues with looping
## if you want to see how the whole brain figures for the manuscript were created, this code is for you
## note that we combined two of the outputs of nilearn to obtain the figures in the manuscript
## note that most figures produced with this script were not chosen for the manuscript

import copy
from nilearn import plotting
from nilearn import datasets
from nibabel import load
import numpy as np
import csv
import os
import matplotlib.pyplot as plt
os.chdir("/data/pt_life/ResearchProjects/LLammer/si_update/Results_whole_brain/")
with open("pths.csv", "r") as csvfile:
    pths = csv.reader(csvfile, delimiter=',')
    pths = list(pths)
pths = pths[1]
fsaverage = datasets.fetch_surf_fsaverage('fsaverage')

## load beta values of model 121
lh_beta = load("lh_beta_121.mgh")
rh_beta = load("rh_beta_121.mgh")
lh_beta = np.asanyarray(lh_beta.dataobj)
rh_beta = np.asanyarray(rh_beta.dataobj)
lh_beta[np.isnan(lh_beta)] = 1
rh_beta[np.isnan(rh_beta)] = 1
max_lh_beta_121 = max(lh_beta)
max_rh_beta_121 = max(rh_beta)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, subplot_kw={'projection': '3d'}, figsize = (7.5, 7.5))
plt.subplots_adjust(wspace=-0.1, hspace=-0.3)
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_left"], stat_map=lh_beta, hemi= "left", view= "lateral", colorbar=False,
                                    axes=ax1, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_left"], stat_map=lh_beta, hemi= "left", view= "medial",
                                    axes=ax2, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=rh_beta, hemi= "right", view= "lateral", colorbar=False,
                                    axes=ax3, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=rh_beta, hemi= "right", view= "medial",
                                    axes=ax4, figure= fig, cmap= "bwr")
fig.savefig("121_pial.eps", dpi = 600)
fig.savefig("121_pial.tiff", dpi = 600)


lh_beta = load("lh_beta_141.mgh")
rh_beta = load("rh_beta_141.mgh")
lh_beta = np.asanyarray(lh_beta.dataobj)
rh_beta = np.asanyarray(rh_beta.dataobj)
lh_beta[np.isnan(lh_beta)] = 1
rh_beta[np.isnan(rh_beta)] = 1
max_lh_beta_141 = max(lh_beta)
max_rh_beta_141 = max(rh_beta)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, subplot_kw={'projection': '3d'}, figsize = (7.5, 7.5))
plt.subplots_adjust(wspace=-0.1, hspace=-0.3)
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_left"], stat_map=lh_beta, hemi= "left", view= "lateral", colorbar=False,
                                    axes=ax1, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_left"], stat_map=lh_beta, hemi= "left", view= "medial",
                                    axes=ax2, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=rh_beta, hemi= "right", view= "lateral", colorbar=False,
                                    axes=ax3, figure= fig, cmap= "bwr")
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=rh_beta, hemi= "right", view= "medial",
                                    axes=ax4, figure= fig, cmap= "bwr")
fig.savefig("141_pial.eps", dpi = 600)
fig.savefig("141_pial.tiff", dpi = 600)


# load p-values
lh_sp = load("lh_sp_121.mgh")
lh_sp = np.asanyarray(lh_sp.dataobj)
lh_sp[np.isnan(lh_sp)] = 1

# plot significant p-values (pths are p-value thresholds from bi-hemispheric FDR correction in matlab)
fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["infl_left"], stat_map=lh_sp, hemi= "left", view= "medial", colorbar=True,
                                    bg_map= fsaverage["sulc_left"], bg_on_data=True, figure= fig, vmax=float(pths[0]))
fig.savefig("121_sig.tiff", dpi = 600)
fig.savefig("121_sig.eps", dpi = 600)


fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_left"], stat_map=lh_sp, hemi= "left", view= "medial", colorbar=True,
                                    bg_map= fsaverage["sulc_left"], bg_on_data=True, figure= fig, vmax=float(pths[0]))
fig.savefig("121_sig_pial.tiff", dpi = 600)
fig.savefig("121_sig_pial.eps", dpi = 600)


# load signed f-values
lh_fval = load("lhf_121.mgh")
lh_fval = np.asanyarray(lh_fval.dataobj)
lh_fval[np.isnan(lh_fval)] = 0
# only keep f-values for significant vertices
lh_f_thresholded = copy.deepcopy(lh_fval)
for n in range(0, len(lh_fval)):
    if lh_sp[n] >= float(pths[0]):
        lh_f_thresholded[n] = [[0]]
# f-values are negative to indicate reduced cortical thickness, negate
lh_fval = lh_fval * -1
lh_f_thresholded = lh_f_thresholded * -1
# find minimum significant f-value for threshold
values = []
for n in range(0, len(lh_f_thresholded)):
    if lh_f_thresholded[n][0][0] != 0:
        values.append(lh_f_thresholded[n][0][0])
thresh = min(values)

# plot significant f-values
fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["infl_left"], stat_map=lh_f_thresholded, hemi= "left", view= "medial", colorbar=True,
                                    bg_map= fsaverage["sulc_left"], bg_on_data=True, figure= fig, threshold=thresh, cmap="gnuplot")
fig.savefig("121_fthresh.eps", dpi = 600)
fig.savefig("121_fthresh.tiff", dpi = 600)

# load p-values
rh_sp = load("rh_sp_121.mgh")
rh_sp = np.asanyarray(rh_sp.dataobj)
rh_sp[np.isnan(rh_sp)] = 1

# plot significant p-values (pths are p-value thresholds from bi-hemispheric FDR correction in matlab)
fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["infl_right"], stat_map=lh_sp, hemi= "right", view= "medial", colorbar=True,
                                    bg_map= fsaverage["sulc_right"], bg_on_data=True, figure= fig, vmax=float(pths[0]))
fig.savefig("121_rh_sig.tiff", dpi = 600)
fig.savefig("121_rh_sig.eps", dpi = 600)


fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=lh_sp, hemi= "right", view= "medial", colorbar=True,
                                    bg_map= fsaverage["sulc_right"], bg_on_data=True, figure= fig, vmax=float(pths[0]))
fig.savefig("121_rh_sig_pial.tiff", dpi = 600)
fig.savefig("121_rh_sig_pial.eps", dpi = 600)

rh_sp = load("rh_sp_141.mgh")
rh_sp = np.asanyarray(rh_sp.dataobj)
rh_sp[np.isnan(rh_sp)] = 1

fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["infl_right"], stat_map=rh_sp, hemi= "right", view= "anterior", colorbar=True,
                                    bg_map= fsaverage["sulc_right"], bg_on_data=True, figure= fig, vmax=float(pths[1]))
fig.savefig("141_sig.tiff", dpi = 600)
fig.savefig("141_sig.eps", dpi = 600)

fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["pial_right"], stat_map=rh_sp, hemi= "right", view= "anterior", colorbar=True,
                                    bg_map= fsaverage["sulc_right"], bg_on_data=True, figure= fig, vmax=float(pths[1]))
fig.savefig("141_sig_pial.tiff", dpi = 600)
fig.savefig("141_sig_pial.eps", dpi = 600)

# load signed f-values
rh_fval = load("rhf_141.mgh")
rh_fval = np.asanyarray(rh_fval.dataobj)
rh_fval[np.isnan(rh_fval)] = 0
# only keep f-values for significant vertices
rh_f_thresholded = copy.deepcopy(rh_fval)
for n in range(0, len(rh_fval)):
    if rh_sp[n] >= float(pths[1]):
        rh_f_thresholded[n] = [[0]]
# f-values are negative to indicate reduced cortical thickness, negate
rh_fval = rh_fval * -1
rh_f_thresholded = rh_f_thresholded * -1
# find minimum significant f-value for threshold
values = []
for n in range(0, len(rh_f_thresholded)):
    if rh_f_thresholded[n][0][0] != 0:
        values.append(rh_f_thresholded[n][0][0])
thresh = min(values)
fig = plt.figure(figsize= (9, 5))
plotting.plot_surf_stat_map(surf_mesh=fsaverage["infl_right"], stat_map=rh_f_thresholded, hemi= "right", view= "anterior", colorbar=True,
                                    bg_map= fsaverage["sulc_right"], bg_on_data=True, figure= fig, threshold=thresh, cmap="gnuplot")
fig.savefig("141_fthresh.eps", dpi = 600)
fig.savefig("141_fthresh.tiff", dpi = 600)


