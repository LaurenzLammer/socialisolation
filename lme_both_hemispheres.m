% change workind directory
cd '/data/pt_life/LLammer/Results/whole_brain'

% add FreeSurfer LME path
addpath(genpath('/afs/cbs.mpg.de/software/freesurfer/6.0.0p1/ubuntu-xenial-amd64/matlab/'));

% load stacked thickness data of all subjects and timepoints for both hemispheres 
[lY,lmri] = fs_read_Y('/data/pt_life/LLammer/Data/lh.thickness.stack1.fwhm10.mgh');
[rY,rmri] = fs_read_Y('/data/pt_life/LLammer/Data/rh.thickness.stack1.fwhm10.mgh');

% load fsaverage data to mask etc.
lhsphere = fs_read_surf('/data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.sphere');
lhcortex = fs_read_label('/data/pt_life_freesurfer/freesurfer_all/fsaverage/label/lh.cortex.label');
rhsphere = fs_read_surf('/data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/rh.sphere');
rhcortex = fs_read_label('/data/pt_life_freesurfer/freesurfer_all/fsaverage/label/rh.cortex.label');

% load table of predictor variables
data = fReadQdec('/data/pt_life/LLammer/Analysis/Preprocessing/long_qdec1.dat');
sID = data(2:end,2); % identify how many timepoints per subject are in the data
data = rmQdecCol(data,1);
data = rmQdecCol(data,1); %remove columns
M = Qdec2num(data); % create predictor matrix containing all predictors

% sort predictor matrix by subjects and create ni (array of timepoints per subject)
[M,lY,ni] = sortData(M,5,lY,sID); 
[M,rY,ni] = sortData(M,5,rY,sID); 

% prepare table for hypothesis-specific threshold values
pcors = table('Size', [1 6], 'VariableTypes', ["double", "double", "double", "double", "double", "double"], 'VariableNames', ["121", "141", "122", "142", "161", "162"], 'RowNames', ["pcor"]);

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*LSNS_bl + b5*LSNS_change) for H1.2.1 & H1.4.1
X1 = [ones(length(M),1) M(:,1) M(:,2) M(:,3) M(:,4) M(:,5)]; 

% fit vertex-wise LME model with random intercepts
lhstats = lme_mass_fit_vw(X1, [1], lY, ni, lhcortex);
rhstats = lme_mass_fit_vw(X1, [1], rY, ni, rhcortex);

% contrast matrix for H1.2.1
CM.C = [0 0 0 0 1 0];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("121") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_121.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_121.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_121.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_121.txt
% mri_surfcluster --in lhsig_121.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_121.txt

% contrast matrix for H1.4.1
CM.C = [0 0 0 0 0 1];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("141") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_141.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_141.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_141.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_141.txt
% mri_surfcluster --in lhsig_141.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_141.txt

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*LSNS_bl + b5*LSNS_change + b6*LSNS_bl*age_change) for H1.6.1 
X3 = [ones(length(M),1) M(:,1) M(:,2) M(:,3) M(:,4) M(:,5) M(:,2).*M(:,4) ]; 

% fit vertex-wise LME model with random intercepts
lhstats = lme_mass_fit_vw(X3, [1], lY, ni, lhcortex);
rhstats = lme_mass_fit_vw(X3, [1], rY, ni, rhcortex);

% contrast matrix for H1.6.1
CM.C = [0 0 0 0 0 0 1];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("161") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_161.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_161.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_161.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_161.txt
% mri_surfcluster --in lhsig_161.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_161.txt

% load and prepare data for model 2
% this qdec table and stack only includes observations without NAs in CESD. Henceforth, it is a bit smaller.

% load stacked thickness data of all subjects and timepoints for both hemispheres 
[lY,lmri] = fs_read_Y('/data/pt_life/LLammer/Data/lh.thickness.stack2.fwhm10.mgh');
[rY,rmri] = fs_read_Y('/data/pt_life/LLammer/Data/rh.thickness.stack2.fwhm10.mgh');

% load table of predictor variables
data = fReadQdec('/data/pt_life/LLammer/Analysis/Preprocessing/long_qdec2.dat');
sID = data(2:end,2); % identify how many timepoints per subject are in the data
data = rmQdecCol(data,1);
data = rmQdecCol(data,1); %remove columns
M = Qdec2num(data); % create predictor matrix containing all predictors

% sort predictor matrix by subjects and create ni (array of timepoints per subject)
[M,lY,ni] = sortData(M,5,lY,sID); 
[M,rY,ni] = sortData(M,5,rY,sID);

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*hypertension + b5*diabetes + b6*education + b7*BMI + b8*CES_D+ b9*LSNS_bl + b10*LSNS_change) for H1.2.2 & H1.4.2
X2 = [ones(length(M),1) M(:,1) M(:,2) M(:,3) M(:,4) M(:,5) M(:,6) M(:,7) M(:,8) M(:,9) M(:,10)]; 

% fit vertex-wise LME model with random intercepts
lhstats = lme_mass_fit_vw(X2, [1], lY, ni, lhcortex);
rhstats = lme_mass_fit_vw(X2, [1], rY, ni, rhcortex);

% contrast matrix for H1.2.2
CM.C = [0 0 0 0 0 0 0 0 0 1 0];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("122") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_122.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_122.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_122.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_122.txt
% mri_surfcluster --in lhsig_122.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_122.txt

% contrast matrix for H1.4.2
CM.C = [0 0 0 0 0 0 0 0 0 0 1];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("142") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_142.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_142.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_142.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_142.txt
% mri_surfcluster --in lhsig_142.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_142.txt



%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*hypertension + b5*diabetes + b6*education + b7*BMI + b8*CES_D+ b9*LSNS_bl + b10*LSNS_change + b11*LSNS_bl*age_change) for H1.6.2 
% M(:,n).*M(:,m) creates interaction term
X4 = [ones(length(M),1) M(:,1) M(:,2) M(:,3) M(:,4) M(:,5) M(:,6) M(:,7) M(:,8) M(:,9) M(:,10) M(:,9).*M(:,2) ]; 

% fit vertex-wise LME model with random intercepts
lhstats = lme_mass_fit_vw(X4, [1], lY, ni, lhcortex);
rhstats = lme_mass_fit_vw(X4, [1], rY, ni, rhcortex);

% contrast matrix for H1.6.2
CM.C = [0 0 0 0 0 0 0 0 0 0 0 1];

F_lhstats = lme_mass_F(lhstats, CM);
F_rhstats = lme_mass_F(rhstats, CM);

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats.pval(lhcortex) F_rhstats.pval(rhcortex) ];
G = [ F_lhstats.sgn(lhcortex) F_rhstats.sgn(rhcortex) ];

%FDR correction
[detvtx,sided_pval,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("162") = pcor;

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats,lmri,'lhsig_162.mgh','sig');
fs_write_fstats(F_rhstats,rmri,'rhsig_162.mgh','sig');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lhsig.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rhsig_162.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_162.txt
% mri_surfcluster --in lhsig_162.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_162.txt

% save the table of threshold values
writetable(pcors,'pcors.csv');


