% change workind directory
cd '/data/pt_life/ResearchProjects/LLammer/Results/whole_brain'

% add FreeSurfer LME path
addpath(genpath('/afs/cbs.mpg.de/software/freesurfer/6.0.0p1/ubuntu-xenial-amd64/matlab/'));

% load stacked thickness data of all subjects and timepoints for both hemispheres 
[lY1,lmri1] = fs_read_Y('/data/pt_life/ResearchProjects/LLammer/Data/lh.thickness.stack1.fwhm10.mgh');
[rY1,rmri1] = fs_read_Y('/data/pt_life/ResearchProjects/LLammer/Data/rh.thickness.stack1.fwhm10.mgh');

% load fsaverage data to mask etc.
lhsphere = fs_read_surf('/data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.sphere');
lhcortex = fs_read_label('/data/pt_life_freesurfer/freesurfer_all/fsaverage/label/lh.cortex.label');
rhsphere = fs_read_surf('/data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/rh.sphere');
rhcortex = fs_read_label('/data/pt_life_freesurfer/freesurfer_all/fsaverage/label/rh.cortex.label');

% load table of predictor variables
data1 = fReadQdec('/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/long_qdec1.dat');
sID1 = data1(2:end,2); % identify how many timepoints per subject are in the data
data1 = rmQdecCol(data1,1);
data1 = rmQdecCol(data1,1); %remove columns
M1 = Qdec2num(data1); % create predictor matrix containing all predictors

% sort predictor matrix by subjects and create ni (array of timepoints per subject)
[M1x,lY1,ni1] = sortData(M1,2,lY1,sID1); % M1x to prevent ordering the design matrix twice 
[M1,rY1,ni1] = sortData(M1,2,rY1,sID1); 

% prepare table for hypothesis-specific threshold values
pcors = table('Size', [1 6], 'VariableTypes', ["double", "double", "double", "double", "double", "double"], 'VariableNames', ["121", "141", "122", "142", "161", "162"], 'RowNames', ["pcor"]);
pths = table('Size', [1 6], 'VariableTypes', ["double", "double", "double", "double", "double", "double"], 'VariableNames', ["121", "141", "122", "142", "161", "162"], 'RowNames', ["pth"]);

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*LSNS_bl + b5*LSNS_change) for H1.2.1 & H1.4.1
X1 = [ones(length(M1),1) M1(:,1) M1(:,2) M1(:,3) M1(:,4) M1(:,5)]; 

% fit vertex-wise LME model with random intercepts
lhstats1 = lme_mass_fit_vw(X1, [1], lY1, ni1, lhcortex);
rhstats1 = lme_mass_fit_vw(X1, [1], rY1, ni1, rhcortex);

% contrast matrix for H1.2.1
CM_121.C = [0 0 0 0 1 0];

F_lhstats_121 = lme_mass_F(lhstats1, CM_121);
F_rhstats_121 = lme_mass_F(rhstats1, CM_121);

%%% 


% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_121,lmri1,'lhsig_121.mgh','sig');
fs_write_fstats(F_rhstats_121,rmri1,'rhsig_121.mgh','sig');

% save coefficient map for baseline social isolation
nv=length(lhstats1);
lh_beta_lsns_base = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats1(i).Bhat)
      lh_beta_lsns_base(i) = lhstats1(i).Bhat(5);
   end;
end;

nv=length(rhstats1);
rh_beta_lsns_base = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats1(i).Bhat)
      rh_beta_lsns_base(i) = rhstats1(i).Bhat(5);
   end;	
end;


lmri1_1 = lmri1;
rmri1_1 = rmri1;
lmri1_1.volsz(4) = 1;
rmri1_1.volsz(4) = 1;
fs_write_Y(lh_beta_lsns_base,lmri1_1,'lh_beta_121.mgh');
fs_write_Y(rh_beta_lsns_base,rmri1_1,'rh_beta_121.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_121.pval(lhcortex) F_rhstats_121.pval(rhcortex) ];
G = [ F_lhstats_121.sgn(lhcortex) F_rhstats_121.sgn(rhcortex) ];

%FDR correction
[detvtx_121,sided_pval_121,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("121") = pcor;
pths.("121") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format
[detvtx_121_lh,lh_sided_pval_121,pth_lh] = lme_mass_FDR2(F_lhstats_121.pval,F_lhstats_121.sgn,lhcortex,0.05,-1);
[detvtx_121_rh,rh_sided_pval_121,pth_rh] = lme_mass_FDR2(F_rhstats_121.pval,F_rhstats_121.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_121,lmri1_1,'lh_sp_121.mgh');
fs_write_Y(rh_sided_pval_121,rmri1_1,'rh_sp_121.mgh');
lh_sided_pval_121 = -log10(lh_sided_pval_121);
rh_sided_pval_121 = -log10(rh_sided_pval_121);
fs_write_Y(lh_sided_pval_121,lmri1_1,'lh_spval_121.mgh');
fs_write_Y(rh_sided_pval_121,rmri1_1,'rh_spval_121.mgh');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=path/lh_spval_121.mgh:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_121.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_121.txt
% mri_surfcluster --in lh_spval_121.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_121.txt

% contrast matrix for H1.4.1
CM_141.C = [0 0 0 0 0 1];

F_lhstats_141 = lme_mass_F(lhstats1, CM_141);
F_rhstats_141 = lme_mass_F(rhstats1, CM_141);

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_141,lmri1,'lhsig_141.mgh','sig');
fs_write_fstats(F_rhstats_141,rmri1,'rhsig_141.mgh','sig');

% save coefficient map for change in social isolation
nv=length(lhstats1);
lh_beta_lsns_change = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats1(i).Bhat)
      lh_beta_lsns_change(i) = lhstats1(i).Bhat(6);
   end;
end;

nv=length(rhstats1);
rh_beta_lsns_change = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats1(i).Bhat)
      rh_beta_lsns_change(i) = rhstats1(i).Bhat(6);
   end;	
end;

fs_write_Y(lh_beta_lsns_change,lmri1_1,'lh_beta_141.mgh');
fs_write_Y(rh_beta_lsns_change,rmri1_1,'rh_beta_141.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_141.pval(lhcortex) F_rhstats_141.pval(rhcortex) ];
G = [ F_lhstats_141.sgn(lhcortex) F_rhstats_141.sgn(rhcortex) ];

%FDR correction
[detvtx_141,sided_pval_141,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("141") = pcor;
pths.("141") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format
[detvtx_141_lh,lh_sided_pval_141,pth_lh] = lme_mass_FDR2(F_lhstats_141.pval,F_lhstats_141.sgn,lhcortex,0.05,-1);
[detvtx_141_rh,rh_sided_pval_141,pth_rh] = lme_mass_FDR2(F_rhstats_141.pval,F_rhstats_141.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_141,lmri1_1,'lh_sp_141.mgh');
fs_write_Y(rh_sided_pval_141,rmri1_1,'rh_sp_141.mgh');
lh_sided_pval_141 = -log10(lh_sided_pval_141);
rh_sided_pval_141 = -log10(rh_sided_pval_141);
fs_write_Y(lh_sided_pval_141,lmri1_1,'lh_spval_141.mgh');
fs_write_Y(rh_sided_pval_141,rmri1_1,'rh_spval_141.mgh');



% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lh_spval_141.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_141.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_141.txt
% mri_surfcluster --in lh_spval_141.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_141.txt

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*LSNS_bl + b5*LSNS_change + b6*LSNS_bl*age_change) for H1.6.1 
X3 = [ones(length(M1),1) M1(:,1) M1(:,2) M1(:,3) M1(:,4) M1(:,5) M1(:,2).*M1(:,4) ]; 

% fit vertex-wise LME model with random intercepts
lhstats_int_1 = lme_mass_fit_vw(X3, [1], lY1, ni1, lhcortex);
rhstats_int_1 = lme_mass_fit_vw(X3, [1], rY1, ni1, rhcortex);

% contrast matrix for H1.6.1
CM_161.C = [0 0 0 0 0 0 1];

F_lhstats_161 = lme_mass_F(lhstats_int_1, CM_161);
F_rhstats_161 = lme_mass_F(rhstats_int_1, CM_161);

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_161,lmri1,'lhsig_161.mgh','sig');
fs_write_fstats(F_rhstats_161,rmri1,'rhsig_161.mgh','sig');

% save coefficient map for the baseline social isolation - change in age interaction
nv=length(lhstats_int_1);
lh_beta_lsns_base_age = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats_int_1(i).Bhat)
      lh_beta_lsns_base_age(i) = lhstats_int_1(i).Bhat(7);
   end;
end;

nv=length(rhstats_int_1);
rh_beta_lsns_base_age = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats_int_1(i).Bhat)
      rh_beta_lsns_base_age(i) = rhstats_int_1(i).Bhat(7);
   end;	
end;

fs_write_Y(lh_beta_lsns_base_age,lmri1_1,'lh_beta_161.mgh');
fs_write_Y(rh_beta_lsns_base_age,rmri1_1,'rh_beta_161.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_161.pval(lhcortex) F_rhstats_161.pval(rhcortex) ];
G = [ F_lhstats_161.sgn(lhcortex) F_rhstats_161.sgn(rhcortex) ];

%FDR correction
[detvtx_161,sided_pval_161,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("161") = pcor;
pths.("161") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format
[detvtx_161_lh,lh_sided_pval_161,pth_lh] = lme_mass_FDR2(F_lhstats_161.pval,F_lhstats_161.sgn,lhcortex,0.05,-1);
[detvtx_161_rh,rh_sided_pval_161,pth_rh] = lme_mass_FDR2(F_rhstats_161.pval,F_rhstats_161.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_161,lmri1_1,'lh_sp_161.mgh');
fs_write_Y(rh_sided_pval_161,rmri1_1,'rh_sp_161.mgh');
lh_sided_pval_161 = -log10(lh_sided_pval_161);
rh_sided_pval_161 = -log10(rh_sided_pval_161);
fs_write_Y(lh_sided_pval_161,lmri1_1,'lh_spval_161.mgh');
fs_write_Y(rh_sided_pval_161,rmri1_1,'rh_spval_161.mgh');

% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lh_spval_161.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_161.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_161.txt
% mri_surfcluster --in lh_spval_161.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_161.txt

% load and prepare data for model 2
% this qdec table and stack only includes observations without NAs in CESD. Henceforth, it is a bit smaller.

% load stacked thickness data of all subjects and timepoints for both hemispheres 
[lY2,lmri2] = fs_read_Y('/data/pt_life/ResearchProjects/LLammer/Data/lh.thickness.stack2.fwhm10.mgh');
[rY2,rmri2] = fs_read_Y('/data/pt_life/ResearchProjects/LLammer/Data/rh.thickness.stack2.fwhm10.mgh');

% load table of predictor variables
data2 = fReadQdec('/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/long_qdec2.dat');
sID2 = data2(2:end,2); % identify how many timepoints per subject are in the data
data2 = rmQdecCol(data2,1);
data2 = rmQdecCol(data2,1); %remove columns
M2 = Qdec2num(data2); % create predictor matrix containing all predictors

% sort predictor matrix by subjects and create ni (array of timepoints per subject)
[M2x,lY2,ni2] = sortData(M2,2,lY2,sID2); 
[M2,rY2,ni2] = sortData(M2,2,rY2,sID2);

%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*hypertension + b5*diabetes + b6*education + b7*BMI + b8*CES_D+ b9*LSNS_bl + b10*LSNS_change) for H1.2.2 & H1.4.2
X2 = [ones(length(M2),1) M2(:,1) M2(:,2) M2(:,3) M2(:,4) M2(:,5) M2(:,6) M2(:,7) M2(:,8) M2(:,9) M2(:,10)]; 

% fit vertex-wise LME model with random intercepts
lhstats2 = lme_mass_fit_vw(X2, [1], lY2, ni2, lhcortex);
rhstats2 = lme_mass_fit_vw(X2, [1], rY2, ni2, rhcortex);

% contrast matrix for H1.2.2
CM_122.C = [0 0 0 0 0 0 0 0 0 1 0];

F_lhstats_122 = lme_mass_F(lhstats2, CM_122);
F_rhstats_122 = lme_mass_F(rhstats2, CM_122);

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_122,lmri2,'lhsig_122.mgh','sig');
fs_write_fstats(F_rhstats_122,rmri2,'rhsig_122.mgh','sig');

% save coefficient map for the baseline social isolation 
nv=length(lhstats2);
lh_beta_lsns_base_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats2(i).Bhat)
      lh_beta_lsns_base_2(i) = lhstats2(i).Bhat(10);
   end;
end;

nv=length(rhstats2);
rh_beta_lsns_base_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats2(i).Bhat)
      rh_beta_lsns_base_2(i) = rhstats2(i).Bhat(10);
   end;	
end;

lmri1_2 = lmri2;
rmri1_2 = rmri2;
lmri1_2.volsz(4) = 1;
rmri1_2.volsz(4) = 1;
fs_write_Y(lh_beta_lsns_base_2,lmri1_2,'lh_beta_122.mgh');
fs_write_Y(rh_beta_lsns_base_2,rmri1_2,'rh_beta_122.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_122.pval(lhcortex) F_rhstats_122.pval(rhcortex) ];
G = [ F_lhstats_122.sgn(lhcortex) F_rhstats_122.sgn(rhcortex) ];

%FDR correction
[detvtx_122,sided_pval_122,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("122") = pcor;
pths.("122") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format 
[detvtx_122_lh,lh_sided_pval_122,pth_lh] = lme_mass_FDR2(F_lhstats_122.pval,F_lhstats_122.sgn,lhcortex,0.05,-1);
[detvtx_122_rh,rh_sided_pval_122,pth_rh] = lme_mass_FDR2(F_rhstats_122.pval,F_rhstats_122.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_122,lmri1_2,'lh_sp_122.mgh');
fs_write_Y(rh_sided_pval_122,rmri1_2,'rh_sp_122.mgh');
lh_sided_pval_122 = -log10(lh_sided_pval_122);
rh_sided_pval_122 = -log10(rh_sided_pval_122);
fs_write_Y(lh_sided_pval_122,lmri1_2,'lh_spval_122.mgh');
fs_write_Y(rh_sided_pval_122,rmri1_2,'rh_spval_122.mgh');



% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lh_spval_122.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_122.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_122.txt
% mri_surfcluster --in lh_spval_122.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_122.txt

% contrast matrix for H1.4.2
CM_142.C = [0 0 0 0 0 0 0 0 0 0 1];

F_lhstats_142 = lme_mass_F(lhstats2, CM_142);
F_rhstats_142 = lme_mass_F(rhstats2, CM_142);

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_142,lmri2,'lhsig_142.mgh','sig');
fs_write_fstats(F_rhstats_142,rmri2,'rhsig_142.mgh','sig');

% save coefficient map for the change in social isolation 
nv=length(lhstats2);
lh_beta_lsns_change_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats2(i).Bhat)
      lh_beta_lsns_change_2(i) = lhstats2(i).Bhat(11);
   end;
end;

nv=length(rhstats2);
rh_beta_lsns_change_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats2(i).Bhat)
      rh_beta_lsns_change_2(i) = rhstats2(i).Bhat(11);
   end;	
end;

fs_write_Y(lh_beta_lsns_change_2,lmri1_2,'lh_beta_142.mgh');
fs_write_Y(rh_beta_lsns_change_2,rmri1_2,'rh_beta_142.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_142.pval(lhcortex) F_rhstats_142.pval(rhcortex) ];
G = [ F_lhstats_142.sgn(lhcortex) F_rhstats_142.sgn(rhcortex) ];

%FDR correction
[detvtx_142,sided_pval_142,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("142") = pcor;
pths.("142") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format
[detvtx_142_lh,lh_sided_pval_142,pth_lh] = lme_mass_FDR2(F_lhstats_142.pval,F_lhstats_142.sgn,lhcortex,0.05,-1);
[detvtx_142_rh,rh_sided_pval_142,pth_rh] = lme_mass_FDR2(F_rhstats_142.pval,F_rhstats_142.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_142,lmri1_2,'lh_sp_142.mgh');
fs_write_Y(rh_sided_pval_142,rmri1_2,'rh_sp_142.mgh');
lh_sided_pval_142 = -log10(lh_sided_pval_142);
rh_sided_pval_142 = -log10(rh_sided_pval_142);
fs_write_Y(lh_sided_pval_142,lmri1_2,'lh_spval_142.mgh');
fs_write_Y(rh_sided_pval_142,rmri1_2,'rh_spval_142.mgh');



% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lh_spval_142.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_142.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_142.txt
% mri_surfcluster --in lh_spval_142.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_142.txt



%build design matrix (CT = b0*intercept + b1*age_bl + b2*age_change + b3*sex + b4*hypertension + b5*diabetes + b6*education + b7*BMI + b8*CES_D+ b9*LSNS_bl + b10*LSNS_change + b11*LSNS_bl*age_change) for H1.6.2 
% M(:,n).*M(:,m) creates interaction term
X4 = [ones(length(M2),1) M2(:,1) M2(:,2) M2(:,3) M2(:,4) M2(:,5) M2(:,6) M2(:,7) M2(:,8) M2(:,9) M2(:,10) M2(:,9).*M2(:,2) ]; 

% fit vertex-wise LME model with random intercepts
lhstats_int_2 = lme_mass_fit_vw(X4, [1], lY2, ni2, lhcortex);
rhstats_int_2 = lme_mass_fit_vw(X4, [1], rY2, ni2, rhcortex);

% contrast matrix for H1.6.2
CM_162.C = [0 0 0 0 0 0 0 0 0 0 0 1];

F_lhstats_162 = lme_mass_F(lhstats_int_2, CM_162);
F_rhstats_162 = lme_mass_F(rhstats_int_2, CM_162);

% write non-FDR-corrected significance map to current directory
fs_write_fstats(F_lhstats_162,lmri2,'lhsig_162.mgh','sig');
fs_write_fstats(F_rhstats_162,rmri2,'rhsig_162.mgh','sig');

% save coefficient map for the baseline social isolation - change in age interaction
nv=length(lhstats_int_2);
lh_beta_lsns_base_age_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(lhstats_int_2(i).Bhat)
      lh_beta_lsns_base_age_2(i) = lhstats_int_2(i).Bhat(12);
   end;
end;

nv=length(rhstats_int_2);
rh_beta_lsns_base_age_2 = zeros(1,nv);
for i=1:nv
   if ~isempty(rhstats_int_2(i).Bhat)
      rh_beta_lsns_base_age_2(i) = rhstats_int_2(i).Bhat(12);
   end;	
end;

fs_write_Y(lh_beta_lsns_base_age_2,lmri1_2,'lh_162.mgh');
fs_write_Y(rh_beta_lsns_base_age_2,rmri1_2,'rh_162.mgh');

% summarise data on each hemisphere to enable FDR-correction over both hemispheres at once
P = [ F_lhstats_162.pval(lhcortex) F_rhstats_162.pval(rhcortex) ];
G = [ F_lhstats_162.sgn(lhcortex) F_rhstats_162.sgn(rhcortex) ];

%FDR correction
[detvtx_162,sided_pval_162,pth] = lme_mass_FDR2(P,G,[],0.05,-1);
pcor = -log10(pth);
pcors.("162") = pcor;
pths.("162") = pth;

% get sided p-values of each hemisphere containing masked vertices to enable saving and change to freesurfer's -log10(p)-format
[detvtx_162_lh,lh_sided_pval_162,pth_lh] = lme_mass_FDR2(F_lhstats_162.pval,F_lhstats_162.sgn,lhcortex,0.05,-1);
[detvtx_162_rh,rh_sided_pval_162,pth_rh] = lme_mass_FDR2(F_rhstats_162.pval,F_rhstats_162.sgn,rhcortex,0.05,-1);
fs_write_Y(lh_sided_pval_162,lmri1_2,'lh_sp_162.mgh');
fs_write_Y(rh_sided_pval_162,rmri1_2,'rh_sp_162.mgh');
lh_sided_pval_162 = -log10(lh_sided_pval_162);
rh_sided_pval_162 = -log10(rh_sided_pval_162);
fs_write_Y(lh_sided_pval_162,lmri1_2,'lh_spval_162.mgh');
fs_write_Y(rh_sided_pval_162,rmri1_2,'rh_spval_162.mgh');


% the maps can be visualised in the freesurfer environment with the following command
% freeview -f /data/pt_life_freesurfer/freesurfer_all/fsaverage/surf/lh.inflated:annot=aparc.annot:annot_outline=1:overlay=lh_spval_162.mgh_path:overlay_threshold=pcor,15 -viewport 3d -layout 1
% thresholds can be modified in freeview

% a table of significant clusters for a hemisphere can be obtained with the following commands
% mri_surfcluster --in rh_spval_162.mgh --thmin pcor --hemi rh --srcsubj fsaverage --sum rh_clusters_162.txt
% mri_surfcluster --in lh_spval_162.mgh --thmin pcor --hemi lh --srcsubj fsaverage --sum lh_clusters_162.txt

% save the tables of threshold values and the current workspace
writetable(pcors,'pcors.csv');
writetable(pths,'pths.csv');
save('workspace.mat');


