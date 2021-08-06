declare -a StringArray=( "74A8D1F7CB" )
for subj in ${StringArray[@]}; 
do

freeview -v ${subj}.long.${subj}_temp/mri/T1.mgz \
	    ${subj}.long.${subj}_temp/mri/brainmask.mgz \
	-f  ${subj}.long.${subj}_temp/surf/lh.pial:edgecolor=red \
	    ${subj}.long.${subj}_temp/surf/rh.pial:edgecolor=red

freeview -v ${subj}_fu.long.${subj}_temp/mri/T1.mgz \
	    ${subj}_fu.long.${subj}_temp/mri/brainmask.mgz \
	-f  ${subj}_fu.long.${subj}_temp/surf/lh.pial:edgecolor=red \
	    ${subj}_fu.long.${subj}_temp/surf/rh.pial:edgecolor=red


eval $freeview
done


