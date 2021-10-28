from os import path
from os import remove
import csv
import shutil
import numpy as np

with open('/data/gh_gr_agingandobesity_share/life_shared/Data/Preprocessed/derivatives/pseudo_mrt_20201214.csv', newline='') as subfile:
    sublist = list(csv.reader(subfile))
sublist.pop(0)
sublist = np.array(sublist)
subs = sublist[:,1]
subs = subs.tolist()
with open('/data/gh_gr_agingandobesity_share/life_shared/Data/Preprocessed/derivatives/FreeSurfer/QA_followup/fu_long_qc.csv', newline='') as donefile:
    donelist = list(csv.reader(donefile))
donelist.pop(0)
donelist = np.array(donelist)
dones = donelist[:,0]
dones = dones.tolist()
for sub in dones:
    if sub in subs:
        subs.remove(sub)

fu_subjects = []
for i in subs:
    fu = "/data/pt_life_freesurfer/freesurfer_all/" + i + "_fu"
    long = fu + ".long." + i + "_temp"
    if path.exists(fu) and not path.exists(long):
        fu_subjects.append(i)

remove("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript")
remove("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript")

for i in fu_subjects:
    if i is fu_subjects[0]:
        print("executable = /afs/cbs.mpg.de/software/scripts/envwrap",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript", "a"))
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all ",
              "-base ", i, "_temp -tp ", i, " -tp ", i, "_fu -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript", "a"))
        print("universe = vanilla",
              "output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log",
              "request_memory = 4000", "request_cpus = 1", "getenv = True", "notification = Error", "queue", "",
              sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript", "a"))
    else:
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all "
              "-base ", i, "_temp -tp ", i, " -tp ", i, "_fu -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript", "a"))
        print("output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log", "queue", "", sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fu_temps_condorscript", "a"))

del_count = 0
for i in fu_subjects:
    temp = "/data/pt_life_freesurfer/freesurfer_all/" + i + "_temp"
    long = "/data/pt_life_freesurfer/freesurfer_all/" + i + ".long." + i + "_temp"
    dirs = [temp, long]
    for x in dirs:
        if path.exists(x):
            shutil.rmtree(x)
            print("deleted: ", x)
            del_count += 1
print("deleted ", del_count, "directories")

for i in fu_subjects:
    if i is fu_subjects[0]:
        print("executable = /afs/cbs.mpg.de/software/scripts/envwrap",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all ",
              "-long ", i, " ", i, "_temp -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("universe = vanilla",
              "output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log",
              "request_memory = 4000", "request_cpus = 1", "getenv = True", "notification = Error", "queue", "",
              sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all "
              "-long ", i, "_fu ", i, "_temp -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log", "queue", "", sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
    else:
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all "
              "-long ", i, " ", i, "_temp -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log", "queue", "", sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all "
              "-long ", i, "_fu ", i, "_temp -all", sep="",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))
        print("output = /data/pt_life/ResearchProjects/LLammer/practice/test.out",
              "error = /data/pt_life/ResearchProjects/LLammer/practice/test.error",
              "log = /data/pt_life/ResearchProjects/LLammer/practice/test.log", "queue", "", sep="\n",
              file=open("/data/pt_life/ResearchProjects/LLammer/Analysis/Preprocessing/further_fus_condorscript", "a"))




