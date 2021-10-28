# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

list = open("/data/hu_lammer/Desktop/list.txt", "r")
listo = list.read().splitlines()
print(listo)
import os
for subj in listo:
    if "_fu" in subj:
        temp = subj[:-3]
        path = "/data/pt_life_freesurfer/freesurfer_all/" + str(temp) + "_fu.long." + str(temp) + "_temp/scripts/IsRunning.lh+rh"
        if os.path.exists(path):
            os.remove(path)
        else:
            print("The file for ", subj, "_fu does not exist", sep= "")
    if "_fu" not in subj:
        path = "/data/pt_life_freesurfer/freesurfer_all/" + str(subj) + ".long." + str(subj) + "_temp/scripts/IsRunning.lh+rh"
        if os.path.exists(path):
            os.remove(path)
        else:
            print("The file for ", subj, " does not exist", sep= "")
