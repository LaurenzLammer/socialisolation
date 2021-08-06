zerofile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list0.txt", "r")
zero = zerofile.read().splitlines()
onefile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list1.txt", "r")
one = onefile.read().splitlines()
twofile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list2.txt", "r")
two = twofile.read().splitlines()
threefile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list3.txt", "r")
three = threefile.read().splitlines()
fourfile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list4.txt", "r")
four = fourfile.read().splitlines()
fivefile = open("/data/pt_life/LLammer/Analysis/Preprocessing/long_tp_list5.txt", "r")
five = fivefile.read().splitlines()
from itertools import chain
from os import path
for subj in chain(zero, one, two, three, four, five):
    bl = "/data/pt_life_freesurfer/freesurfer_all/" + subj
    fu = "/data/pt_life_freesurfer/freesurfer_all/" + subj + "_fu"
    blpath = bl + ".long." + subj + "_temp"
    fupath = fu + ".long." + subj + "_temp"
    bllogpath = blpath + "/scripts/recon-all.log"
    fulogpath = fupath + "/scripts/recon-all.log"

    if path.exists(bl) and path.exists(fu):
        if not path.exists(blpath):
            print(subj, file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
        if not path.exists(fupath):
            print(subj, "_fu", sep="", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
        if path.exists(blpath):
            with open(bllogpath, "r") as bllog:
                if "finished without error at" not in bllog.read():
                    print(subj, file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
        if path.exists(fupath):
            with open(fulogpath, "r") as fulog:
                if "finished without error at" not in fulog.read():
                    print(subj, "_fu", sep="", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))

    if path.exists(bl) and not path.exists(fu):
        if not path.exists(blpath):
            print(subj, file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
        if path.exists(blpath):
            with open(bllogpath, "r") as bllog:
                if "finished without error at" not in bllog.read():
                    print(subj, file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
    if path.exists(fu) and not path.exists(bl):
        if not path.exists(fupath):
            print(subj, "_fu", sep="", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
        if path.exists(fupath):
            with open(fulogpath, "r") as fulog:
                if "finished without error at" not in fulog.read():
                    print(subj, "_fu", sep="", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/failedlongs.txt", "a"))
                    
