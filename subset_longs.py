from os import path
oldfile = open("/data/hu_lammer/Desktop/temps.txt", "r")
old = oldfile.read().splitlines()
newfile = open("/data/hu_lammer/Desktop/real_temps.txt", "r")
new = newfile.read().splitlines()
for i in range(len(old)):
    old[i] = old[i][:-5]
todo = []
for i in range(len(new)):
   if old.count(new[i]) < 1:
        todo.append(new[i]) 
for i in todo:
    bl = "/data/pt_life_freesurfer/freesurfer_all/" + i
    fu = "/data/pt_life_freesurfer/freesurfer_all/" + i + "_fu"
    if i == todo[0]:
        if path.exists(bl) and path.exists(fu):

            print("executable = /afs/cbs.mpg.de/software/scripts/envwrap", \
                  file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, " ", i, "_temp -all", sep="", \
                 file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("universe = vanilla", \
                  "output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "request_memory = 4000", \
              "request_cpus = 1", \
              "getenv = True", \
              "notification = Error","queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, "_fu ", i, "_temp -all", sep="", \
              file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
        elif path.exists(bl) and not path.exists(fu):
            print("executable = /afs/cbs.mpg.de/software/scripts/envwrap", \
                  file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, " ", i, "_temp -all", sep="", \
                 file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("universe = vanilla", \
                  "output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "request_memory = 4000", \
              "request_cpus = 1", \
              "getenv = True", \
              "notification = Error","queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
        elif path.exists(fu) and not path.exists(bl):
            print("executable = /afs/cbs.mpg.de/software/scripts/envwrap", \
                  file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, "_fu ", i, "_temp -all", sep="", \
                 file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("universe = vanilla", \
                  "output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "request_memory = 4000", \
              "request_cpus = 1", \
              "getenv = True", \
              "notification = Error","queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
    if i != todo[0]:
        if path.exists(bl) and path.exists(fu):    
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, " ", i, "_temp -all", sep="", \
              file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, "_fu ", i, "_temp -all", sep="", \
              file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
        elif path.exists(bl) and not path.exists(fu):
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, " ", i, "_temp -all", sep="", \
              file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
        elif path.exists(fu) and not path.exists(bl):
            print("arguments = FREESURFER --version 5.3.0 --subjectsdir /data/pt_life_freesurfer/freesurfer_all recon-all -long ", i, "_fu ", i, "_temp -all", sep="", \
              file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
            print("output = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/output/test.out", \
              "error = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/error/test.error", \
              "log = /data/pt_life/LLammer/practice/freesurfer_practice/life.subs/log/test.log", \
              "queue", "", sep="\n", file=open("/data/pt_life/LLammer/Analysis/Preprocessing/subset_longs_condorscript", "a"))
        

  