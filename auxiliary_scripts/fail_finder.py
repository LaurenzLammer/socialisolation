import os
tempfile = open("/data/hu_lammer/Desktop/subjects.txt", "r")
blfile = open("/data/hu_lammer/Desktop/bl_subjects.txt", "r")
fufile = open("/data/hu_lammer/Desktop/fu_subjects.txt", "r")
temp = tempfile.read().splitlines()
bl = blfile.read().splitlines()
fu = fufile.read().splitlines()
temp = temp[1:]
bl = bl[1:]
fu = fu[1:]
bl.remove('BDB486F522')
bl.remove('2CF0AC6B5A')
bl.remove('573DFF0165')

fu.remove('305DAA97CA')
fu.remove('3ABE1F4C41')
fu.remove('9675AE8517')
fu.remove('5064E981F9')
fu.remove('8C61CA36E5')
fu.remove('1F692731FF')
fu.remove('3E34EDEBF6')
fu.remove('7E56B243D3')
fu.remove('7494189EA4')
    fu.remove('2CF0AC6B5A')
for x in temp:
    logpath = "/data/pt_life_freesurfer/freesurfer_all/" + x + "_temp/scripts/recon-all.log"
    print(logpath)
    with open(logpath, "r") as log:
        if "finished without error at" not in log.read():
            print(x, file=open("/data/hu_lammer/Desktop/failed_temps.txt", "a"))

os.remove("/data/hu_lammer/Desktop/failed_bls.txt")
for x in bl:
    logpath = "/data/pt_life_freesurfer/freesurfer_all/" + x + ".long." + x + "_temp/scripts/recon-all.log"
    print(logpath)
    with open(logpath, "r") as log:
        if "finished without error at" not in log.read():
            print(x, file=open("/data/hu_lammer/Desktop/failed_bls.txt", "a"))

os.remove("/data/hu_lammer/Desktop/failed_fus.txt")
for x in fu:
    logpath = "/data/pt_life_freesurfer/freesurfer_all/" + x + "_fu.long." + x + "_temp/scripts/recon-all.log"
    print(logpath)
    with open(logpath, "r") as log:
        if "finished without error at" not in log.read():
            print(x, file=open("/data/hu_lammer/Desktop/failed_fus.txt", "a"))
