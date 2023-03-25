import pandas as pd
import glob
import os
os.chdir('/data/pt_life/ResearchProjects/LLammer/si_update/Results_whole_brain/')
file_list = glob.glob('*_clusters_*.txt')
overview = []
for file in file_list:
    name = file[:-4]
    with open(file, newline='') as results:
        lines = results.readlines()
    commentlines = []
    resultlines = []
    for line in lines:
        if line.startswith("#"):
            commentlines.append(line)
        else:
            resultlines.append(line)
    if len(resultlines) >= 1:
        resultlines.insert(0, commentlines[-1])

        for n in range(len(resultlines)):
            resultlines[n] = resultlines[n].replace("# ", "")
            resultlines[n] = ' '.join(resultlines[n].split())
            resultlines[n] = resultlines[n].replace("\n", "")
            resultlines[n] = resultlines[n].split(" ")
        header = resultlines.pop(0)
        for n in range(len(resultlines)):
            resultlines[n].insert(0,name)
            overview.append(resultlines[n])


overview = pd.DataFrame(overview)
header.insert(0,"model")
overview.to_csv(path_or_buf= "cluster_overview.csv", index=False, header=header)
