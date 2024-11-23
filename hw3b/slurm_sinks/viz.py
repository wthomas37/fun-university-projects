import numpy as np
import pandas as pd
import scipy.io as sio
import matplotlib.pyplot as plt

foh = open("oh_output.txt", "r")

fus1000 = open("us1000_output.txt", "r")
fus2000 = open("us2000_output.txt", "r")
fus4000 = open("us4000_output.txt", "r")
fus8000 = open("us8000_output.txt", "r")

fes1000 = open("es1000_output.txt", "r")
fes2000 = open("es2000_output.txt", "r")
fes4000 = open("es4000_output.txt", "r")
fes8000 = open("es8000_output.txt", "r")

fawe = open("awe_output.txt", "r")

foh_lines = foh.readlines()

fus1000_lines = fus1000.readlines()
fus2000_lines = fus2000.readlines()
fus4000_lines = fus4000.readlines()
fus8000_lines = fus8000.readlines()

fes1000_lines = fes1000.readlines()
fes2000_lines = fes2000.readlines()
fes4000_lines = fes4000.readlines()
fes8000_lines = fes8000.readlines()

fawe_lines = fawe.readlines()

foh.close()
fus1000.close()
fus2000.close()
fus4000.close()
fus8000.close()
fes1000.close()
fes2000.close()
fes4000.close()
fes8000.close()
fawe.close()

# OVERHEAD DF

oh_df = pd.DataFrame(columns=('version', 'work', 'lock', 'time'))

for line in foh_lines:
    segmented = line.split(" ")
    version_val = int(segmented[0])
    work_val = int(segmented[1])
    lock_val = int(segmented[2])
    time_val = float(segmented[3])
    oh_df.loc[len(oh_df.index)] = [version_val, work_val, lock_val, time_val]

# print(oh_df)

oh_df_lf = oh_df[:6].reset_index(drop=True)
oh_df_ttas = oh_df[(oh_df.lock == 1)].reset_index(drop=True)
oh_df_mutex = oh_df[(oh_df.lock == 2)].reset_index(drop=True)

print(oh_df_lf)
print(oh_df_ttas)
print(oh_df_mutex)

oh_df_ttas_ratio = oh_df_ttas
oh_df_ttas_ratio['time'] = oh_df_lf['time'] / oh_df_ttas['time']

oh_df_mutex_ratio = oh_df_mutex
oh_df_mutex_ratio['time'] = oh_df_lf['time'] / oh_df_mutex['time']

# print(oh_df_ttas_ratio)
# print(oh_df_mutex_ratio)

# plt.plot(oh_df_ttas_ratio['work'], oh_df_ttas_ratio['time'])
# plt.plot(oh_df_mutex_ratio['work'], oh_df_mutex_ratio['time'])
# plt.legend(['TTAS', 'Mutex'])
# plt.title('Idle Lock Overhead')
# plt.ylabel('Speedup')
# plt.xlabel('Work')
# plt.show()

# UNIFORM SPEEDUP

us1000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
us2000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
us4000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
us8000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))

for idx in range(len(fus1000_lines)):
    segmented1 = fus1000_lines[idx].split(" ")
    segmented2 = fus2000_lines[idx].split(" ")
    segmented4 = fus4000_lines[idx].split(" ")
    segmented8 = fus8000_lines[idx].split(" ")

    us1000_df.loc[len(us1000_df.index)] = [int(segmented1[0]), int(segmented1[1]), int(segmented1[2]), float(segmented1[3])]
    us2000_df.loc[len(us2000_df.index)] = [int(segmented2[0]), int(segmented2[1]), int(segmented2[2]), float(segmented2[3])]
    us4000_df.loc[len(us4000_df.index)] = [int(segmented4[0]), int(segmented4[1]), int(segmented4[2]), float(segmented4[3])]
    us8000_df.loc[len(us8000_df.index)] = [int(segmented8[0]), int(segmented8[1]), int(segmented8[2]), float(segmented8[3])]

# make the serial dataframes
us1000_df_serial = us1000_df[(us1000_df.version == -1)].reset_index(drop=True)
us2000_df_serial = us2000_df[(us2000_df.version == -1)].reset_index(drop=True)
us4000_df_serial = us4000_df[(us4000_df.version == -1)].reset_index(drop=True)
us8000_df_serial = us8000_df[(us8000_df.version == -1)].reset_index(drop=True)

# make the lockfree dataframes
us1000_df_lf = us1000_df[(us1000_df.version == 0)].reset_index(drop=True)
us2000_df_lf = us2000_df[(us2000_df.version == 0)].reset_index(drop=True)
us4000_df_lf = us4000_df[(us4000_df.version == 0)].reset_index(drop=True)
us8000_df_lf = us8000_df[(us8000_df.version == 0)].reset_index(drop=True)

# make the ttas dataframes
us1000_df_ttas = us1000_df[(us1000_df.lock == 1)].reset_index(drop=True)
us2000_df_ttas = us2000_df[(us2000_df.lock == 1)].reset_index(drop=True)
us4000_df_ttas = us4000_df[(us4000_df.lock == 1)].reset_index(drop=True)
us8000_df_ttas = us8000_df[(us8000_df.lock == 1)].reset_index(drop=True)

# make the mutex dataframes
us1000_df_mutex = us1000_df[(us1000_df.lock == 2)].reset_index(drop=True)
us2000_df_mutex = us2000_df[(us2000_df.lock == 2)].reset_index(drop=True)
us4000_df_mutex = us4000_df[(us4000_df.lock == 2)].reset_index(drop=True)
us8000_df_mutex = us8000_df[(us8000_df.lock == 2)].reset_index(drop=True)

# lock-free ratio dataframes
us1000_df_lf_ratio = us1000_df_lf
us2000_df_lf_ratio = us2000_df_lf
us4000_df_lf_ratio = us4000_df_lf
us8000_df_lf_ratio = us8000_df_lf

us1000_df_lf_ratio['time'] = us1000_df_serial['time'] / us1000_df_lf['time']
us2000_df_lf_ratio['time'] = us2000_df_serial['time'] / us2000_df_lf['time']
us4000_df_lf_ratio['time'] = us4000_df_serial['time'] / us4000_df_lf['time']
us8000_df_lf_ratio['time'] = us8000_df_serial['time'] / us8000_df_lf['time']

# ttas ratio dataframes
us1000_df_ttas_ratio = us1000_df_ttas
us2000_df_ttas_ratio = us2000_df_ttas
us4000_df_ttas_ratio = us4000_df_ttas
us8000_df_ttas_ratio = us8000_df_ttas

us1000_df_ttas_ratio['time'] = us1000_df_serial['time'] / us1000_df_ttas['time']
us2000_df_ttas_ratio['time'] = us2000_df_serial['time'] / us2000_df_ttas['time']
us4000_df_ttas_ratio['time'] = us4000_df_serial['time'] / us4000_df_ttas['time']
us8000_df_ttas_ratio['time'] = us8000_df_serial['time'] / us8000_df_ttas['time']

# mutex ratio dataframes
us1000_df_mutex_ratio = us1000_df_mutex
us2000_df_mutex_ratio = us2000_df_mutex
us4000_df_mutex_ratio = us4000_df_mutex
us8000_df_mutex_ratio = us8000_df_mutex

us1000_df_mutex_ratio['time'] = us1000_df_serial['time'] / us1000_df_mutex['time']
us2000_df_mutex_ratio['time'] = us2000_df_serial['time'] / us2000_df_mutex['time']
us4000_df_mutex_ratio['time'] = us4000_df_serial['time'] / us4000_df_mutex['time']
us8000_df_mutex_ratio['time'] = us8000_df_serial['time'] / us8000_df_mutex['time']

# 1000 plot
# plt.plot(us1000_df_lf_ratio['workers'], us1000_df_lf_ratio['time'])
# plt.plot(us1000_df_ttas_ratio['workers'], us1000_df_ttas_ratio['time'])
# plt.plot(us1000_df_mutex_ratio['workers'], us1000_df_mutex_ratio['time'])
# plt.title("Uniform Speedup W = 1000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 2000 plot
# plt.plot(us2000_df_lf_ratio['workers'], us2000_df_lf_ratio['time'])
# plt.plot(us2000_df_ttas_ratio['workers'], us2000_df_ttas_ratio['time'])
# plt.plot(us2000_df_mutex_ratio['workers'], us2000_df_mutex_ratio['time'])
# plt.title("Uniform Speedup W = 2000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 4000 plot
# plt.plot(us4000_df_lf_ratio['workers'], us4000_df_lf_ratio['time'])
# plt.plot(us4000_df_ttas_ratio['workers'], us4000_df_ttas_ratio['time'])
# plt.plot(us4000_df_mutex_ratio['workers'], us4000_df_mutex_ratio['time'])
# plt.title("Uniform Speedup W = 4000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 8000 plot
# plt.plot(us8000_df_lf_ratio['workers'], us8000_df_lf_ratio['time'])
# plt.plot(us8000_df_ttas_ratio['workers'], us8000_df_ttas_ratio['time'])
# plt.plot(us8000_df_mutex_ratio['workers'], us8000_df_mutex_ratio['time'])
# plt.title("Uniform Speedup W = 8000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()

# EXPONENTIAL SPEEDUP


es1000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
es2000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
es4000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))
es8000_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))

for idx in range(len(fes1000_lines)):
    segmented1 = fes1000_lines[idx].split(" ")
    segmented2 = fes2000_lines[idx].split(" ")
    segmented4 = fes4000_lines[idx].split(" ")
    segmented8 = fes8000_lines[idx].split(" ")

    es1000_df.loc[len(es1000_df.index)] = [int(segmented1[0]), int(segmented1[1]), int(segmented1[2]), float(segmented1[3])]
    es2000_df.loc[len(es2000_df.index)] = [int(segmented2[0]), int(segmented2[1]), int(segmented2[2]), float(segmented2[3])]
    es4000_df.loc[len(es4000_df.index)] = [int(segmented4[0]), int(segmented4[1]), int(segmented4[2]), float(segmented4[3])]
    es8000_df.loc[len(es8000_df.index)] = [int(segmented8[0]), int(segmented8[1]), int(segmented8[2]), float(segmented8[3])]

# make the serial dataframes
es1000_df_serial = es1000_df[(es1000_df.version == -1)].reset_index(drop=True)
es2000_df_serial = es2000_df[(es2000_df.version == -1)].reset_index(drop=True)
es4000_df_serial = es4000_df[(es4000_df.version == -1)].reset_index(drop=True)
es8000_df_serial = es8000_df[(es8000_df.version == -1)].reset_index(drop=True)

# make the lockfree dataframes
es1000_df_lf = es1000_df[(es1000_df.version == 0)].reset_index(drop=True)
es2000_df_lf = es2000_df[(es2000_df.version == 0)].reset_index(drop=True)
es4000_df_lf = es4000_df[(es4000_df.version == 0)].reset_index(drop=True)
es8000_df_lf = es8000_df[(es8000_df.version == 0)].reset_index(drop=True)

# make the ttas dataframes
es1000_df_ttas = es1000_df[(es1000_df.lock == 1)].reset_index(drop=True)
es2000_df_ttas = es2000_df[(es2000_df.lock == 1)].reset_index(drop=True)
es4000_df_ttas = es4000_df[(es4000_df.lock == 1)].reset_index(drop=True)
es8000_df_ttas = es8000_df[(es8000_df.lock == 1)].reset_index(drop=True)

# make the mutex dataframes
es1000_df_mutex = es1000_df[(es1000_df.lock == 2)].reset_index(drop=True)
es2000_df_mutex = es2000_df[(es2000_df.lock == 2)].reset_index(drop=True)
es4000_df_mutex = es4000_df[(es4000_df.lock == 2)].reset_index(drop=True)
es8000_df_mutex = es8000_df[(es8000_df.lock == 2)].reset_index(drop=True)

# lock-free ratio dataframes
es1000_df_lf_ratio = es1000_df_lf
es2000_df_lf_ratio = es2000_df_lf
es4000_df_lf_ratio = es4000_df_lf
es8000_df_lf_ratio = es8000_df_lf

es1000_df_lf_ratio['time'] = es1000_df_serial['time'] / es1000_df_lf['time']
es2000_df_lf_ratio['time'] = es2000_df_serial['time'] / es2000_df_lf['time']
es4000_df_lf_ratio['time'] = es4000_df_serial['time'] / es4000_df_lf['time']
es8000_df_lf_ratio['time'] = es8000_df_serial['time'] / es8000_df_lf['time']

# ttas ratio dataframes
es1000_df_ttas_ratio = es1000_df_ttas
es2000_df_ttas_ratio = es2000_df_ttas
es4000_df_ttas_ratio = es4000_df_ttas
es8000_df_ttas_ratio = es8000_df_ttas

es1000_df_ttas_ratio['time'] = es1000_df_serial['time'] / es1000_df_ttas['time']
es2000_df_ttas_ratio['time'] = es2000_df_serial['time'] / es2000_df_ttas['time']
es4000_df_ttas_ratio['time'] = es4000_df_serial['time'] / es4000_df_ttas['time']
es8000_df_ttas_ratio['time'] = es8000_df_serial['time'] / es8000_df_ttas['time']

# mutex ratio dataframes
es1000_df_mutex_ratio = es1000_df_mutex
es2000_df_mutex_ratio = es2000_df_mutex
es4000_df_mutex_ratio = es4000_df_mutex
es8000_df_mutex_ratio = es8000_df_mutex

es1000_df_mutex_ratio['time'] = es1000_df_serial['time'] / es1000_df_mutex['time']
es2000_df_mutex_ratio['time'] = es2000_df_serial['time'] / es2000_df_mutex['time']
es4000_df_mutex_ratio['time'] = es4000_df_serial['time'] / es4000_df_mutex['time']
es8000_df_mutex_ratio['time'] = es8000_df_serial['time'] / es8000_df_mutex['time']

# 1000 plot
# plt.plot(es1000_df_lf_ratio['workers'], es1000_df_lf_ratio['time'])
# plt.plot(es1000_df_ttas_ratio['workers'], es1000_df_ttas_ratio['time'])
# plt.plot(es1000_df_mutex_ratio['workers'], es1000_df_mutex_ratio['time'])
# plt.title("Exponential Speedup W = 1000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 2000 plot
# plt.plot(es2000_df_lf_ratio['workers'], es2000_df_lf_ratio['time'])
# plt.plot(es2000_df_ttas_ratio['workers'], es2000_df_ttas_ratio['time'])
# plt.plot(es2000_df_mutex_ratio['workers'], es2000_df_mutex_ratio['time'])
# plt.title("Exponential Speedup W = 2000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 4000 plot
# plt.plot(es4000_df_lf_ratio['workers'], es4000_df_lf_ratio['time'])
# plt.plot(es4000_df_ttas_ratio['workers'], es4000_df_ttas_ratio['time'])
# plt.plot(es4000_df_mutex_ratio['workers'], es4000_df_mutex_ratio['time'])
# plt.title("Exponential Speedup W = 4000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()
#
# # 8000 plot
# plt.plot(es8000_df_lf_ratio['workers'], es8000_df_lf_ratio['time'])
# plt.plot(es8000_df_ttas_ratio['workers'], es8000_df_ttas_ratio['time'])
# plt.plot(es8000_df_mutex_ratio['workers'], es8000_df_mutex_ratio['time'])
# plt.title("Exponential Speedup W = 8000")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['LockFree', 'TTAS', 'Mutex'])
# plt.show()

# AWESOME

awe_df = pd.DataFrame(columns=('version', 'workers', 'lock', 'time'))

for line in fawe_lines:
    segmented = line.split(" ")
    version = int(segmented[0])
    workers = int(segmented[1])
    lock = int(segmented[2])
    time = float(segmented[3])
    awe_df.loc[len(awe_df.index)] = [version, workers, lock, time]

awe_df_lf = awe_df[(awe_df.version == 0)].reset_index(drop=True)
awe_df_ttas = awe_df[(awe_df.lock == 1)].reset_index(drop=True)
awe_df_mutex = awe_df[(awe_df.lock == 2)].reset_index(drop=True)

awe_df_ttas_ratio = awe_df_ttas
awe_df_mutex_ratio = awe_df_mutex

awe_df_ttas_ratio['time'] = awe_df_lf['time'] / awe_df_ttas['time']
awe_df_mutex_ratio['time'] = awe_df_lf['time'] / awe_df_mutex['time']

# plt.plot(awe_df_ttas_ratio['workers'], awe_df_ttas_ratio['time'])
# plt.plot(awe_df_mutex_ratio['workers'], awe_df_mutex_ratio['time'])
# plt.title("Awesome Speedup")
# plt.ylabel("Speedup")
# plt.xlabel("Workers")
# plt.legend(['TTAS', 'Mutex'])
# plt.show()
