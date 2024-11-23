import numpy as np
import pandas as pd
import scipy.io as sio
import matplotlib.pyplot as plt

fls = open("ls_output.txt", "r")
foh = open("oh_output.txt", "r")
ffair = open("fair_test_output.txt", "r")

ls_lines = fls.readlines()
oh_lines = foh.readlines()
fair_lines = ffair.readlines()

fls.close()
foh.close()
ffair.close()

# OVERHEAD DF
oh_df = pd.DataFrame(columns=('lock', 'time'))

for line in oh_lines:
    segmented = line.split(" ")
    lock_val = segmented[2]
    time = float(segmented[3].strip())
    oh_df.loc[len(oh_df.index)] = [lock_val, time]

# print(oh_df)

# LOCK SCALING DF

ls_df = pd.DataFrame(columns=('num_threads', 'lock', 'time'))

for line in ls_lines:
    segmented = line.split(" ")
    num_threads = int(segmented[1])
    lock_val = int(segmented[2])
    time = float(segmented[3].strip())
    ls_df.loc[len(ls_df.index)] = [num_threads, lock_val, time]

# print(ls_df)

# FAIRNESS TEST DF

fair_df = pd.DataFrame(columns=('tid', 'lock', 'cs_count'))

for line in fair_lines:
    segmented = line.split(" ")
    if int(segmented[1]) != 14:
        continue
    tid = int(segmented[2])
    lock_val = int(segmented[3])
    cs_count = float(segmented[4].strip())
    fair_df.loc[len(fair_df.index)] = [tid, lock_val, cs_count]

# print(fair_df)

# CREATE THE OH TABLE

# print(oh_df)

oh_df_ratio = oh_df[:4]
serial = oh_df[4:]

serial_time = serial.iloc[0]['time']
# oh_df_ratio['time'] = serial_time / oh_df_ratio['time']
# print(oh_df_ratio)


# DATA VIZ FOR LS

# print(ls_df)

ls_df_ratio = ls_df
ls_df_ratio['time'] = serial_time / ls_df_ratio['time']

ls_df_ratio = ls_df_ratio.sort_values(by=['lock', 'num_threads'], ascending = True).reset_index(drop=True)

ls_df_0 = ls_df_ratio[ls_df_ratio['lock'] == 0].sort_values(by=['num_threads'], ascending = True).reset_index(drop=True)
ls_df_1 = ls_df_ratio[ls_df_ratio['lock'] == 1].sort_values(by=['num_threads'], ascending = True).reset_index(drop=True)
ls_df_2 = ls_df_ratio[ls_df_ratio['lock'] == 2].sort_values(by=['num_threads'], ascending = True).reset_index(drop=True)
ls_df_3 = ls_df_ratio[ls_df_ratio['lock'] == 3].sort_values(by=['num_threads'], ascending = True).reset_index(drop=True)

print(ls_df_0)

# plt.plot(ls_df_0['num_threads'], ls_df_0['time'])
# plt.title('TAS Lock Scaling')
# plt.ylabel('ratio')
# plt.xlabel('n values')
# plt.show()

print(ls_df_1)

# plt.plot(ls_df_1['num_threads'], ls_df_1['time'])
# plt.title('TTAS Lock Scaling')
# plt.ylabel('ratio')
# plt.xlabel('n values')
# plt.show()

print(ls_df_2)
#
# plt.plot(ls_df_2['num_threads'], ls_df_2['time'])
# plt.title('Mutex Lock Scaling')
# plt.ylabel('ratio')
# plt.xlabel('n values')
# plt.show()

print(ls_df_3)
#
# plt.plot(ls_df_3['num_threads'], ls_df_3['time'])
# plt.title('Anderson Lock Scaling')
# plt.ylabel('ratio')
# plt.xlabel('n values')
# plt.show()

# DATA VIZ FOR FAIRNESS
# print(fair_df)
#
# fair_df_0 = fair_df[fair_df['lock'] == 0].reset_index(drop=True)
# fair_df_1 = fair_df[fair_df['lock'] == 1].reset_index(drop=True)
# fair_df_2 = fair_df[fair_df['lock'] == 2].reset_index(drop=True)
# fair_df_3 = fair_df[fair_df['lock'] == 3].reset_index(drop=True)
#
# print(fair_df_0)
#
# plt.bar(fair_df_0['tid'], fair_df_0['cs_count'], color = 'maroon', width = 0.4)
# plt.xlabel("Thread ID")
# plt.ylabel("Number of Times in Critical Section")
# plt.title("Fairness Representation: 14 Thread Computation with TAS")
# plt.show()
#
# print(fair_df_1)
#
# plt.bar(fair_df_1['tid'], fair_df_1['cs_count'], color = 'maroon', width = 0.4)
# plt.xlabel("Thread ID")
# plt.ylabel("Number of Times in Critical Section")
# plt.title("Fairness Representation: 14 Thread Computation with TTAS")
# plt.show()
#
# print(fair_df_2)
#
# plt.bar(fair_df_2['tid'], fair_df_2['cs_count'], color = 'maroon', width = 0.4)
# plt.xlabel("Thread ID")
# plt.ylabel("Number of Times in Critical Section")
# plt.title("Fairness Representation: 14 Thread Computation with Mutex")
# plt.show()
#
# print(fair_df_3)
#
# plt.bar(fair_df_3['tid'], fair_df_3['cs_count'], color = 'maroon', width = 0.4)
# plt.xlabel("Thread ID")
# plt.ylabel("Number of Times in Critical Section")
# plt.title("Fairness Representation: 14 Thread Computation with ALock")
# plt.show()


































# DATA VIZ FOR FAIRNESS
