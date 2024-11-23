import os
import numpy as np
import pandas as pd


def idle_lock_oh():
    # empty the times.txt file
    open("performance_output/times.txt", "w").close()
    foh = open("slurm_sinks/oh_output.txt", "w")

    strat_vals = [0, 1]
    w_vals = [25, 50, 100, 200, 400, 800]
    lock_vals = [0, 1, 2]

    for strat in strat_vals:
        for lock in lock_vals:
            if (strat == 0 and lock != 0) or (strat != 0 and lock == 0):
                continue
            for w_val in w_vals:
                for trial in range(1,6):
                    cmd = "./run 1000 1 "+str(w_val)+" 1 " + str(trial)+ " 1 8 " + str(lock) + " " + str(strat)
                    print(cmd)
                    os.system(cmd)
                ftimes = open("performance_output/times.txt", "r")
                time_lines = ftimes.readlines()
                ftimes.close()
                open("performance_output/times.txt", "w").close()

                time_vals = []

                for time_line in time_lines:
                    time_line.strip()
                    time = time_line.split(" ")[9]
                    time_vals.append(float(time))

                median_value = np.median(time_vals)
                file_input = str(strat) + " " + str(w_val) + " " + str(lock) + " " + str(median_value) +"\n"
                # print(file_input)
                foh.write(file_input)
    foh.close()

def uniform_speedup():
    fus1000 = open("slurm_sinks/us1000_output.txt", "w")
    fus2000 = open("slurm_sinks/us2000_output.txt", "w")
    fus4000 = open("slurm_sinks/us4000_output.txt", "w")
    fus8000 = open("slurm_sinks/us8000_output.txt", "w")

    open("performance_output/times.txt", "w").close()

    versions = [0, 1]
    n_vals = [1, 2, 3, 7, 13, 27]
    w_vals = [1000, 2000, 4000, 8000]
    strat_vals = [0, 1]
    lock_vals = [0, 1, 2]

    for w_val in w_vals:
        for n_val in n_vals:
            for trial in range(1,6):
                cmd = "./run 1000 "+str(n_val)+" "+str(w_val)+" 1 "+str(trial)+" 0 "
                os.system(cmd)
            ftimes = open("performance_output/times.txt", "r")
            time_lines = ftimes.readlines()
            ftimes.close()
            open("performance_output/times.txt", "w").close()

            time_vals = []

            for time_line in time_lines:
                time_line.strip()
                time = time_line.split(" ")[6]
                time_vals.append(float(time))
            median_value = np.median(time_vals)
            file_input = "-1 " + str(n_val) + " -1 " + str(median_value) + "\n"
            print(file_input)

            if w_val == 1000:
                fus1000.write(file_input)
            elif w_val == 2000:
                fus2000.write(file_input)
            elif w_val == 4000:
                fus4000.write(file_input)
            elif w_val == 8000:
                fus8000.write(file_input)

    for w_val in w_vals:
        for n_val in n_vals:
            for strat in strat_vals:
                for lock in lock_vals:
                    if (strat == 0 and lock != 0) or (strat != 0 and lock == 0):
                        continue
                    for trial in range(1, 6):
                        cmd = "./run 1000 "+str(n_val)+" "+str(w_val)+" 1 " + str(trial) + " 1 8 " + str(lock) + " " + str(strat)
                        os.system(cmd)
                    ftimes = open("performance_output/times.txt", "r")
                    time_lines = ftimes.readlines()
                    ftimes.close()
                    open("performance_output/times.txt", "w").close()

                    time_vals = []

                    for time_line in time_lines:
                        time_line.strip()
                        time = time_line.split(" ")[9]
                        time_vals.append(float(time))

                    median_value = np.median(time_vals)
                    file_input = str(strat) + " " + str(n_val) + " " + str(lock) + " " + str(median_value) + "\n"
                    print(file_input)

                    if w_val == 1000:
                        fus1000.write(file_input)
                    elif w_val == 2000:
                        fus2000.write(file_input)
                    elif w_val == 4000:
                        fus4000.write(file_input)
                    elif w_val == 8000:
                        fus8000.write(file_input)
    fus1000.close()
    fus2000.close()
    fus4000.close()
    fus8000.close()

def exponential_speedup():
    fes1000 = open("slurm_sinks/es1000_output.txt", "w")
    fes2000 = open("slurm_sinks/es2000_output.txt", "w")
    fes4000 = open("slurm_sinks/es4000_output.txt", "w")
    fes8000 = open("slurm_sinks/es8000_output.txt", "w")

    open("performance_output/times.txt", "w").close()

    versions = [0, 1]
    n_vals = [1, 2, 3, 7, 13, 27]
    w_vals = [1000, 2000, 4000, 8000]
    strat_vals = [0, 1]
    lock_vals = [0, 1, 2]

    for w_val in w_vals:
        for n_val in n_vals:
            for trial in range(1, 11):
                cmd = "./run 1000 "+str(n_val)+" "+str(w_val)+" 0 "+str(trial)+" 0 "
                os.system(cmd)
            ftimes = open("performance_output/times.txt", "r")
            time_lines = ftimes.readlines()
            ftimes.close()
            open("performance_output/times.txt", "w").close()

            time_vals = []

            for time_line in time_lines:
                time_line.strip()
                time = time_line.split(" ")[6]
                time_vals.append(float(time))
            median_value = np.median(time_vals)
            file_input = "-1 " + str(n_val) + " -1 " + str(median_value) + "\n"
            print(file_input)

            if w_val == 1000:
                fes1000.write(file_input)
            elif w_val == 2000:
                fes2000.write(file_input)
            elif w_val == 4000:
                fes4000.write(file_input)
            elif w_val == 8000:
                fes8000.write(file_input)

    for w_val in w_vals:
        for n_val in n_vals:
            for strat in strat_vals:
                for lock in lock_vals:
                    if (strat == 0 and lock != 0) or (strat != 0 and lock == 0):
                        continue
                    for trial in range(1, 11):
                        cmd = "./run 1000 "+str(n_val)+" "+str(w_val)+" 0 " + str(trial) + " 1 8 " + str(lock) + " " + str(strat)
                        os.system(cmd)
                    ftimes = open("performance_output/times.txt", "r")
                    time_lines = ftimes.readlines()
                    ftimes.close()
                    open("performance_output/times.txt", "w").close()

                    time_vals = []

                    for time_line in time_lines:
                        time_line.strip()
                        time = time_line.split(" ")[9]
                        time_vals.append(float(time))

                    median_value = np.median(time_vals)
                    file_input = str(strat) + " " + str(n_val) + " " + str(lock) + " " + str(median_value) + "\n"
                    print(file_input)

                    if w_val == 1000:
                        fes1000.write(file_input)
                    elif w_val == 2000:
                        fes2000.write(file_input)
                    elif w_val == 4000:
                        fes4000.write(file_input)
                    elif w_val == 8000:
                        fes8000.write(file_input)
    fes1000.close()
    fes2000.close()
    fes4000.close()
    fes8000.close()

def awesome():
    open("performance_output/times.txt", "w").close()
    fawe = open("slurm_sinks/awe_output.txt", "w")

    strat_vals = [0, 2]
    n_vals = [1, 2, 3, 7, 13, 27]
    lock_vals = [0, 1, 2]

    for strat in strat_vals:
        for n_val in n_vals:
            for lock in lock_vals:
                if (strat == 0 and lock != 0) or (strat != 0 and lock == 0):
                    continue
                for trial in range(1, 11):
                    cmd = "./run 1000 " + str(n_val)+" 8000 0 " + str(trial) + " 1 8 " + str(lock) + " " + str(strat)
                    # print(cmd)
                    os.system(cmd)
                ftimes = open("performance_output/times.txt", "r")
                time_lines = ftimes.readlines()
                ftimes.close()
                open("performance_output/times.txt", "w").close()

                time_vals = []

                for time_line in time_lines:
                    time_line.strip()
                    time = time_line.split(" ")[9]
                    time_vals.append(float(time))

                median_value = np.median(time_vals)
                file_input = str(strat) + " " +str(n_val) + " " + str(lock) + " " + str(median_value) +"\n"
                print(file_input)

                fawe.write(file_input)
    fawe.close()




def main():
    idle_lock_oh()
    uniform_speedup()
    exponential_speedup()
    awesome()

if __name__ == '__main__':
    main()
