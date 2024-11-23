import os
import numpy as np
import pandas as pd

def idle_lock_oh(big_number):
    # empty the times.txt file
    open("performance_output/times.txt", "w").close()

    foh = open("slurm_sinks/oh_output.txt", "w")

    # Run the Serial version quickly
    # os.system("./run "+str(big_number)+" S 0 0 0")

    lock_vals = [0, 1, 2, 3, 4]

    for lock in lock_vals:
        for trial in range(6):
            if lock != 4:
                cmd = "./run "+str(big_number)+" C 1 "+str(lock)+" 0"
                print(cmd)
                os.system(cmd)
            else:
                os.system("./run "+str(big_number)+" S 0 0 0")
        ftimes = open("performance_output/times.txt", "r")
        time_lines = ftimes.readlines()
        ftimes.close()
        open("performance_output/times.txt", "w").close()

        time_vals = []

        for time_line in time_lines:
            time_line.strip()
            time = time_line.split(" ")[4]
            time_vals.append(float(time))

        median_value = np.median(time_vals)
        file_input = str(big_number)+" 1 "+str(lock)+" "+str(median_value)+"\n"
        print(file_input)
        foh.write(file_input)
    foh.close()



def lock_scaling(big_number):

    fls = open("slurm_sinks/ls_output.txt", "w")
    open("performance_output/times.txt", "w").close()

    n_vals = [1, 2, 4, 8, 14]
    lock_vals = [0, 1, 2, 3]

    for n in n_vals:
        for lock in lock_vals:
            for trial in range(6):
                cmd = "./run "+str(big_number)+" C "+str(n)+" "+str(lock)+" 0"
                print(cmd)
                os.system(cmd)
            ftimes = open("performance_output/times.txt", "r")
            time_lines = ftimes.readlines()
            ftimes.close()
            open("performance_output/times.txt", "w").close()

            time_vals = []

            for time_line in time_lines:
                time_line.strip()
                time = time_line.split(" ")[4]
                time_vals.append(float(time))

            median_value = np.median(time_vals)
            file_input = str(big_number)+" "+str(n)+" "+str(lock)+" "+str(median_value)+"\n"
            print(file_input)
            fls.write(file_input)
    fls.close()

def fairness(big_number):
    ff = open("slurm_sinks/fair_test_output.txt", "w")
    open("performance_output/fairness.txt", "w").close()

    n_vals = [1, 2, 4, 8, 14]
    lock_vals = [0, 1, 2, 3]

    for n in n_vals:
        for lock in lock_vals:
            for trial in range(6):
                cmd = "./run "+str(big_number)+" C "+str(n)+" "+str(lock)+" 1"
                print(cmd)
                os.system(cmd)
            ffairness = open("performance_output/fairness.txt", "r")
            fair_lines = ffairness.readlines()
            ffairness.close()
            open("performance_output/fairness.txt", "r").close()

            for thread in range(n):
                fair_vals = []
                for fair_line in fair_lines:
                    fair_line.strip()
                    segmented = fair_line.split(" ")
                    thread_num = int(segmented[3])

                    if thread_num == thread:
                        fair_vals.append(int(segmented[4]))
                median_value = np.median(fair_vals)
                file_input = str(big_number)+" "+str(n)+" "+str(thread)+" "+str(lock)+" "+str(median_value)+"\n"
                print(file_input)
                ff.write(file_input)
    ff.close()



def main():
    idle_lock_oh(1000000)
    lock_scaling(1000000)
    fairness(1000000)

if __name__ == '__main__':
    main()
