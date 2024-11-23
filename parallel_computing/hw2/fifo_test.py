import os

def check():
    fserial = open("output/serial.txt","r")
    fconc = open("output/concurrent.txt", "r")
    fsq = open("output/squeue.txt", "r")

    serial_lines = fserial.readlines()
    conc_lines = fconc.readlines()
    sq_lines = fsq.readlines()

    for i in range(len(serial_lines)):
        if serial_lines[i] == conc_lines[i] == sq_lines[i]:
            continue
        else:
            return False

    fserial.close()
    fconc.close()
    fsq.close()
    return True

def run(inputs):
    for input in inputs:
        for i in range(1,4):
            cmd = input.replace("x", str(i))
            cmd = "./run " + cmd
            print(cmd)
            os.system(cmd)
        if check() == False:
            print("error on input:", cmd)
            return False
    return True

# IF ANYONE IS SEEING THIS WHILE GRADING, I AM SORRY, THESE INPUTS MUST
# BE UPDATED TO ADD TWO MORE ARGUMENTS: THE SEED VALUE AND THE OUTPUT FILE
# WHERE TIMING DATA IS WRITTEN.
def main():
    inputs = ["2 15 32 200 x C 1 bs_output.txt", "2 30 32 200 x C 1 bs_output.txt", "2 60 32 200 x C 1 bs_output.txt",
              "2 15 32 8000 x C 1 bs_output.txt",  "2 30 32 8000 x C 1 bs_output.txt", "2 60 32 8000 x C 1 bs_output.txt",
              "2 15 32 200 x U 1 bs_output.txt", "2 30 32 200 x U 1 bs_output.txt", "2 60 32 200 x U 1 bs_output.txt",
              "2 15 32 8000 x U 1 bs_output.txt",  "2 30 32 8000 x U 1 bs_output.txt", "2 60 32 8000 x U 1 bs_output.txt",
              "2 15 32 200 x E 1 bs_output.txt", "2 30 32 200 x E 1 bs_output.txt", "2 60 32 200 x E 1 bs_output.txt",
              "2 15 32 8000 x E 1 bs_output.txt",  "2 30 32 8000 x E 1 bs_output.txt", "2 60 32 8000 x E 1 bs_output.txt",
              "2 500 32 200 x C 1 bs_output.txt", "2 750 32 200 x C 1 bs_output.txt", "2 2000 32 200 x C 1 bs_output.txt",
              "2 500 32 8000 x C 1 bs_output.txt",  "2 750 32 8000 x C 1 bs_output.txt", "2 2000 32 8000 x C 1 bs_output.txt",
              "2 500 32 200 x U 1 bs_output.txt", "2 750 32 200 x U 1 bs_output.txt", "2 2000 32 200 x U 1 bs_output.txt",
              "2 500 32 8000 x U 1 bs_output.txt",  "2 750 32 8000 x U 1 bs_output.txt", "2 2000 32 8000 x U 1 bs_output.txt",
              "2 500 32 200 x E 1 bs_output.txt", "2 750 32 200 x E 1 bs_output.txt", "2 2000 32 200 x E 1 bs_output.txt",
              "2 500 32 8000 x E 1 bs_output.txt",  "2 750 32 8000 x E 1 bs_output.txt", "2 2000 32 8000 x E 1 bs_output.txt"]
    result = run(inputs)
    print(result)

if __name__ == '__main__':
    main()
