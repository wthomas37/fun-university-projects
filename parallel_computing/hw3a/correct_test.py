import os
import filecmp

# IF THIS IS RUN, MAKE SURE TO CHANGE FILE PERMISSIONS IN MAIN.C TO "W" AND NOT "A"
# ALSO, SORRY, BUT YOU HAVE TO UNCOMMENT ALL THE PRINT STATEMENTS WITHIN EACH LOCK CASE
# IN THE CONCURRENT FUNCTION (THEY ARE PIPED INTO SINK FILES FOR TESTING)

def order_check():
    cmd = "./run 750000 S 0 0 0 > test_output/serial_order.txt"
    os.system(cmd)

    n_vals = [1, 2, 4, 8, 14]
    type_vals = [(0, "tas_order.txt"), (1, "ttas_order.txt"), (2, "mutex_order.txt"),  (3, "alock_order.txt")]
    for n in n_vals:
        for type in type_vals:
            cmd = "./run 750000 C "+str(n)+" "+str(type[0])+" 0 > test_output/"+type[1]
            print(cmd)
            os.system(cmd)
        # At this point, check for correctness
        for type in type_vals:
            comp_file = "test_output/"+type[1]
            print("Comparing equivalence between serial and data held in "+comp_file)
            print(filecmp.cmp("test_output/serial_order.txt", comp_file, shallow=False))

def sum_of_cs(big_number):
    cmd_base = "./run "+str(big_number)+" C "

    n_vals = [1,2,4,8,14]
    type_vals = [(0, "tas_order.txt"), (1, "ttas_order.txt"), (2, "mutex_order.txt")]

    for n in n_vals:
        for type in type_vals:
            cmd = cmd_base + str(n)+" "+str(type[0])+" 1 > test_output/"+type[1]
            print(cmd)
            os.system(cmd)

            ffair = open("performance_output/fairness.txt")
            fair_lines = ffair.readlines()
            ffair.close()

            sum = 0
            for line in fair_lines:
                line.strip()
                sum += int(line.split(' ')[4])
            if sum == big_number:
                fcount = open("test_output/"+type[1])
                count_lines = fcount.readlines()
                fcount.close()
                if int(count_lines[len(count_lines)-1].strip()) == big_number:
                    print('True')
                    continue
            print('False')

    n_vals_al = [1,2,3,4]
    type_val = (3, "alock_order.txt")

    # just eyeball the values; they seem pretty even, and they all add up to big num. weak fifo invariant
    for n_val in n_vals_al:
        cmd = cmd_base + str(n_val)+" "+str(type_val[0])+" 1 > test_output/"+type_val[1]
        print(cmd)
        os.system(cmd)
        alock_fair = open("performance_output/fairness.txt")
        alock_fair_lines = alock_fair.readlines()
        alock_fair.close()

        sum = 0

        for aline in alock_fair_lines:
            val = int(aline.split(' ')[4])
            sum+=val
            print(val)
        print(sum)



def main():
    # order_check()
    sum_of_cs(750000)


if __name__ == '__main__':
    main()
