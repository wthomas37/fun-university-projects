import os

def parallel_overhead(po):
    types = [1, 3]
    w_vals = [200, 400, 800]
    n_vals = [2, 9, 14]

    for i in range(1,6):
        for type in types:
            for w_val in w_vals:
                for n_val in n_vals:
                    t = (2 ** 20)//(n_val*w_val)
                    cmd = "./run "+str(n_val)+" "+str(t)+" 32 "+str(w_val)+" "+str(type)+" U "+str(i)+" "+po
                    os.system(cmd)

def dispatcher_rate(dr):
    n_vals = [2,3,5,9,14,28]
    for i in range(1,6):
        for n_val in n_vals:
            t = (2**20)//(n_val - 1)
            cmd = "./run "+str(n_val)+" "+str(t)+" 32 1 2 U "+str(i)+" "+dr
            os.system(cmd)

def constant_load(cl):
    types = [1,2]
    w_vals = [1000,2000,4000,8000]
    n_vals = [2, 3, 5, 9, 14, 28]
    t = 2**15

    for i in range(1, 4):
        for type in types:
            for w_val in w_vals:
                for n_val in n_vals:
                    cmd = "./run "+str(n_val)+" "+str(t)+" 32 "+str(w_val)+" "+str(type)+" C "+str(i)+" "+cl
                    os.system(cmd)

def uniform_load(ul):
    types = [1,2]
    w_vals = [1000,2000,4000,8000]
    n_vals = [2, 3, 5, 9, 14, 28]
    t = 2**15

    for i in range(1,6):
        for type in types:
            for w_val in w_vals:
                for n_val in n_vals:
                    cmd = "./run "+str(n_val)+" "+str(t)+" 32 "+str(w_val)+" "+str(type)+" U "+str(i)+" "+ul
                    os.system(cmd)

def exponential_load(el):
    types = [1,2]
    w_vals = [1000,2000,4000,8000]
    n_vals = [2, 3, 5, 9, 14, 28]
    t = 2**15

    for i in range(1,6):
        for type in types:
            for w_val in w_vals:
                for n_val in n_vals:
                    cmd = "./run "+str(n_val)+" "+str(t)+" 32 "+str(w_val)+" "+str(type)+" E "+str(i)+" "+el
                    os.system(cmd)





def main():
    # re open every file to clear whatever is in there
    po = "output/parallel_overhead.txt"
    dr = "output/dispatcher_rate.txt"
    cl = "output/constant_load.txt"
    ul = "output/uniform_load.txt"
    el = "output/exponential_load.txt"

    fpo = open(po, "w")
    fpo.close()

    fdr = open(dr, "w")
    fdr.close()

    fcl = open(cl, "w")
    fcl.close()

    ful = open(ul, "w")
    ful.close()

    fel = open(el, "w")
    fel.close()

    parallel_overhead(po)
    dispatcher_rate(dr)
    constant_load(cl)
    uniform_load(ul)
    exponential_load(el)



if __name__ == '__main__':
    main()
