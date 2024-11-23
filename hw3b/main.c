# include <stdio.h>
# include <stdlib.h>
# include <assert.h>
# include <math.h>
# include <pthread.h>
# include "utils/stopwatch.h"
# include "utils/fingerprint.h"
# include "utils/packetsource.h"

// –––––––––––––––––––––––––––LOCK STRUCTS––––––––––––––––––––––––––––––––––––––
typedef struct {
    volatile int state;
} ttas_lock_t;

typedef union lock_t{
    ttas_lock_t ttas_lock;
    pthread_mutex_t mutex;
} lock_t;

void ttas_lock_lock(ttas_lock_t * ttas_lock){
    while(1){
        while(ttas_lock->state){
            //spin until state holds False
            // printf("I AM SPEEENING\n");
        }
        __sync_synchronize();
        if(!__sync_lock_test_and_set(&(ttas_lock->state), 1)){
            return;
        }
    }
}

void ttas_lock_unlock(ttas_lock_t * ttas_lock){
     __sync_lock_test_and_set(&(ttas_lock->state), 0);
}

// –––––––––––––––––––––––––––LAMPORT QUEUE STUFF–––––––––––––––––––––––––––––––

typedef struct {
    volatile int head;
    volatile int tail;
    int depth;
    volatile Packet_t** to_do;
} lamp_queue_t;

// lamport queue initializer
lamp_queue_t* q_init(int depth){
    lamp_queue_t* rv = (lamp_queue_t*) malloc(sizeof(lamp_queue_t));
    rv->head = 0;
    rv->tail = 0;
    rv->depth = depth;

    volatile Packet_t** to_do = (volatile Packet_t**)malloc(sizeof(Packet_t*) * depth);
    rv->to_do = to_do;

    return rv;
}

// queue enqueue function
int enq(lamp_queue_t* queue, volatile Packet_t* packet){
    __sync_synchronize();
    if(queue->tail - queue->head == queue->depth){
        return 0;
    }
    __sync_synchronize();
    queue->to_do[queue->tail % queue->depth] = packet;
    queue->tail++;
    return 1;
}

// queue dequeue function
volatile Packet_t* deq(lamp_queue_t* queue){
    // printf("head, tail = %d, %d\n", head, tail);
    __sync_synchronize();
    if(queue->tail - queue->head == 0){
        return NULL;
    }
    __sync_synchronize();
    volatile Packet_t* rv = queue->to_do[queue->head % queue->depth];
    queue->head++;
    return rv;
}

// returns True if the capacity of the queue is less than a certain threshold
// which is a percentage of form .xy
int q_sparsity(lamp_queue_t * queue, double percent){
    int upper_capacity = (int) queue->depth * percent;
    if(queue->tail - queue->head <= upper_capacity){
        return 1;
    }
    return 0;
}

// –––––––––––––––––––––––––––DISPATCH, WORKER STRUCTS––––––––––––––––––––––––––

// struct for the passed dispatch thread data
typedef struct {
    int tid;
    int strategy;
    int l_type;
    int n;
    int t;
    int d;
    int u;
    PacketSource_t * packet_source;
} disp_data_t;

// struct for the passed worker thread data
typedef struct {
    int tid;
    int lock_type;
    lock_t * lock;
    volatile void ** aux_lock;
    int strategy;
    int t;
    lamp_queue_t * queue;
    volatile void ** aux_queue;
    volatile int * help_flag;
    volatile int * helping_flag;
    volatile void ** decrement_pointer;
    volatile int t_update;
    // FILE* file;
} worker_data_t;

// –––––––––––––––––––––––––––HELPER FUNCTIONS––––––––––––––––––––––––––––––––––

// literally just sums an array so the dispatch function knows when to finish
int array_sum(volatile int* arr, int n){
    int rv = 0;
    for(int i = 0; i < n; i++){
        rv += arr[i];
    }
    return rv;
}

// handles which packet to get; that is, constant, uniform, exponential
volatile Packet_t* get_packet(PacketSource_t* packet_source, int i, int u){
    switch(u){
        case 0:
            // printf("Exponential\n");
            return getExponentialPacket(packet_source, i);
        case 1:
            // printf("Uniform\n");
            return getUniformPacket(packet_source, i);
        default:
            printf("u can be 0, 1. no clue how you got here \n");
            exit(1);
    }
}

// given a lock_t and what type it is, call the lock function
void poly_locker(lock_t * lock, int lock_type){
    switch(lock_type){
        case 1:
        {
            // TTAS
            // printf("going to lock ttas\n");
            ttas_lock_lock(&lock->ttas_lock);
            break;
        }
        case 2:
        {
            pthread_mutex_lock(&lock->mutex);
            break;
        }
        default:
        {
            printf("you should literally never be here\n");
        }
    }
}

void poly_unlocker(lock_t * lock, int lock_type){
    switch(lock_type){
        case 1:
        {
            // printf("going to unlock ttas\n");
            ttas_lock_unlock(&lock->ttas_lock);
            break;
        }
        case 2:
        {
            pthread_mutex_unlock(&lock->mutex);
            break;
        }
        default:
        {
            printf("you should literally never be here\n");
        }
    }
}



// the worker function; reads a queue, does checksums.
void* worker(void* threadarg){

    worker_data_t* worker_data;
    worker_data = (worker_data_t*) threadarg;

    // unpack all the stuff

    // int tid = worker_data->tid;

    int t = worker_data->t;

    lamp_queue_t* queue = worker_data->queue;
    volatile void ** aux_queue = worker_data->aux_queue;

    int strat = worker_data->strategy;
    int lock_type = worker_data->lock_type;

    lock_t * lock = worker_data->lock;

    // if this is a strategy that uses locks, check that the lock exists
    if(strat != 0){
        if(lock == NULL){
            printf("SOMETHING IS VERY WRONG\n");
            exit(1);
        }
    }

    volatile void ** aux_lock = worker_data->aux_lock;
    volatile int * help_flag = worker_data->help_flag;
    volatile void ** decrement_pointer = worker_data->decrement_pointer;

    // FILE* file = worker_data->file;
    if(t <= 0){
        printf("positive non-zero t is needed\n");
        exit(1);
    }

    while(t > 0){
        volatile Packet_t * task;

        if(strat == 0){
            //LF
            while((task = deq(queue)) == NULL){
                // wait for queue to carry something
            }
        }else if(strat == 1){
            //HQ
            poly_locker(lock, lock_type);
            while((task = deq(queue)) == NULL){
                // wait for queue to carry something
                // printf("is this where i am hanging\n");
            }
            poly_unlocker(lock, lock_type);

        }else{
            //AWESOME
            poly_locker(lock, lock_type);
            // check if our queue has anything. dispatch is stalled so we can
            // hang on to the lock
            while((task = deq(queue)) == NULL){
                // if we are here, our queue is empty.
                // check to see if any other queues need help
                if(__sync_lock_test_and_set(help_flag, 0)){
                    // here, we have the proprietary right to help the queue
                    __sync_lock_test_and_set(worker_data->helping_flag, 1);
                    volatile Packet_t * aux_task = NULL;

                    lock_t * lock_to_help = (lock_t *) *aux_lock;
                    lamp_queue_t * queue_to_help = (lamp_queue_t *) *aux_queue;


                    if(*help_flag){
                        // checking to make sure the help is still needed
                        poly_locker(lock_to_help, lock_type);
                        __sync_synchronize();
                        aux_task = deq(queue_to_help);
                        if(aux_task != NULL){
                            volatile int * sub_pointer = (volatile int *) *decrement_pointer;
                            __sync_fetch_and_add(sub_pointer, 1);
                        }
                        poly_unlocker(lock_to_help, lock_type);
                    }
                    __sync_lock_test_and_set(worker_data->helping_flag, 0);
                    if(aux_task != NULL){
                        getFingerprint(aux_task->iterations, aux_task->seed);

                    }
                }
            }
            poly_unlocker(lock, lock_type);
        }
        getFingerprint(task->iterations, task->seed);
        t--;
        // update t in case other threads have completed some of your tasks
        int packets_completed = __sync_lock_test_and_set(&worker_data->t_update, 0);
        t -= packets_completed;

    }
    return NULL;
}


void * dispatch(void * threadarg){
    disp_data_t * dispatch_data;
    dispatch_data = (disp_data_t *) threadarg;

    // unpack everything

    int strategy = dispatch_data->strategy;
    int l_type = dispatch_data->l_type;
    int n = dispatch_data->n;
    int t = dispatch_data->t;
    int d = dispatch_data->d;
    int u = dispatch_data->u;

    PacketSource_t * packet_source = dispatch_data->packet_source;

    volatile void * aux_queue = NULL;
    volatile void * aux_lock = NULL;
    volatile void * decrement_pointer = NULL;

    volatile int help_flag = 0;

    // prepare the workers

    pthread_t workers[n];
    worker_data_t worker_info[n];

    // just fills the relevant arrays
    volatile int count_array[n];
    lock_t lock_array[n];
    volatile int helping_flags[n];

    for(int i = 0; i < n; i++){
        count_array[i] = t;
        helping_flags[i] = 0;
        if(strategy){
            if(l_type == 1){
                ttas_lock_t ttas_lock;
                ttas_lock.state = 0;
                lock_array[i].ttas_lock = ttas_lock;
            }else if(l_type == 2){
                pthread_mutex_t mutex;
                if(pthread_mutex_init(&mutex, NULL)){
                    printf("weird issue with mutex init \n");
                    exit(1);
                }
                lock_array[i].mutex = mutex;
            }
        }
    }

    // the meat and potatoes

    while(array_sum(count_array, n)){
        for(int i = 0; i < n; i++){
            if(count_array[i] == t){
                // if we are here, the corresponding thread tid = i has not been made
                worker_info[i].tid = i;
                worker_info[i].t = t;
                worker_info[i].queue = q_init(d);
                worker_info[i].aux_queue = &aux_queue;
                worker_info[i].aux_lock = &aux_lock;
                worker_info[i].lock_type = l_type;
                worker_info[i].help_flag = &help_flag;
                worker_info[i].strategy = strategy;
                worker_info[i].helping_flag = &helping_flags[i];
                worker_info[i].decrement_pointer = &decrement_pointer;
                worker_info[i].t_update = 0;

                // create the correct lock, but do not bother if strat == 0
                if(strategy){
                    worker_info[i].lock = &lock_array[i];
                }else{
                    worker_info[i].lock = NULL;
                }

                // printf("creating pthread with id = %d\n", i);
                pthread_create(&workers[i], NULL, worker, (void*)&worker_info[i]);

                // next, we must give the worker the first task

                volatile Packet_t *packet = get_packet(packet_source, i, u);
                // we infinite loop while enq is returning 0 (queue is full)
                volatile int enq_state;
                while(!(enq_state = enq(worker_info[i].queue, packet))){
                    // printf("waiting here trying to enq\n");
                }
                count_array[i]--;
            }else if(count_array[i] == 0){
                // if we are here, the worker has t jobs queued, so we move on
                continue;
            }else{
                // the 'normal' case. we give the worker a packet on decrement
                volatile Packet_t * packet = get_packet(packet_source, i, u);
                volatile int enq_state;

                if(strategy == 2){
                    if(!(enq_state = enq(worker_info[i].queue, packet))){
                        // we are here because enq returned 0, meaning queue is full
                        aux_queue = worker_info[i].queue;
                        aux_lock = worker_info[i].lock;
                        decrement_pointer = &worker_info[i].t_update;
                        __sync_lock_test_and_set(&help_flag, 1);

                        while(!(enq_state = enq(worker_info[i].queue, packet))){
                            // spin
                            // printf("I am here, waiting, could use help\n");
                        }

                        __sync_lock_test_and_set(&help_flag, 0);
                        // whether or not the help was taken, set back to 0

                        // at this point, the overloaded queue has been cleared out
                        // before we continue, we must make sure that every thread
                        // that went to help has finished de-queueing a packet

                        // printf("going in to check for other help flags\n");
                        while((array_sum(helping_flags, n))){
                            //essentially, wait until every thread that aims
                            // to help has finished dequeuing a packet
                            // printf("waiting for help flags\n");
                        }
                        // printf("all help flags 0\n");
                        __sync_synchronize();

                        aux_queue = NULL;
                        aux_lock = NULL;
                        decrement_pointer = NULL;

                    }
                }else{
                    while(!(enq_state = enq(worker_info[i].queue, packet))){
                        // spin without the aux assignment
                    }
                }
                count_array[i]--;
            }
        }
    }

    // if we are here, rejoice for the array is empty. let's join the threads.
    for(int i = 0; i < n; i++){
        pthread_join(workers[i], NULL);
        printf("worker %d joined\n", i);
    }
    printf("Concurrent Finished\n");
    return NULL;


}

// carries out the serial implementation
int serial_helper(int n, PacketSource_t* packet_source, int t, int u){
    for(int i = 0; i < n; i++){
        // printf("packet source number %d\n", i);
        for(int j = 0; j < t; j++){
            volatile Packet_t* packet = get_packet(packet_source, i, u);
            getFingerprint(packet->iterations, packet->seed);
        }
    }
    printf("Serial Finished\n");
    return 0;
}


// –––––––––––––––––––––––––––MAIN STUFF––––––––––––––––––––––––––––––––––––––––

/*
    Main takes as input:
        numpackets/source, numsources, meanwork, u/e flag, experiment #,
        parallel/serial, queue depth (8), locktype, strategy

*/

int main(int argc, char* argv[]){
    if(argc < 7){
        printf("in smallest arg case, need at least 6 arguments\n");
        exit(1);
    }
    // number of packets to be sent from each generator
    int t = atoi(argv[1]);

    // number of packet sources / worker threads
    int n = atoi(argv[2]);

    // mean work
    int w = atoi(argv[3]);

    // uniform flag
    int u = atoi(argv[4]);

    assert(u == 1 || u == 0);

    // experiment #
    int exp_number = atoi(argv[5]);

    // parallel or serial; 0 for serial, 1 for parallel
    int mode = atoi(argv[6]);

    assert(t > 0);
    assert(n > 0);
    assert(w > 0);
    assert(exp_number > 0);

    // printf("INITIAL INPUTS: num_packets: %d, num_workers: %d, mean_work: %d, uniform_flag: %d, exp_number: %d, mode: %d\n",
    //                                                 t, n, w, u, exp_number, mode);

    PacketSource_t* packet_source = createPacketSource(w, n, exp_number);

    FILE * file = fopen("performance_output/times.txt", "a");
    if(file == NULL){
        printf("Something is very wrong\n");
        exit(1);
    }

    if(mode == 1){

        // printf("Running Concurrent Implementation\n");

        if(argc != 10){
            printf("Need 9 inputs for Concurrent Implementation");
        }

        int d = atoi(argv[7]);
        assert(d > 0);

        int l_type = atoi(argv[8]);
        // l_type is 0 for no lock, 1 for ttas, 2 for mutex
        assert(l_type == 0 || l_type == 1 || l_type == 2);

        int strat = atoi(argv[9]);
        // strat is 0 for LF, 1 for HQ, 2 for Awesome
        assert(strat == 0 || strat == 1 || strat == 2);

        if(((strat == 1) || (strat == 2)) && l_type == 0){
            printf("l_type must be 1 (ttas), 2(mutex) for strategies 1 (HQ) and 2 (Awesome)\n");
            exit(1);
        }

        // printf("SPECIAL CONCURRENT INPUTS\n");
        // printf("depth = %d\n lock = %d\n strat = %d\n", d, l_type, strat);

        disp_data_t dispatch_data;
        dispatch_data.tid = n + 1;
        dispatch_data.strategy = strat;
        dispatch_data.l_type = l_type;
        dispatch_data.n = n;
        dispatch_data.t = t;
        dispatch_data.d = d;
        dispatch_data.u = u;
        dispatch_data.packet_source = packet_source;

        pthread_t thread_dispatch;

        StopWatch_t* watch1 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
        startTimer(watch1);

        pthread_create(&thread_dispatch, NULL, dispatch, (void*)&dispatch_data);
        pthread_join(thread_dispatch, NULL);

        stopTimer(watch1);
        double elapsed_time = getElapsedTime(watch1);
        free(watch1);

        char input[300];
        sprintf(input, "%d %d %d %d %d %d %d %d %d %f", t, n, w, u, exp_number, mode, d, l_type, strat, elapsed_time);
        fprintf(file, "%s\n", input);

    }else if (mode == 0){
        // printf("Running SerialPacket Implementation\n");

        StopWatch_t * watch2 = (StopWatch_t*) malloc(sizeof(StopWatch_t));
        startTimer(watch2);

        serial_helper(n, packet_source, t, u);

        stopTimer(watch2);
        double elapsed_time = getElapsedTime(watch2);
        free(watch2);

        char input[300];
        sprintf(input, "%d %d %d %d %d %d %f", t, n, w, u, exp_number, mode, elapsed_time);
        fprintf(file, "%s\n", input);

    }else{
        printf("invalid input for mode\n");
        exit(1);
    }

    return 0;
}
