# include <stdio.h>
# include <stdlib.h>
# include <assert.h>
# include <math.h>
# include <pthread.h>
# include "utils/stopwatch.h"
# include "utils/fingerprint.h"
# include "utils/packetsource.h"

// struct for the passed dispatch thread data
typedef struct {
    int tid;
    int n;
    int t;
    int d;
    char p;
    PacketSource_t * packet_source;
} disp_data_t;

// struct for the lamport queue
typedef struct {
    volatile int head;
    volatile int tail;
    int depth;
    volatile Packet_t** to_do;
} lamp_queue_t;

// struct for the passed worker thread data
typedef struct {
    int tid;
    int t;
    lamp_queue_t * queue;
    FILE* file;
} worker_data_t;



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

// handles which packet to get; that is, constant, uniform, exponential
volatile Packet_t* get_packet(PacketSource_t* packet_source, int i, char p){
    switch(p){
        case 'C':
            // printf("Constant\n");
            return getConstantPacket(packet_source, i);
        case 'U':
            // printf("Uniform\n");
            return getUniformPacket(packet_source, i);
        case 'E':
            // printf("Exponential\n");
            return getExponentialPacket(packet_source, i);
        default:
            printf("How you got here is truly beyond me \n");
            exit(1);
    }
}

// carries out the serial implementation
int serial_helper(int n, PacketSource_t* packet_source, int t, char p){
    // must uncomment output code for fifo_test.py
    FILE* file = fopen("output/serial.txt", "w");
    if(NULL == file){
        printf("file cannot be opened\n");
        exit(1);
    }
    for(int i = 0; i < n; i++){
        for(int j = 0; j < t; j++){
            volatile Packet_t* packet = get_packet(packet_source, i, p);
            long checksum = getFingerprint(packet->iterations, packet->seed);
            char in[20];
            sprintf(in, "%ld", checksum);
            fprintf(file, "%s", in);
            fprintf(file, "\n");
        }
    }
    printf("Serial Finished\n");
    fclose(file);
    return 0;
}



// literally just sums an array so the dispatch function knows when to finish
int array_sum(int* arr, int n){
    int rv = 0;
    for(int i = 0; i < n; i++){
        rv += arr[i];
    }
    return rv;
}

void sq_worker(lamp_queue_t* queue, int t, FILE* file){

    volatile Packet_t* task;
    task = deq(queue);
    if(task == NULL){
        printf("you should never be here\n");
        exit(1);
    }
    long checksum = getFingerprint(task->iterations, task->seed);

    char in[20];
    sprintf(in, "%ld", checksum);
    fprintf(file, "%s", in);
    fprintf(file, "\n");


    // printf("checksum: %ld\n", checksum);
    // printf("just finished task number %d\n", t);

}

// like the concurrent dispatch function but worse in most ways
void* sq_dispatch(void* threadarg){

    FILE* file = fopen("output/squeue.txt", "w");
    if(NULL == file){
        printf("file cannot be opened\n");
        exit(1);
    }


    disp_data_t* dispatch_data;
    dispatch_data = (disp_data_t*) threadarg;

    int n = dispatch_data->n;
    int t = dispatch_data->t;
    int d = dispatch_data->d;
    char p = dispatch_data->p;
    PacketSource_t* packet_source = dispatch_data->packet_source;

    // printf("Dispatch thread inputs: n = %d, t = %d, d = %d, p = %c\n",
            // n,t,d,p);

    // printf("Dispatch thread id = %d\n", dispatch_data->tid);

    int count_array[n];
    lamp_queue_t* queue_list[n];

    for(int i = 0; i < n; i++){
        count_array[i] = t;
        queue_list[i] = q_init(d);
    }

    while(array_sum(count_array, n)){
        for(int i = 0; i < n; i++){
            if(count_array[i] == 0){
                continue;
            }
            volatile Packet_t *packet = get_packet(packet_source, i, p);
            enq(queue_list[i], packet);
            sq_worker(queue_list[i], count_array[i], file);
            count_array[i]--;
        }
    }
    // printf("is done\n");
    fclose(file);
    printf("sq finished\n");
    return NULL;
}

// the worker function; reads a queue, does checksums.
void* worker(void* threadarg){

    worker_data_t* worker_data;
    worker_data = (worker_data_t*) threadarg;
    int t = worker_data->t;
    lamp_queue_t* queue = worker_data->queue;
    FILE* file = worker_data->file;

    if(t <= 0){
        printf("positive non-zero t is needed\n");
        exit(1);
    }

    while(t > 0){
        volatile Packet_t* task;
        while((task = deq(queue)) == NULL){
            // printf("Am I stuck here??? \n");
        }
        long checksum = getFingerprint(task->iterations, task->seed);

        char in[20];
        sprintf(in, "%ld", checksum);
        fprintf(file, "%s", in);
        fprintf(file, "\n");

        printf("just finished task number %d\n", t);
        t--;
    }
    return NULL;
}

// the concurrent dispatch function; handles all work allocation, creates worker
// threads, does a whole lot of stuff tbh
void* dispatch(void* threadarg){

    FILE* file = fopen("output/concurrent.txt", "w");
    if(NULL == file){
        printf("file cannot be opened\n");
        exit(1);
    }



    // unpacking everything
    disp_data_t* dispatch_data;
    dispatch_data = (disp_data_t*) threadarg;

    int n = dispatch_data->n;
    int t = dispatch_data->t;
    int d = dispatch_data->d;
    char p = dispatch_data->p;
    PacketSource_t* packet_source = dispatch_data->packet_source;

    // printf("Dispatch thread inputs: n = %d, t = %d, d = %d, p = %c\n",
            // n,t,d,p);

    // printf("Dispatch thread id = %d\n", dispatch_data->tid);

    // making the worker data management stuff
    pthread_t workers[n];
    worker_data_t worker_info[n];

    // just fills the count array
    int count_array[n];
    for(int i = 0; i < n; i++){
        count_array[i] = t;
    }

    while(array_sum(count_array, n)){
        // printf("Current array sum = %d\n", array_sum(count_array, n));
        for(int i = 0; i < n; i++){
            if(count_array[i] == t){
                // if we are here, the corresponding thread tid = i has not been made
                worker_info[i].tid = i;
                worker_info[i].t = t;
                worker_info[i].queue = q_init(d);
                worker_info[i].file = file;
                // printf("creating pthread with id = %d\n", i);
                pthread_create(&workers[i], NULL, worker, (void*)&worker_info[i]);

                // next, we must give the worker the first task

                volatile Packet_t *packet = get_packet(packet_source, i, p);
                // we infinite loop while enq is returning 0 (queue is full)
                volatile int enq_state;
                while(!(enq_state = enq(worker_info[i].queue, packet))){
                    // printf("waiting here trying to enq\n");
                }
                count_array[i]--;
            }
            else if(count_array[i] == 0){
                // if we are here, the worker has t jobs queued, so we move on
                continue;
            }else{
                // the 'normal' case. we give the worker a packet on decrement

                volatile Packet_t *packet = get_packet(packet_source, i, p);
                volatile int enq_state;
                while(!(enq_state = enq(worker_info[i].queue, packet))){
                    // printf("waiting here trying to enq\n");
                }
                count_array[i]--;
            }
        }
    }

    // if we are here, rejoice for the array is empty. let's join the threads.
    // printf("if you're here this is something pretty good ngl\n");
    for(int i = 0; i < n; i++){
        pthread_join(workers[i], NULL);
        printf("worker %d joined\n", i);
    }
    fclose(file);
    printf("Concurrent Finished\n");
    return NULL;
}

void time_handler(int n, int t, int d, int w, int v,
                            char p, int s, char* output, double time){
    FILE* file = fopen(output, "a");
    if(file == NULL){
        printf("wrong file name. or something else. who knows\n");
        exit(1);
    }
    char input[300];

    sprintf(input, "The inputs were: n = %d, t = %d, d = %d, w = %d, v = %d, p = %c, s = %d, output = %s, and everything took %f time \n",
                            n,t,d,w,v,p,s,output,time);
    fprintf(file, "%s", input);
    fclose(file);
}

int main(int argc, char* argv[]){
    // program inputs: n, T, D, W, V, P, S, output file (depending on experiment)
    if(argc != 9){
        printf("Insufficient program inputs; 8 required\n");
        exit(1);
    }
    int n = atoi(argv[1]);
    // num threads; num workers + 1
    int t = atoi(argv[2]);
    // packets per packetsource
    int d = atoi(argv[3]);
    // queue depth
    int w = atoi(argv[4]);
    // mean work per packet
    int v = atoi(argv[5]);
    // v refers to 'version' of code. 1 is serial, 2 is parallel,
    // 3 is serial-queue
    char p = argv[6][0];
    // packet type; can be C, U, E for constant, uniform, exponential
    int s = atoi(argv[7]);

    char* output = argv[8];

    printf("The inputs were: n = %d, t = %d, d = %d, w = %d, v = %d, p = %c, s = %d, ouptut = %s\n",n,t,d,w,v,p,s, output);

    if(p != 'C' && p != 'U' && p != 'E'){
        printf("input must be C, U, or E for constant, uniform, exponential\n");
        exit(1);
    }

    n--;

    assert(t > 0);
    assert(d > 0);
    assert(n > 0);
    assert(w > 0);
    assert(s > 0);

    // Allocate packet generators
    PacketSource_t* packet_source = createPacketSource(w, n, s);

    if(v == 1){
        StopWatch_t* watch1 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
        startTimer(watch1);
        printf("Computing Serially\n");
        serial_helper(n, packet_source, t, p);
        stopTimer(watch1);
        double elapsed_time = getElapsedTime(watch1);
        free(watch1);
        time_handler(n, t, d, w, v, p, s, output, elapsed_time);
    }else{
        disp_data_t dispatch_data;
        dispatch_data.tid = n + 1;
        dispatch_data.n = n;
        dispatch_data.t = t;
        dispatch_data.d = d;
        dispatch_data.p = p;
        dispatch_data.packet_source = packet_source;
        pthread_t thread_dispatch;

        if(v == 2){
            StopWatch_t* watch2 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
            startTimer(watch2);

            printf("Computing Concurrently\n");
            pthread_create(&thread_dispatch, NULL, dispatch, (void*)&dispatch_data);
            pthread_join(thread_dispatch, NULL);

            stopTimer(watch2);
            double elapsed_time = getElapsedTime(watch2);
            free(watch2);
            time_handler(n, t, d, w, v, p, s, output, elapsed_time);
        }else if(v == 3){
            StopWatch_t* watch3 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
            startTimer(watch3);

            printf("Computing via the Serial Queue\n");
            pthread_create(&thread_dispatch, NULL, sq_dispatch, (void*)&dispatch_data);
            pthread_join(thread_dispatch, NULL);

            stopTimer(watch3);
            double elapsed_time = getElapsedTime(watch3);
            free(watch3);
            time_handler(n, t, d, w, v, p, s, output, elapsed_time);

        }else{
            printf("God knows what you input for v lol");
            exit(1);
        }
    }


    // printf("It ran, it is done\n");
    deletePacketSource(packet_source);
    return 0;
}
