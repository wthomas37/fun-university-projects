# include <stdio.h>
# include <stdlib.h>
# include <assert.h>
# include <math.h>
# include <pthread.h>
# include "utils/stopwatch.h"




// –––––––––––––––––––––––––––LOCK STRUCTS––––––––––––––––––––––––––––––––––––––

typedef struct {
    volatile int state;
} tas_lock_t;

typedef struct {
    volatile int state;
} ttas_lock_t;

typedef struct {
    volatile int tail;
    int size;
    volatile int * flag;
} ander_lock_t;

typedef union lock_t{
    tas_lock_t tas_lock;
    ttas_lock_t ttas_lock;
    pthread_mutex_t mutex;
    ander_lock_t ander_lock;
} lock_t;

// –––––––––––––––––––––––––––MISC––––––––––––––––––––––––––––––––––––––––––––––

typedef struct {
    int big_number;
    int tid;
    int lock_type;
    lock_t * lock;
    int times_in_cs;
    volatile int * count;
} thr_arg_t;

// –––––––––––––––––––––––––––FUNCTIONS FOR ALOCK–––––––––––––––––––––––––––––––
void ander_lock_init(ander_lock_t * ander_lock, int capacity){
    ander_lock->size = capacity * 17;
    ander_lock->tail = 0;
    ander_lock->flag = (volatile int *)malloc(sizeof(volatile int) * ander_lock->size);

    for(int i = 0; i < ander_lock->size; i+=17){
        ander_lock->flag[i] = 0;
    }
    ander_lock->flag[0] = 1;
}

void ander_lock_lock(ander_lock_t * ander_lock, volatile int * slot){
    *slot = __sync_fetch_and_add(&(ander_lock->tail), 17) % ander_lock->size;
    // printf("The value of the slot is %d\n", *slot);
    // printf("The value at flag[slot] is %d\n", ander_lock->flag[*slot]);
    // __sync_synchronize();
    while(!(ander_lock->flag[*slot])){}
}

void ander_lock_unlock(ander_lock_t * ander_lock, volatile int * slot){
    ander_lock->flag[*slot] = 0;
    __sync_synchronize();
    ander_lock->flag[((*slot) + 17) % ander_lock->size] = 1;
}

// –––––––––––––––––––––––––––FUNCTIONS FOR TAS–––––––––––––––––––––––––––––––––

void tas_lock_lock(tas_lock_t * tas_lock){
    while(__sync_lock_test_and_set(&(tas_lock->state), 1)){
        //spin
    }
}

void tas_lock_unlock(tas_lock_t * tas_lock){
    __sync_lock_test_and_set(&(tas_lock->state), 0);
}

// –––––––––––––––––––––––––––FUNCTIONS FOR TTAS––––––––––––––––––––––––––––––––

void ttas_lock_lock(ttas_lock_t * ttas_lock){
    while(1){
        while(ttas_lock->state){
            //spin until state holds False
        }
        // __sync_synchronize();
        if(!__sync_lock_test_and_set(&(ttas_lock->state), 1))
            return;
    }
}

void ttas_lock_unlock(ttas_lock_t * ttas_lock){
    __sync_lock_test_and_set(&(ttas_lock->state), 0);
}

// –––––––––––––––––––––––––––MAIN FUNCTIONS––––––––––––––––––––––––––––––––––––

void* concurrent_count(void* thread_arg){
    // unpack everything in thread_arg
    thr_arg_t * my_data;
    my_data = (thr_arg_t*) thread_arg;

    int big_number = my_data->big_number;
    // int tid = my_data->tid;
    int lock_type = my_data->lock_type;
    // int test_version = my_data->test_version;
    int cs_count = 0;

    volatile int* count = my_data->count;
    lock_t* lock = my_data->lock;

    volatile int slot = 0;

    switch(lock_type){
        case 0:
        {
            while(*count < big_number){
                tas_lock_lock(&lock->tas_lock);
                if(*count < big_number){
                    cs_count++;
                    (*count)++;
                    // printf("%d incremented by tid: %d\n", *count, tid);
                   //  printf("%d\n", *count);
                }
                tas_lock_unlock(&lock->tas_lock);
            }
            break;
        }
        case 1:
        {
            while(*count < big_number){
                ttas_lock_lock(&lock->ttas_lock);
                if(*count < big_number){
                    cs_count++;
                    (*count)++;
                    // printf("%d incremented by tid: %d\n", *count, tid);
                    // printf("%d\n", *count);
                }
                ttas_lock_unlock(&lock->ttas_lock);
            }
            break;
        }
        case 2:
        {
            while(*count < big_number){
                pthread_mutex_lock(&lock->mutex);
                if(*count < big_number){
                    cs_count++;
                    (*count)++;
                    // printf("%d incremented by tid: %d\n", *count, tid);
                   // printf("%d\n", *count);
                }
                pthread_mutex_unlock(&lock->mutex);
            }
            break;
        }
        case 3:
        {
            while(*count < big_number){
                // printf("Going in for the lock\n");
                ander_lock_lock(&lock->ander_lock, &slot);
                if(*count < big_number){
                    cs_count++;
                    (*count)++;
                    // printf("%d incremented by tid: %d\n", *count, tid);
                   // printf("%d\n", *count);
                }
                ander_lock_unlock(&lock->ander_lock, &slot);
                // printf("relinquished da lock\n");
            }
            break;
        }
        default:
            printf("literally no reason you should or could be here\n");
            exit(1);
    }

    // printf("The value of the counter is %d\n", *count);
    //printf("%d\n", *count);
    my_data->times_in_cs = cs_count;
    return NULL;
}

void serial_count(int big_number){
    // printf("COUNTING BEGINNING\n");
    volatile int count = 0;
    while(count < big_number){
        count++;
      //  printf("%d\n", count);
    }
    // printf("COUNTING FINISHED\n");

}

/*
 * Main takes as input:
        B: int Big Number (Should be 750000)
        V: {'S', 'C'}; S is Serial, C is Concurrent
        n: int num_threads
        L: String lock_type, {"TAS", "TTAS", "Mutex", "ALock"}, where TAS is 0,
            TTAS is 1, Mutex is 2, ALock is 3.
        T: {1, 0}. 1, record frequency. 0, do not.
 */
int main(int argc, char* argv[]){
    if(argc != 6){
        printf("5 inputs required; (B, V, n, L, T)\n");
        exit(1);
    }
    int big_number = atoi(argv[1]);
    // The number to count to
    char version = argv[2][0];

    FILE * file = fopen("performance_output/times.txt", "a");
    if(file == NULL){
        printf("Something is very wrong\n");
        exit(1);
    }

    // S,C for Serial, Concurrent
    // Note, nothing matters after this point if version is S
    if(version == 'C'){
        int n = atoi(argv[3]);
        if(n < 1){
            printf("positive, non-zero n is needed\n");
            exit(1);
        }
        // num_threads

        int lock_type = atoi(argv[4]);
        if(lock_type < 0 || lock_type > 3){
            printf("lock types can be 0, 1, 2, 3 for TAS, TTAS, Mutex, ALock\n");
            exit(1);
        }
        // lock_type

        int test_version = atoi(argv[5]);
        if(test_version != 0 && test_version != 1){
            printf("test version can be 0 (no freq recording) or 1 (freq recording)\n");
            exit(1);
        }

        // printf("Running Concurrent Version. The inputs were:\nBIG NUMBER = %d\nNumber of Threads: %d\nLock Type: %d\nTest Version: %d\n", big_number,
                // n, lock_type, test_version);

        pthread_t tid[n];
        thr_arg_t thr_arg[n];
        volatile int count = 0;
        union lock_t lock;

        switch(lock_type){
            case 0:
            {
                // printf("Concurrent w/ TAS\n");
                tas_lock_t tas_lock;
                tas_lock.state = 0;
                lock.tas_lock = tas_lock;

                break;
            }
            case 1:
            {
                // printf("Concurrent w/ TTAS\n");
                ttas_lock_t ttas_lock;
                ttas_lock.state = 0;
                lock.ttas_lock = ttas_lock;

                break;
            }
            case 2:
            {
                // printf("Concurrent w/ Mutex\n");
                pthread_mutex_t mutex;
                if(pthread_mutex_init(&mutex, NULL)){
                    printf("weird issue with mutex init \n");
                    exit(1);
                }
                lock.mutex = mutex;
                break;
            }
            case 3:
            {
                // printf("Concurrent w/ ALock\n");
                ander_lock_t ander_lock;
                ander_lock_init(&ander_lock, n);
                lock.ander_lock = ander_lock;
                break;
            }
            default:
                printf("this should be impossible\n");
                exit(1);
        }

        StopWatch_t* watch1 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
        startTimer(watch1);

        for(int i = 0; i < n; i++){
            thr_arg[i].big_number = big_number;
            thr_arg[i].tid = i;
            thr_arg[i].lock_type = lock_type;
            thr_arg[i].lock = &lock;
            thr_arg[i].times_in_cs = 0;
            thr_arg[i].count = &count;

            pthread_create(&tid[i], NULL, concurrent_count, (void*)&thr_arg[i]);
        }

        for(int i = 0; i < n; i++){
            // printf("times in cs: %d\n", thr_arg[i].times_in_cs);
            pthread_join(tid[i], NULL);
            // printf("joined thread number %d\n", i);
        }

        stopTimer(watch1);
        double elapsed_time = getElapsedTime(watch1);
        free(watch1);

        char input[300];
        sprintf(input, "%d %c %d %d %f", big_number, version, n, lock_type, elapsed_time);
        fprintf(file, "%s\n", input);

        // Record number of times in the CS if we want it
        if(test_version){
            FILE* fair_file = fopen("performance_output/fairness.txt", "a");
            if(fair_file == NULL){
                printf("something is very f'd up");
                exit(1);
            }

            for(int i = 0; i < n; i++){
                char finput[300];
                sprintf(finput, "%d %d %d %d %d\n", big_number, n, lock_type, i, thr_arg[i].times_in_cs);
                fprintf(fair_file, "%s", finput);
            }
        }



    }else{
        // printf("Running Serial Version. The inputs were:\nBIG NUMBER = %d\n", big_number);
        StopWatch_t* watch2 = (StopWatch_t*)malloc(sizeof(StopWatch_t));
        startTimer(watch2);

        serial_count(big_number);

        stopTimer(watch2);
        double elapsed_time = getElapsedTime(watch2);
        free(watch2);

        char input[300];
        sprintf(input, "%d %c %d %d %f", big_number, version, 0, 0, elapsed_time);
        fprintf(file, "%s\n", input);
    }

    fclose(file);
    return 0;
}
