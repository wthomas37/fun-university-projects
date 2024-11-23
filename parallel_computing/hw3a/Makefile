
run: main.o stopwatch.o
		gcc -Wall -Werror -O3 main.o stopwatch.o -o run -lpthread

main.o: main.c
		gcc -Wall -Werror -O3 -c main.c

stopwatch.o: utils/stopwatch.c utils/stopwatch.h
		gcc -Wall -Werror -O3 -c utils/stopwatch.c

clean:
		rm *.o run
