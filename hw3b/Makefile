run: main.o packetsource.o stopwatch.o fingerprint.o generators.o crc32.o
		gcc -Wall -Werror -O3 main.o packetsource.o stopwatch.o fingerprint.o generators.o crc32.o -o run -lm -lpthread

main.o: main.c
		gcc -Wall -Werror -O3 -c main.c

packetsource.o: utils/packetsource.c utils/packetsource.h
		gcc -Wall -Werror -O3 -c utils/packetsource.c

stopwatch.o: utils/stopwatch.c utils/stopwatch.h
		gcc -Wall -Werror -O3 -c utils/stopwatch.c

fingerprint.o: utils/fingerprint.c utils/fingerprint.h
		gcc -Wall -Werror -O3 -c utils/fingerprint.c

generators.o: utils/generators.c utils/generators.h
		gcc -Wall -Werror -O3 -c utils/generators.c

crc32.o: utils/crc32.c utils/crc32.h
		gcc -Wall -Werror -O3 -c utils/crc32.c


clean:
		rm *.o run
