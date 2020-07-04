typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

#define TERMRED "\033[0;31m"
#define TERMBOLDRED "\033[1;31m"
#define TERMGREEN "\033[0;32m"
#define TERMBOLDGREEN "\033[1;32m"
#define TERMYELLOW "\033[0;33m"
#define TERMBOLDYELLOW "\033[01;33m"
#define TERMBLUE "\033[0;34m"
#define TERMBOLDBLUE "\033[1;34m"
#define TERMMAGENTA "\033[0;35m"
#define TERMBOLDMAGENTA "\033[1;35m"
#define TERMCYAN "\033[0;36m"
#define TERMBOLDCYAN "\033[1;36m"
#define TERMRESET "\033[0m"

#if BENCHMARK
#define benchmark_start() clock_t t = clock();
#define benchmark_end(name) t = clock() - t; printf("%s%s: %.0fms%s\n", TERMGREEN, name, ((double)t) / CLOCKS_PER_SEC * 1000, TERMRESET); 
#else
#define benchmark_start() {}
#define benchmark_end(name) {}
#endif

char* string_concat(const char *s1, const char *s2)
{
	char *result = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(result, s1);
	strcat(result, s2);
	return result;
}

char* read_file(const char* filename, u64* length) {
	char * buffer = 0;
	FILE * f = fopen (filename, "rb");

	if (f) {
		fseek (f, 0, SEEK_END);
		*length = ftell (f);
		fseek (f, 0, SEEK_SET);
		buffer = malloc (*length+1);
		if (buffer) {
			fread (buffer, 1, *length, f);
		}
		fclose (f);
		buffer[*length] = '\0';
	}

	if (!buffer) {
		fprintf(stderr, "%sFailed to read file: %s%s\n", TERMRED, filename, TERMRESET);
		exit(1);
	}
	return buffer;
}
