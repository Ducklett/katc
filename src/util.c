typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

#define TERMRED() printf("\033[0;31m")
#define TERMBOLDRED() printf("\033[1;31m")
#define TERMGREEN() printf("\033[0;32m")
#define TERMBOLDGREEN() printf("\033[1;32m")
#define TERMYELLOW() printf("\033[0;33m")
#define TERMBOLDYELLOW() printf("\033[01;33m")
#define TERMBLUE() printf("\033[0;34m")
#define TERMBOLDBLUE() printf("\033[1;34m")
#define TERMMAGENTA() printf("\033[0;35m")
#define TERMBOLDMAGENTA() printf("\033[1;35m")
#define TERMCYAN() printf("\033[0;36m")
#define TERMBOLDCYAN() printf("\033[1;36m")
#define TERMRESET() printf("\033[0m")

#define benchmark_start() clock_t t = clock(); int acc = 0;
#define benchmark_end(name) t = clock() - t; TERMGREEN(); printf("%s: %.0fms\n", name, ((double)t) / CLOCKS_PER_SEC * 1000); TERMRESET();

char* read_file(char* filename, u64* length) {
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
        TERMRED();
        printf("Failed to read file: %s\n", filename);
        TERMRESET();
        exit(1);
    }
    return buffer;
}
