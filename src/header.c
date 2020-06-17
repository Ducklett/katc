
// https://en.wikipedia.org/wiki/Single_Compilation_Unit
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

#include "syntax/syntaxTree.c"
#include "diagnostics.c"
#include "syntax/lexer.c"
#include "syntax/parser.c"

#define benchmark_start() clock_t t = clock(); int acc = 0;
#define benchmark_end(name) t = clock() - t; printf("%s: %.0fms\n", name, ((double)t) / CLOCKS_PER_SEC * 1000);

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
        printf("Failed to read file: %s\n", filename);
        exit(1);
    }
    return buffer;
}