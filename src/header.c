
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
