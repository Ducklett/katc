
// https://en.wikipedia.org/wiki/Single_Compilation_Unit
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include "../lib/argparse.c"
#include "util.c"
#include "syntax/syntaxTree.c"
#include "diagnostics.h"
#include "syntax/lexer.c"
#include "syntax/parser.c"
#include "Binding/ast.c"
#include "Binding/binder.c"
#include "emit/Cemitter.c"
#include "diagnostics.c"