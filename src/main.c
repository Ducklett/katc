
// https://en.wikipedia.org/wiki/Single_Compilation_Unit
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

#include "syntax/syntaxTree.c"
#include "diagnostics.c"
#include "syntax/lexer.c"
#include "syntax/parser.c"

#define benchmark_start() clock_t t = clock(); int acc = 0;
#define benchmark_end(name) t = clock() - t; printf("%s: %.0fms\n", name, ((double)t) / CLOCKS_PER_SEC * 1000);

int main() {
	char text[] = "10 + 10 * (20 + 10)";

	lexer l = { .text = text, .text_length = sizeof(text), .index = 0, };

	parser p = { .lexer = l, .nodeIndex = 0, .binaryExpressionIndex = 0, };

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);

	benchmark_start();
	parser_parse(&p, &diagnostics);
	benchmark_end("Parsing");

	if (diagnostics.index==0)
		print_syntaxtree(text, &p.root, 0, true);
	else
		print_diagnostics(&diagnostics, text);

	return 0;
}
