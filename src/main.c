
// https://en.wikipedia.org/wiki/Single_Compilation_Unit
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "syntax/syntaxTree.c"
#include "diagnostics.c"
#include "syntax/lexer.c"
#include "syntax/parser.c"

int main()
{
	char text[] = "10 + 10 * (20 + 10)";

	lexer l = { .text = text, .text_length = sizeof(text), .index = 0, };

	parser p = { .lexer = l, .nodeIndex = 0, .binaryExpressionIndex = 0, };

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);
	parser_parse(&p, &diagnostics);

	if (diagnostics.index==0)
		print_syntaxtree(text, &p.root, 0, true);
	else
		print_diagnostics(&diagnostics, text);

	return 0;
}
