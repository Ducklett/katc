#include "header.c"

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
