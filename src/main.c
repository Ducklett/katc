#include "header.c"

int main() {
	char text[] = "{ x := 3 x = -x { xyz 20 30} }";

	lexer l = { .text = text, .text_length = sizeof(text), .index = 0, };

	parser p = { 0 };
	p.lexer = l;

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);

	benchmark_start();
	parser_parse(&p, &diagnostics);
	benchmark_end("Parsing");

	bool verbose = false;

	if (diagnostics.index==0)
		print_syntaxtree(text, &p.root, 0, verbose);
	else
		print_diagnostics(&diagnostics, text);

	return 0;
}
