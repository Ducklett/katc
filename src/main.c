#include "header.c"

int main() {
	u64 length;
	char* text;

	{
		benchmark_start();
		text = read_file("test.es", &length);
		benchmark_end("File read");
	}

	lexer l = { .text = text, .text_length = length, .index = 0, };

	parser p = { 0 };
	p.lexer = l;

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);

	benchmark_start();
	parser_parse(&p, &diagnostics);
	benchmark_end("Parsing");

	bool verbose = true;

	if (diagnostics.index==0)
		print_syntaxtree(text, &p.root, 0, verbose);
	else
		print_diagnostics(&diagnostics, text);

	return 0;
}
