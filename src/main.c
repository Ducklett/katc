#define BENCHMARK true
#include "header.c"

int main(int argc, char **argv) {

	ast *result = calloc(1, sizeof(ast));

	if (argc < 2) {
		fprintf(stderr, "%sMust supply a file:%s\n", TERMRED, TERMRESET);
		printf("kc <filename>");
		exit(1);
	}

    bool verbose = true;
    bool parseOnly = false;

	benchmark_start();
	char *filename = argv[1];
	bool success = create_ast(filename, result, parseOnly);
	benchmark_end("Total");

	if (!success) {
		print_diagnostics(&result->diagnostics, result->text);
		return 1;
	} 

	// if (parseOnly) print_syntaxtree(result->text, &result->parser.root, 0, verbose);
	// else print_ast(result->text, &result->root, 0, verbose);

	emit_c_from_ast(result);

	return 0;
}
