#define BENCHMARK true
#include "header.c"

int main(int argc, char **argv) {

	ast *result = calloc(1, sizeof(ast));

	if (argc < 2) {
		fprintf(stderr, "%sMust supply a file:%s\n", TERMRED, TERMRESET);
		printf("kc <filename>");
		exit(1);
	}

	benchmark_start();
	char *filename = argc >= 2 ? argv[1] : "test.kc";
	bool success = create_ast(filename, result);
	benchmark_end("Total");

	if (!success) {
		print_diagnostics(&result->diagnostics, result->text);
		return 1;
	} 

    // bool verbose = true;
	// print_ast(result->text, &result->root, 0, verbose);
	emit_c_from_ast(result);

	return 0;
}
