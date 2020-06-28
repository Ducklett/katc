#include "header.c"

int main() {

	ast *result = calloc(1, sizeof(ast));

	benchmark_start();
	bool success = create_ast("test.kc", result);
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
