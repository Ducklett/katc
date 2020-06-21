#include "header.c"

int main() {

	ast result = {0};

	if (!create_ast("test.es", &result)) {
		print_diagnostics(&result.diagnostics, result.text);
		return 1;
	} 

    bool verbose = true;
	print_ast(result.text, &result.root, 0, verbose);

	return 0;
}
