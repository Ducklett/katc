typedef struct ast {
    parser parser;
    diagnosticContainer diagnostics;
} ast;

int create_ast(char* filename, ast* tree) {
	u64 length;
	char* text;
	{
		benchmark_start();
		text = read_file(filename, &length);
		benchmark_end("File read");
	}

    if (!create_syntaxtree(text, length, &tree->parser, &tree->diagnostics)) {
		print_diagnostics(&tree->diagnostics, text);
        return 0;
    }

    bool verbose = true;
	print_syntaxtree(text, &tree->parser.root, 0, verbose);

	return 1;
}