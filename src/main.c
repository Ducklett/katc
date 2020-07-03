 #define BENCHMARK true
 #include "header.c"

static const char *const usage[] = {
    "kc <entrypoint> [options] ",
    NULL,
};

int main(int argc, const char **argv) {

	int run = 0;
	const char *outputType = NULL;
	const char *outputTypes = "syntaxtree|ast|c|bin";
	const char *outputName = NULL;

	struct argparse_option options[] = {
    	OPT_HELP(),
    	OPT_GROUP("Basic options"),
    	OPT_BOOLEAN('r', "run", &run, "run as script"),
    	OPT_STRING('t', "output-type", &outputType, outputTypes),
    	OPT_STRING('o', "output-name", &outputName, "name for the output file"),
    	OPT_END(),
	};

	struct argparse argparse;
	argparse_init(&argparse, options, usage, 0);
	argparse_describe(&argparse, "\nThe kat language compiler","");
	argc = argparse_parse(&argparse, argc, argv);

	if (argc > 1) {
		fprintf(stderr, "%sMust supply only one file as entrypoint.%s\n", TERMRED, TERMRESET);
		exit(1);
	}

    bool parseOnly = outputType != NULL && !strcmp(outputType, "syntaxtree");
    bool isAst = outputType != NULL && !strcmp(outputType, "ast");
	bool isC = outputType != NULL && !strcmp(outputType, "c");
	bool isBinary = outputType != NULL && !strcmp(outputType, "bin");

	if (outputType != NULL && !parseOnly && !isAst && !isC && !isBinary) {
		fprintf(stderr, "%sInvalid output type '%s', options are %s.%s\n", TERMRED, outputType, outputTypes, TERMRESET);
		exit(1);
	}

	const char* entrypoint = argv[0];

	ast *result = calloc(1, sizeof(ast));


	benchmark_start();
	bool success = create_ast(entrypoint, result, parseOnly);
	benchmark_end("Total");

	if (!success) {
		print_diagnostics(&result->diagnostics, result->text);
		return 1;
	} 

    bool verbose = true;

	if (parseOnly) print_syntaxtree(result->text, &result->parser.root, 0, verbose);
	else if (isAst) print_ast(result->text, &result->root, 0, verbose);
	else emit_c_from_ast(result, outputName, run, isC);

	return 0;
}
