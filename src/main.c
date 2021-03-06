 #define BENCHMARK true
 #include "header.c"

static const char *const usage[] = {
	"kc <entrypoint> [options] ",
	NULL,
};

int main(int argc, const char **argv) {

	int run = false;
	const char *outputType = NULL;
	const char *outputTypes = "syntaxtree|ast|ast-graph|c|bin";
	const char *outputName = NULL;

	int disableConstantFolding = false;

	struct argparse_option options[] = {
		OPT_HELP(),
		OPT_GROUP("Basic options"),
		OPT_BOOLEAN('r', "run", &run, "run as script"),
		OPT_STRING('t', "output-type", &outputType, outputTypes),
		OPT_STRING('o', "output-name", &outputName, "name for the output file"),
		OPT_GROUP("Feature flags"),
		OPT_BOOLEAN('\0', "disable-constant-folding", &disableConstantFolding, "disable constant folding"),
		OPT_END(),
	};


	struct argparse argparse;
	argparse_init(&argparse, options, usage, 0);
	argparse_describe(&argparse, "\nThe kat language compiler","");
	argc = argparse_parse(&argparse, argc, argv);

	if (argc == 0) panic("Must supply a file as entrypoint.");

	if (argc > 1) panic("Must supply only one file as entrypoint.");
	

	feature_constantfolding = !disableConstantFolding;

	bool parseOnly = outputType != NULL && !strcmp(outputType, "syntaxtree");
	bool isAst = outputType != NULL && !strcmp(outputType, "ast");
	bool isAstGraph = outputType != NULL && !strcmp(outputType, "ast-graph");
	bool isC = outputType != NULL && !strcmp(outputType, "c");
	bool isBinary = outputType != NULL && !strcmp(outputType, "bin");

	if (outputType != NULL && !parseOnly && !isAst && !isAstGraph && !isC && !isBinary)
		panic("Invalid output type '%s', options are %s.", outputType, outputTypes);

	const char* entrypoint = argv[0];

	ast astResult = {0};
	parser parseResult = {0};


	benchmark_start();
	bool success = create_ast(entrypoint, &astResult, &parseResult, parseOnly);
	benchmark_end("Total");

	if (!success) {
		print_diagnostics(&astResult.diagnostics, astResult.text, parseResult.lines, sb_count(parseResult.lines));
		return 1;
	} 

	bool verbose = true;

	if (parseOnly) {
		print_syntaxtree(astResult.text, &parseResult.root, 0, verbose);
		return 0;
	}

	arena_destroy(parser_arena);

	if (isAst) print_ast(astResult.text, &astResult.root, 0, verbose);
	else if (isAstGraph) print_ast_graph(astResult.text, &astResult.root, stdout);
	else emit_c_from_ast(&astResult, outputName, run, isC);

	arena_destroy(binder_arena);
	arena_destroy(string_arena);

	return 0;
}
