typedef struct diagnostic {
	u8 kind;
	textspan span;
	u32 param1;
	u32 param2;
	u32 param3;
} diagnostic;

typedef struct diagnosticContainer {
	u8 index;
	diagnostic diagnostics[10];
} diagnosticContainer;

enum diagnosticKind {
	unexpectedCharacterDiagnostic,
	badTokenDiagnostic,
	unexpectedTokenDiagnostic,
	undefinedUnaryOperatorDiagnostic,
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u32 param1, u32 param2, u32 param3);
void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText);