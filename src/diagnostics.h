typedef struct diagnostic {
	u8 kind;
	textspan span;
	u64 param1;
	u64 param2;
	u64 param3;
} diagnostic;

typedef struct diagnosticContainer {
	u8 index;
	diagnostic diagnostics[10];
} diagnosticContainer;

enum diagnosticKind {
	unexpectedCharacterDiagnostic,
	unterminatedCommentDiagnostic,
	badTokenDiagnostic,
	charEmptyDiagnostic,
	charTooLongDiagnostic,
	leadingZerosOnBase10NumberDiagnostic,
	invalidHexadecimalNumberDiagnostic,
	invalidBinaryNumberDiagnostic,
	unexpectedTokenDiagnostic,
	notAnAssignmentOperatorDiagnostic,
	notInLoopDiagnostic,
	illegalRangeDiagnostic,
	illegalPrimaryExpressionDiagnostic,
	illegalIncrementOrDecrementDiagnostic,
	statementNotAllowedHereDiagnostic,
	nonConstantDiagnostic,
	valueOutOfBoundsDiagnostic,
	undefinedUnaryOperatorDiagnostic,
	undefinedBinaryOperatorDiagnostic,
	variableNotInitializedDiagnostic,
	redeclarationOfSymbolDiagnostic,
	referenceToUndefinedVariableDiagnostic,
	cannotAssignDiagnostic,
	cannotAssignConstantDiagnostic,
	variableCannotBeVoidDiagnostic,
	unresolvedTypeDiagnostic,
	emptyCaseStatementDiagnostic,
	oneArgumentCastDiagnostic,
	illegalCastDiagnostic,
	illegalImplicitCastDiagnostic,
	invalidSwitchTypeDiagnostic,
	duplicateSwitchValueDiagnostic,
	argCountDoensntMatchDiagnostic,
};

static const char *diagnosticMetaText[] = {
	"unexpectedCharacterDiagnostic",
	"unterminatedCommentDiagnostic",
	"badTokenDiagnostic",
	"charEmptyDiagnostic",
	"charTooLongDiagnostic",
	"leadingZerosOnBase10NumberDiagnostic",
	"invalidHexadecimalNumberDiagnostic",
	"invalidBinaryNumberDiagnostic",
	"unexpectedTokenDiagnostic",
	"notAnAssignmentOperatorDiagnostic",
	"notInLoopDiagnostic",
	"illegalRangeDiagnostic",
	"illegalPrimaryExpressionDiagnostic",
	"illegalIncrementOrDecrementDiagnostic",
	"statementNotAllowedHereDiagnostic",
	"nonConstantDiagnostic"
	"valueOutOfBoundsDiagnostic"
	"undefinedUnaryOperatorDiagnostic",
	"undefinedBinaryOperatorDiagnostic",
	"variableNotInitializedDiagnostic",
	"redeclarationOfSymbolDiagnostic",
	"referenceToUndefinedVariableDiagnostic",
	"cannotAssignDiagnostic",
	"cannotAssignConstantDiagnostic",
	"variableCannotBeVoidDiagnostic",
	"unresolvedTypeDiagnostic",
	"emptyCaseStatementDiagnostic",
	"oneArgumentCastDiagnostic,"
	"illegalCastDiagnostic",
	"illegalImplicitCastDiagnostic",
	"invalidSwitchTypeDiagnostic",
	"duplicateSwitchValueDiagnostic",
	"argCountDoensntMatchDiagnostic",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u64 param1, u64 param2, u64 param3);
void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText);