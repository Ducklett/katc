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
	charEmptyDiagnostic,
	charTooLongDiagnostic,
	leadingZerosOnBase10NumberDiagnostic,
	invalidHexadecimalNumberDiagnostic,
	invalidBinaryNumberDiagnostic,
	unexpectedTokenDiagnostic,
	notAnAssignmentOperatorDiagnostic,
	illegalRangeDiagnostic,
	illegalPrimaryExpressionDiagnostic,
	illegalIncrementOrDecrementDiagnostic,
	nonConstantDiagnostic,
	undefinedUnaryOperatorDiagnostic,
	undefinedBinaryOperatorDiagnostic,
	redeclarationOfVariableDiagnostic,
	referenceToUndefinedVariableDiagnostic,
	cannotAssignDiagnostic,
	cannotAssignConstantDiagnostic,
	variableCannotBeVoidDiagnostic,
	cannotConvertDiagnostic,
	unresolvedTypeDiagnostic,
	emptyCaseStatementDiagnostic,
};

static const char *diagnosticMetaText[] = {
	"unexpectedCharacterDiagnostic",
	"badTokenDiagnostic",
	"charEmptyDiagnostic",
	"charTooLongDiagnostic",
	"leadingZerosOnBase10NumberDiagnostic",
	"invalidHexadecimalNumberDiagnostic",
	"invalidBinaryNumberDiagnostic",
	"unexpectedTokenDiagnostic",
	"notAnAssignmentOperatorDiagnostic",
	"illegalRangeDiagnostic",
	"illegalPrimaryExpressionDiagnostic",
	"illegalIncrementOrDecrementDiagnostic",
	"nonConstantDiagnostic"
	"undefinedUnaryOperatorDiagnostic",
	"undefinedBinaryOperatorDiagnostic",
	"redeclarationOfVariableDiagnostic",
	"referenceToUndefinedVariableDiagnostic",
	"cannotAssignDiagnostic",
	"cannotAssignConstantDiagnostic",
	"variableCannotBeVoidDiagnostic",
	"cannotConvertDiagnostic",
	"unresolvedTypeDiagnostic",
	"emptyCaseStatementDiagnostic",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u32 param1, u32 param2, u32 param3);
void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText);