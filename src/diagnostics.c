static const char *diagnosticText[] = {
	"Unexpected character '%c', expected '%c' (%d,%d)\n",
	"unterminated comment, exprected */ (%d,%d)\n",
	"bad token '%c' (%d,%d)\n",
	"character literals cannot be empty (%d,%d)\n",
	"character literals cannot contain more than one character (%d,%d)\n",
	"number literals should not have leading zeroes (%d,%d)\n",
	"'%c' is an invalid hexadecimal number (%d,%d)\n",
	"'%c' is an invalid binary number (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
	"unexpected token of kind '%s', expected an assignment operator (=, +=, -=, *=, /=, %=, <<=, >>=, &=, ^=, |=) (%d,%d)\n",
	"'%s' cannot be used in %s. (%d,%d)\n",
	"a value of type '%s' is not legal in a range expression. (%d,%d)\n",
	"unexpected token '%s' while parsing primary expression. (%d,%d)\n",
	"increment/decrement operators can only be used on variables. (%d,%d)\n",
	"statements of this kind are not allowed here. (%d,%d)\n",
	"only constant expressions are allowed here. (did you reference a variable?) (%d,%d)\n",
	"value is too %s for %s and will %s, use an explicit cast if this is intentional. (%d,%d)\n",
	"variables must be declared with an explicit type in this context. (%d,%d)\n",
	"undefined unary operator '%s' for value of type '%s' (%d,%d)\n",
	"undefined binary operator '%s' for values of type '%s' and '%s' (%d,%d)\n",
	"variable '%s' is not initialized. (%d,%d)\n",
	"symbol '%s' is already declared in this scope. (%d,%d)\n",
	"symbol '%s' is undefined. (%d,%d)\n",
	"cannot assign expression of type '%s' to variable '%s' of type '%s'. (%d,%d)\n",
	"'%s' is constant and cannot be assigned to. (%d,%d)\n",
	"variable '%s' cannot be void. (%d,%d)\n",
	"unresolved type '%s'. (%d,%d)\n",
	"case statements should have at least one branch. (%d,%d)\n",
	"cast should take exactly one argument. (%d,%d)\n",
	"cast from '%s' to '%s' doesn't exist. (%d,%d)\n",
	"cannot implicitly convert from '%s' to '%s', an explicit conversion exists. (are you missing a cast?) (%d,%d)\n",
	"a value of type '%s' cannot be used in switch statements. (%d,%d)\n",
	"duplicate value in switch case. (%d,%d)\n",
	"argument count does not match in call to '%s'. (%d,%d)\n",
	"ternary values must be of the same type, got %s and %s. (%d,%d)\n",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u64 param1, u64 param2, u64 param3) {
	if (d->index >= 10) return;

	diagnostic dia = { kind, span, param1, param2, param3, };
	d->diagnostics[d->index++] = dia;
}

typedef struct diagnosticLocation {
	int line_start;
	int line_end;
	int character_start;
	int character_end;
} diagnosticLocation;

diagnosticLocation diagnostic_location_from_span(textspan s, lineInfo *lines, int lineCount) {
	diagnosticLocation d = {-1};

	printf("%d %d\n", s.start, s.length);

	for (int i = 0;i < lineCount; i++) {

		textspan linespan = lines[i].line_span;

		if (linespan.start <= s.start &&
		   (linespan.start + linespan.length) >= s.start && d.line_start == -1) {
			d.character_start = s.start - linespan.start + 1;
			d.line_start = i+1;
		}

		if (d.line_start != -1 &&
		   (linespan.start + linespan.length) <= (s.start + s.length) &&
		   (linespan.start + linespan.length) >= (s.start + s.length)) {

			d.character_end = (s.start + s.length) - linespan.start;
			d.line_end = i+1;
			break;
		}
	}

	printf("%d %d %d %d\n", d.line_start, d.line_end, d.character_start, d.character_end);
	return d;
}

void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText, lineInfo *lines, int lineCount) {
	for (int i = 0; i < diagnostics->index; i++) {
		diagnostic d = diagnostics->diagnostics[i];
		diagnosticLocation dl = diagnostic_location_from_span(d.span, lines, lineCount);
		fprintf(stderr, TERMRED);
		switch (d.kind) {
		case unexpectedCharacterDiagnostic: {
			char param1 = d.param1;
			if (param1 == '\r' || param1 == '\n' || param1 == '\0') param1 = ' ';
			fprintf(stderr, diagnosticText[d.kind], param1, d.param2, dl.line_start, dl.character_start); 
		} break;
		case unterminatedCommentDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case badTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start], dl.line_start, dl.character_start); break;
		case charEmptyDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case charTooLongDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case leadingZerosOnBase10NumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case invalidHexadecimalNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start], dl.line_start, dl.character_start); break;
		case invalidBinaryNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start], dl.line_start, dl.character_start); break;
		case unexpectedTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], dl.line_start, dl.character_start); break;
		case notAnAssignmentOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], dl.line_start, dl.character_start); break;
		case notAllowedInContextDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], dl.line_start, dl.character_start); break;
		case variableMustHaveTypeInCurrentContextDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case illegalRangeDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], dl.line_start, dl.character_start); break;
		case illegalPrimaryExpressionDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], dl.line_start, dl.character_start); break;
		case illegalIncrementOrDecrementDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case statementNotAllowedHereDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case nonConstantDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case valueOutOfBoundsDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param2 ? "big" : "small", astKindText[d.param1], d.param2 ? "overflow" : "underflow", dl.line_start, dl.character_start); break;
		case undefinedUnaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astKindText[d.param2], dl.line_start, dl.character_start); break;
		case undefinedBinaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astKindText[d.param2], astKindText[d.param3], dl.line_start, dl.character_start); break;
		case variableNotInitializedDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1, dl.line_start, dl.character_start); break;
		case redeclarationOfSymbolDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1, dl.line_start, dl.character_start); break;
		case referenceToUndefinedVariableDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, dl.line_start, dl.character_start);
			free(identifierText);
		} break;
		case cannotAssignDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], astKindText[d.param2], identifierText, astKindText[d.param1], dl.line_start, dl.character_start);
			free(identifierText);
		} break;
		case cannotAssignConstantDiagnostic: {
			char* identifierText = ast_substring(sourceText, *(textspan*)d.param1, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, dl.line_start, dl.character_start);
			free(identifierText);
		} break;
		case variableCannotBeVoidDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, dl.line_start, dl.character_start);
			free(identifierText);
		} break;
		case unresolvedTypeDiagnostic: {
			char* typeText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], typeText, dl.line_start, dl.character_start);
			free(typeText);
		} break;
		case emptyCaseStatementDiagnostic: fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case oneArgumentCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case illegalCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2], dl.line_start, dl.character_start); break;
		case illegalImplicitCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2], dl.line_start, dl.character_start); break;
		case invalidSwitchTypeDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], dl.line_start, dl.character_start); break;
		case duplicateSwitchValueDiagnostic: fprintf(stderr, diagnosticText[d.kind], dl.line_start, dl.character_start); break;
		case argCountDoensntMatchDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.param1, dl.line_start, dl.character_start); break;
		case ternaryTypesMustBeEqualDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2], dl.line_start, dl.character_start); break;
		default: panic("Unhandled case %s in print_diagnostics", diagnosticMetaText[d.kind]);
		}
		fprintf(stderr, TERMRESET);

		int lineStart;
		for (lineStart = d.span.start; sourceText[lineStart] != '\n' && lineStart > 0; lineStart--) {}
		for (int i=lineStart;i<d.span.start;i++) fprintf(stderr, "%c", sourceText[i]);
		for (int i=0;i<d.span.length;i++) fprintf(stderr, "%s%c%s", TERMRED, sourceText[d.span.start + i], TERMRESET);
		for (int i=(d.span.start + d.span.length); sourceText[i] != '\n' && sourceText[i] != '\0';i++) fprintf(stderr, "%c", sourceText[i]);

		fprintf(stderr, "\n");
	}
}
