static const char *diagnosticText[] = {
	"Unexpected character '%c', expected '%c'\n",
	"unterminated comment, exprected */\n",
	"bad token '%c'\n",
	"character literals cannot be empty\n",
	"character literals cannot contain more than one character\n",
	"number literals should not have leading zeroes\n",
	"'%c' is an invalid hexadecimal number\n",
	"'%c' is an invalid binary number\n",
	"unexpected token of kind '%s', expected '%s'\n",
	"unexpected token of kind '%s', expected an assignment operator (=, +=, -=, *=, /=, %=, <<=, >>=, &=, ^=, |=)\n",
	"'%s' cannot be used in %s.\n",
	"a value of type '%s' is not legal in a range expression.\n",
	"unexpected token '%s' while parsing primary expression.\n",
	"increment/decrement operators can only be used on variables.\n",
	"statements of this kind are not allowed here.\n",
	"only constant expressions are allowed here. (did you reference a variable?)\n",
	"value is too %s for %s and will %s, use an explicit cast if this is intentional.\n",
	"variables must be declared with an explicit type in this context.\n",
	"undefined unary operator '%s' for value of type '%s'\n",
	"undefined binary operator '%s' for values of type '%s' and '%s'\n",
	"variable '%s' is not initialized.\n",
	"symbol '%s' is already declared in this scope.\n",
	"symbol '%s' is undefined.\n",
	"cannot assign expression of type '%s' to variable '%s' of type '%s'.\n",
	"'%s' is constant and cannot be assigned to.\n",
	"variable '%s' cannot be void.\n",
	"unresolved type '%s'.\n",
	"case statements should have at least one branch.\n",
	"cast should take exactly one argument.\n",
	"cast from '%s' to '%s' doesn't exist.\n",
	"cannot implicitly convert from '%s' to '%s', an explicit conversion exists. (are you missing a cast?)\n",
	"a value of type '%s' cannot be used in switch statements.\n",
	"duplicate value in switch case.\n",
	"argument count does not match in call to '%s'.\n",
	"ternary values must be of the same type, got %s and %s.\n",
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

	// printf("%d %d\n", s.start, s.length);

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

	// printf("%d %d %d %d\n", d.line_start, d.line_end, d.character_start, d.character_end);
	return d;
}

void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText, lineInfo *lines, int lineCount) {
	for (int i = 0; i < diagnostics->index; i++) {
		diagnostic d = diagnostics->diagnostics[i];
		diagnosticLocation dl = diagnostic_location_from_span(d.span, lines, lineCount);

		fprintf(stderr, "%s (%d, %d) ", d.span.filename, dl.line_start, dl.character_start);

		fprintf(stderr, TERMRED);
		switch (d.kind) {
		case unexpectedCharacterDiagnostic: {
			char param1 = d.param1;
			if (param1 == '\r' || param1 == '\n' || param1 == '\0') param1 = ' ';
			fprintf(stderr, diagnosticText[d.kind], param1, d.param2, dl.line_start, dl.character_start); 
		} break;
		case unterminatedCommentDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case badTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start]); break;
		case charEmptyDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case charTooLongDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case leadingZerosOnBase10NumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case invalidHexadecimalNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start]); break;
		case invalidBinaryNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[dl.line_start]); break;
		case unexpectedTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2]); break;
		case notAnAssignmentOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1]); break;
		case notAllowedInContextDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2]); break;
		case variableMustHaveTypeInCurrentContextDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case illegalRangeDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1]); break;
		case illegalPrimaryExpressionDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1]); break;
		case illegalIncrementOrDecrementDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case statementNotAllowedHereDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case nonConstantDiagnostic:
			fprintf(stderr, diagnosticText[d.kind]); break;
		case valueOutOfBoundsDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param2 ? "big" : "small", astKindText[d.param1], d.param2 ? "overflow" : "underflow"); break;
		case undefinedUnaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astKindText[d.param2]); break;
		case undefinedBinaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astKindText[d.param2], astKindText[d.param3]); break;
		case variableNotInitializedDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1); break;
		case redeclarationOfSymbolDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1); break;
		case referenceToUndefinedVariableDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText);
			free(identifierText);
		} break;
		case cannotAssignDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], astKindText[d.param2], identifierText, astKindText[d.param1]);
			free(identifierText);
		} break;
		case cannotAssignConstantDiagnostic: {
			char* identifierText = ast_substring(sourceText, *(textspan*)d.param1, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText);
			free(identifierText);
		} break;
		case variableCannotBeVoidDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, dl.line_start, dl.character_start);
			free(identifierText);
		} break;
		case unresolvedTypeDiagnostic: {
			char* typeText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], typeText);
			free(typeText);
		} break;
		case emptyCaseStatementDiagnostic: fprintf(stderr, diagnosticText[d.kind]); break;
		case oneArgumentCastDiagnostic: fprintf(stderr, diagnosticText[d.kind]); break;
		case illegalCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2]); break;
		case illegalImplicitCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2]); break;
		case invalidSwitchTypeDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1]); break;
		case duplicateSwitchValueDiagnostic: fprintf(stderr, diagnosticText[d.kind]); break;
		case argCountDoensntMatchDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.param1); break;
		case ternaryTypesMustBeEqualDiagnostic: fprintf(stderr, diagnosticText[d.kind], astKindText[d.param1], astKindText[d.param2]); break;
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
