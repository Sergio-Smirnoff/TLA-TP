#include "Generator.h"

/* MODULE INTERNAL STATE */

const char _indentationCharacter = ' ';
const char _indentationSize = 4;
static Logger * _logger = NULL;
static FILE * _outputFile = NULL;

void initializeGeneratorModule() {
	_logger = createLogger("Generator");
	_outputFile = fopen("Automaton.java", "w");
}

void shutdownGeneratorModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
	if (_outputFile != NULL) {
		fclose(_outputFile);
	}
}

/** PRIVATE FUNCTIONS */

static void _generatePrologue(void);
static void _generateProgram(automaton * automaton);
static void _generateEpilogue(void);
static char * _indentation(const unsigned int indentationLevel);
static void _output(const unsigned int indentationLevel, const char * const format, ...);

/**
 * Generates the output of the program.
 */
static void _generateProgram(automaton * automaton) {
	if(_outputFile == NULL) {
		write_java_initialization(automaton, 1);
	}
	write_java_initialization(automaton, fileno(_outputFile));
}

/**
 * Creates the prologue of the generated output, a Latex document that renders
 * a tree thanks to the Forest package.
 *
 * @see https://ctan.dcc.uchile.cl/graphics/pgf/contrib/forest/forest-doc.pdf
 */
static void _generatePrologue(void) {
	_output(0, "%s",
		"//INSERT PACKAGE NAME HERE\n"
		"package Your_package;\n"
		"\n"
		"import java.util.*;\n"
		"import java.util.function.Function;\n"
		"\n"
		"//EDIT THIS IMPORT TO MATCH YOUR PACKAGE\n"
		"import static Your_package.Automaton.Token.*;\n"
		"\n"
		"\n"
		"public abstract class Automaton {\n"
		"\n"
		"    // EDIT THIS ENUM TO MATCH YOUR TOKENS\n"
		"    public enum Token {\n"
		"        PUT_YOUR_USED_TOKENS_HERE, UNKNOWN, IGNORE_FOR_NOW;\n"
		"\n"
		"\n"
		"        private String stringContent;\n"
		"        private Integer intContent;\n"
		"        private Boolean boolContent;\n"
		"        private Double doubleContent;\n"
		"        private Boolean hasParams = false;\n"
		"\n"
		"        public void setStringContent(String stringContent) {\n"
		"            this.stringContent = stringContent;\n"
		"            this.hasParams = true;\n"
		"        }\n"
		"\n"
		"        public void setIntContent(Integer intContent) {\n"
		"            this.intContent = intContent;\n"
		"            this.hasParams = true;\n"
		"        }\n"
		"\n"
		"        public void setBoolContent(Boolean boolContent) {\n"
		"            this.boolContent = boolContent;\n"
		"            this.hasParams = true;\n"
		"        }\n"
		"\n"
		"        public void setDoubleContent(Double doubleContent) {\n"
		"            this.doubleContent = doubleContent;\n"
		"            this.hasParams = true;\n"
		"        }\n"
		"\n"
		"        public String getStringContent() {\n"
		"            return stringContent;\n"
		"        }\n"
		"\n"
		"        public Integer getIntContent() {\n"
		"            return intContent;\n"
		"        }\n"
		"\n"
		"        public Boolean getBoolContent() {\n"
		"            return boolContent;\n"
		"        }\n"
		"\n"
		"        public Double getDoubleContent() {\n"
		"            return doubleContent;\n"
		"        }\n"
		"\n"
		"        @Override\n"
		"        public String toString() {\n"
		"            StringBuilder sb = new StringBuilder();\n"
		"            sb.append(this.name());\n"
		"            if (hasParams) {\n"
		"                sb.append(\"{ \");\n"
		"                if (this.stringContent != null) {\n"
		"                    sb.append(\"stringContent: \").append(this.stringContent).append(\" \");\n"
		"                }\n"
		"                if (this.intContent != null) {\n"
		"                    sb.append(\"intContent: \").append(this.intContent).append(\" \");\n"
		"                }\n"
		"                if (this.boolContent != null) {\n"
		"                    sb.append(\"boolContent: \").append(this.boolContent).append(\" \");\n"
		"                }\n"
		"                if (this.doubleContent != null) {\n"
		"                    sb.append(\"doubleContent: \").append(this.doubleContent).append(\" \");\n"
		"                }\n"
		"                sb.append(\"}\");\n"
		"            }\n"
		"            return sb.toString();\n"
		"        }\n"
		"    }\n"
		"\n"
		"    private static State initialState;\n"
		"    private static final List<State> states = new ArrayList<>();\n"
		"    private static final StateTracker stateTracker = new StateTracker();\n"
		"\n"
		"    public static int newState(Token token) {\n"
		"        return newState((s) -> token);\n"
		"    }\n"
		"\n"
		"    public static int newState(Function<StateTracker, Token> tokenGenerator) {\n"
		"        State state = new State(tokenGenerator);\n"
		"        if (initialState == null)\n"
		"            initialState = state;\n"
		"        states.add(state);\n"
		"        return states.size() - 1;\n"
		"    }\n"
		"\n"
		"    public static void setInitialState(int index) {\n"
		"        initialState = states.get(index);\n"
		"    }\n"
		"\n"
		"    public static void setTransition(int from, int to, char symbol) {\n"
		"        states.get(from).setTransition(states.get(to), symbol);\n"
		"    }\n"
		"\n"
		"    private static void manageState(char symbol) {\n"
		"        Automaton.stateTracker.lexeme.append(symbol);\n"
		"        if (symbol == '\\n') {\n"
		"            Automaton.stateTracker.attribute.row++;\n"
		"            Automaton.stateTracker.attribute.column = 1;\n"
		"        } else {\n"
		"            Automaton.stateTracker.attribute.column++;\n"
		"        }\n"
		"    }\n"
		"\n"
		"    private static void foundTokenManageState(Token token) {\n"
		"        Automaton.stateTracker.lexeme = new StringBuilder();\n"
		"        Automaton.stateTracker.token = token;\n"
		"    }\n"
		"\n"
		"    private static void updateStateInRange(char[] charArray, int rangeStart, int rangeEnd) {\n"
		"        for (int i = rangeStart; i < rangeEnd; i++) {\n"
		"            manageState(charArray[i]);\n"
		"        }\n"
		"    }\n"
		"\n"
		"    private record TokenAndConsume(Token token, int consumeIndex) {\n"
		"    }\n"
		"\n"
		"    private static TokenAndConsume getNextToken(char[] charArray, int readIndex) {\n"
		"        State current = initialState;\n"
		"        Function<StateTracker, Token> foundToken = null;\n"
		"        int consumeIndex = readIndex, startIndex = readIndex;\n"
		"        while (current != null && readIndex < charArray.length) {\n"
		"            Function<StateTracker, Token> aux = current.tokenGenerator;\n"
		"            if (aux != null) {\n"
		"                foundToken = aux;\n"
		"                consumeIndex = readIndex;\n"
		"            }\n"
		"            current = current.getTransition(charArray[readIndex]);\n"
		"            readIndex++;\n"
		"        }\n"
		"        Function<StateTracker, Token> aux;\n"
		"        if (current != null && (aux = current.tokenGenerator) != null) {\n"
		"            foundToken = aux;\n"
		"            consumeIndex = readIndex;\n"
		"        }\n"
		"        updateStateInRange(charArray, startIndex, consumeIndex);\n"
		"        return new TokenAndConsume(foundToken != null ? foundToken.apply(stateTracker) : null, consumeIndex);\n"
		"    }\n"
		"\n"
		"    public static List<Token> getTokenList(String s) {\n"
		"        TokenAndConsume tokenAndConsume;\n"
		"        int i;\n"
		"        char[] chars = s.toCharArray();\n"
		"        List<Token> tokens = new ArrayList<>();\n"
		"        for (i = 0; i < chars.length; ) {\n"
		"            tokenAndConsume = getNextToken(chars, i);\n"
		"            foundTokenManageState(tokenAndConsume.token);\n"
		"            if (tokenAndConsume.token == null) {\n"
		"                tokens.add(Token.UNKNOWN);\n"
		"                break;\n"
		"            }\n"
		"            tokens.add(tokenAndConsume.token);\n"
		"            i = tokenAndConsume.consumeIndex;\n"
		"        }\n"
		"        return tokens;\n"
		"    }\n"
		"\n"
		"    public static class StateTracker {\n"
		"        private StringBuilder lexeme = new StringBuilder();\n"
		"        private Token token;\n"
		"        public final Attribute attribute = new Attribute();\n"
		"\n"
		"        public String getLexeme() {\n"
		"            return lexeme.toString();\n"
		"        }\n"
		"\n"
		"        public Token getToken() {\n"
		"            return token;\n"
		"        }\n"
		"\n"
		"        public Attribute getAttribute() {\n"
		"            return attribute;\n"
		"        }\n"
		"\n"
		"        public static class Attribute {\n"
		"            // User managed\n"
		"            public Integer id, num;\n"
		"            // Non-user managed\n"
		"            private Integer row = 0, column = 1;\n"
		"\n"
		"            public Integer getRow() {\n"
		"                return row;\n"
		"            }\n"
		"\n"
		"            public Integer getColumn() {\n"
		"                return column;\n"
		"            }\n"
		"        }\n"
		"    }\n"
		"\n"
		"    private static class State {\n"
		"        private final Map<Character, State> transitions;\n"
		"        private final Function<StateTracker, Token> tokenGenerator;\n"
		"\n"
		"        private State(Function<StateTracker, Token> tokenGenerator) {\n"
		"            this.tokenGenerator = tokenGenerator;\n"
		"            this.transitions = new HashMap<>();\n"
		"        }\n"
		"\n"
		"        public void setTransition(State to, char symbol) {\n"
		"            transitions.put(symbol, to);\n"
		"        }\n"
		"\n"
		"        public State getTransition(char symbol) {\n"
		"            return transitions.get(symbol);\n"
		"        }\n"
		"    }\n"
		"\n"
		"    private static boolean initialized = false;\n"
		"\n"
		"    public static void initialize() {\n"
		"        if (initialized)\n"
		"            throw new IllegalStateException();\n"
		"        initialized = true;\n"
	);
}

/**
 * Creates the epilogue of the generated output, that is, the final lines that
 * completes a valid Latex document.
 */
static void _generateEpilogue() {
	_output(0, "%s",
		"    }\n"
		"}"
	);
}

/**
 * Generates an indentation string for the specified level.
 */
static char * _indentation(const unsigned int level) {
	return indentation(_indentationCharacter, level, _indentationSize);
}

/**
 * Outputs a formatted string to standard output. The "fflush" instruction
 * allows to see the output even close to a failure, because it drops the
 * buffering.
 */
static void _output(const unsigned int indentationLevel, const char * const format, ...) {
	va_list arguments;
	va_start(arguments, format);
	char * indentation = _indentation(indentationLevel);
	char * effectiveFormat = concatenate(2, indentation, format);
	if(_outputFile != NULL) {
		vfprintf(_outputFile, effectiveFormat, arguments);
		fflush(_outputFile);
	} else {
		vfprintf(stdout, effectiveFormat, arguments);
		fflush(stdout);
	}
	free(effectiveFormat);
	free(indentation);
	va_end(arguments);
}

/** PUBLIC FUNCTIONS */

void generate(CompilerState * compilerState) {
	logDebugging(_logger, "Generating final output...");
	_generatePrologue();
	_generateProgram(compilerState->automaton);
	_generateEpilogue();
	logDebugging(_logger, "Generation is done.");
}
