import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DeterministicFiniteAutomaton {

    private State initialState;
    private final List<State> states;

    public DeterministicFiniteAutomaton() {
        this.states = new ArrayList<>();
    }

    public State newState(Token token) {
        State state = new State(token);
        states.add(state);
        return state;
    }

    public int newStateGetIndex(Token token) {
        State state = new State(token);
        states.add(state);
        return states.size() - 1;
    }

    public DeterministicFiniteAutomaton setInitialState(State state) {
        this.initialState = state;
        return this;
    }

    public DeterministicFiniteAutomaton setInitialState(int index) {
        this.initialState = states.get(index);
        return this;
    }

    public DeterministicFiniteAutomaton setTransition(int from, int to, char symbol) {
        states.get(from).setTransition(states.get(to), symbol);
        return this;
    }

    public List<Token> getTokenList(String s) {
        char[] chars = s.toCharArray();
        List<Token> tokens = new ArrayList<>();
        State currentState = initialState;

        for (char c : chars) {
            if (currentState == null)
                break;
            currentState = currentState.getTransition(c);
            if (currentState.token != null) {
                tokens.add(currentState.token);
            }
        }

        return tokens;
    }

    public static class State {
        private final Map<Character, State> transitions;
        private final Token token;

        private State(Token token) {
            this.token = token;
            this.transitions = new HashMap<>();
        }

        public void setTransition(State to, char symbol) {
            transitions.put(symbol, to);
        }

        public State getTransition(char symbol) {
            return transitions.get(symbol);
        }
    }

    public static class Token {

        private final String lexeme;
        private final int tokenType;
        private static final Map<Integer, Token> TOKEN_CACHE = new HashMap<>();

        private Token(int tokenType, String lexeme) {
            this.tokenType = tokenType;
            this.lexeme = lexeme;
        }

        public Token newToken(int tokenType, String lexeme) {
            if (lexeme == null)
                return TOKEN_CACHE.computeIfAbsent(tokenType, (token) -> new Token(tokenType, null));
            return new Token(tokenType, lexeme);
        }

        public String getLexeme() {
            return lexeme;
        }

        public int getTokenType() {
            return tokenType;
        }
    }
}