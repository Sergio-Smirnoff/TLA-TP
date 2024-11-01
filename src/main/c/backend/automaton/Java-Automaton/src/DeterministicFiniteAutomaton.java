import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DeterministicFiniteAutomaton {

    private static State initialState;
    private static final List<State> states = new ArrayList<>();

    private DeterministicFiniteAutomaton() {}

    public static State newStateGetState(Token token) {
        State state = new State(token);
        states.add(state);
        return state;
    }

    public static int newState(Token token) {
        State state = new State(token);
        states.add(state);
        return states.size() - 1;
    }

    public static void setInitialState(State state) {
        initialState = state;
    }

    public static void setInitialState(int index) {
        initialState = states.get(index);
    }

    public static void setTransition(int from, int to, char symbol) {
        states.get(from).setTransition(states.get(to), symbol);
    }

    public static List<Token> getTokenList(String s) {
        char[] chars = s.toCharArray();
        List<Token> tokens = new ArrayList<>();
        State currentState = initialState;

        for (char c : chars) {
            if (currentState == null) break;
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

        private String lexeme;
        private final int tokenType;

        public Token(int tokenType) {
            this.tokenType = tokenType;
        }

        public String getLexeme() {
            return lexeme;
        }

        public void setLexeme(String lexeme) {
            this.lexeme = lexeme;
        }

        public int getTokenType() {
            return tokenType;
        }
    }
}