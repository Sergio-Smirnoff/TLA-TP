import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public class DeterministicFiniteAutomaton {

    private State initialState;
    private final List<State> states;

    public DeterministicFiniteAutomaton() {
        this.states = new ArrayList<State>();
    }

    public State newState(Token token) {
        State state = new State(token);
        states.add(state);
        return state;
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
            this.transitions = new HashMap<Character, State>();
        }

        public void setTransition(State to, char symbol) {
            transitions.put(symbol, to);
        }

        public State getTransition(char symbol) {
            return transitions.get(symbol);
        }
    }

    public static class Token {
        public enum TokenType {

        }
    }
}