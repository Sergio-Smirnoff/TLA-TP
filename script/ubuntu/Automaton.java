//INSERT PACKAGE NAME HERE
package Your_package;

import java.util.*;
import java.util.function.Function;

//EDIT THIS IMPORT TO MATCH YOUR PACKAGE
import static Your_package.Automaton.Token.*;


public abstract class Automaton {

    // EDIT THIS ENUM TO MATCH YOUR TOKENS
    public enum Token {
        PUT_YOUR_USED_TOKENS_HERE, UNKNOWN;


        private String stringContent;
        private Integer intContent;
        private Boolean boolContent;
        private Double doubleContent;
        private Boolean hasParams = false;

        public Token setStringContent(String stringContent) {
            this.stringContent = stringContent;
            this.hasParams = true;
            return this;
        }

        public Token setIntContent(Integer intContent) {
            this.intContent = intContent;
            this.hasParams = true;
            return this;
        }

        public Token setBoolContent(Boolean boolContent) {
            this.boolContent = boolContent;
            this.hasParams = true;
            return this;
        }

        public Token setDoubleContent(Double doubleContent) {
            this.doubleContent = doubleContent;
            this.hasParams = true;
            return this;
        }

        public String getStringContent() {
            return stringContent;
        }

        public Integer getIntContent() {
            return intContent;
        }

        public Boolean getBoolContent() {
            return boolContent;
        }

        public Double getDoubleContent() {
            return doubleContent;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(this.name());
            if (hasParams) {
                sb.append("{ ");
                if (this.stringContent != null) {
                    sb.append("stringContent: ").append(this.stringContent).append(" ");
                }
                if (this.intContent != null) {
                    sb.append("intContent: ").append(this.intContent).append(" ");
                }
                if (this.boolContent != null) {
                    sb.append("boolContent: ").append(this.boolContent).append(" ");
                }
                if (this.doubleContent != null) {
                    sb.append("doubleContent: ").append(this.doubleContent).append(" ");
                }
                sb.append("}");
            }
            return sb.toString();
        }
    }

    private static State initialState;
    private static final List<State> states = new ArrayList<>();
    private static StateTracker stateTracker = new StateTracker();

    public static int newState(Token token) {
        return newState((s) -> token);
    }

    public static int newState(Function<StateTracker, Token> tokenGenerator) {
        State state = new State(tokenGenerator);
        if (initialState == null)
            initialState = state;
        states.add(state);
        return states.size() - 1;
    }

    public static void setInitialState(int index) {
        initialState = states.get(index);
    }

    public static void setTransition(int from, int to, char symbol) {
        states.get(from).setTransition(states.get(to), symbol);
    }

    private static void manageState(char symbol) {
        Automaton.stateTracker.lexeme.append(symbol);
        if (symbol == '\n') {
            Automaton.stateTracker.attribute.row++;
            Automaton.stateTracker.attribute.column = 1;
        } else {
            Automaton.stateTracker.attribute.column++;
        }
    }

    private static void foundTokenManageState(Token token) {
        Automaton.stateTracker.lexeme = new StringBuilder();
		 if(token != null)
	        Automaton.stateTracker.token = token;
    }

    private static void updateStateInRange(char[] charArray, int rangeStart, int rangeEnd) {
        for (int i = rangeStart; i < rangeEnd; i++) {
            manageState(charArray[i]);
        }
    }

    private record TokenAndConsume(Token token, int consumeIndex) {
    }

    private static TokenAndConsume getNextToken(char[] charArray, int readIndex) {
        State current = initialState;
        Function<StateTracker, Token> foundToken = null;
        int consumeIndex = readIndex, startIndex = readIndex;
        while (current != null && readIndex < charArray.length) {
            Function<StateTracker, Token> aux = current.tokenGenerator;
            if (aux != null) {
                foundToken = aux;
                consumeIndex = readIndex;
            }
            current = current.getTransition(charArray[readIndex]);
            readIndex++;
        }
        Function<StateTracker, Token> aux;
        if (current != null && (aux = current.tokenGenerator) != null) {
            foundToken = aux;
            consumeIndex = readIndex;
        }
        updateStateInRange(charArray, startIndex, consumeIndex);
        if(foundToken == null)
		 	throw new NoSuchElementException("Error on row %d, from column %d to column %d: %s".formatted(stateTracker.attribute.row, startIndex, consumeIndex, stateTracker.lexeme));
        return new TokenAndConsume(foundToken.apply(stateTracker), consumeIndex);
    }

    public static List<Token> getTokenList(String s) {
	 stateTracker = new StateTracker();
        TokenAndConsume tokenAndConsume;
        int i;
        char[] chars = s.toCharArray();
        List<Token> tokens = new ArrayList<>();
        for (i = 0; i < chars.length; ) {
            tokenAndConsume = getNextToken(chars, i);
            foundTokenManageState(tokenAndConsume.token);
			 if(tokenAndConsume.token != null)
            	tokens.add(tokenAndConsume.token);
            i = tokenAndConsume.consumeIndex;
        }
        return tokens;
    }

    public static class StateTracker {
        private StringBuilder lexeme = new StringBuilder();
        private Token token;
        public final Attribute attribute = new Attribute();

        public String getLexeme() {
            return lexeme.toString();
        }

        public Token getToken() {
            return token;
        }

        public Attribute getAttribute() {
            return attribute;
        }

        public static class Attribute {
            // User managed
            public Integer id, num;
            // Non-user managed
            private Integer row = 0, column = 1;

            public Integer getRow() {
                return row;
            }

            public Integer getColumn() {
                return column;
            }
        }
    }

    private static class State {
        private final Map<Character, State> transitions;
        private final Function<StateTracker, Token> tokenGenerator;

        private State(Function<StateTracker, Token> tokenGenerator) {
            this.tokenGenerator = tokenGenerator;
            this.transitions = new HashMap<>();
        }

        public void setTransition(State to, char symbol) {
            transitions.put(symbol, to);
        }

        public State getTransition(char symbol) {
            return transitions.get(symbol);
        }
    }

    private static boolean initialized = false;

    public static void initialize() {
        if (initialized)
            throw new IllegalStateException();
        initialized = true;
        /*0*/Automaton.newState((Function<StateTracker, Token>) null);
        /*1*/Automaton.newState(var -> MANY_AB);
        /*2*/Automaton.newState((Function<StateTracker, Token>) null);
        /*3*/Automaton.newState(var -> MANY_AB);
        /*4*/Automaton.newState((Function<StateTracker, Token>) null);
        /*5*/Automaton.newState(var -> MANY_AB);


        Automaton.setTransition(1, 2, 'a');
        Automaton.setTransition(2, 2, 'a');
        Automaton.setTransition(2, 3, 'b');
        Automaton.setTransition(3, 4, 'a');
        Automaton.setTransition(3, 3, 'b');
        Automaton.setTransition(4, 4, 'a');
        Automaton.setTransition(4, 5, 'b');
        Automaton.setTransition(5, 5, 'b');

    }
}