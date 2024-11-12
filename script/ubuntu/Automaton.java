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
        /*1*/Automaton.newState(var -> ANYTHING);
        /*2*/Automaton.newState(var -> ANYTHING);


        Automaton.setTransition(0, 1, '\t');
        Automaton.setTransition(0, 1, '\n');
        Automaton.setTransition(0, 1, '\r');
        Automaton.setTransition(0, 1, ' ');
        Automaton.setTransition(0, 1, '!');
        Automaton.setTransition(0, 1, '"');
        Automaton.setTransition(0, 1, '#');
        Automaton.setTransition(0, 1, '$');
        Automaton.setTransition(0, 1, '%');
        Automaton.setTransition(0, 1, '&');
        Automaton.setTransition(0, 1, '\'');
        Automaton.setTransition(0, 1, '(');
        Automaton.setTransition(0, 1, ')');
        Automaton.setTransition(0, 1, '*');
        Automaton.setTransition(0, 1, '+');
        Automaton.setTransition(0, 1, ',');
        Automaton.setTransition(0, 1, '-');
        Automaton.setTransition(0, 1, '.');
        Automaton.setTransition(0, 1, '/');
        Automaton.setTransition(0, 1, '0');
        Automaton.setTransition(0, 1, '1');
        Automaton.setTransition(0, 1, '2');
        Automaton.setTransition(0, 1, '3');
        Automaton.setTransition(0, 1, '4');
        Automaton.setTransition(0, 1, '5');
        Automaton.setTransition(0, 1, '6');
        Automaton.setTransition(0, 1, '7');
        Automaton.setTransition(0, 1, '8');
        Automaton.setTransition(0, 1, '9');
        Automaton.setTransition(0, 1, ':');
        Automaton.setTransition(0, 1, ';');
        Automaton.setTransition(0, 1, '<');
        Automaton.setTransition(0, 1, '=');
        Automaton.setTransition(0, 1, '>');
        Automaton.setTransition(0, 1, '?');
        Automaton.setTransition(0, 1, '@');
        Automaton.setTransition(0, 1, 'A');
        Automaton.setTransition(0, 1, 'B');
        Automaton.setTransition(0, 1, 'C');
        Automaton.setTransition(0, 1, 'D');
        Automaton.setTransition(0, 1, 'E');
        Automaton.setTransition(0, 1, 'F');
        Automaton.setTransition(0, 1, 'G');
        Automaton.setTransition(0, 1, 'H');
        Automaton.setTransition(0, 1, 'I');
        Automaton.setTransition(0, 1, 'J');
        Automaton.setTransition(0, 1, 'K');
        Automaton.setTransition(0, 1, 'L');
        Automaton.setTransition(0, 1, 'M');
        Automaton.setTransition(0, 1, 'N');
        Automaton.setTransition(0, 1, 'O');
        Automaton.setTransition(0, 1, 'P');
        Automaton.setTransition(0, 1, 'Q');
        Automaton.setTransition(0, 1, 'R');
        Automaton.setTransition(0, 1, 'S');
        Automaton.setTransition(0, 1, 'T');
        Automaton.setTransition(0, 1, 'U');
        Automaton.setTransition(0, 1, 'V');
        Automaton.setTransition(0, 1, 'W');
        Automaton.setTransition(0, 1, 'X');
        Automaton.setTransition(0, 1, 'Y');
        Automaton.setTransition(0, 1, 'Z');
        Automaton.setTransition(0, 1, '[');
        Automaton.setTransition(0, 1, '\\');
        Automaton.setTransition(0, 1, ']');
        Automaton.setTransition(0, 1, '^');
        Automaton.setTransition(0, 1, '_');
        Automaton.setTransition(0, 1, '`');
        Automaton.setTransition(0, 1, 'a');
        Automaton.setTransition(0, 1, 'b');
        Automaton.setTransition(0, 1, 'c');
        Automaton.setTransition(0, 1, 'd');
        Automaton.setTransition(0, 1, 'e');
        Automaton.setTransition(0, 1, 'f');
        Automaton.setTransition(0, 1, 'g');
        Automaton.setTransition(0, 1, 'h');
        Automaton.setTransition(0, 1, 'i');
        Automaton.setTransition(0, 1, 'j');
        Automaton.setTransition(0, 1, 'k');
        Automaton.setTransition(0, 1, 'l');
        Automaton.setTransition(0, 1, 'm');
        Automaton.setTransition(0, 1, 'n');
        Automaton.setTransition(0, 1, 'o');
        Automaton.setTransition(0, 1, 'p');
        Automaton.setTransition(0, 1, 'q');
        Automaton.setTransition(0, 1, 'r');
        Automaton.setTransition(0, 1, 's');
        Automaton.setTransition(0, 1, 't');
        Automaton.setTransition(0, 1, 'u');
        Automaton.setTransition(0, 1, 'v');
        Automaton.setTransition(0, 1, 'w');
        Automaton.setTransition(0, 1, 'x');
        Automaton.setTransition(0, 1, 'y');
        Automaton.setTransition(0, 1, 'z');
        Automaton.setTransition(0, 1, '{');
        Automaton.setTransition(0, 1, '|');
        Automaton.setTransition(0, 1, '}');
        Automaton.setTransition(0, 1, '~');
        Automaton.setTransition(1, 2, '\t');
        Automaton.setTransition(1, 2, '\n');
        Automaton.setTransition(1, 2, '\r');
        Automaton.setTransition(1, 2, ' ');
        Automaton.setTransition(1, 2, '!');
        Automaton.setTransition(1, 2, '"');
        Automaton.setTransition(1, 2, '#');
        Automaton.setTransition(1, 2, '$');
        Automaton.setTransition(1, 2, '%');
        Automaton.setTransition(1, 2, '&');
        Automaton.setTransition(1, 2, '\'');
        Automaton.setTransition(1, 2, '(');
        Automaton.setTransition(1, 2, ')');
        Automaton.setTransition(1, 2, '*');
        Automaton.setTransition(1, 2, '+');
        Automaton.setTransition(1, 2, ',');
        Automaton.setTransition(1, 2, '-');
        Automaton.setTransition(1, 2, '.');
        Automaton.setTransition(1, 2, '/');
        Automaton.setTransition(1, 2, '0');
        Automaton.setTransition(1, 2, '1');
        Automaton.setTransition(1, 2, '2');
        Automaton.setTransition(1, 2, '3');
        Automaton.setTransition(1, 2, '4');
        Automaton.setTransition(1, 2, '5');
        Automaton.setTransition(1, 2, '6');
        Automaton.setTransition(1, 2, '7');
        Automaton.setTransition(1, 2, '8');
        Automaton.setTransition(1, 2, '9');
        Automaton.setTransition(1, 2, ':');
        Automaton.setTransition(1, 2, ';');
        Automaton.setTransition(1, 2, '<');
        Automaton.setTransition(1, 2, '=');
        Automaton.setTransition(1, 2, '>');
        Automaton.setTransition(1, 2, '?');
        Automaton.setTransition(1, 2, '@');
        Automaton.setTransition(1, 2, 'A');
        Automaton.setTransition(1, 2, 'B');
        Automaton.setTransition(1, 2, 'C');
        Automaton.setTransition(1, 2, 'D');
        Automaton.setTransition(1, 2, 'E');
        Automaton.setTransition(1, 2, 'F');
        Automaton.setTransition(1, 2, 'G');
        Automaton.setTransition(1, 2, 'H');
        Automaton.setTransition(1, 2, 'I');
        Automaton.setTransition(1, 2, 'J');
        Automaton.setTransition(1, 2, 'K');
        Automaton.setTransition(1, 2, 'L');
        Automaton.setTransition(1, 2, 'M');
        Automaton.setTransition(1, 2, 'N');
        Automaton.setTransition(1, 2, 'O');
        Automaton.setTransition(1, 2, 'P');
        Automaton.setTransition(1, 2, 'Q');
        Automaton.setTransition(1, 2, 'R');
        Automaton.setTransition(1, 2, 'S');
        Automaton.setTransition(1, 2, 'T');
        Automaton.setTransition(1, 2, 'U');
        Automaton.setTransition(1, 2, 'V');
        Automaton.setTransition(1, 2, 'W');
        Automaton.setTransition(1, 2, 'X');
        Automaton.setTransition(1, 2, 'Y');
        Automaton.setTransition(1, 2, 'Z');
        Automaton.setTransition(1, 2, '[');
        Automaton.setTransition(1, 2, '\\');
        Automaton.setTransition(1, 2, ']');
        Automaton.setTransition(1, 2, '^');
        Automaton.setTransition(1, 2, '_');
        Automaton.setTransition(1, 2, '`');
        Automaton.setTransition(1, 2, 'a');
        Automaton.setTransition(1, 2, 'b');
        Automaton.setTransition(1, 2, 'c');
        Automaton.setTransition(1, 2, 'd');
        Automaton.setTransition(1, 2, 'e');
        Automaton.setTransition(1, 2, 'f');
        Automaton.setTransition(1, 2, 'g');
        Automaton.setTransition(1, 2, 'h');
        Automaton.setTransition(1, 2, 'i');
        Automaton.setTransition(1, 2, 'j');
        Automaton.setTransition(1, 2, 'k');
        Automaton.setTransition(1, 2, 'l');
        Automaton.setTransition(1, 2, 'm');
        Automaton.setTransition(1, 2, 'n');
        Automaton.setTransition(1, 2, 'o');
        Automaton.setTransition(1, 2, 'p');
        Automaton.setTransition(1, 2, 'q');
        Automaton.setTransition(1, 2, 'r');
        Automaton.setTransition(1, 2, 's');
        Automaton.setTransition(1, 2, 't');
        Automaton.setTransition(1, 2, 'u');
        Automaton.setTransition(1, 2, 'v');
        Automaton.setTransition(1, 2, 'w');
        Automaton.setTransition(1, 2, 'x');
        Automaton.setTransition(1, 2, 'y');
        Automaton.setTransition(1, 2, 'z');
        Automaton.setTransition(1, 2, '{');
        Automaton.setTransition(1, 2, '|');
        Automaton.setTransition(1, 2, '}');
        Automaton.setTransition(1, 2, '~');
        Automaton.setTransition(2, 2, '\t');
        Automaton.setTransition(2, 2, '\n');
        Automaton.setTransition(2, 2, '\r');
        Automaton.setTransition(2, 2, ' ');
        Automaton.setTransition(2, 2, '!');
        Automaton.setTransition(2, 2, '"');
        Automaton.setTransition(2, 2, '#');
        Automaton.setTransition(2, 2, '$');
        Automaton.setTransition(2, 2, '%');
        Automaton.setTransition(2, 2, '&');
        Automaton.setTransition(2, 2, '\'');
        Automaton.setTransition(2, 2, '(');
        Automaton.setTransition(2, 2, ')');
        Automaton.setTransition(2, 2, '*');
        Automaton.setTransition(2, 2, '+');
        Automaton.setTransition(2, 2, ',');
        Automaton.setTransition(2, 2, '-');
        Automaton.setTransition(2, 2, '.');
        Automaton.setTransition(2, 2, '/');
        Automaton.setTransition(2, 2, '0');
        Automaton.setTransition(2, 2, '1');
        Automaton.setTransition(2, 2, '2');
        Automaton.setTransition(2, 2, '3');
        Automaton.setTransition(2, 2, '4');
        Automaton.setTransition(2, 2, '5');
        Automaton.setTransition(2, 2, '6');
        Automaton.setTransition(2, 2, '7');
        Automaton.setTransition(2, 2, '8');
        Automaton.setTransition(2, 2, '9');
        Automaton.setTransition(2, 2, ':');
        Automaton.setTransition(2, 2, ';');
        Automaton.setTransition(2, 2, '<');
        Automaton.setTransition(2, 2, '=');
        Automaton.setTransition(2, 2, '>');
        Automaton.setTransition(2, 2, '?');
        Automaton.setTransition(2, 2, '@');
        Automaton.setTransition(2, 2, 'A');
        Automaton.setTransition(2, 2, 'B');
        Automaton.setTransition(2, 2, 'C');
        Automaton.setTransition(2, 2, 'D');
        Automaton.setTransition(2, 2, 'E');
        Automaton.setTransition(2, 2, 'F');
        Automaton.setTransition(2, 2, 'G');
        Automaton.setTransition(2, 2, 'H');
        Automaton.setTransition(2, 2, 'I');
        Automaton.setTransition(2, 2, 'J');
        Automaton.setTransition(2, 2, 'K');
        Automaton.setTransition(2, 2, 'L');
        Automaton.setTransition(2, 2, 'M');
        Automaton.setTransition(2, 2, 'N');
        Automaton.setTransition(2, 2, 'O');
        Automaton.setTransition(2, 2, 'P');
        Automaton.setTransition(2, 2, 'Q');
        Automaton.setTransition(2, 2, 'R');
        Automaton.setTransition(2, 2, 'S');
        Automaton.setTransition(2, 2, 'T');
        Automaton.setTransition(2, 2, 'U');
        Automaton.setTransition(2, 2, 'V');
        Automaton.setTransition(2, 2, 'W');
        Automaton.setTransition(2, 2, 'X');
        Automaton.setTransition(2, 2, 'Y');
        Automaton.setTransition(2, 2, 'Z');
        Automaton.setTransition(2, 2, '[');
        Automaton.setTransition(2, 2, '\\');
        Automaton.setTransition(2, 2, ']');
        Automaton.setTransition(2, 2, '^');
        Automaton.setTransition(2, 2, '_');
        Automaton.setTransition(2, 2, '`');
        Automaton.setTransition(2, 2, 'a');
        Automaton.setTransition(2, 2, 'b');
        Automaton.setTransition(2, 2, 'c');
        Automaton.setTransition(2, 2, 'd');
        Automaton.setTransition(2, 2, 'e');
        Automaton.setTransition(2, 2, 'f');
        Automaton.setTransition(2, 2, 'g');
        Automaton.setTransition(2, 2, 'h');
        Automaton.setTransition(2, 2, 'i');
        Automaton.setTransition(2, 2, 'j');
        Automaton.setTransition(2, 2, 'k');
        Automaton.setTransition(2, 2, 'l');
        Automaton.setTransition(2, 2, 'm');
        Automaton.setTransition(2, 2, 'n');
        Automaton.setTransition(2, 2, 'o');
        Automaton.setTransition(2, 2, 'p');
        Automaton.setTransition(2, 2, 'q');
        Automaton.setTransition(2, 2, 'r');
        Automaton.setTransition(2, 2, 's');
        Automaton.setTransition(2, 2, 't');
        Automaton.setTransition(2, 2, 'u');
        Automaton.setTransition(2, 2, 'v');
        Automaton.setTransition(2, 2, 'w');
        Automaton.setTransition(2, 2, 'x');
        Automaton.setTransition(2, 2, 'y');
        Automaton.setTransition(2, 2, 'z');
        Automaton.setTransition(2, 2, '{');
        Automaton.setTransition(2, 2, '|');
        Automaton.setTransition(2, 2, '}');
        Automaton.setTransition(2, 2, '~');

    }
}