
#ifndef AUTM_H
#define AUTM_H

#include <stdint.h>

typedef void *token_t;
#define UNKNOWN_TOKEN (token_t)1

typedef struct rule
{
    uint64_t *next_indices;
    uint64_t next_indices_size;
    uint64_t next_indices_dim;
    char matcher;
} rule;

typedef struct automaton_state
{
    rule *delta;
    uint64_t delta_size;
    uint64_t delta_dim;
    uint8_t throws_token;
    token_t token;
    char min_symbol;
    char max_symbol;
} automaton_state;

typedef struct automaton
{
    automaton_state *initial_state;
    automaton_state **states;
    uint64_t states_size;
    uint64_t states_dim;
    char min_symbol;
    char max_symbol;
} automaton;

typedef struct automaton_iterator
{
    automaton *automaton;
    uint64_t state_index;
} automaton_iterator;

typedef struct state_iterator
{
    automaton_state *state;
    uint64_t rule_index;
} state_iterator;

char get_transition_matcher(rule *rule);

uint64_t *get_to_state_indices(rule *rule);
uint64_t get_to_state_indices_size(rule *rule);

void free_automaton_iterator(automaton_iterator *iterator);
void free_state_iterator(state_iterator *iterator);
automaton_iterator *get_automaton_iterator(automaton *a);
state_iterator *get_state_iterator(automaton_state *s);

automaton_state *get_next_state(automaton_iterator *iterator);
rule *get_next_rule(state_iterator *iterator);
char has_next_state(automaton_iterator *iterator);
char has_next_rule(state_iterator *iterator);
token_t get_token(automaton_state *s);
char throws_token(automaton_state *s);

uint64_t new_state(automaton *automaton, uint8_t throws_token, token_t token);

uint64_t new_state_get_index(automaton *automaton, uint8_t throws_token, token_t token);

automaton_state *get_state(const automaton *automaton, uint64_t index);

void set_initial_state(automaton *automaton, automaton_state *initial_state);
automaton *new_automaton();

/**
 * @brief set the transition of a state to another state given a matching symbol
 *
 * @param a
 * @param from
 * @param to
 * @param matcher
 * @return char 1 if succesful, 0 otherwise
 */
char set_transition(automaton *a, uint64_t from_index, uint64_t to_index, char matcher);

/**
 * @brief get the next state given a state and a symbol
 *
 * @param a
 * @param state
 * @param symbol
 * @return state* NULL if no transition for the given symbol exists
 */
automaton_state *next_state(const automaton *a, const automaton_state *state, char symbol);

/**
 * @brief returns a number representing the next token obtained from a string
 *
 * @param automaton
 * @param string_p pointer to a string, will be advanced to the index where the next token matching occurs
 * @return uint64_t*
 */
token_t get_next_token(const automaton *automaton, const char **string_p);

/**
 * @brief produces a NULL terminated array of tokens in the buffer given. If not NULL terminated, ran out of space
 *
 * @note if the string does not match any token, UNKNOWN_TOKEN will be returned as a token
 * @param automaton
 * @param string
 * @return uint64_t* the buffer given
 */
token_t *get_token_stream(const automaton *automaton, const char *string, token_t *buffer, uint64_t buffer_size);

/**
 * @brief whether an automaton accepts the given string
 *
 * @param a
 * @param string
 * @return char
 */
char accepts(const automaton *a, const char *string);

/**
 * @brief produces an equivalent deterministic automaton
 *
 * @param automaton
 * @return automaton
 */
automaton *get_deterministic_equivalent(const automaton *automaton);

/**
 * @brief Writes to a file descriptor Java syntax code for initializing a clone of an automaton
 *
 * @param a
 * @param file_descriptor
 */
void write_java_initialization(const automaton *a, int file_descriptor);

// boolean hasNextLine(automaton *a);
// char* nextLine(automaton *a);

/**
 * @brief Set the token thrown by a state
 *
 * @note Will not change the token thrown by a state that already throws a token
 * @param a
 * @param state_index
 * @param token
 * @return char 0 if the state already threw a token, 1 otherwise
 */
char set_token(automaton *a, uint64_t state_index, token_t token);

void free_state(automaton_state *state);
void free_automaton(automaton *automaton);

#endif
