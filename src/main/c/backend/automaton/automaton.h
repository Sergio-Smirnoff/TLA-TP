
#ifndef AUTM_H
#define AUTM_H

#include <stdint.h>

typedef struct rule
{
    struct state *next;
    char matcher;
} rule;

typedef struct state
{
    rule *delta;
    uint64_t delta_size;
    uint64_t delta_dim;
    uint8_t throws_token;
    uint64_t token;
} state;

typedef struct token_mapping
{
    uint64_t token_number;
    char *token_name;
} token_mapping;

typedef struct automaton
{
    state *initial_state;
    state **states;
    uint64_t states_size;
    uint64_t states_dim;
} automaton;

state *new_state(automaton *automaton, uint8_t throws_token, uint64_t token);

void set_initial_state(automaton *automaton, state *initial_state);
automaton *new_automaton();

/**
 * @brief set the transition of a state to another state given a matching symbol
 *
 * @param from
 * @param to
 * @param matcher
 * @return char 1 if succesful, 0 otherwise
 */
char set_transition(state *from, state *to, char matcher);

/**
 * @brief get the next state given a state and a symbol
 *
 * @param state
 * @param symbol
 * @return state* NULL if no transition for the given symbol exists
 */
state *next_state(state *state, char symbol);

/**
 * @brief returns a number representing the next token obtained from a string
 *
 * @param automaton
 * @param string_p pointer to a string, will be advanced to the index where the next token matching occurs
 * @return uint64_t*
 */
uint64_t get_next_token(automaton *automaton, const char **string_p);

/**
 * @brief produces a 0 terminated array of tokens in the buffer given. If not 0 terminated, ran out of space
 *
 * @param automaton
 * @param string
 * @return uint64_t* the buffer given
 */
uint64_t *get_token_stream(automaton *automaton, const char *string, uint64_t *buffer, uint64_t buffer_size);

/**
 * @brief produces an equivalent deterministic automaton
 *
 * @param automaton
 * @return automaton
 */
automaton *get_deterministic_equivalent(automaton *automaton);

void free_state(state *state);
void free_automaton(automaton *automaton);

#endif
