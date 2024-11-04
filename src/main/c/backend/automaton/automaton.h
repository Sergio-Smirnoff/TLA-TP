
#ifndef AUTM_H
#define AUTM_H

#include <stdint.h>

typedef struct rule
{
    uint64_t *next_indices;
    uint64_t next_indices_size;
    uint64_t next_indices_dim;
    char matcher;
} rule;

typedef struct State
{
    rule *delta;
    uint64_t delta_size;
    uint64_t delta_dim;
    uint8_t throws_token;
    uint64_t token;
} State;

typedef struct token_mapping
{
    uint64_t token_number;
    char *token_name;
} token_mapping;

typedef struct automaton
{
    State *initial_state;
    State **states;
    uint64_t states_size;
    uint64_t states_dim;
    char min_symbol;
    char max_symbol;
} automaton;

uint64_t new_state(automaton *automaton, uint8_t throws_token, uint64_t token);

uint64_t new_state_get_index(automaton *automaton, uint8_t throws_token, uint64_t token);

State *get_state(const automaton *automaton, uint64_t index);

void set_initial_state(automaton *automaton, State *initial_state);
automaton *new_automaton();

/**
 * @brief set the transition of a State to another State given a matching symbol
 *
 * @param a
 * @param from
 * @param to
 * @param matcher
 * @return char 1 if succesful, 0 otherwise
 */
char set_transition(automaton *a, uint64_t from_index, uint64_t to_index, char matcher);

/**
 * @brief get the next State given a State and a symbol
 *
 * @param a
 * @param State
 * @param symbol
 * @return State* NULL if no transition for the given symbol exists
 */
State *next_state(const automaton *a, const State *State, char symbol);

/**
 * @brief returns a number representing the next token obtained from a string
 *
 * @param automaton
 * @param string_p pointer to a string, will be advanced to the index where the next token matching occurs
 * @return uint64_t*
 */
uint64_t get_next_token(const automaton *automaton, const char **string_p);

/**
 * @brief produces a 0 terminated array of tokens in the buffer given. If not 0 terminated, ran out of space
 *
 * @param automaton
 * @param string
 * @return uint64_t* the buffer given
 */
uint64_t *get_token_stream(const automaton *automaton, const char *string, uint64_t *buffer, uint64_t buffer_size);

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

void free_state(State *State);
void free_automaton(automaton *automaton);

#endif
