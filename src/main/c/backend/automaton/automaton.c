#include "automaton.h"
#include <stdlib.h>
#define BLOCK 32

void resize_automaton(automaton *automaton)
{
    automaton->states_dim *= 2;
    automaton->states = realloc(automaton->states, automaton->states_dim);
}

void check_resize_automaton(automaton *automaton)
{
    if (automaton->states_dim == automaton->states_size)
        resize_automaton(automaton);
}

state *new_state(automaton *automaton, uint8_t throws_token, uint64_t token)
{
    state *state = malloc(sizeof(state));
    state->delta = calloc(BLOCK, sizeof(rule));
    state->delta_dim = BLOCK;
    state->delta_size = 0;
    state->throws_token = throws_token;
    state->token = token;
    check_resize_automaton(automaton);
    automaton->states[automaton->states_size++] = state;
    return state;
}

void set_initial_state(automaton *automaton, state *initial_state)
{
    automaton->initial_state = initial_state;
}

automaton *new_automaton()
{
    automaton *automaton = malloc(sizeof(automaton));
    automaton->states = malloc(sizeof(state *) * BLOCK);
    automaton->states_dim = BLOCK;
    automaton->states_size = 0;
    return automaton;
}

void resize_state(state *state)
{
    state->delta_dim *= 2;
    state->delta = realloc(state->delta, state->delta_dim);
}

void check_resize_state(state *state)
{
    if (state->delta_dim == state->delta_size)
        resize_state(state);
}

char set_transition(state *from, state *to, char matcher)
{
    check_resize_state(from);
    from->delta[from->delta_size].matcher = matcher;
    from->delta[from->delta_size++].next = to;
}

rule find_rule(state *state, char symbol)
{
    uint64_t size = state->delta_size;
    for (uint64_t i = 0; i < size; i++)
        if (state->delta[i].matcher == symbol)
        {
            return state->delta[i];
        }
    rule to_return;
    to_return.matcher = 0;
    to_return.next = NULL;
    return to_return;
}

state *next_state(state *state, char symbol)
{
    rule rule = find_rule(state, symbol);
    return rule.next;
}

uint64_t get_next_token(automaton *automaton, const char **string_p)
{
    state *current = automaton->initial_state;
    while (current != NULL && *string_p[0])
    {
        if (current->throws_token)
            return current->token;
        current = next_state(current, *string_p[0]);
        (*string_p)++;
    }
    if (current->throws_token)
        return current->token;
    return -1;
}

uint64_t *get_token_stream(automaton *automaton, const char *string, uint64_t *buffer, uint64_t buffer_size)
{
    uint64_t token, i;
    for (i = 0; i < buffer_size && string[0]; i++)
    {
        token = get_next_token(automaton, &string);
        buffer[i] = token;
        if (token == -1)
        {
            i++;
            break;
        }
    }
    if (i < buffer_size)
        buffer[i] = 0;
    return buffer;
}

automaton *get_deterministic_equivalent(automaton *automaton)
{
    // haha no
    return automaton;
}

void free_state(state *state)
{
    free(state->delta);
    free(state);
}
void free_automaton(automaton *automaton)
{
    for (int i = 0; i < automaton->states_size; i++)
        free_state(automaton->states[i]);
    free(automaton->states);
    free(automaton);
}