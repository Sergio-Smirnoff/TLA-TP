#include "automaton.h"
#include <stdlib.h>
#define BLOCK 32

void resize_automaton(automaton *a)
{
    a->states_dim *= 2;
    a->states = realloc(a->states, a->states_dim);
}

void check_resize_automaton(automaton *a)
{
    if (a->states_dim == a->states_size)
        resize_automaton(a);
}

uint64_t new_state(automaton *a, uint8_t throws_token, uint64_t token)
{
    state *n_state = malloc(sizeof(state));
    n_state->delta = calloc(BLOCK, sizeof(rule));
    n_state->delta_dim = BLOCK;
    n_state->delta_size = 0;
    n_state->throws_token = throws_token;
    n_state->token = token;
    check_resize_automaton(a);
    a->states[a->states_size] = n_state;
    return a->states_size++;
}

void set_initial_state(automaton *a, state *initial_state)
{
    a->initial_state = initial_state;
}

automaton *new_automaton()
{
    automaton *n_automaton = malloc(sizeof(automaton));
    n_automaton->states = malloc(sizeof(state *) * BLOCK);
    n_automaton->states_dim = BLOCK;
    n_automaton->states_size = 0;
    return n_automaton;
}

void resize_state(state *s)
{
    s->delta_dim *= 2;
    s->delta = realloc(s->delta, s->delta_dim);
}

void check_resize_state(state *s)
{
    if (s->delta_dim == s->delta_size)
        resize_state(s);
}

char set_transition(automaton *a, uint64_t from_index, uint64_t to_index, char matcher)
{
    if(from_index >= a->states_size || to_index >= a->states_size)
        return 0;
    state *from = get_state(a, from_index);
    check_resize_state(from);
    from->delta[from->delta_size].matcher = matcher;
    from->delta[from->delta_size++].next_index = to_index;
    return 1;
}

rule find_rule(state *s, char symbol)
{
    uint64_t size = s->delta_size;
    for (uint64_t i = 0; i < size; i++)
        if (s->delta[i].matcher == symbol)
        {
            return s->delta[i];
        }
    rule to_return;
    to_return.matcher = 0;
    to_return.next_index = -1;
    return to_return;
}

state *next_state(automaton *a, state *s, char symbol)
{
    rule rule = find_rule(s, symbol);
    return get_state(a, rule.next_index);
}

uint64_t get_next_token(automaton *a, const char **string_p)
{
    state *current = a->initial_state;
    while (current != NULL && *string_p[0])
    {
        if (current->throws_token)
            return current->token;
        current = next_state(a, current, *string_p[0]);
        (*string_p)++;
    }
    if (current->throws_token)
        return current->token;
    return -1;
}

uint64_t *get_token_stream(automaton *a, const char *string, uint64_t *buffer, uint64_t buffer_size)
{
    uint64_t token, i;
    for (i = 0; i < buffer_size && string[0]; i++)
    {
        token = get_next_token(a, &string);
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

automaton *get_deterministic_equivalent(automaton *a)
{
    // haha no
    return a;
}

state *get_state(automaton *a, uint64_t index)
{
    if (a->states_size > index)
        return a->states[index];
    return NULL;
}

void free_state(state *s)
{
    free(s->delta);
    free(s);
}

void free_automaton(automaton *a)
{
    for (int i = 0; i < a->states_size; i++)
        free_state(a->states[i]);
    free(a->states);
    free(a);
}