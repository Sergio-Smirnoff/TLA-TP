#include "automaton.h"
#include <stdlib.h>
#include <stdio.h>
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

void resize_rule(rule *r)
{
    r->next_indices_dim *= 2;
    r->next_indices = realloc(r->next_indices, sizeof(uint64_t) * r->next_indices_dim);
}

void check_resize_rule(rule *r)
{
    if (r->next_indices_dim == r->next_indices_size)
        resize_rule(r);
}

rule *find_rule(const state *s, char symbol)
{
    uint64_t size = s->delta_size;
    for (uint64_t i = 0; i < size; i++)
        if (s->delta[i].matcher == symbol)
        {
            return s->delta + i;
        }
    return NULL;
}

char set_state_transition(state *from, uint64_t to_index, char matcher)
{
    rule *r;
    if ((r = find_rule(from, matcher)) != NULL)
    {
        check_resize_rule(r);
        r->next_indices[r->next_indices_size++] = to_index;
        return 1;
    }

    check_resize_state(from);
    from->delta[from->delta_size].matcher = matcher;
    from->delta[from->delta_size].next_indices_dim = 1;
    from->delta[from->delta_size].next_indices_size = 1;
    from->delta[from->delta_size].next_indices = malloc(sizeof(uint64_t) * 1);
    from->delta[from->delta_size++].next_indices[0] = to_index;
    return 1;
}

char force_set_transition(const automaton *a, uint64_t from_index, uint64_t to_index, char matcher)
{
    if (from_index >= a->states_size)
        return 0;
    state *from = get_state(a, from_index);
    return set_state_transition(from, to_index, matcher);
}

char set_transition(const automaton *a, uint64_t from_index, uint64_t to_index, char matcher)
{
    if (to_index >= a->states_size)
        return 0;
    return force_set_transition(a, from_index, to_index, matcher);
}

state *next_state(const automaton *a, const state *s, char symbol)
{
    rule *rule = find_rule(s, symbol);
    if (rule == NULL)
        return NULL;
    return get_state(a, rule->next_indices[0]);
}

uint64_t get_next_token(const automaton *a, const char **string_p)
{
    state *current = a->initial_state;
    const char *s = *string_p;
    uint64_t found_token = -1;
    while (current != NULL && *s)
    {
        if (current->throws_token)
        {
            // store the most recent token found
            found_token = current->token;
            // consume the string up to that token
            *string_p = s;
        }
        current = next_state(a, current, *s);
        s++;
    }
    if (current != NULL && current->throws_token)
    {
        found_token = current->token;
        // consume the string up to that token
        *string_p = s;
    }
    return found_token;
}

uint64_t *get_token_stream(const automaton *a, const char *string, uint64_t *buffer, uint64_t buffer_size)
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

state *get_state(const automaton *a, uint64_t index)
{
    if (a->states_size > index)
        return a->states[index];
    return NULL;
}

void free_state(state *s)
{
    for (uint64_t i = 0; i < s->delta_size; i++)
        free(s->delta[i].next_indices);
    free(s->delta);
    free(s);
}

void free_automaton(automaton *a)
{
    for (uint64_t i = 0; i < a->states_size; i++)
        free_state(a->states[i]);
    free(a->states);
    free(a);
}

void add_all_transitions(state *from, state *to)
{
    for (uint64_t i = 0; i < from->delta_size; i++)
        for (uint64_t j = 0; j < to->delta[i].next_indices_size; j++)
            set_state_transition(from, to->delta[i].next_indices[j], to->delta->matcher);
}

typedef struct delta_table_entry
{
    uint64_t *state_indices;
    uint64_t state_indices_size;
} delta_table_entry;

typedef struct delta_table
{
    delta_table_entry **entries;
    uint64_t entries_size;
    uint64_t entries_dim;
} delta_table;

void resize_delta_table(delta_table *table)
{
    table->entries_dim *= 2;
    table->entries = realloc(table->entries, table->entries_dim);
}

void check_delta_table_resize(delta_table *table)
{
    if (table->entries_size == table->entries_dim)
        resize_delta_table(table);
}

void free_delta_table_entry(delta_table_entry *entry)
{
    free(entry->state_indices);
    free(entry);
}

void free_delta_table(delta_table *table)
{
    for (uint64_t i = 0; i < table->entries_size; i++)
        free_delta_table_entry(table->entries[i]);
    free(table->entries);
    free(table);
}

uint64_t find_state_index(const uint64_t *state_indices, uint64_t state_indices_size, const uint64_t index)
{
    for (uint64_t i = 0; i < state_indices_size; i++)
        if (state_indices[i] == index)
            return i;
    return -1;
}

char are_equal_entries(const uint64_t *state_indices_1, uint64_t state_indices_size_1, const uint64_t *state_indices_2, uint64_t state_indices_size_2)
{
    if (state_indices_size_1 != state_indices_size_2)
        return 0;
    for (uint64_t i = 0; i < state_indices_size_1; i++)
        if (find_state_index(state_indices_2, state_indices_size_2, state_indices_1[i]) == -1)
            return 0;
    return 1;
}

uint64_t find_entry_index(const delta_table *table, const uint64_t *state_indices, uint64_t state_indices_size)
{
    // Table search is from end to beginning because the table is used in such a way that searches are more likely to be at the ends. Hence, this might be slightly faster
    // This implementation is like O(n⁴), this change barely matters, but it kinda makes me feel better knowing that it's there.
    for (uint64_t i = 0; i < table->entries_size; i++)
    {
        if (are_equal_entries(table->entries[i]->state_indices, table->entries[i]->state_indices_size, state_indices, state_indices_size))
            return i;
    }
    return -1;
}

uint64_t load_entry_column(delta_table *table, uint64_t *state_indices, uint64_t state_indices_size)
{
    check_delta_table_resize(table);
    delta_table_entry *new_entry = malloc(sizeof(delta_table_entry));
    table->entries[table->entries_size] = new_entry;
    new_entry->state_indices = state_indices;
    new_entry->state_indices_size = state_indices_size;
    return table->entries_size++;
}

char array_contains(const uint64_t *array, uint64_t array_size, uint64_t value)
{
    for (uint64_t i = 0; i < array_size; i++)
        if (array[i] == value)
            return 1;
    return 0;
}

char populate_entry(const automaton *a, automaton *dfa, delta_table *table, uint64_t index)
{
    if (index >= table->entries_size || table->entries[index]->state_indices == NULL || !table->entries[index]->state_indices_size)
        return 0;

    delta_table_entry *entry = table->entries[index];
    char throws_token = 0;
    uint64_t token = 0;
    for (uint64_t i = 0; i < entry->state_indices_size; i++)
    {
        state *state_in_column = get_state(a, entry->state_indices[i]);
        if (state_in_column->throws_token)
        {
            throws_token = 1;
            token = state_in_column->token;
        }
    }
    uint64_t state_equivalent_index = new_state(dfa, throws_token, token);
    for (unsigned char matcher = 0; matcher <= 127; matcher++)
    {
        uint64_t *state_indices = malloc(a->states_size * sizeof(uint64_t));
        uint64_t state_indices_size = 0;

        for (uint64_t state_index = 0; state_index < entry->state_indices_size; state_index++)
        {
            state *current_state = get_state(a, entry->state_indices[state_index]);
            for (uint64_t rule_index = 0; rule_index < current_state->delta_size; rule_index++)
            {
                rule current_rule = current_state->delta[rule_index];
                if (current_rule.matcher == matcher)
                {
                    for (uint64_t transition_index = 0; transition_index < current_rule.next_indices_size; transition_index++)
                    {
                        if (!array_contains(state_indices, state_indices_size, current_rule.next_indices[transition_index]))
                            state_indices[state_indices_size++] = current_rule.next_indices[transition_index];
                    }
                }
            }
        }
        if (!state_indices_size)
            free(state_indices);
        else
        {
            uint64_t entry_index = find_entry_index(table, state_indices, state_indices_size);

            if (entry_index == -1)
            {
                entry_index = load_entry_column(table, state_indices, state_indices_size);
            }
            else
            {
                free(state_indices);
            }
            force_set_transition(dfa, state_equivalent_index, entry_index, matcher);
        }
    }
    return 1;
}

delta_table *new_delta_table()
{
    delta_table *table = malloc(sizeof(delta_table));
    table->entries = malloc(sizeof(delta_table_entry) * BLOCK);
    table->entries_size = 0;
    table->entries_dim = BLOCK;
    return table;
}

automaton *get_deterministic_equivalent(const automaton *a)
{
    automaton *dfa = new_automaton();
    delta_table *table = new_delta_table();
    for (uint64_t state_index = 0; state_index < a->states_size; state_index++)
    {
        uint64_t *index = malloc(sizeof(uint64_t) * 1);
        *index = state_index;
        load_entry_column(table, index, 1);
    }
    for (uint64_t state_index = 0; table->entries_size > dfa->states_size; state_index++)
    {
        populate_entry(a, dfa, table, state_index);
    }


    set_initial_state(dfa, dfa->states[0]);

    free_delta_table(table);

    return dfa;
}
