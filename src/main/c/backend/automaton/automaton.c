#include "automaton.h"
#include "closed_hashing.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define BLOCK 32
#define DTE(x) ((delta_table_entry *)(x))
#define BIG_PRIME 1000000007

char array_contains(const uint64_t *array, uint64_t array_size, uint64_t value)
{
    for (uint64_t i = 0; i < array_size; i++)
        if (array[i] == value)
            return 1;
    return 0;
}

void bubble_sort(uint64_t *arr, uint64_t size)
{
    char sorted = 0;
    while (!sorted)
    {
        sorted = 1;
        for (uint64_t i = 0; i < size - 1; i++)
        {
            if (arr[i] > arr[i + 1])
            {
                uint64_t aux = arr[i + 1];
                arr[i + 1] = arr[i];
                arr[i] = aux;
                sorted = 0;
            }
        }
    }
}

void resize_automaton(automaton *a)
{
    a->states_dim *= 2;
    a->states = realloc(a->states, a->states_dim * sizeof(automaton_state *));
}

void check_resize_automaton(automaton *a)
{
    if (a->states_dim == a->states_size)
        resize_automaton(a);
}

uint64_t new_state(automaton *a, uint8_t throws_token, token_t token)
{
    automaton_state *n_state = malloc(sizeof(automaton_state));
    n_state->delta = calloc(BLOCK, sizeof(rule));
    n_state->delta_dim = BLOCK;
    n_state->delta_size = 0;
    n_state->throws_token = throws_token;
    n_state->token = token;
    n_state->min_symbol = n_state->max_symbol = UNINITIALIZED_BOUND;
    check_resize_automaton(a);
    a->states[a->states_size] = n_state;
    if (a->initial_state_index == -1)
        a->initial_state_index = a->states_size;
    return a->states_size++;
}

void set_initial_state(automaton *a, uint64_t initial_state_index)
{
    if (initial_state_index < a->states_size)
        a->initial_state_index = initial_state_index;
}

automaton *new_automaton()
{
    automaton *n_automaton = malloc(sizeof(automaton));
    n_automaton->states = malloc(sizeof(automaton_state *) * BLOCK);
    n_automaton->states_dim = BLOCK;
    n_automaton->states_size = 0;
    n_automaton->min_symbol = n_automaton->max_symbol = UNINITIALIZED_BOUND;
    n_automaton->initial_state_index = -1;
    return n_automaton;
}

char set_token(automaton *a, uint64_t state_index, token_t token)
{
    if (state_index >= a->states_size || a->states[state_index]->throws_token)
        return 0;
    a->states[state_index]->throws_token = 1;
    a->states[state_index]->token = token;
    return 1;
}

token_t unset_token(automaton *a, uint64_t state_index)
{
    if (state_index >= a->states_size || !a->states[state_index]->throws_token)
        return (token_t)0;
    if (!a->states[state_index]->throws_token)
        return (token_t)0;
    a->states[state_index]->throws_token = 0;
    return a->states[state_index]->token;
}

void resize_state(automaton_state *s)
{
    s->delta_dim *= 2;
    s->delta = realloc(s->delta, s->delta_dim * sizeof(rule));
}

void check_resize_state(automaton_state *s)
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

rule *find_rule(const automaton_state *s, char symbol)
{
    uint64_t size = s->delta_size;
    for (uint64_t i = 0; i < size; i++)
        if (s->delta[i]->matcher == symbol)
        {
            return s->delta[i];
        }
    return NULL;
}

char set_state_transition(automaton_state *from, uint64_t to_index, char matcher)
{
    rule *r;
    if ((r = find_rule(from, matcher)) != NULL)
    {
        if (array_contains(r->next_indices, r->next_indices_size, to_index))
            return 0;
        check_resize_rule(r);
        r->next_indices[r->next_indices_size++] = to_index;
        return 1;
    }

    check_resize_state(from);
    rule *new_rule = malloc(sizeof(rule) * 1);
    from->delta[from->delta_size++] = new_rule;
    new_rule->matcher = matcher;
    new_rule->next_indices_dim = 1;
    new_rule->next_indices_size = 1;
    new_rule->next_indices = malloc(sizeof(uint64_t) * 1);
    new_rule->next_indices[0] = to_index;
    return 1;
}

void check_automaton_matcher_bounds(automaton *a, char matcher)
{
    if (matcher < 0)
        return;
    if (a->min_symbol == UNINITIALIZED_BOUND)
    {
        a->min_symbol = matcher;
        a->max_symbol = matcher;
    }
    else
    {
        if (a->min_symbol > matcher)
            a->min_symbol = matcher;
        else if (a->max_symbol < matcher)
            a->max_symbol = matcher;
    }
}

void check_state_matcher_bounds(automaton_state *s, char matcher)
{
    if (matcher < 0)
        return;
    if (s->min_symbol == UNINITIALIZED_BOUND)
    {
        s->min_symbol = matcher;
        s->max_symbol = matcher;
    }
    else
    {
        if (s->min_symbol > matcher)
            s->min_symbol = matcher;
        else if (s->max_symbol < matcher)
            s->max_symbol = matcher;
    }
}

char force_set_transition(automaton *a, uint64_t from_index, uint64_t to_index, char matcher)
{
    if (from_index >= a->states_size)
        return 0;
    check_automaton_matcher_bounds(a, matcher);
    automaton_state *from = get_state(a, from_index);
    check_state_matcher_bounds(from, matcher);
    return set_state_transition(from, to_index, matcher);
}

char set_transition(automaton *a, uint64_t from_index, uint64_t to_index, char matcher)
{
    if (to_index >= a->states_size)
        return 0;
    return force_set_transition(a, from_index, to_index, matcher);
}

automaton_state *next_state(const automaton *a, const automaton_state *s, char symbol)
{
    rule *rule = find_rule(s, symbol);
    if (rule == NULL)
        return NULL;
    return get_state(a, rule->next_indices[0]);
}

token_t get_next_token(const automaton *a, const char **string_p)
{
    automaton_state *current = a->states[a->initial_state_index];
    const char *s = *string_p;
    token_t found_token = UNKNOWN_TOKEN;
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

char accepts(const automaton *a, const char *string)
{
    automaton_state *current = a->states[a->initial_state_index];
    while (*string)
    {
        if (current == NULL)
            return 0;
        current = next_state(a, current, *string);
        string++;
    }
    return current != NULL && current->throws_token;
}

token_t *get_token_stream(const automaton *a, const char *string, token_t *buffer, uint64_t buffer_size)
{
    token_t token;
    uint64_t i;
    for (i = 0; i < buffer_size && string[0]; i++)
    {
        token = get_next_token(a, &string);
        buffer[i] = token;
        if (token == UNKNOWN_TOKEN)
        {
            i++;
            break;
        }
    }
    if (i < buffer_size)
        buffer[i] = 0;
    return buffer;
}

automaton_state *get_state(const automaton *a, uint64_t index)
{
    if (a->states_size > index)
        return a->states[index];
    return NULL;
}

void free_state(automaton_state *s)
{
    for (uint64_t i = 0; i < s->delta_size; i++)
    {
        free(s->delta[i]->next_indices);
        free(s->delta[i]);
    }
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

typedef struct delta_table_entry
{
    uint64_t *state_indices;
    uint64_t state_indices_size;
    uint64_t state_index;
} delta_table_entry;

typedef struct delta_table
{
    delta_table_entry **entries;
    hashset *entries_set;
    uint64_t entries_size;
    uint64_t entries_dim;
} delta_table;

void resize_delta_table(delta_table *table)
{
    table->entries_dim *= 2;
    table->entries = realloc(table->entries, table->entries_dim * sizeof(delta_table_entry));
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
    free_hashset(table->entries_set);
    free(table);
}

char are_equal_sorted_entries(const uint64_t *state_indices_1, uint64_t state_indices_size_1, const uint64_t *state_indices_2, uint64_t state_indices_size_2)
{
    if (state_indices_size_1 != state_indices_size_2)
        return 0;
    for (uint64_t i = 0; i < state_indices_size_1; i++)
        if (state_indices_1[i] != state_indices_2[i])
            return 0;
    return 1;
}

uint64_t load_entry_column(delta_table *table, uint64_t *state_indices, uint64_t state_indices_size)
{
    check_delta_table_resize(table);
    delta_table_entry *new_entry = malloc(sizeof(delta_table_entry));
    table->entries[table->entries_size] = new_entry;
    new_entry->state_indices = state_indices;
    new_entry->state_indices_size = state_indices_size;
    new_entry->state_index = table->entries_size;
    hashset_add(table->entries_set, new_entry);
    return table->entries_size++;
}

char populate_delta_table_entry(const automaton *a, automaton *dfa, delta_table *table, uint64_t index)
{
    if (index >= table->entries_size || table->entries[index]->state_indices == NULL || !table->entries[index]->state_indices_size)
        return 0;

    delta_table_entry *entry = table->entries[index];
    char throws_token = 0;
    token_t token = 0;
    for (uint64_t i = 0; i < entry->state_indices_size; i++)
    {
        automaton_state *state_in_column = get_state(a, entry->state_indices[i]);
        if (state_in_column->throws_token)
        {
            throws_token = 1;
            token = state_in_column->token;
            break;
        }
    }
    uint64_t state_equivalent_index = new_state(dfa, throws_token, token);
    for (unsigned char matcher = a->min_symbol; matcher <= a->max_symbol; matcher++)
    {
        uint64_t *state_indices = malloc(a->states_size * sizeof(uint64_t));
        uint64_t state_indices_size = 0;

        for (uint64_t state_index = 0; state_index < entry->state_indices_size; state_index++)
        {
            automaton_state *current_state = get_state(a, entry->state_indices[state_index]);
            if (current_state->min_symbol == UNINITIALIZED_BOUND || current_state->min_symbol > matcher || current_state->max_symbol < matcher)
                continue;
            for (uint64_t rule_index = 0; rule_index < current_state->delta_size; rule_index++)
            {
                rule *current_rule = current_state->delta[rule_index];
                if (current_rule->matcher == matcher)
                {
                    for (uint64_t transition_index = 0; transition_index < current_rule->next_indices_size; transition_index++)
                    {
                        if (!array_contains(state_indices, state_indices_size, current_rule->next_indices[transition_index]))
                            state_indices[state_indices_size++] = current_rule->next_indices[transition_index];
                    }
                }
            }
        }
        if (!state_indices_size)
            free(state_indices);
        else
        {
            delta_table_entry mock_entry;
            mock_entry.state_indices = state_indices;
            mock_entry.state_indices_size = state_indices_size;
            mock_entry.state_index = 0;
            bubble_sort(state_indices, state_indices_size);

            void *entry = hashset_get(table->entries_set, &mock_entry);
            uint64_t entry_index;

            if (entry == NULL)
            {
                entry_index = load_entry_column(table, state_indices, state_indices_size);
            }
            else
            {
                entry_index = DTE(entry)->state_index;
                free(state_indices);
            }
            force_set_transition(dfa, state_equivalent_index, entry_index, matcher);
        }
    }
    return 1;
}

uint64_t hash_delta_table_entries(const void *element)
{
    // This hashing function is terrible
    uint64_t sum = 0;
    for (int i = 0; i < DTE(element)->state_indices_size; i++)
        sum = sum * 31 + DTE(element)->state_indices[i];
    return sum % BIG_PRIME;
}

char compare_delta_table_entries(const void *elem1, const void *elem2)
{
    return are_equal_sorted_entries(DTE(elem1)->state_indices, DTE(elem1)->state_indices_size, DTE(elem2)->state_indices, DTE(elem2)->state_indices_size);
}

void free_delta_table_entries(void *elem)
{
    // The hashset is used in parallel to an array, the elements are freed from that array
    return;
}

delta_table *new_delta_table()
{
    delta_table *table = malloc(sizeof(delta_table));
    table->entries = malloc(sizeof(delta_table_entry) * BLOCK);
    table->entries_size = 0;
    table->entries_dim = BLOCK;
    table->entries_set = new_hashset(hash_delta_table_entries, compare_delta_table_entries, free_delta_table_entries, BLOCK);
    return table;
}

char remove_transitions_by_matcher(automaton_state *s, char matcher)
{
    char removed = 0;
    for (uint64_t i = 0; i < s->delta_size; i++)
    {
        if (s->delta[i]->matcher == matcher)
        {
            removed = 1;
            free(s->delta[i]->next_indices);
            free(s->delta[i]);
        }
        if (i < s->delta_size - 1)
            s->delta[i] = s->delta[i + removed];
    }
    s->delta_size -= removed;
    return removed;
}

void merge_lambda_rules(automaton *a, automaton_state *to, uint64_t to_index, uint64_t from_index, uint64_t *ignore_state_indices, uint64_t ignore_state_indices_size)
{
    if (to_index == from_index)
        return;
    automaton_state *from = get_state(a, from_index);
    if (from->throws_token && !(to->throws_token))
    {
        to->throws_token = 1;
        to->token = from->token;
    }
    for (uint64_t rule_index = 0; rule_index < from->delta_size; rule_index++)
        if (from->delta[rule_index]->matcher != LAMBDA)
            for (uint64_t next_state_index = 0; next_state_index < from->delta[rule_index]->next_indices_size; next_state_index++)
                set_transition(a, to_index, from->delta[rule_index]->next_indices[next_state_index], from->delta[rule_index]->matcher);

    rule *lambda_rule = find_rule(from, LAMBDA);
    if (lambda_rule != NULL)
    {
        for (uint64_t next_state_index = 0; next_state_index < lambda_rule->next_indices_size; next_state_index++)
        {
            if (!array_contains(ignore_state_indices, ignore_state_indices_size, lambda_rule->next_indices[next_state_index]))
            {
                ignore_state_indices[ignore_state_indices_size++] = lambda_rule->next_indices[next_state_index];
                merge_lambda_rules(a, to, to_index, lambda_rule->next_indices[next_state_index], ignore_state_indices, ignore_state_indices_size);
            }
        }
    }
}

/* There is a more efficient way to implement this algorithm, it requires that the automaton contain no lambda-transition-cycles, this is not the case
** In essence: for every lambda transition in every state, recursively solve every lambda transition in every state led to by those transitions, removing the lambda transition before returning
** That solution is O(n) time and memory (Though solving the cycles beforehand is O(n) time and likely O(n²) memory)
** This solution is O(n²) time and O(n) memory. For the purposes of this ADT, that is sufficient.
*/
void solve_lambda_transitions(automaton *a)
{
    for (uint64_t current_state_index = 0; current_state_index < a->states_size; current_state_index++)
    {
        automaton_state *current_state = get_state(a, current_state_index);
        rule *lambda_rule = find_rule(current_state, LAMBDA);
        if (lambda_rule != NULL)
        {
            for (uint64_t next_state_index = 0; next_state_index < lambda_rule->next_indices_size; next_state_index++)
            {
                uint64_t *ignore_state_indices = malloc(sizeof(uint64_t) * a->states_size);
                ignore_state_indices[0] = current_state_index;
                merge_lambda_rules(a, current_state, current_state_index, lambda_rule->next_indices[next_state_index], ignore_state_indices, 1);
                free(ignore_state_indices);
            }
        }
    }
    for (uint64_t current_state_index = 0; current_state_index < a->states_size; current_state_index++)
        remove_transitions_by_matcher(get_state(a, current_state_index), LAMBDA);
}

automaton *get_deterministic_equivalent(automaton *a)
{
    solve_lambda_transitions(a);
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
        populate_delta_table_entry(a, dfa, table, state_index);
    }

    set_initial_state(dfa, a->initial_state_index);

    free_delta_table(table);

    return dfa;
}

typedef struct minimization_table_entry
{
    uint64_t *state_indices;
    uint64_t state_indices_size;
    uint64_t state_indices_dim;
    uint64_t state_index;
} minimization_table_entry;

typedef struct minimization_table
{
    minimization_table_entry **entries;
    uint64_t entries_size;
    uint64_t entries_dim;
    const automaton *source;
    automaton *result;
} minimization_table;
void print_minimization_table(const minimization_table *table);

minimization_table *new_minimization_table(const automaton *source, automaton *result)
{
    minimization_table *table = malloc(sizeof(minimization_table));
    table->entries = malloc(sizeof(minimization_table_entry) * BLOCK);
    table->entries_size = 0;
    table->entries_dim = BLOCK;
    table->source = source;
    table->result = result;
    return table;
}

uint64_t find_state_in_minimization_table_entry(const minimization_table_entry *entry, uint64_t state_index)
{
    for (uint64_t search_state_index = 0; search_state_index < entry->state_indices_size; search_state_index++)
    {
        if (entry->state_indices[search_state_index] == state_index)
            return search_state_index;
    }
    return -1;
}

uint64_t find_state_in_minimization_table(const minimization_table *table, uint64_t state_index)
{
    for (uint64_t entry_index = 0; entry_index < table->entries_size; entry_index++)
    {
        if (find_state_in_minimization_table_entry(table->entries[entry_index], state_index) != -1)
            return entry_index;
    }
    return -1;
}

void resize_minimization_table_entry(minimization_table_entry *entry)
{
    entry->state_indices_dim *= 2;
    entry->state_indices = realloc(entry->state_indices, entry->state_indices_dim * sizeof(uint64_t));
}

void check_minimization_table_entry_resize(minimization_table_entry *entry)
{
    if (entry->state_indices_size == entry->state_indices_dim)
        resize_minimization_table_entry(entry);
}

void resize_minimization_table(minimization_table *table)
{
    table->entries_dim *= 2;
    table->entries = realloc(table->entries, table->entries_dim * sizeof(minimization_table_entry));
}

void check_minimization_table_resize(minimization_table *table)
{
    if (table->entries_size == table->entries_dim)
        resize_minimization_table(table);
}

void add_state_to_minimization_table_entry(minimization_table_entry *entry, uint64_t state_index)
{
    check_minimization_table_entry_resize(entry);
    entry->state_indices[entry->state_indices_size++] = state_index;
}

uint64_t new_minimization_table_entry(minimization_table *table, uint64_t first_state_index)
{
    uint64_t block = BLOCK > 0 ? BLOCK : 1;
    minimization_table_entry *entry = malloc(sizeof(minimization_table_entry));
    entry->state_indices = malloc(sizeof(uint64_t) * block);
    entry->state_indices_dim = block;
    entry->state_indices_size = 1;
    entry->state_indices[0] = first_state_index;
    automaton_state *source_state = get_state(table->source, first_state_index);
    entry->state_index = new_state(table->result, throws_token(source_state), get_token(source_state));

    check_minimization_table_resize(table);
    table->entries[table->entries_size] = entry;
    return table->entries_size++;
}

void free_minimization_table_entry(minimization_table_entry *entry)
{
    free(entry->state_indices);
    free(entry);
}

void free_minimization_table(minimization_table *table)
{
    for (uint64_t i = 0; i < table->entries_size; i++)
        free_minimization_table_entry(table->entries[i]);
    free(table->entries);
    free(table);
}

void add_state_by_token_to_minimization_table(minimization_table *table, compare_token are_equals, uint64_t state_index)
{
    automaton_state *state = get_state(table->source, state_index);
    for (uint64_t entry_index = 0; entry_index < table->entries_size; entry_index++)
    {
        automaton_state *entry_state = get_state(table->result, table->entries[entry_index]->state_index);
        if (throws_token(entry_state) == throws_token(state) && ((!throws_token(entry_state) && !throws_token(state)) || are_equals(get_token(entry_state), get_token(state))))
        {
            add_state_to_minimization_table_entry(table->entries[entry_index], state_index);
            return;
        }
    }
    new_minimization_table_entry(table, state_index);
}

void populate_minimization_table_entry(minimization_table *table, uint64_t entry_index)
{
    automaton_state *templating_state = get_state(table->source, table->entries[entry_index]->state_indices[0]);
    for (uint64_t rule_index = 0; rule_index < templating_state->delta_size; rule_index++)
    {
        set_transition(table->result, table->entries[entry_index]->state_index, find_state_in_minimization_table(table, templating_state->delta[rule_index]->next_indices[0]), templating_state->delta[rule_index]->matcher);
    }
}

void remap_transitions(minimization_table *table, uint64_t previous_entry_index, uint64_t new_entry_index, uint64_t moved_state_index)
{
    for (uint64_t entry_index = 0; entry_index < table->entries_size; entry_index++)
    {
        automaton_state *current_state = get_state(table->result, table->entries[entry_index]->state_index);
        automaton_state *current_state_template = get_state(table->source, table->entries[entry_index]->state_indices[0]);
        for (uint64_t rule_index = 0; rule_index < current_state_template->delta_size; rule_index++)
        {
            if (current_state_template->delta[rule_index]->next_indices[0] == moved_state_index)
            {
                current_state->delta[rule_index]->next_indices[0] = new_entry_index;
            }
        }
    }
}

char state_belongs_in_entry(const minimization_table *table, uint64_t state_index, uint64_t entry_index)
{
    minimization_table_entry *entry = table->entries[entry_index];
    automaton_state *state = get_state(table->source, state_index);
    automaton_state *entry_state = get_state(table->result, table->entries[entry_index]->state_index);
    if (entry_state->delta_size != state->delta_size)
        return 0;
    for (uint64_t rule_index = 0; rule_index < table->source->states[state_index]->delta_size; rule_index++)
    {
        if (table->source->states[state_index]->delta[rule_index]->matcher != table->result->states[table->entries[entry_index]->state_index]->delta[rule_index]->matcher || (find_state_in_minimization_table(table, table->source->states[state_index]->delta[rule_index]->next_indices[0]) != table->result->states[table->entries[entry_index]->state_index]->delta[rule_index]->next_indices[0]))
            return 0;
    }
    return 1;
}

uint64_t try_add_to_entries(minimization_table *table, const uint64_t *entry_indices, uint64_t entry_indices_size, uint64_t state_index)
{
    for (uint64_t entry_index = 0; entry_index < entry_indices_size; entry_index++)
    {
        if (state_belongs_in_entry(table, state_index, entry_indices[entry_index]))
        {
            add_state_to_minimization_table_entry(table->entries[entry_indices[entry_index]], state_index);
            return entry_indices[entry_index];
        }
    }
    return -1;
}

char remove_state_from_entry(minimization_table *table, uint64_t entry_index, uint64_t state_index)
{
    if (entry_index >= table->entries_size || state_index >= table->entries[entry_index]->state_indices_size)
        return 0;
    table->entries[entry_index]->state_indices_size--;
    for (uint64_t i = state_index; i < table->entries[entry_index]->state_indices_size; i++)
    {
        table->entries[entry_index]->state_indices[i] = table->entries[entry_index]->state_indices[i + 1];
    }
    return 1;
}

char check_entry_transitions(minimization_table *table, uint64_t entry_index)
{
    uint64_t moved = 0;
    minimization_table_entry *entry = table->entries[entry_index];
    uint64_t *new_entry_indices = malloc(sizeof(uint64_t) * table->source->states_size);
    uint64_t new_entry_indices_size = 0;
    uint64_t iteration_limit = entry->state_indices_size;
    // begins in 1, since the entry has the same transitions as the first state entered into it, that state will always belong
    for (uint64_t state_index = 1; state_index < iteration_limit; state_index++)
    {
        if (!state_belongs_in_entry(table, table->entries[entry_index]->state_indices[state_index], entry_index))
        {
            uint64_t added_to = try_add_to_entries(table, new_entry_indices, new_entry_indices_size, table->entries[entry_index]->state_indices[state_index]);
            if (added_to == -1)
            {
                new_entry_indices[new_entry_indices_size++] = new_minimization_table_entry(table, table->entries[entry_index]->state_indices[state_index]);
                populate_minimization_table_entry(table, new_entry_indices[new_entry_indices_size - 1]);
                remap_transitions(table, entry_index, new_entry_indices[new_entry_indices_size - 1], table->entries[entry_index]->state_indices[state_index]);
            }
            else
            {
                remap_transitions(table, entry_index, added_to, table->entries[entry_index]->state_indices[state_index]);
            }
            remove_state_from_entry(table, entry_index, state_index);
            iteration_limit--;
            state_index--;
        }
    }
    free(new_entry_indices);
    return moved > 0;
}

char check_transitions(minimization_table *table)
{
    char made_changes = 0;
    uint64_t iteration_limit = table->entries_size;
    for (uint64_t entry_index = 0; entry_index < iteration_limit; entry_index++)
    {
        made_changes = check_entry_transitions(table, entry_index) || made_changes;
    }
    return made_changes && table->entries_size < table->source->states_size;
}

uint64_t add_reachable_states(const automaton *source, char *reachables, uint64_t state_index)
{
    automaton_state *state = get_state(source, state_index);
    uint64_t reachables_count = 0;
    for (uint64_t rule_index = 0; rule_index < state->delta_size; rule_index++)
    {
        if (!reachables[state->delta[rule_index]->next_indices[0]])
        {
            reachables[state->delta[rule_index]->next_indices[0]] = 1;
            reachables_count += 1 + add_reachable_states(source, reachables, state->delta[rule_index]->next_indices[0]);
        }
    }
    return reachables_count;
}

/**
 * @brief Get a memory allocated array representing whether each state corresponding to an index is reachable
 *
 * @note result[i] == 0 if state with index i is unreachable. result[i] == 1 if state with index i is reachable
 * @note only works on deterministic finite automata
 * @param source
 * @return char*
 */
char *get_reachable_state_indices(const automaton *source)
{
    char *reachables = calloc(source->states_size, sizeof(char));
    uint64_t reachables_count = 0;
    reachables_count += add_reachable_states(source, reachables, source->initial_state_index);
    reachables[source->initial_state_index] = 1;
    reachables_count++;
    return reachables;
}

automaton *get_minimal_equivalent(const automaton *dfa, compare_token are_equals)
{
    automaton *minimal_a = new_automaton();
    minimization_table *table = new_minimization_table(dfa, minimal_a);
    char *reachables = get_reachable_state_indices(dfa);
    for (uint64_t state_index = 0; state_index < dfa->states_size; state_index++)
    {
        if (reachables[state_index])
            add_state_by_token_to_minimization_table(table, are_equals, state_index);
    }
    free(reachables);
    for (uint64_t entry_index = 0; entry_index < table->entries_size; entry_index++)
    {
        populate_minimization_table_entry(table, entry_index);
    }
    print_minimization_table(table);
    while (check_transitions(table))
    {
        print_minimization_table(table);
    }
    print_minimization_table(table);
    free_minimization_table(table);
    return minimal_a;
}

char get_transition_matcher(rule *rule)
{
    return rule->matcher;
}

uint64_t *get_to_state_indices(rule *rule)
{
    return rule->next_indices;
}

uint64_t get_to_state_indices_size(rule *rule)
{
    return rule->next_indices_size;
}

void free_automaton_iterator(automaton_iterator *iterator)
{
    free(iterator);
}
void free_state_iterator(state_iterator *iterator)
{
    free(iterator);
}

automaton_iterator *get_automaton_iterator(automaton *a)
{
    automaton_iterator *iterator = malloc(sizeof(automaton_iterator));
    iterator->automaton = a;
    iterator->state_index = 0;
    return iterator;
}
state_iterator *get_state_iterator(automaton_state *s)
{
    state_iterator *iterator = malloc(sizeof(state_iterator));
    iterator->state = s;
    iterator->rule_index = 0;
    return iterator;
}

automaton_state *get_next_state(automaton_iterator *iterator)
{
    if (has_next_state(iterator))
        return iterator->automaton->states[iterator->state_index++];
    return NULL;
}

rule *get_next_rule(state_iterator *iterator)
{
    if (has_next_rule(iterator))
        return iterator->state->delta[iterator->rule_index++];
    return NULL;
}

char has_next_state(automaton_iterator *iterator)
{
    return iterator->state_index < iterator->automaton->states_size;
}

char has_next_rule(state_iterator *iterator)
{
    return iterator->rule_index < iterator->state->delta_size;
}

char throws_token(automaton_state *s)
{
    return s->throws_token;
}

token_t get_token(automaton_state *s)
{
    return s->token;
}

void print_minimization_table(const minimization_table *table)
{
    printf("Printing minimization table\n");
    for (uint64_t i = 0; i < table->entries_size; i++)
    {
        printf("Entry: %ld, Index: %ld: States: {", i, table->entries[i]->state_index);
        for (uint64_t j = 0; j < table->entries[i]->state_indices_size; j++)
        {
            printf("%ld", table->entries[i]->state_indices[j]);
            if (j < table->entries[i]->state_indices_size - 1)
                printf(", ");
        }
        printf("}\n");
    }
    printf("\n");
}
