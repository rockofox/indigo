{-# LANGUAGE QuasiQuotes #-}

module X86.Runtime (cRuntimeSource) where

import Text.RawString.QQ

cRuntimeSource :: String
cRuntimeSource =
    [r|
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define STACK_CAPACITY 65536
#define MEMORY_CAPACITY 65536

typedef enum {
    TAG_NONE = 0,
    TAG_INT = 1,
    TAG_FLOAT = 2,
    TAG_DOUBLE = 3,
    TAG_STRING = 4,
    TAG_BOOL = 5,
    TAG_LIST = 6,
    TAG_CHAR = 7,
    TAG_FUNCREF = 8,
    TAG_MAP = 9,
    TAG_TYPEQUERY = 10,
    TAG_CPTR = 11
} IndigoTag;

typedef uint64_t IndigoValue;

#define QNAN_MASK      0xFFF0000000000000ULL
#define TAG_BITS(t)    (0xFFF0000000000000ULL | (((uint64_t)(t) & 0xFULL) << 48))
#define PAYLOAD_MASK   0x0000FFFFFFFFFFFFULL

static inline bool is_double(IndigoValue v) {
    return (v & QNAN_MASK) != QNAN_MASK;
}

static inline int64_t indigo_tag(IndigoValue v) {
    if (is_double(v)) return TAG_DOUBLE;
    return (int64_t)((v >> 48) & 0xFULL);
}

static inline int64_t indigo_payload(IndigoValue v) {
    if (is_double(v)) return (int64_t)v;
    int64_t p = (int64_t)(v & PAYLOAD_MASK);
    if (((v >> 48) & 0xFULL) == TAG_INT && (p & 0x0000800000000000ULL)) {
        p |= 0xFFFF000000000000ULL;
    }
    return p;
}

static inline IndigoValue indigo_pack(int64_t tag, int64_t payload) {
    if (tag == TAG_DOUBLE) {
        IndigoValue v;
        memcpy(&v, &payload, sizeof(uint64_t));
        if ((v & QNAN_MASK) == QNAN_MASK) {
            v = 0x7FF8000000000000ULL;
        }
        return v;
    }
    return TAG_BITS(tag) | ((uint64_t)payload & PAYLOAD_MASK);
}

static inline IndigoValue pack_double(double d) {
    IndigoValue v;
    memcpy(&v, &d, sizeof(double));
    if ((v & QNAN_MASK) == QNAN_MASK) {
        v = 0x7FF8000000000000ULL;
    }
    return v;
}

static inline IndigoValue pack_float(float f) {
    uint32_t u;
    memcpy(&u, &f, sizeof(float));
    return TAG_BITS(TAG_FLOAT) | (uint64_t)u;
}

static inline IndigoValue pack_int(int64_t n) {
    return TAG_BITS(TAG_INT) | ((uint64_t)n & PAYLOAD_MASK);
}

static inline IndigoValue pack_bool(bool b) {
    return TAG_BITS(TAG_BOOL) | (b ? 1ULL : 0ULL);
}

static inline IndigoValue pack_ptr(int64_t tag, void* ptr) {
    return TAG_BITS(tag) | ((uint64_t)ptr & PAYLOAD_MASK);
}

bool indigo_is_truthy(IndigoValue v) {
    if (is_double(v)) {
        double d;
        memcpy(&d, &v, sizeof(double));
        return d != 0.0;
    }
    return (indigo_payload(v) != 0);
}

typedef struct IndigoList {
    int64_t length;
    int64_t capacity;
    IndigoValue* items;
    IndigoValue inline_items[2];
} IndigoList;

typedef struct IndigoMapEntry {
    char* key;
    IndigoValue val;
    struct IndigoMapEntry* next;
} IndigoMapEntry;

typedef struct IndigoMap {
    IndigoMapEntry* head;
} IndigoMap;

#define MAX_LOCALS 128
#define FRAME_POOL_CAPACITY 8192

typedef struct IndigoFrame {
    IndigoValue locals[MAX_LOCALS];
    uint64_t has_local_mask[2];
    struct IndigoFrame* prev;
} IndigoFrame;

typedef struct IndigoFuncRef {
    void* code_ptr;
    char* name;
    int64_t arg_count;
    IndigoValue* args;
    IndigoValue locals[MAX_LOCALS];
    uint64_t has_local_mask[2];
} IndigoFuncRef;

IndigoValue g_stack[STACK_CAPACITY];
int64_t g_sp = 0;

IndigoValue g_sidestack[STACK_CAPACITY];
int64_t g_ssp = 0;

IndigoValue g_memory[MEMORY_CAPACITY];

IndigoFrame g_frame_pool[FRAME_POOL_CAPACITY];
int64_t g_frame_pool_sp = 0;
IndigoFrame* g_current_frame = NULL;

static inline int64_t float_to_payload(float f) {
    uint32_t u;
    memcpy(&u, &f, sizeof(float));
    return (int64_t)u;
}

static inline float payload_to_float(int64_t p) {
    uint32_t u = (uint32_t)p;
    float f;
    memcpy(&f, &u, sizeof(float));
    return f;
}

static inline int64_t double_to_payload(double d) {
    int64_t p = 0;
    memcpy(&p, &d, sizeof(double));
    return p;
}

static inline double payload_to_double(int64_t p) {
    double d = 0.0;
    memcpy(&d, &p, sizeof(double));
    return d;
}

static inline double to_double_val(IndigoValue v) {
    if (is_double(v)) {
        double d;
        memcpy(&d, &v, sizeof(double));
        return d;
    }
    int64_t tag = indigo_tag(v);
    if (tag == TAG_FLOAT) return (double)payload_to_float(indigo_payload(v));
    if (tag == TAG_INT) return (double)indigo_payload(v);
    return 0.0;
}

static inline float to_float_val(IndigoValue v) {
    if (is_double(v)) {
        double d;
        memcpy(&d, &v, sizeof(double));
        return (float)d;
    }
    int64_t tag = indigo_tag(v);
    if (tag == TAG_FLOAT) return payload_to_float(indigo_payload(v));
    if (tag == TAG_INT) return (float)indigo_payload(v);
    return 0.0f;
}

static inline char* heap_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s);
    char* res = (char*)malloc(len + 1);
    if (!res) {
        fprintf(stderr, "Out of memory in heap_strdup\n");
        exit(1);
    }
    memcpy(res, s, len + 1);
    return res;
}

void indigo_push(int64_t tag, int64_t payload) {
    if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
        fprintf(stderr, "Stack overflow\n");
        exit(1);
    }
    g_stack[g_sp++] = indigo_pack(tag, payload);
}

static inline void indigo_push_val(IndigoValue v) {
    if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
        fprintf(stderr, "Stack overflow\n");
        exit(1);
    }
    g_stack[g_sp++] = v;
}

IndigoValue indigo_pop(void) {
    if (__builtin_expect(g_sp <= 0, 0)) {
        fprintf(stderr, "Stack underflow\n");
        exit(1);
    }
    return g_stack[--g_sp];
}

IndigoValue indigo_peek(void) {
    if (__builtin_expect(g_sp <= 0, 0)) {
        fprintf(stderr, "Stack underflow in peek\n");
        exit(1);
    }
    return g_stack[g_sp - 1];
}

void indigo_dup(void) {
    if (__builtin_expect(g_sp > 0, 1)) {
        if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
            fprintf(stderr, "Stack overflow in dup\n");
            exit(1);
        }
        g_stack[g_sp] = g_stack[g_sp - 1];
        g_sp++;
    }
}

void indigo_swp(void) {
    if (__builtin_expect(g_sp >= 2, 1)) {
        IndigoValue a = g_stack[g_sp - 1];
        g_stack[g_sp - 1] = g_stack[g_sp - 2];
        g_stack[g_sp - 2] = a;
    } else {
        fprintf(stderr, "Swp: expected at least 2 values\n");
        exit(1);
    }
}

void indigo_enter_frame(void) {
    if (__builtin_expect(g_frame_pool_sp >= FRAME_POOL_CAPACITY, 0)) {
        fprintf(stderr, "Call frame stack overflow\n");
        exit(1);
    }
    IndigoFrame* frame = &g_frame_pool[g_frame_pool_sp++];
    frame->has_local_mask[0] = 0;
    frame->has_local_mask[1] = 0;
    frame->prev = g_current_frame;
    g_current_frame = frame;
}

void indigo_enter_frame_inherit(void) {
    if (__builtin_expect(g_frame_pool_sp >= FRAME_POOL_CAPACITY, 0)) {
        fprintf(stderr, "Call frame stack overflow\n");
        exit(1);
    }
    IndigoFrame* frame = &g_frame_pool[g_frame_pool_sp++];
    if (g_current_frame) {
        memcpy(frame->locals, g_current_frame->locals, sizeof(frame->locals));
        frame->has_local_mask[0] = g_current_frame->has_local_mask[0];
        frame->has_local_mask[1] = g_current_frame->has_local_mask[1];
    } else {
        frame->has_local_mask[0] = 0;
        frame->has_local_mask[1] = 0;
    }
    frame->prev = g_current_frame;
    g_current_frame = frame;
}

void indigo_leave_frame(void) {
    if (!g_current_frame || g_frame_pool_sp <= 0) return;
    g_current_frame = g_current_frame->prev;
    g_frame_pool_sp--;
}

void indigo_storesidestack(void) {
    g_ssp = g_sp;
    if (g_sp > 0) {
        memcpy(g_sidestack, g_stack, g_sp * sizeof(IndigoValue));
    }
}

void indigo_loadsidestack(void) {
    if (g_ssp > 0) {
        g_sp = g_ssp;
        memcpy(g_stack, g_sidestack, g_ssp * sizeof(IndigoValue));
    }
}

void indigo_clearsidestack(void) {
    g_ssp = 0;
}

void indigo_lstore_slot(int64_t slot) {
    if (__builtin_expect(g_sp <= 0, 0)) {
        fprintf(stderr, "Stack underflow in lstore\n");
        exit(1);
    }
    IndigoValue val = g_stack[--g_sp];
    if (__builtin_expect(!g_current_frame, 0)) {
        indigo_enter_frame();
    }
    if (__builtin_expect(slot >= 0 && slot < MAX_LOCALS, 1)) {
        g_current_frame->locals[slot] = val;
        g_current_frame->has_local_mask[slot / 64] |= (1ULL << (slot % 64));
    }
}

void indigo_lload_slot(int64_t slot) {
    if (__builtin_expect(slot >= 0 && slot < MAX_LOCALS, 1)) {
        int word_idx = (int)(slot / 64);
        uint64_t bit = (1ULL << (slot % 64));
        IndigoFrame* f = g_current_frame;
        while (f) {
            if (f->has_local_mask[word_idx] & bit) {
                if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
                    fprintf(stderr, "Stack overflow in lload\n");
                    exit(1);
                }
                g_stack[g_sp++] = f->locals[slot];
                return;
            }
            f = f->prev;
        }
    }
    if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
        fprintf(stderr, "Stack overflow in lload\n");
        exit(1);
    }
    g_stack[g_sp++] = TAG_BITS(TAG_NONE);
}

void indigo_store(int64_t idx) {
    if (__builtin_expect(g_sp <= 0, 0)) {
        fprintf(stderr, "Stack underflow in store\n");
        exit(1);
    }
    IndigoValue val = g_stack[--g_sp];
    if (idx >= 0 && idx < MEMORY_CAPACITY) {
        g_memory[idx] = val;
    }
}

void indigo_load(int64_t idx) {
    if (__builtin_expect(g_sp >= STACK_CAPACITY, 0)) {
        fprintf(stderr, "Stack overflow in load\n");
        exit(1);
    }
    if (idx >= 0 && idx < MEMORY_CAPACITY) {
        g_stack[g_sp++] = g_memory[idx];
    } else {
        g_stack[g_sp++] = TAG_BITS(TAG_NONE);
    }
}

IndigoList* indigo_alloc_list(int64_t capacity) {
    IndigoList* list = (IndigoList*)malloc(sizeof(IndigoList));
    list->length = 0;
    list->capacity = capacity > 0 ? capacity : 2;
    if (list->capacity <= 2) {
        list->items = list->inline_items;
    } else {
        list->items = (IndigoValue*)malloc(sizeof(IndigoValue) * list->capacity);
    }
    return list;
}

void indigo_list_append(IndigoList* list, IndigoValue val) {
    if (list->length >= list->capacity) {
        int64_t old_cap = list->capacity;
        list->capacity = list->capacity * 2 + 4;
        if (old_cap <= 2 && list->items == list->inline_items) {
            IndigoValue* new_items = (IndigoValue*)malloc(sizeof(IndigoValue) * list->capacity);
            memcpy(new_items, list->inline_items, sizeof(IndigoValue) * list->length);
            list->items = new_items;
        } else if (old_cap == 0) {
            IndigoValue* new_items = (IndigoValue*)malloc(sizeof(IndigoValue) * list->capacity);
            if (list->length > 0 && list->items != NULL) {
                memcpy(new_items, list->items, sizeof(IndigoValue) * list->length);
            }
            list->items = new_items;
        } else {
            list->items = (IndigoValue*)realloc(list->items, sizeof(IndigoValue) * list->capacity);
        }
    }
    list->items[list->length++] = val;
}

void indigo_pack_list(int64_t n) {
    if (__builtin_expect(g_sp < n, 0)) {
        fprintf(stderr, "Stack underflow in pack_list\n");
        exit(1);
    }
    IndigoList* list = indigo_alloc_list(n);
    list->length = n;
    g_sp -= n;
    memcpy(list->items, &g_stack[g_sp], sizeof(IndigoValue) * n);
    indigo_push_val(pack_ptr(TAG_LIST, list));
}

void indigo_unpack_list(void) {
    if (__builtin_expect(g_sp <= 0, 0)) {
        fprintf(stderr, "Stack underflow in unpack_list\n");
        exit(1);
    }
    IndigoValue v = g_stack[--g_sp];
    if (indigo_tag(v) == TAG_LIST) {
        IndigoList* list = (IndigoList*)indigo_payload(v);
        if (__builtin_expect(g_sp + list->length > STACK_CAPACITY, 0)) {
            fprintf(stderr, "Stack overflow in unpack_list\n");
            exit(1);
        }
        memcpy(&g_stack[g_sp], list->items, sizeof(IndigoValue) * list->length);
        g_sp += list->length;
    } else {
        fprintf(stderr, "Invalid type for unpack list\n");
        exit(1);
    }
}

static bool is_char_list(IndigoList* list) {
    for (int64_t i = 0; i < list->length; i++) {
        if (indigo_tag(list->items[i]) != TAG_CHAR) return false;
    }
    return true;
}

static char* char_list_to_string(IndigoList* list) {
    char* str = (char*)malloc(list->length + 1);
    for (int64_t i = 0; i < list->length; i++) {
        str[i] = (char)indigo_payload(list->items[i]);
    }
    str[list->length] = '\0';
    return str;
}

void indigo_list_add(int64_t n) {
    if (n == 0) {
        IndigoList* list = indigo_alloc_list(0);
        indigo_push_val(pack_ptr(TAG_LIST, list));
        return;
    }
    if (__builtin_expect(g_sp < n, 0)) {
        fprintf(stderr, "Stack underflow in list_add\n");
        exit(1);
    }
    int64_t total_capacity = 0;
    bool has_non_char = false;
    for (int64_t i = 0; i < n; i++) {
        IndigoValue item = g_stack[g_sp - 1 - i];
        int64_t itag = indigo_tag(item);
        if (itag == TAG_LIST) {
            total_capacity += ((IndigoList*)indigo_payload(item))->length;
        } else if (itag == TAG_STRING) {
            total_capacity += strlen((char*)indigo_payload(item));
        } else {
            total_capacity += 1;
            if (itag != TAG_CHAR) has_non_char = true;
        }
    }
    IndigoList* res = indigo_alloc_list(total_capacity);
    for (int64_t i = 0; i < n; i++) {
        IndigoValue item = g_stack[g_sp - 1 - i];
        int64_t itag = indigo_tag(item);
        if (itag == TAG_LIST) {
            IndigoList* l = (IndigoList*)indigo_payload(item);
            if (l->length > 0) {
                memcpy(res->items + res->length, l->items, sizeof(IndigoValue) * l->length);
                res->length += l->length;
            }
        } else if (itag == TAG_STRING) {
            char* s = (char*)indigo_payload(item);
            size_t slen = strlen(s);
            for (size_t j = 0; j < slen; j++) {
                res->items[res->length++] = TAG_BITS(TAG_CHAR) | ((uint64_t)s[j] & 0xFF);
            }
        } else {
            res->items[res->length++] = item;
        }
    }
    g_sp -= n;
    if (!has_non_char && is_char_list(res)) {
        char* str = char_list_to_string(res);
        indigo_push_val(pack_ptr(TAG_STRING, str));
    } else {
        indigo_push_val(pack_ptr(TAG_LIST, res));
    }
}

void indigo_concat(int64_t n) {
    if (n == 0) {
        IndigoList* list = indigo_alloc_list(0);
        indigo_push_val(pack_ptr(TAG_LIST, list));
        return;
    }
    if (__builtin_expect(g_sp < n, 0)) {
        fprintf(stderr, "Stack underflow in concat\n");
        exit(1);
    }
    int64_t total_len = 0;
    for (int64_t i = 0; i < n; i++) {
        IndigoValue item = g_stack[g_sp - 1 - i];
        if (indigo_tag(item) == TAG_LIST) {
            total_len += ((IndigoList*)indigo_payload(item))->length;
        } else {
            total_len += 1;
        }
    }
    IndigoList* res = indigo_alloc_list(total_len);
    for (int64_t i = 0; i < n; i++) {
        IndigoValue item = g_stack[g_sp - 1 - i];
        if (indigo_tag(item) == TAG_LIST) {
            IndigoList* l = (IndigoList*)indigo_payload(item);
            if (l->length > 0) {
                memcpy(res->items + res->length, l->items, sizeof(IndigoValue) * l->length);
                res->length += l->length;
            }
        } else {
            res->items[res->length++] = item;
        }
    }
    g_sp -= n;
    indigo_push_val(pack_ptr(TAG_LIST, res));
}

void indigo_index(void) {
    IndigoValue val = indigo_pop();
    IndigoValue container = indigo_pop();
    int64_t idx = indigo_payload(val);
    int64_t ctag = indigo_tag(container);
    if (ctag == TAG_LIST) {
        IndigoList* list = (IndigoList*)indigo_payload(container);
        if (idx >= 0 && idx < list->length) {
            indigo_push_val(list->items[idx]);
        } else {
            fprintf(stderr, "List index out of bounds: %ld (list length: %ld)\n", idx, list->length);
            exit(1);
        }
    } else if (ctag == TAG_STRING) {
        char* str = (char*)indigo_payload(container);
        int64_t len = (int64_t)strlen(str);
        if (idx >= 0 && idx < len) {
            indigo_push(TAG_CHAR, (int64_t)str[idx]);
        } else {
            fprintf(stderr, "String index out of bounds: %ld\n", idx);
            exit(1);
        }
    } else {
        fprintf(stderr, "Invalid types for index\n");
        exit(1);
    }
}

void indigo_slice(void) {
    IndigoValue end_v = indigo_pop();
    IndigoValue start_v = indigo_pop();
    IndigoValue container = indigo_pop();
    int64_t ctag = indigo_tag(container);
    if (ctag == TAG_LIST) {
        IndigoList* list = (IndigoList*)indigo_payload(container);
        int64_t len = list->length;
        int64_t s = (indigo_tag(start_v) == TAG_INT) ? indigo_payload(start_v) : 0;
        int64_t e = (indigo_tag(end_v) == TAG_INT) ? indigo_payload(end_v) : len;
        if (s < 0) s = len + s;
        if (e < 0) e = len + e;
        if (s < 0) s = 0;
        if (e > len) e = len;
        if (s > len) s = len;
        if (e < s) e = s;
        int64_t new_len = e - s;
        IndigoList* res = (IndigoList*)malloc(sizeof(IndigoList));
        res->length = new_len;
        res->capacity = 0;
        res->items = list->items + s;
        indigo_push_val(pack_ptr(TAG_LIST, res));
    } else if (ctag == TAG_STRING) {
        char* str = (char*)indigo_payload(container);
        int64_t len = (int64_t)strlen(str);
        int64_t s = (indigo_tag(start_v) == TAG_INT) ? indigo_payload(start_v) : 0;
        int64_t e = (indigo_tag(end_v) == TAG_INT) ? indigo_payload(end_v) : len;
        if (s < 0) s = len + s;
        if (e < 0) e = len + e;
        if (s < 0) s = 0;
        if (e > len) e = len;
        if (s > len) s = len;
        if (e < s) e = s;
        int64_t new_len = e - s;
        char* res = (char*)malloc(new_len + 1);
        memcpy(res, str + s, new_len);
        res[new_len] = '\0';
        indigo_push_val(pack_ptr(TAG_STRING, res));
    } else {
        fprintf(stderr, "Invalid types for slice\n");
        exit(1);
    }
}

void indigo_length(void) {
    IndigoValue v = indigo_pop();
    int64_t tag = indigo_tag(v);
    if (tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)indigo_payload(v);
        indigo_push_val(pack_int(list->length));
    } else if (tag == TAG_STRING) {
        char* str = (char*)indigo_payload(v);
        indigo_push_val(pack_int((int64_t)strlen(str)));
    } else {
        fprintf(stderr, "Length: expected list or string\n");
        exit(1);
    }
}

IndigoMap* indigo_alloc_map(void) {
    IndigoMap* map = (IndigoMap*)malloc(sizeof(IndigoMap));
    map->head = NULL;
    return map;
}

void indigo_map_insert(IndigoMap* map, const char* key, IndigoValue val) {
    IndigoMapEntry* cur = map->head;
    while (cur) {
        if (strcmp(cur->key, key) == 0) {
            cur->val = val;
            return;
        }
        cur = cur->next;
    }
    IndigoMapEntry* entry = (IndigoMapEntry*)malloc(sizeof(IndigoMapEntry));
    entry->key = heap_strdup(key);
    entry->val = val;
    entry->next = map->head;
    map->head = entry;
}

IndigoValue* indigo_map_get(IndigoMap* map, const char* key) {
    IndigoMapEntry* cur = map->head;
    while (cur) {
        if (strcmp(cur->key, key) == 0) {
            return &cur->val;
        }
        cur = cur->next;
    }
    return NULL;
}

void indigo_pack_map(int64_t n) {
    if (__builtin_expect(g_sp < n, 0)) {
        fprintf(stderr, "Stack underflow in pack_map\n");
        exit(1);
    }
    IndigoMap* map = indigo_alloc_map();
    for (int64_t i = n - 1; i >= 0; i -= 2) {
        IndigoValue val = g_stack[g_sp - 1 - i];
        IndigoValue key_v = g_stack[g_sp - 1 - (i - 1)];
        char* key = (char*)indigo_payload(key_v);
        indigo_map_insert(map, key, val);
    }
    g_sp -= n;
    indigo_push_val(pack_ptr(TAG_MAP, map));
}

void indigo_access(const char* field) {
    IndigoValue v = indigo_pop();
    if (indigo_tag(v) == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)indigo_payload(v);
        IndigoValue* val = indigo_map_get(map, field);
        if (val) {
            indigo_push_val(*val);
        } else {
            fprintf(stderr, "Field not found: %s\n", field);
            exit(1);
        }
    } else {
        fprintf(stderr, "Cannot access field on non-map\n");
        exit(1);
    }
}

void indigo_aaccess(void) {
    IndigoValue key_v = indigo_pop();
    IndigoValue map_v = indigo_pop();
    if (indigo_tag(key_v) == TAG_STRING && indigo_tag(map_v) == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)indigo_payload(map_v);
        IndigoValue* val = indigo_map_get(map, (char*)indigo_payload(key_v));
        if (val) {
            indigo_push_val(*val);
        } else {
            indigo_push_val(TAG_BITS(TAG_NONE));
        }
    } else {
        fprintf(stderr, "Invalid types for aaccess\n");
        exit(1);
    }
}

void indigo_keys(void) {
    IndigoValue v = indigo_pop();
    if (indigo_tag(v) == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)indigo_payload(v);
        IndigoList* list = indigo_alloc_list(8);
        IndigoMapEntry* cur = map->head;
        while (cur) {
            IndigoValue k = pack_ptr(TAG_STRING, heap_strdup(cur->key));
            indigo_list_append(list, k);
            cur = cur->next;
        }
        indigo_push_val(pack_ptr(TAG_LIST, list));
    } else {
        fprintf(stderr, "Keys: expected map\n");
        exit(1);
    }
}

void indigo_update(void) {
    IndigoValue val = indigo_pop();
    IndigoValue key_v = indigo_pop();
    IndigoValue map_v = indigo_pop();
    if (indigo_tag(key_v) == TAG_STRING && indigo_tag(map_v) == TAG_MAP) {
        IndigoMap* orig = (IndigoMap*)indigo_payload(map_v);
        IndigoMap* new_map = indigo_alloc_map();
        IndigoMapEntry* cur = orig->head;
        while (cur) {
            indigo_map_insert(new_map, cur->key, cur->val);
            cur = cur->next;
        }
        indigo_map_insert(new_map, (char*)indigo_payload(key_v), val);
        indigo_push_val(pack_ptr(TAG_MAP, new_map));
    } else {
        fprintf(stderr, "Update: expected map and string key\n");
        exit(1);
    }
}


void indigo_push_pf(void* code_ptr, const char* name, int64_t n_args) {
    IndigoFuncRef* ref = (IndigoFuncRef*)malloc(sizeof(IndigoFuncRef));
    ref->code_ptr = code_ptr;
    ref->name = heap_strdup(name);
    ref->arg_count = n_args;
    ref->args = (IndigoValue*)malloc(sizeof(IndigoValue) * (n_args > 0 ? n_args : 1));
    for (int64_t i = n_args - 1; i >= 0; i--) {
        ref->args[i] = indigo_pop();
    }
    if (g_current_frame) {
        memcpy(ref->locals, g_current_frame->locals, sizeof(ref->locals));
        ref->has_local_mask[0] = g_current_frame->has_local_mask[0];
        ref->has_local_mask[1] = g_current_frame->has_local_mask[1];
    } else {
        ref->has_local_mask[0] = 0;
        ref->has_local_mask[1] = 0;
    }
    indigo_push_val(pack_ptr(TAG_FUNCREF, ref));
}

void indigo_make_closure(int64_t n_args) {
    IndigoValue code_ptr_v = indigo_pop();
    IndigoFuncRef* ref = (IndigoFuncRef*)malloc(sizeof(IndigoFuncRef));
    ref->code_ptr = (void*)indigo_payload(code_ptr_v);
    ref->name = "closure";
    ref->arg_count = n_args;
    ref->args = (IndigoValue*)malloc(sizeof(IndigoValue) * (n_args > 0 ? n_args : 1));
    for (int64_t i = n_args - 1; i >= 0; i--) {
        ref->args[i] = indigo_pop();
    }
    if (g_current_frame) {
        memcpy(ref->locals, g_current_frame->locals, sizeof(ref->locals));
        ref->has_local_mask[0] = g_current_frame->has_local_mask[0];
        ref->has_local_mask[1] = g_current_frame->has_local_mask[1];
    } else {
        ref->has_local_mask[0] = 0;
        ref->has_local_mask[1] = 0;
    }
    indigo_push_val(pack_ptr(TAG_FUNCREF, ref));
}

void* indigo_calls_prepare(void) {
    IndigoValue v = indigo_pop();
    if (indigo_tag(v) == TAG_FUNCREF) {
        IndigoFuncRef* ref = (IndigoFuncRef*)indigo_payload(v);
        for (int64_t i = 0; i < ref->arg_count; i++) {
            indigo_push_val(ref->args[i]);
        }
        indigo_enter_frame();
        memcpy(g_current_frame->locals, ref->locals, sizeof(ref->locals));
        g_current_frame->has_local_mask[0] = ref->has_local_mask[0];
        g_current_frame->has_local_mask[1] = ref->has_local_mask[1];
        return ref->code_ptr;
    } else {
        fprintf(stderr, "Cannot call non-function reference\n");
        exit(1);
    }
}

void indigo_add(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in add\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    if (__builtin_expect(xtag == TAG_INT && ytag == TAG_INT, 1)) {
        *px = pack_int(indigo_payload(*px) + indigo_payload(*py));
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) + to_float_val(*py));
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        *px = pack_double(to_double_val(*px) + to_double_val(*py));
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) + to_float_val(*py));
    } else if (xtag == TAG_STRING && ytag == TAG_STRING) {
        char* a = (char*)indigo_payload(*px);
        char* b = (char*)indigo_payload(*py);
        size_t la = strlen(a);
        size_t lb = strlen(b);
        char* res = (char*)malloc(la + lb + 1);
        memcpy(res, a, la);
        memcpy(res + la, b, lb + 1);
        *px = pack_ptr(TAG_STRING, res);
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        *px = TAG_BITS(TAG_CHAR) | ((indigo_payload(*px) + indigo_payload(*py)) & PAYLOAD_MASK);
    } else {
        fprintf(stderr, "Cannot add incompatible types\n");
        exit(1);
    }
    g_sp--;
}

void indigo_sub(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in sub\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    if (__builtin_expect(xtag == TAG_INT && ytag == TAG_INT, 1)) {
        *px = pack_int(indigo_payload(*px) - indigo_payload(*py));
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) - to_float_val(*py));
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        *px = pack_double(to_double_val(*px) - to_double_val(*py));
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) - to_float_val(*py));
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        *px = TAG_BITS(TAG_CHAR) | ((indigo_payload(*px) - indigo_payload(*py)) & PAYLOAD_MASK);
    } else {
        fprintf(stderr, "Cannot subtract incompatible types\n");
        exit(1);
    }
    g_sp--;
}

void indigo_mul(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in mul\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    if (__builtin_expect(xtag == TAG_INT && ytag == TAG_INT, 1)) {
        *px = pack_int(indigo_payload(*px) * indigo_payload(*py));
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) * to_float_val(*py));
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        *px = pack_double(to_double_val(*px) * to_double_val(*py));
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) * to_float_val(*py));
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        *px = TAG_BITS(TAG_CHAR) | ((indigo_payload(*px) * indigo_payload(*py)) & PAYLOAD_MASK);
    } else {
        fprintf(stderr, "Cannot multiply incompatible types\n");
        exit(1);
    }
    g_sp--;
}

void indigo_div(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in div\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    if (__builtin_expect(xtag == TAG_INT && ytag == TAG_INT, 1)) {
        if (indigo_payload(*py) == 0) { fprintf(stderr, "Division by zero\n"); exit(1); }
        *px = pack_int(indigo_payload(*px) / indigo_payload(*py));
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) / to_float_val(*py));
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        *px = pack_double(to_double_val(*px) / to_double_val(*py));
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        *px = pack_float(to_float_val(*px) / to_float_val(*py));
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        if (indigo_payload(*py) == 0) { fprintf(stderr, "Division by zero\n"); exit(1); }
        *px = TAG_BITS(TAG_CHAR) | ((indigo_payload(*px) / indigo_payload(*py)) & PAYLOAD_MASK);
    } else {
        fprintf(stderr, "Cannot divide incompatible types\n");
        exit(1);
    }
    g_sp--;
}

void indigo_mod(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in mod\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    if (indigo_tag(*px) == TAG_INT && indigo_tag(*py) == TAG_INT) {
        int64_t y = indigo_payload(*py);
        if (y == 0) { fprintf(stderr, "Modulo by zero\n"); exit(1); }
        int64_t res = indigo_payload(*px) % y;
        if (res < 0) res += y;
        *px = pack_int(res);
    } else {
        fprintf(stderr, "Mod: expected two integers\n");
        exit(1);
    }
    g_sp--;
}

void indigo_pow(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in pow\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    if (xtag == TAG_INT && ytag == TAG_INT) {
        int64_t base = indigo_payload(*px);
        int64_t exp = indigo_payload(*py);
        int64_t res = 1;
        while (exp > 0) {
            if (exp & 1) res *= base;
            base *= base;
            exp >>= 1;
        }
        *px = pack_int(res);
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        double res = pow(to_double_val(*px), to_double_val(*py));
        *px = pack_double(res);
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        float res = powf(to_float_val(*px), to_float_val(*py));
        *px = pack_float(res);
    } else {
        fprintf(stderr, "Cannot compute power for incompatible types\n");
        exit(1);
    }
    g_sp--;
}

void indigo_abs(void) {
    if (__builtin_expect(g_sp <= 0, 0)) { fprintf(stderr, "Stack underflow in abs\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 1];
    int64_t tag = indigo_tag(*px);
    if (tag == TAG_INT) {
        *px = pack_int(llabs(indigo_payload(*px)));
    } else if (tag == TAG_FLOAT) {
        *px = pack_float(fabsf(to_float_val(*px)));
    } else if (tag == TAG_DOUBLE) {
        *px = pack_double(fabs(to_double_val(*px)));
    } else {
        fprintf(stderr, "Cannot take absolute value\n");
        exit(1);
    }
}

bool indigo_values_equal(IndigoValue x, IndigoValue y) {
    if (x == y) return true;
    int64_t xtag = indigo_tag(x);
    int64_t ytag = indigo_tag(y);
    if (xtag == TAG_STRING && ytag == TAG_LIST) {
        char* str = (char*)indigo_payload(x);
        IndigoList* list = (IndigoList*)indigo_payload(y);
        if ((int64_t)strlen(str) != list->length) return false;
        for (int64_t i = 0; i < list->length; i++) {
            if (indigo_tag(list->items[i]) != TAG_CHAR || indigo_payload(list->items[i]) != (int64_t)str[i]) return false;
        }
        return true;
    }
    if (xtag == TAG_LIST && ytag == TAG_STRING) {
        return indigo_values_equal(y, x);
    }
    if (xtag != ytag) {
        if ((xtag == TAG_INT || xtag == TAG_FLOAT || xtag == TAG_DOUBLE) &&
            (ytag == TAG_INT || ytag == TAG_FLOAT || ytag == TAG_DOUBLE)) {
            return to_double_val(x) == to_double_val(y);
        }
        return false;
    }
    switch (xtag) {
        case TAG_NONE: return true;
        case TAG_INT:
        case TAG_BOOL:
        case TAG_CHAR:
        case TAG_CPTR:
            return indigo_payload(x) == indigo_payload(y);
        case TAG_FLOAT:
            return to_float_val(x) == to_float_val(y);
        case TAG_DOUBLE:
            return to_double_val(x) == to_double_val(y);
        case TAG_STRING:
            return strcmp((char*)indigo_payload(x), (char*)indigo_payload(y)) == 0;
        case TAG_LIST: {
            IndigoList* la = (IndigoList*)indigo_payload(x);
            IndigoList* lb = (IndigoList*)indigo_payload(y);
            if (la->length != lb->length) return false;
            for (int64_t i = 0; i < la->length; i++) {
                if (!indigo_values_equal(la->items[i], lb->items[i])) return false;
            }
            return true;
        }
        case TAG_MAP: {
            IndigoMap* ma = (IndigoMap*)indigo_payload(x);
            IndigoMap* mb = (IndigoMap*)indigo_payload(y);
            IndigoMapEntry* ca = ma->head;
            while (ca) {
                IndigoValue* vb = indigo_map_get(mb, ca->key);
                if (!vb || !indigo_values_equal(ca->val, *vb)) return false;
                ca = ca->next;
            }
            IndigoMapEntry* cb = mb->head;
            while (cb) {
                IndigoValue* va = indigo_map_get(ma, cb->key);
                if (!va) return false;
                cb = cb->next;
            }
            return true;
        }
        default:
            return x == y;
    }
}

void indigo_eq(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in eq\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    bool eq = (*px == *py) ? true : indigo_values_equal(*px, *py);
    *px = pack_bool(eq);
    g_sp--;
}

void indigo_neq(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in neq\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    bool eq = (*px == *py) ? true : indigo_values_equal(*px, *py);
    *px = pack_bool(!eq);
    g_sp--;
}

void indigo_lt(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in lt\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    bool res = false;
    if (xtag == TAG_INT && ytag == TAG_INT) {
        res = indigo_payload(*px) < indigo_payload(*py);
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        res = to_float_val(*px) < to_float_val(*py);
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        res = to_double_val(*px) < to_double_val(*py);
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        res = to_float_val(*px) < to_float_val(*py);
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        res = indigo_payload(*px) < indigo_payload(*py);
    } else if (xtag == TAG_STRING && ytag == TAG_STRING) {
        res = strcmp((char*)indigo_payload(*px), (char*)indigo_payload(*py)) < 0;
    } else if (xtag == TAG_BOOL && ytag == TAG_BOOL) {
        res = indigo_payload(*px) < indigo_payload(*py);
    } else {
        fprintf(stderr, "Cannot compare incompatible types\n");
        exit(1);
    }
    *px = pack_bool(res);
    g_sp--;
}

void indigo_gt(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in gt\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    int64_t xtag = indigo_tag(*px);
    int64_t ytag = indigo_tag(*py);
    bool res = false;
    if (xtag == TAG_INT && ytag == TAG_INT) {
        res = indigo_payload(*px) > indigo_payload(*py);
    } else if (xtag == TAG_FLOAT && ytag == TAG_FLOAT) {
        res = to_float_val(*px) > to_float_val(*py);
    } else if (xtag == TAG_DOUBLE || ytag == TAG_DOUBLE) {
        res = to_double_val(*px) > to_double_val(*py);
    } else if (xtag == TAG_FLOAT || ytag == TAG_FLOAT) {
        res = to_float_val(*px) > to_float_val(*py);
    } else if (xtag == TAG_CHAR && ytag == TAG_CHAR) {
        res = indigo_payload(*px) > indigo_payload(*py);
    } else if (xtag == TAG_STRING && ytag == TAG_STRING) {
        res = strcmp((char*)indigo_payload(*px), (char*)indigo_payload(*py)) > 0;
    } else if (xtag == TAG_BOOL && ytag == TAG_BOOL) {
        res = indigo_payload(*px) > indigo_payload(*py);
    } else {
        fprintf(stderr, "Cannot compare incompatible types\n");
        exit(1);
    }
    *px = pack_bool(res);
    g_sp--;
}

void indigo_not(void) {
    if (__builtin_expect(g_sp <= 0, 0)) { fprintf(stderr, "Stack underflow in not\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 1];
    if (indigo_tag(*px) == TAG_BOOL) {
        *px = pack_bool(indigo_payload(*px) == 0);
    } else {
        fprintf(stderr, "Not: expected boolean\n");
        exit(1);
    }
}

void indigo_and(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in and\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    if (indigo_tag(*px) == TAG_BOOL && indigo_tag(*py) == TAG_BOOL) {
        *px = pack_bool(indigo_payload(*px) && indigo_payload(*py));
        g_sp--;
    } else {
        fprintf(stderr, "And: expected boolean\n");
        exit(1);
    }
}

void indigo_or(void) {
    if (__builtin_expect(g_sp < 2, 0)) { fprintf(stderr, "Stack underflow in or\n"); exit(1); }
    IndigoValue* px = &g_stack[g_sp - 2];
    IndigoValue* py = &g_stack[g_sp - 1];
    if (indigo_tag(*px) == TAG_BOOL && indigo_tag(*py) == TAG_BOOL) {
        *px = pack_bool(indigo_payload(*px) || indigo_payload(*py));
        g_sp--;
    } else {
        fprintf(stderr, "Or: expected boolean\n");
        exit(1);
    }
}

static void buf_append_str(char** buf, size_t* len, size_t* cap, const char* s) {
    size_t slen = strlen(s);
    while (*len + slen + 1 > *cap) {
        *cap *= 2;
        *buf = (char*)realloc(*buf, *cap);
    }
    memcpy(*buf + *len, s, slen);
    *len += slen;
    (*buf)[*len] = '\0';
}

static void buf_append_char(char** buf, size_t* len, size_t* cap, char c) {
    if (*len + 2 > *cap) {
        *cap *= 2;
        *buf = (char*)realloc(*buf, *cap);
    }
    (*buf)[(*len)++] = c;
    (*buf)[*len] = '\0';
}

static void format_value_to_buf(IndigoValue v, char** buf, size_t* len, size_t* cap) {
    char tmp[64];
    int64_t tag = indigo_tag(v);
    switch (tag) {
        case TAG_INT:
            snprintf(tmp, sizeof(tmp), "%ld", indigo_payload(v));
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_FLOAT:
            snprintf(tmp, sizeof(tmp), "%g", to_float_val(v));
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_DOUBLE:
            snprintf(tmp, sizeof(tmp), "%g", to_double_val(v));
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_STRING:
            buf_append_str(buf, len, cap, (char*)indigo_payload(v));
            break;
        case TAG_BOOL:
            buf_append_str(buf, len, cap, indigo_payload(v) ? "True" : "False");
            break;
        case TAG_CHAR:
            buf_append_char(buf, len, cap, (char)indigo_payload(v));
            break;
        case TAG_NONE:
            buf_append_str(buf, len, cap, "None");
            break;
        case TAG_LIST: {
            IndigoList* l = (IndigoList*)indigo_payload(v);
            buf_append_str(buf, len, cap, "[");
            for (int64_t i = 0; i < l->length; i++) {
                if (i > 0) buf_append_str(buf, len, cap, ",");
                format_value_to_buf(l->items[i], buf, len, cap);
            }
            buf_append_str(buf, len, cap, "]");
            break;
        }
        default:
            buf_append_str(buf, len, cap, "?");
            break;
    }
}

static char* format_list_to_string(IndigoList* l) {
    size_t cap = 256;
    size_t len = 0;
    char* buf = (char*)malloc(cap);
    buf[0] = '\0';
    format_value_to_buf(pack_ptr(TAG_LIST, l), &buf, &len, &cap);
    return buf;
}

void indigo_cast(void) {
    IndigoValue to = indigo_pop();
    IndigoValue from = indigo_pop();
    int64_t from_tag = indigo_tag(from);
    int64_t to_tag = indigo_tag(to);
    if (from_tag == TAG_INT) {
        int64_t x = indigo_payload(from);
        if (to_tag == TAG_INT) indigo_push_val(pack_int(x));
        else if (to_tag == TAG_FLOAT) indigo_push_val(pack_float((float)x));
        else if (to_tag == TAG_DOUBLE) indigo_push_val(pack_double((double)x));
        else if (to_tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%ld", x);
            indigo_push_val(pack_ptr(TAG_STRING, heap_strdup(buf)));
        } else if (to_tag == TAG_BOOL) indigo_push_val(pack_bool(x != 0));
        else if (to_tag == TAG_CHAR) indigo_push(TAG_CHAR, x);
        else if (to_tag == TAG_NONE) indigo_push(TAG_NONE, 0);
        else indigo_push_val(from);
    } else if (from_tag == TAG_FLOAT) {
        float x = to_float_val(from);
        if (to_tag == TAG_INT) indigo_push_val(pack_int((int64_t)roundf(x)));
        else if (to_tag == TAG_FLOAT) indigo_push_val(from);
        else if (to_tag == TAG_DOUBLE) indigo_push_val(pack_double((double)x));
        else if (to_tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%g", x);
            indigo_push_val(pack_ptr(TAG_STRING, heap_strdup(buf)));
        } else if (to_tag == TAG_BOOL) indigo_push_val(pack_bool(x != 0.0f));
        else indigo_push_val(from);
    } else if (from_tag == TAG_DOUBLE) {
        double x = to_double_val(from);
        if (to_tag == TAG_INT) indigo_push_val(pack_int((int64_t)round(x)));
        else if (to_tag == TAG_FLOAT) indigo_push_val(pack_float((float)x));
        else if (to_tag == TAG_DOUBLE) indigo_push_val(from);
        else if (to_tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%g", x);
            indigo_push_val(pack_ptr(TAG_STRING, heap_strdup(buf)));
        } else if (to_tag == TAG_BOOL) indigo_push_val(pack_bool(x != 0.0));
        else indigo_push_val(from);
    } else if (from_tag == TAG_STRING) {
        char* x = (char*)indigo_payload(from);
        if (to_tag == TAG_INT) indigo_push_val(pack_int(atol(x)));
        else if (to_tag == TAG_FLOAT) indigo_push_val(pack_float((float)atof(x)));
        else if (to_tag == TAG_DOUBLE) indigo_push_val(pack_double(atof(x)));
        else if (to_tag == TAG_STRING) indigo_push_val(from);
        else if (to_tag == TAG_BOOL) indigo_push_val(pack_bool(strlen(x) > 0));
        else if (to_tag == TAG_CHAR) indigo_push(TAG_CHAR, x[0]);
        else indigo_push_val(from);
    } else if (from_tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)indigo_payload(from);
        if (to_tag == TAG_STRING) {
            if (is_char_list(list)) {
                indigo_push_val(pack_ptr(TAG_STRING, char_list_to_string(list)));
            } else {
                char* str = format_list_to_string(list);
                indigo_push_val(pack_ptr(TAG_STRING, str));
            }
        } else {
            indigo_push_val(from);
        }
    } else {
        indigo_push_val(from);
    }
}

void indigo_typeof(void) {
    IndigoValue v = indigo_pop();
    const char* type_name = "Unknown";
    int64_t tag = indigo_tag(v);
    switch (tag) {
        case TAG_INT: type_name = "Int"; break;
        case TAG_FLOAT: type_name = "Float"; break;
        case TAG_DOUBLE: type_name = "Double"; break;
        case TAG_STRING: type_name = "String"; break;
        case TAG_BOOL: type_name = "Bool"; break;
        case TAG_LIST: type_name = "List"; break;
        case TAG_NONE: type_name = "None"; break;
        case TAG_CHAR: type_name = "Char"; break;
        case TAG_FUNCREF: type_name = "FuncRef"; break;
        case TAG_MAP: {
            IndigoMap* m = (IndigoMap*)indigo_payload(v);
            IndigoValue* name_val = indigo_map_get(m, "__name");
            if (name_val && indigo_tag(*name_val) == TAG_STRING) {
                type_name = (char*)indigo_payload(*name_val);
            } else {
                type_name = "Map";
            }
            break;
        }
        case TAG_TYPEQUERY: type_name = "TypeQuery"; break;
        case TAG_CPTR: type_name = "CPtr"; break;
        default: type_name = "Unknown"; break;
    }
    indigo_push_val(pack_ptr(TAG_STRING, heap_strdup(type_name)));
}

void indigo_type_eq(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    bool eq = false;
    int64_t xtag = indigo_tag(x);
    int64_t ytag = indigo_tag(y);
    if (ytag == TAG_TYPEQUERY) {
        char* q = (char*)indigo_payload(y);
        if (strcmp(q, "Any") == 0) {
            eq = true;
        } else if (xtag == TAG_MAP) {
            IndigoMap* m = (IndigoMap*)indigo_payload(x);
            IndigoValue* name_val = indigo_map_get(m, "__name");
            if (name_val && indigo_tag(*name_val) == TAG_STRING) {
                eq = (strcmp((char*)indigo_payload(*name_val), q) == 0);
            }
            if (!eq) {
                IndigoValue* traits_val = indigo_map_get(m, "__traits");
                if (traits_val && indigo_tag(*traits_val) == TAG_LIST) {
                    IndigoList* tl = (IndigoList*)indigo_payload(*traits_val);
                    for (int64_t i = 0; i < tl->length; i++) {
                        if (indigo_tag(tl->items[i]) == TAG_STRING && strcmp((char*)indigo_payload(tl->items[i]), q) == 0) {
                            eq = true;
                            break;
                        }
                    }
                }
            }
        }
    } else if (xtag == TAG_MAP && ytag == TAG_MAP) {
        IndigoMap* ma = (IndigoMap*)indigo_payload(x);
        IndigoMap* mb = (IndigoMap*)indigo_payload(y);
        IndigoValue* na = indigo_map_get(ma, "__name");
        IndigoValue* nb = indigo_map_get(mb, "__name");
        if (na && nb && indigo_tag(*na) == TAG_STRING && indigo_tag(*nb) == TAG_STRING) {
            eq = strcmp((char*)indigo_payload(*na), (char*)indigo_payload(*nb)) == 0;
        } else {
            eq = true;
        }
    } else {
        eq = (xtag == ytag);
    }
    indigo_push_val(pack_bool(eq));
}

static void print_indigo_value(IndigoValue v) {
    int64_t tag = indigo_tag(v);
    switch (tag) {
        case TAG_INT:
            printf("%ld", indigo_payload(v));
            break;
        case TAG_FLOAT:
            printf("%g", to_float_val(v));
            break;
        case TAG_DOUBLE:
            printf("%g", to_double_val(v));
            break;
        case TAG_STRING:
            printf("%s", (char*)indigo_payload(v));
            break;
        case TAG_BOOL:
            printf("%s", indigo_payload(v) ? "True" : "False");
            break;
        case TAG_CHAR:
            putchar((int)indigo_payload(v));
            break;
        case TAG_NONE:
            printf("None");
            break;
        case TAG_CPTR:
            printf("CPtr %ld", indigo_payload(v));
            break;
        case TAG_FUNCREF: {
            IndigoFuncRef* r = (IndigoFuncRef*)indigo_payload(v);
            printf("<fn %s>", r->name ? r->name : "anonymous");
            break;
        }
        case TAG_LIST: {
            IndigoList* l = (IndigoList*)indigo_payload(v);
            printf("[");
            for (int64_t i = 0; i < l->length; i++) {
                if (i > 0) printf(", ");
                print_indigo_value(l->items[i]);
            }
            printf("]");
            break;
        }
        case TAG_MAP: {
            IndigoMap* m = (IndigoMap*)indigo_payload(v);
            IndigoValue* name_val = indigo_map_get(m, "__name");
            if (name_val && indigo_tag(*name_val) == TAG_STRING) {
                printf("%s", (char*)indigo_payload(*name_val));
            }
            printf("{");
            IndigoMapEntry* cur = m->head;
            bool first = true;
            while (cur) {
                if (strncmp(cur->key, "__", 2) != 0) {
                    if (!first) printf(", ");
                    printf("%s: ", cur->key);
                    print_indigo_value(cur->val);
                    first = false;
                }
                cur = cur->next;
            }
            printf("}");
            break;
        }
        default:
            printf("?");
            break;
    }
}

void indigo_builtin_print(void) {
    IndigoValue v = indigo_pop();
    print_indigo_value(v);
    fflush(stdout);
}

void indigo_builtin_getline(void) {
    char buf[4096];
    if (fgets(buf, sizeof(buf), stdin)) {
        size_t len = strlen(buf);
        if (len > 0 && buf[len - 1] == '\n') {
            buf[len - 1] = '\0';
        }
        indigo_push_val(pack_ptr(TAG_STRING, heap_strdup(buf)));
    } else {
        indigo_push_val(pack_ptr(TAG_STRING, heap_strdup("")));
    }
}

void indigo_builtin_getchar(void) {
    int c = getchar();
    if (c != EOF) {
        indigo_push(TAG_CHAR, (int64_t)c);
    } else {
        indigo_push(TAG_NONE, 0);
    }
}

void indigo_builtin_random(void) {
    float r = (float)rand() / (float)RAND_MAX;
    indigo_push_val(pack_float(r));
}

void indigo_exit(void) {
    IndigoValue v = indigo_pop();
    int code = (indigo_tag(v) == TAG_INT) ? (int)indigo_payload(v) : 0;
    exit(code);
}

void indigo_label_not_found(const char* name) {
    fprintf(stderr, "Label not found: %s\n", name);
    exit(1);
}
|]
