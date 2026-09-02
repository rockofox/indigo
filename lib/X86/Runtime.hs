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

typedef struct {
    int64_t tag;
    int64_t payload;
} IndigoValue;

typedef struct IndigoList {
    int64_t length;
    int64_t capacity;
    IndigoValue* items;
} IndigoList;

typedef struct IndigoMapEntry {
    char* key;
    IndigoValue val;
    struct IndigoMapEntry* next;
} IndigoMapEntry;

typedef struct IndigoMap {
    IndigoMapEntry* head;
} IndigoMap;

#define MAX_LOCALS 1024

typedef struct IndigoFrame {
    IndigoValue locals[MAX_LOCALS];
    bool has_local[MAX_LOCALS];
    struct IndigoFrame* prev;
} IndigoFrame;

typedef struct IndigoFuncRef {
    void* code_ptr;
    char* name;
    int64_t arg_count;
    IndigoValue* args;
    IndigoValue locals[MAX_LOCALS];
    bool has_local[MAX_LOCALS];
} IndigoFuncRef;

IndigoValue g_stack[STACK_CAPACITY];
int64_t g_sp = 0;

IndigoValue g_sidestack[STACK_CAPACITY];
int64_t g_ssp = 0;

IndigoValue g_memory[MEMORY_CAPACITY];

#define FRAME_POOL_CAPACITY 65536
IndigoFrame g_frame_pool[FRAME_POOL_CAPACITY];
int64_t g_frame_pool_sp = 0;
IndigoFrame* g_current_frame = NULL;

static inline int64_t float_to_payload(float f) {
    int64_t p = 0;
    memcpy(&p, &f, sizeof(float));
    return p;
}

static inline float payload_to_float(int64_t p) {
    float f = 0.0f;
    memcpy(&f, &p, sizeof(float));
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
    if (g_sp >= STACK_CAPACITY) {
        fprintf(stderr, "Stack overflow\n");
        exit(1);
    }
    g_stack[g_sp].tag = tag;
    g_stack[g_sp].payload = payload;
    g_sp++;
}

IndigoValue indigo_pop(void) {
    if (g_sp <= 0) {
        fprintf(stderr, "Stack underflow\n");
        exit(1);
    }
    g_sp--;
    return g_stack[g_sp];
}

IndigoValue indigo_peek(void) {
    if (g_sp <= 0) {
        fprintf(stderr, "Stack underflow in peek\n");
        exit(1);
    }
    return g_stack[g_sp - 1];
}

void indigo_dup(void) {
    if (g_sp > 0) {
        IndigoValue v = g_stack[g_sp - 1];
        indigo_push(v.tag, v.payload);
    }
}

void indigo_swp(void) {
    if (g_sp >= 2) {
        IndigoValue a = g_stack[g_sp - 1];
        IndigoValue b = g_stack[g_sp - 2];
        g_stack[g_sp - 1] = b;
        g_stack[g_sp - 2] = a;
    } else {
        fprintf(stderr, "Swp: expected at least 2 values\n");
        exit(1);
    }
}

void indigo_enter_frame(void) {
    if (g_frame_pool_sp >= FRAME_POOL_CAPACITY) {
        fprintf(stderr, "Call frame stack overflow\n");
        exit(1);
    }
    IndigoFrame* frame = &g_frame_pool[g_frame_pool_sp++];
    memset(frame->has_local, 0, sizeof(frame->has_local));
    frame->prev = g_current_frame;
    g_current_frame = frame;
}

void indigo_enter_frame_inherit(void) {
    if (g_frame_pool_sp >= FRAME_POOL_CAPACITY) {
        fprintf(stderr, "Call frame stack overflow\n");
        exit(1);
    }
    IndigoFrame* frame = &g_frame_pool[g_frame_pool_sp++];
    if (g_current_frame) {
        memcpy(frame->locals, g_current_frame->locals, sizeof(frame->locals));
        memcpy(frame->has_local, g_current_frame->has_local, sizeof(frame->has_local));
    } else {
        memset(frame->has_local, 0, sizeof(frame->has_local));
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
    for (int64_t i = 0; i < g_sp && i < STACK_CAPACITY; i++) {
        g_sidestack[i] = g_stack[i];
    }
}

void indigo_loadsidestack(void) {
    if (g_ssp > 0) {
        g_sp = g_ssp;
        for (int64_t i = 0; i < g_ssp && i < STACK_CAPACITY; i++) {
            g_stack[i] = g_sidestack[i];
        }
    }
}

void indigo_clearsidestack(void) {
    g_ssp = 0;
}

void indigo_lstore_slot(int64_t slot) {
    IndigoValue val = indigo_pop();
    if (!g_current_frame) {
        indigo_enter_frame();
    }
    if (slot >= 0 && slot < MAX_LOCALS) {
        g_current_frame->locals[slot] = val;
        g_current_frame->has_local[slot] = true;
    }
}

void indigo_lload_slot(int64_t slot) {
    IndigoFrame* f = g_current_frame;
    while (f) {
        if (slot >= 0 && slot < MAX_LOCALS && f->has_local[slot]) {
            indigo_push(f->locals[slot].tag, f->locals[slot].payload);
            return;
        }
        f = f->prev;
    }
    indigo_push(0, 0);
}

void indigo_store(int64_t idx) {
    IndigoValue val = indigo_pop();
    if (idx >= 0 && idx < MEMORY_CAPACITY) {
        g_memory[idx] = val;
    }
}

void indigo_load(int64_t idx) {
    if (idx >= 0 && idx < MEMORY_CAPACITY) {
        indigo_push(g_memory[idx].tag, g_memory[idx].payload);
    } else {
        indigo_push(TAG_NONE, 0);
    }
}

void indigo_mov(int64_t idx, int64_t tag, int64_t payload) {
    if (idx >= 0 && idx < MEMORY_CAPACITY) {
        g_memory[idx].tag = tag;
        g_memory[idx].payload = payload;
    }
}

IndigoList* indigo_alloc_list(int64_t capacity) {
    IndigoList* list = (IndigoList*)malloc(sizeof(IndigoList));
    list->length = 0;
    list->capacity = capacity > 0 ? capacity : 8;
    list->items = (IndigoValue*)malloc(sizeof(IndigoValue) * list->capacity);
    return list;
}

void indigo_list_append(IndigoList* list, IndigoValue val) {
    if (list->length >= list->capacity) {
        int64_t old_cap = list->capacity;
        list->capacity = list->capacity * 2 + 4;
        if (old_cap == 0) {
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
    IndigoList* list = indigo_alloc_list(n);
    list->length = n;
    for (int64_t i = n - 1; i >= 0; i--) {
        list->items[i] = indigo_pop();
    }
    indigo_push(TAG_LIST, (int64_t)list);
}

void indigo_unpack_list(void) {
    IndigoValue v = indigo_pop();
    if (v.tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)v.payload;
        for (int64_t i = 0; i < list->length; i++) {
            indigo_push(list->items[i].tag, list->items[i].payload);
        }
    } else {
        fprintf(stderr, "Invalid type for unpack list\n");
        exit(1);
    }
}

static bool is_char_list(IndigoList* list) {
    for (int64_t i = 0; i < list->length; i++) {
        if (list->items[i].tag != TAG_CHAR) return false;
    }
    return true;
}

static char* char_list_to_string(IndigoList* list) {
    char* str = (char*)malloc(list->length + 1);
    for (int64_t i = 0; i < list->length; i++) {
        str[i] = (char)list->items[i].payload;
    }
    str[list->length] = '\0';
    return str;
}

void indigo_list_add(int64_t n) {
    IndigoValue* popped = (IndigoValue*)malloc(sizeof(IndigoValue) * n);
    for (int64_t i = 0; i < n; i++) {
        popped[i] = indigo_pop();
    }
    IndigoList* res = indigo_alloc_list(16);
    for (int64_t i = 0; i < n; i++) {
        if (popped[i].tag == TAG_LIST) {
            IndigoList* l = (IndigoList*)popped[i].payload;
            for (int64_t j = 0; j < l->length; j++) {
                indigo_list_append(res, l->items[j]);
            }
        } else if (popped[i].tag == TAG_STRING) {
            char* s = (char*)popped[i].payload;
            size_t slen = strlen(s);
            for (size_t j = 0; j < slen; j++) {
                IndigoValue ch = { TAG_CHAR, (int64_t)s[j] };
                indigo_list_append(res, ch);
            }
        } else {
            indigo_list_append(res, popped[i]);
        }
    }
    free(popped);
    if (is_char_list(res)) {
        char* str = char_list_to_string(res);
        indigo_push(TAG_STRING, (int64_t)str);
    } else {
        indigo_push(TAG_LIST, (int64_t)res);
    }
}

void indigo_concat(int64_t n) {
    if (n == 0) {
        IndigoList* list = indigo_alloc_list(0);
        indigo_push(TAG_LIST, (int64_t)list);
        return;
    }
    IndigoValue* popped = (IndigoValue*)malloc(sizeof(IndigoValue) * n);
    for (int64_t i = 0; i < n; i++) {
        popped[i] = indigo_pop();
    }
    IndigoList* res = indigo_alloc_list(16);
    for (int64_t i = 0; i < n; i++) {
        if (popped[i].tag == TAG_LIST) {
            IndigoList* l = (IndigoList*)popped[i].payload;
            for (int64_t j = 0; j < l->length; j++) {
                indigo_list_append(res, l->items[j]);
            }
        } else {
            indigo_list_append(res, popped[i]);
        }
    }
    free(popped);
    indigo_push(TAG_LIST, (int64_t)res);
}

void indigo_index(void) {
    IndigoValue val = indigo_pop();
    IndigoValue container = indigo_pop();
    int64_t idx = val.payload;
    if (container.tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)container.payload;
        if (idx >= 0 && idx < list->length) {
            indigo_push(list->items[idx].tag, list->items[idx].payload);
        } else {
            fprintf(stderr, "List index out of bounds: %ld (list length: %ld)\n", idx, list->length);
            exit(1);
        }
    } else if (container.tag == TAG_STRING) {
        char* str = (char*)container.payload;
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
    if (container.tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)container.payload;
        int64_t len = list->length;
        int64_t s = (start_v.tag == TAG_INT) ? start_v.payload : 0;
        int64_t e = (end_v.tag == TAG_INT) ? end_v.payload : len;
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
        indigo_push(TAG_LIST, (int64_t)res);
    } else if (container.tag == TAG_STRING) {
        char* str = (char*)container.payload;
        int64_t len = (int64_t)strlen(str);
        int64_t s = (start_v.tag == TAG_INT) ? start_v.payload : 0;
        int64_t e = (end_v.tag == TAG_INT) ? end_v.payload : len;
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
        indigo_push(TAG_STRING, (int64_t)res);
    } else {
        fprintf(stderr, "Invalid type for slice\n");
        exit(1);
    }
}

void indigo_length(void) {
    if (g_sp <= 0) {
        indigo_push(TAG_INT, -1);
        return;
    }
    IndigoValue v = indigo_pop();
    if (v.tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)v.payload;
        indigo_push(TAG_INT, list->length);
    } else if (v.tag == TAG_STRING) {
        char* str = (char*)v.payload;
        indigo_push(TAG_INT, (int64_t)strlen(str));
    } else {
        fprintf(stderr, "Invalid type for length\n");
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
    IndigoMap* map = indigo_alloc_map();
    IndigoValue* popped = (IndigoValue*)malloc(sizeof(IndigoValue) * n);
    for (int64_t i = 0; i < n; i++) {
        popped[i] = indigo_pop();
    }
    for (int64_t i = 0; i + 1 < n; i += 2) {
        if (popped[i].tag == TAG_STRING) {
            indigo_map_insert(map, (char*)popped[i].payload, popped[i + 1]);
        }
    }
    free(popped);
    indigo_push(TAG_MAP, (int64_t)map);
}

void indigo_access(const char* key) {
    IndigoValue v = indigo_pop();
    if (v.tag == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)v.payload;
        IndigoValue* val = indigo_map_get(map, key);
        if (val) {
            indigo_push(val->tag, val->payload);
        } else {
            fprintf(stderr, "Field '%s' not found in struct\n", key);
            exit(1);
        }
    } else {
        fprintf(stderr, "Invalid type for access '%s'\n", key);
        exit(1);
    }
}

void indigo_aaccess(void) {
    IndigoValue key_v = indigo_pop();
    IndigoValue map_v = indigo_pop();
    if (key_v.tag == TAG_STRING && map_v.tag == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)map_v.payload;
        IndigoValue* val = indigo_map_get(map, (char*)key_v.payload);
        if (val) {
            indigo_push(val->tag, val->payload);
        } else {
            indigo_push(TAG_NONE, 0);
        }
    } else {
        fprintf(stderr, "AAccess: expected String and Map\n");
        exit(1);
    }
}

void indigo_keys(void) {
    IndigoValue v = indigo_pop();
    if (v.tag == TAG_MAP) {
        IndigoMap* map = (IndigoMap*)v.payload;
        IndigoList* list = indigo_alloc_list(8);
        IndigoMapEntry* cur = map->head;
        while (cur) {
            IndigoValue k = { TAG_STRING, (int64_t)heap_strdup(cur->key) };
            indigo_list_append(list, k);
            cur = cur->next;
        }
        indigo_push(TAG_LIST, (int64_t)list);
    } else {
        fprintf(stderr, "Invalid type for keys\n");
        exit(1);
    }
}

void indigo_update(void) {
    IndigoValue val = indigo_pop();
    IndigoValue key_v = indigo_pop();
    IndigoValue map_v = indigo_pop();
    if (key_v.tag == TAG_STRING && map_v.tag == TAG_MAP) {
        IndigoMap* orig = (IndigoMap*)map_v.payload;
        IndigoMap* copy = indigo_alloc_map();
        IndigoMapEntry* cur = orig->head;
        while (cur) {
            indigo_map_insert(copy, cur->key, cur->val);
            cur = cur->next;
        }
        indigo_map_insert(copy, (char*)key_v.payload, val);
        indigo_push(TAG_MAP, (int64_t)copy);
    } else {
        fprintf(stderr, "Update: expected value, String key, and Map\n");
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
        memcpy(ref->has_local, g_current_frame->has_local, sizeof(ref->has_local));
    } else {
        memset(ref->has_local, 0, sizeof(ref->has_local));
    }
    indigo_push(TAG_FUNCREF, (int64_t)ref);
}

void* indigo_calls_prepare(void) {
    IndigoValue v = indigo_pop();
    if (v.tag == TAG_FUNCREF) {
        IndigoFuncRef* ref = (IndigoFuncRef*)v.payload;
        for (int64_t i = 0; i < ref->arg_count; i++) {
            indigo_push(ref->args[i].tag, ref->args[i].payload);
        }
        indigo_enter_frame();
        memcpy(g_current_frame->locals, ref->locals, sizeof(ref->locals));
        memcpy(g_current_frame->has_local, ref->has_local, sizeof(ref->has_local));
        return ref->code_ptr;
    } else {
        fprintf(stderr, "Cannot call non-function reference\n");
        exit(1);
    }
}

void indigo_add(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        indigo_push(TAG_INT, x.payload + y.payload);
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        float res = payload_to_float(x.payload) + payload_to_float(y.payload);
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        double res = payload_to_double(x.payload) + payload_to_double(y.payload);
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else if (x.tag == TAG_STRING && y.tag == TAG_STRING) {
        char* a = (char*)x.payload;
        char* b = (char*)y.payload;
        size_t la = strlen(a);
        size_t lb = strlen(b);
        char* res = (char*)malloc(la + lb + 1);
        memcpy(res, a, la);
        memcpy(res + la, b, lb + 1);
        indigo_push(TAG_STRING, (int64_t)res);
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        indigo_push(TAG_CHAR, (x.payload + y.payload));
    } else {
        fprintf(stderr, "Cannot add incompatible types\n");
        exit(1);
    }
}

void indigo_sub(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        indigo_push(TAG_INT, x.payload - y.payload);
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        float res = payload_to_float(x.payload) - payload_to_float(y.payload);
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        double res = payload_to_double(x.payload) - payload_to_double(y.payload);
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        indigo_push(TAG_CHAR, (x.payload - y.payload));
    } else {
        fprintf(stderr, "Cannot subtract incompatible types\n");
        exit(1);
    }
}

void indigo_mul(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        indigo_push(TAG_INT, x.payload * y.payload);
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        float res = payload_to_float(x.payload) * payload_to_float(y.payload);
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        double res = payload_to_double(x.payload) * payload_to_double(y.payload);
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        indigo_push(TAG_CHAR, (x.payload * y.payload));
    } else {
        fprintf(stderr, "Cannot multiply incompatible types\n");
        exit(1);
    }
}

void indigo_div(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (y.payload == 0 && (y.tag == TAG_INT || y.tag == TAG_CHAR)) {
        fprintf(stderr, "Division by zero\n");
        exit(1);
    }
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        indigo_push(TAG_INT, x.payload / y.payload);
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        float res = payload_to_float(x.payload) / payload_to_float(y.payload);
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        double res = payload_to_double(x.payload) / payload_to_double(y.payload);
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        indigo_push(TAG_CHAR, (x.payload / y.payload));
    } else {
        fprintf(stderr, "Cannot divide incompatible types\n");
        exit(1);
    }
}

void indigo_mod(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        if (y.payload == 0) {
            fprintf(stderr, "Modulo by zero\n");
            exit(1);
        }
        int64_t res = x.payload % y.payload;
        if (res < 0) res += y.payload;
        indigo_push(TAG_INT, res);
    } else {
        fprintf(stderr, "Mod: expected two integers\n");
        exit(1);
    }
}

void indigo_pow(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        int64_t base = x.payload;
        int64_t exp = y.payload;
        int64_t res = 1;
        while (exp > 0) {
            if (exp & 1) res *= base;
            base *= base;
            exp >>= 1;
        }
        indigo_push(TAG_INT, res);
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        float res = powf(payload_to_float(x.payload), payload_to_float(y.payload));
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        double res = pow(payload_to_double(x.payload), payload_to_double(y.payload));
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else {
        fprintf(stderr, "Cannot compute power for incompatible types\n");
        exit(1);
    }
}

void indigo_abs(void) {
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_INT) {
        indigo_push(TAG_INT, llabs(x.payload));
    } else if (x.tag == TAG_FLOAT) {
        float res = fabsf(payload_to_float(x.payload));
        indigo_push(TAG_FLOAT, float_to_payload(res));
    } else if (x.tag == TAG_DOUBLE) {
        double res = fabs(payload_to_double(x.payload));
        indigo_push(TAG_DOUBLE, double_to_payload(res));
    } else {
        fprintf(stderr, "Cannot take absolute value\n");
        exit(1);
    }
}

bool indigo_values_equal(IndigoValue x, IndigoValue y) {
    if (x.tag == TAG_STRING && y.tag == TAG_LIST) {
        char* str = (char*)x.payload;
        IndigoList* list = (IndigoList*)y.payload;
        if ((int64_t)strlen(str) != list->length) return false;
        for (int64_t i = 0; i < list->length; i++) {
            if (list->items[i].tag != TAG_CHAR || list->items[i].payload != (int64_t)str[i]) return false;
        }
        return true;
    }
    if (x.tag == TAG_LIST && y.tag == TAG_STRING) {
        return indigo_values_equal(y, x);
    }
    if (x.tag != y.tag) return false;
    switch (x.tag) {
        case TAG_NONE: return true;
        case TAG_INT:
        case TAG_BOOL:
        case TAG_CHAR:
        case TAG_CPTR:
            return x.payload == y.payload;
        case TAG_FLOAT:
            return payload_to_float(x.payload) == payload_to_float(y.payload);
        case TAG_DOUBLE:
            return payload_to_double(x.payload) == payload_to_double(y.payload);
        case TAG_STRING:
            return strcmp((char*)x.payload, (char*)y.payload) == 0;
        case TAG_LIST: {
            IndigoList* la = (IndigoList*)x.payload;
            IndigoList* lb = (IndigoList*)y.payload;
            if (la->length != lb->length) return false;
            for (int64_t i = 0; i < la->length; i++) {
                if (!indigo_values_equal(la->items[i], lb->items[i])) return false;
            }
            return true;
        }
        case TAG_MAP: {
            IndigoMap* ma = (IndigoMap*)x.payload;
            IndigoMap* mb = (IndigoMap*)y.payload;
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
            return x.payload == y.payload;
    }
}

void indigo_eq(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    indigo_push(TAG_BOOL, indigo_values_equal(x, y) ? 1 : 0);
}

void indigo_neq(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    indigo_push(TAG_BOOL, indigo_values_equal(x, y) ? 0 : 1);
}

void indigo_lt(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    bool res = false;
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        res = x.payload < y.payload;
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        res = payload_to_float(x.payload) < payload_to_float(y.payload);
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        res = payload_to_double(x.payload) < payload_to_double(y.payload);
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        res = x.payload < y.payload;
    } else if (x.tag == TAG_STRING && y.tag == TAG_STRING) {
        res = strcmp((char*)x.payload, (char*)y.payload) < 0;
    } else if (x.tag == TAG_BOOL && y.tag == TAG_BOOL) {
        res = x.payload < y.payload;
    } else {
        fprintf(stderr, "Cannot compare incompatible types\n");
        exit(1);
    }
    indigo_push(TAG_BOOL, res ? 1 : 0);
}

void indigo_gt(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    bool res = false;
    if (x.tag == TAG_INT && y.tag == TAG_INT) {
        res = x.payload > y.payload;
    } else if (x.tag == TAG_FLOAT && y.tag == TAG_FLOAT) {
        res = payload_to_float(x.payload) > payload_to_float(y.payload);
    } else if (x.tag == TAG_DOUBLE && y.tag == TAG_DOUBLE) {
        res = payload_to_double(x.payload) > payload_to_double(y.payload);
    } else if (x.tag == TAG_CHAR && y.tag == TAG_CHAR) {
        res = x.payload > y.payload;
    } else if (x.tag == TAG_STRING && y.tag == TAG_STRING) {
        res = strcmp((char*)x.payload, (char*)y.payload) > 0;
    } else if (x.tag == TAG_BOOL && y.tag == TAG_BOOL) {
        res = x.payload > y.payload;
    } else {
        fprintf(stderr, "Cannot compare incompatible types\n");
        exit(1);
    }
    indigo_push(TAG_BOOL, res ? 1 : 0);
}

void indigo_not(void) {
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_BOOL) {
        indigo_push(TAG_BOOL, x.payload ? 0 : 1);
    } else {
        fprintf(stderr, "Not: expected boolean\n");
        exit(1);
    }
}

void indigo_and(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_BOOL && y.tag == TAG_BOOL) {
        indigo_push(TAG_BOOL, (x.payload && y.payload) ? 1 : 0);
    } else {
        fprintf(stderr, "And: expected boolean\n");
        exit(1);
    }
}

void indigo_or(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    if (x.tag == TAG_BOOL && y.tag == TAG_BOOL) {
        indigo_push(TAG_BOOL, (x.payload || y.payload) ? 1 : 0);
    } else {
        fprintf(stderr, "Or: expected boolean\n");
        exit(1);
    }
}

static void buf_append_str(char** buf, size_t* len, size_t* cap, const char* str) {
    size_t slen = strlen(str);
    while (*len + slen + 1 >= *cap) {
        *cap = (*cap) * 2 + slen + 16;
        *buf = (char*)realloc(*buf, *cap);
    }
    memcpy(*buf + *len, str, slen);
    *len += slen;
    (*buf)[*len] = '\0';
}

static void buf_append_char(char** buf, size_t* len, size_t* cap, char c) {
    char s[2] = { c, '\0' };
    buf_append_str(buf, len, cap, s);
}

static void format_value_to_buf(IndigoValue v, char** buf, size_t* len, size_t* cap) {
    char tmp[64];
    switch (v.tag) {
        case TAG_INT:
            snprintf(tmp, sizeof(tmp), "%ld", v.payload);
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_FLOAT:
            snprintf(tmp, sizeof(tmp), "%g", payload_to_float(v.payload));
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_DOUBLE:
            snprintf(tmp, sizeof(tmp), "%g", payload_to_double(v.payload));
            buf_append_str(buf, len, cap, tmp);
            break;
        case TAG_STRING:
            buf_append_str(buf, len, cap, (char*)v.payload);
            break;
        case TAG_BOOL:
            buf_append_str(buf, len, cap, v.payload ? "True" : "False");
            break;
        case TAG_CHAR:
            buf_append_char(buf, len, cap, (char)v.payload);
            break;
        case TAG_NONE:
            buf_append_str(buf, len, cap, "None");
            break;
        case TAG_LIST: {
            IndigoList* l = (IndigoList*)v.payload;
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
    format_value_to_buf((IndigoValue){ TAG_LIST, (int64_t)l }, &buf, &len, &cap);
    return buf;
}

void indigo_cast(void) {
    IndigoValue to = indigo_pop();
    IndigoValue from = indigo_pop();
    if (from.tag == TAG_INT) {
        int64_t x = from.payload;
        if (to.tag == TAG_INT) indigo_push(TAG_INT, x);
        else if (to.tag == TAG_FLOAT) indigo_push(TAG_FLOAT, float_to_payload((float)x));
        else if (to.tag == TAG_DOUBLE) indigo_push(TAG_DOUBLE, double_to_payload((double)x));
        else if (to.tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%ld", x);
            indigo_push(TAG_STRING, (int64_t)heap_strdup(buf));
        } else if (to.tag == TAG_BOOL) indigo_push(TAG_BOOL, x != 0 ? 1 : 0);
        else if (to.tag == TAG_CHAR) indigo_push(TAG_CHAR, x);
        else if (to.tag == TAG_NONE) indigo_push(TAG_NONE, 0);
        else indigo_push(from.tag, from.payload);
    } else if (from.tag == TAG_FLOAT) {
        float x = payload_to_float(from.payload);
        if (to.tag == TAG_INT) indigo_push(TAG_INT, (int64_t)roundf(x));
        else if (to.tag == TAG_FLOAT) indigo_push(TAG_FLOAT, from.payload);
        else if (to.tag == TAG_DOUBLE) indigo_push(TAG_DOUBLE, double_to_payload((double)x));
        else if (to.tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%g", x);
            indigo_push(TAG_STRING, (int64_t)heap_strdup(buf));
        } else if (to.tag == TAG_BOOL) indigo_push(TAG_BOOL, x != 0.0f ? 1 : 0);
        else indigo_push(from.tag, from.payload);
    } else if (from.tag == TAG_DOUBLE) {
        double x = payload_to_double(from.payload);
        if (to.tag == TAG_INT) indigo_push(TAG_INT, (int64_t)round(x));
        else if (to.tag == TAG_FLOAT) indigo_push(TAG_FLOAT, float_to_payload((float)x));
        else if (to.tag == TAG_DOUBLE) indigo_push(TAG_DOUBLE, from.payload);
        else if (to.tag == TAG_STRING) {
            char buf[64];
            snprintf(buf, sizeof(buf), "%g", x);
            indigo_push(TAG_STRING, (int64_t)heap_strdup(buf));
        } else if (to.tag == TAG_BOOL) indigo_push(TAG_BOOL, x != 0.0 ? 1 : 0);
        else indigo_push(from.tag, from.payload);
    } else if (from.tag == TAG_STRING) {
        char* x = (char*)from.payload;
        if (to.tag == TAG_INT) indigo_push(TAG_INT, atol(x));
        else if (to.tag == TAG_FLOAT) indigo_push(TAG_FLOAT, float_to_payload((float)atof(x)));
        else if (to.tag == TAG_DOUBLE) indigo_push(TAG_DOUBLE, double_to_payload(atof(x)));
        else if (to.tag == TAG_STRING) indigo_push(TAG_STRING, from.payload);
        else if (to.tag == TAG_BOOL) indigo_push(TAG_BOOL, strlen(x) > 0 ? 1 : 0);
        else if (to.tag == TAG_CHAR) indigo_push(TAG_CHAR, x[0]);
        else indigo_push(from.tag, from.payload);
    } else if (from.tag == TAG_LIST) {
        IndigoList* list = (IndigoList*)from.payload;
        if (to.tag == TAG_STRING) {
            if (is_char_list(list)) {
                indigo_push(TAG_STRING, (int64_t)char_list_to_string(list));
            } else {
                char* str = format_list_to_string(list);
                indigo_push(TAG_STRING, (int64_t)str);
            }
        } else {
            indigo_push(from.tag, from.payload);
        }
    } else {
        indigo_push(from.tag, from.payload);
    }
}

void indigo_typeof(void) {
    IndigoValue v = indigo_pop();
    const char* type_name = "Unknown";
    switch (v.tag) {
        case TAG_INT: type_name = "Int"; break;
        case TAG_FLOAT: type_name = "Float"; break;
        case TAG_DOUBLE: type_name = "Double"; break;
        case TAG_STRING: type_name = "String"; break;
        case TAG_BOOL: type_name = "Bool"; break;
        case TAG_LIST: type_name = "List"; break;
        case TAG_NONE: type_name = "None"; break;
        case TAG_CHAR: type_name = "Char"; break;
        case TAG_FUNCREF: type_name = "FuncRef"; break;
        case TAG_CPTR: type_name = "CPtr"; break;
        case TAG_MAP: {
            IndigoMap* m = (IndigoMap*)v.payload;
            IndigoValue* name_val = indigo_map_get(m, "__name");
            if (name_val && name_val->tag == TAG_STRING) {
                type_name = (char*)name_val->payload;
            } else {
                type_name = "Map";
            }
            break;
        }
        default: type_name = "Unknown"; break;
    }
    indigo_push(TAG_STRING, (int64_t)heap_strdup(type_name));
}

void indigo_type_eq(void) {
    IndigoValue y = indigo_pop();
    IndigoValue x = indigo_pop();
    bool eq = false;
    if (y.tag == TAG_TYPEQUERY) {
        char* q = (char*)y.payload;
        if (x.tag == TAG_INT && strcmp(q, "Int") == 0) eq = true;
        else if (x.tag == TAG_FLOAT && strcmp(q, "Float") == 0) eq = true;
        else if (x.tag == TAG_DOUBLE && strcmp(q, "Double") == 0) eq = true;
        else if (x.tag == TAG_STRING && strcmp(q, "String") == 0) eq = true;
        else if (x.tag == TAG_BOOL && strcmp(q, "Bool") == 0) eq = true;
        else if (x.tag == TAG_LIST && strcmp(q, "List") == 0) eq = true;
        else if (x.tag == TAG_NONE && strcmp(q, "None") == 0) eq = true;
        else if (x.tag == TAG_CHAR && strcmp(q, "Char") == 0) eq = true;
        else if (x.tag == TAG_MAP) {
            IndigoMap* m = (IndigoMap*)x.payload;
            IndigoValue* name_val = indigo_map_get(m, "__name");
            IndigoValue* traits_val = indigo_map_get(m, "__traits");
            if (name_val && name_val->tag == TAG_STRING && strcmp((char*)name_val->payload, q) == 0) {
                eq = true;
            } else if (traits_val && traits_val->tag == TAG_LIST) {
                IndigoList* tl = (IndigoList*)traits_val->payload;
                for (int64_t i = 0; i < tl->length; i++) {
                    if (tl->items[i].tag == TAG_STRING && strcmp((char*)tl->items[i].payload, q) == 0) {
                        eq = true;
                        break;
                    }
                }
            }
        }
    } else if (x.tag == TAG_MAP && y.tag == TAG_MAP) {
        IndigoMap* ma = (IndigoMap*)x.payload;
        IndigoMap* mb = (IndigoMap*)y.payload;
        IndigoValue* na = indigo_map_get(ma, "__name");
        IndigoValue* nb = indigo_map_get(mb, "__name");
        if (na && nb && na->tag == TAG_STRING && nb->tag == TAG_STRING) {
            eq = strcmp((char*)na->payload, (char*)nb->payload) == 0;
        } else {
            eq = true;
        }
    } else {
        eq = x.tag == y.tag;
    }
    indigo_push(TAG_BOOL, eq ? 1 : 0);
}

static void print_indigo_value(IndigoValue v) {
    switch (v.tag) {
        case TAG_INT:
            printf("%ld", v.payload);
            break;
        case TAG_FLOAT:
            printf("%g", payload_to_float(v.payload));
            break;
        case TAG_DOUBLE:
            printf("%g", payload_to_double(v.payload));
            break;
        case TAG_STRING:
            printf("%s", (char*)v.payload);
            break;
        case TAG_BOOL:
            printf("%s", v.payload ? "True" : "False");
            break;
        case TAG_CHAR:
            putchar((int)v.payload);
            break;
        case TAG_NONE:
            printf("None");
            break;
        case TAG_CPTR:
            printf("CPtr %ld", v.payload);
            break;
        case TAG_FUNCREF: {
            IndigoFuncRef* r = (IndigoFuncRef*)v.payload;
            printf("<%s(%ld)>", r->name ? r->name : "fn", r->arg_count);
            break;
        }
        case TAG_LIST: {
            IndigoList* l = (IndigoList*)v.payload;
            printf("[");
            for (int64_t i = 0; i < l->length; i++) {
                if (i > 0) printf(",");
                print_indigo_value(l->items[i]);
            }
            printf("]");
            break;
        }
        case TAG_MAP: {
            IndigoMap* m = (IndigoMap*)v.payload;
            IndigoValue* name_val = indigo_map_get(m, "__name");
            if (name_val && name_val->tag == TAG_STRING) {
                printf("%s{", (char*)name_val->payload);
            } else {
                printf("{");
            }
            bool first = true;
            IndigoMapEntry* cur = m->head;
            while (cur) {
                if (strcmp(cur->key, "__name") != 0 && strcmp(cur->key, "__traits") != 0) {
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
        indigo_push(TAG_STRING, (int64_t)heap_strdup(buf));
    } else {
        indigo_push(TAG_STRING, (int64_t)heap_strdup(""));
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
    indigo_push(TAG_FLOAT, float_to_payload(r));
}

void indigo_exit(void) {
    IndigoValue v = indigo_pop();
    int code = (v.tag == TAG_INT) ? (int)v.payload : 0;
    exit(code);
}

void indigo_label_not_found(const char* name) {
    fprintf(stderr, "Label not found: %s\n", name);
    exit(1);
}
|]
