#ifndef __BITSTRING_H__
#define __BITSTRING_H__

#include <stdlib.h>
#include <stdint.h>

typedef uint32_t bs_base_t;

typedef struct {
    size_t size;
    bs_base_t *string;
} bitstring_t;

const size_t bs_base_size = sizeof(bs_base_t);

static inline void bitstring_initialize(bitstring_t *bs, size_t size) {
    bs->size = size;
    bs->string = (uint32_t *)malloc(size / bs_base_size + 1);
    memset(bs->string, 0, size / bs_base_size + 1);
    return bs;
}

static inline void bitstring_delete(bitstring_t bs) {
    free(bs.string);
}

static inline void bitstring_set(bitstring_t bs, size_t el) {
    size_t i = el / bs_base_size;
    bs_base_t mask = 1 << (el % bs_base_size);
    bs.string[i] |= mask;
}

static inline void bitstring_clear(bitstring_t bs, size_t el) {
    size_t i = el / bs_base_size;
    bs_base_t mask = 1 << (el % bs_base_size);
    bs.string[i] &= !mask;
}

static inline int bitstring_is_set(const bitstring_t bs, size_t el) {
    size_t i = el / bs_base_size;
    bs_base_t mask = 1 << (el % bs_base_size);
    return ((bs.string[i] & mask) != 0);
}

static inline int bitstring_is_clear(const bitstring_t bs, size_t el) {
    return !bitstring_is_set(bs, el);
}

#endif
