#ifndef __BITSTRING_H__
#define __BITSTRING_H__

#include <stdlib.h>
#include <stdint.h>

#include <R.h> /* for R_INLINE */

typedef uint8_t bs_base_t;

typedef struct {
    bs_base_t *string;
} bitstring_t;

const size_t bs_base_size = 8 * sizeof(bs_base_t);

#define BIT_INDEX(B) ((B) >> 3)
#define BIT_MASK(B)  (1 << ((B) & 0x7))

static R_INLINE size_t bitstring_size(size_t nbits) {
    return ((((nbits) - 1) >> 3) + 1);
}

static R_INLINE void bitstring_initialize(bitstring_t *bs, size_t nbits) {
    const size_t size = bitstring_size(nbits);
    bs->string = (uint8_t *)malloc(size);
    if (NULL == bs->string) 
        error("Could not allocate bitstring of size %i.", size);
    memset(bs->string, 0, size);
}

static R_INLINE void bitstring_delete(bitstring_t bs) {
    free(bs.string);
    bs.string = NULL;
}

static R_INLINE void bitstring_set(bitstring_t bs, size_t bit) {
    bs.string[BIT_INDEX(bit)] |= BIT_MASK(bit);
}

static R_INLINE void bitstring_clear(bitstring_t bs, size_t bit) {
    bs.string[BIT_INDEX(bit)] &= ~BIT_MASK(bit);
}

static R_INLINE int bitstring_is_set(const bitstring_t bs, size_t bit) {
    return bs.string[BIT_INDEX(bit)] & BIT_MASK(bit);
}

static R_INLINE int bitstring_is_clear(const bitstring_t bs, size_t bit) {
    return !bitstring_is_set(bs, bit);
}

#endif
