#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef union {
    char data[16];

    struct {
        uint8_t filler[15],
            /* how many free bytes in this stack allocated string
             * same idea as fbstring
             */
            space_left : 4,
            /* if it is on heap, set to 1 */
            is_ptr : 1, flag1 : 1, flag2 : 1, flag3 : 1;
    };

    /* heap allocated */
    struct {
        char *ptr;
        /* supports strings up to 2^54 - 1 bytes */
        size_t size : 54,
            /* capacity is always a power of 2 (unsigned)-1 */
            capacity : 6;
        /* the last 4 bits are important flags */
    };
} xs;

static inline bool xs_is_ptr(const xs *x) { return x->is_ptr; }
static inline size_t xs_size(const xs *x)
{
    return xs_is_ptr(x) ? x->size : 15 - x->space_left;
}
static inline char *xs_data(const xs *x)
{
    return xs_is_ptr(x) ? (char *) x->ptr : (char *) x->data;
}
static inline size_t xs_capacity(const xs *x)
{
    return xs_is_ptr(x) ? ((size_t) 1 << x->capacity) - 1 : 15;
}

#define xs_literal_empty() \
    (xs) { .space_left = 15 }

#define xs_ref_counter(xs) \
    *(xs->ptr + xs->size + 1)

#define MAX_REF_COUNTER 255

static inline int ilog2(uint32_t n) { return 32 - __builtin_clz(n) - 1; }

xs *xs_new(xs *x, const void *p)
{
    *x = xs_literal_empty();
    size_t len = strlen(p) + 1;
    if (len > 16) {
        x->capacity = ilog2(len) + 1;
        x->size = len - 1;
        x->is_ptr = true;
        x->ptr = malloc((size_t) 1 << x->capacity);
        memcpy(x->ptr, p, len);
        xs_ref_counter(x) = 1;
    } else {
        memcpy(x->data, p, len);
        x->space_left = 15 - (len - 1);
        x->is_ptr = false;
    }
    return x;
}

#define xs_tmp(x) \
    xs_new(&xs_literal_empty(), x)

/* grow up to specified size */
xs *xs_grow(xs *x, size_t len)
{
    if (len <= xs_capacity(x))
        return x;
    len = ilog2(len) + 1;
    if (xs_is_ptr(x))
        x->ptr = realloc(x->ptr, (size_t) 1 << len);
    else {
        char buf[16];
        memcpy(buf, x->data, 16);
        x->ptr = malloc((size_t) 1 << len);
        memcpy(x->ptr, buf, 16);
    }
    x->is_ptr = true;
    x->capacity = len;
    return x;
}

static inline xs *xs_newempty(xs *x)
{
    *x = xs_literal_empty();
    return x;
}

static inline xs *xs_free(xs *x)
{
    if (xs_is_ptr(x))
        free(xs_data(x));
    return xs_newempty(x);
}

static inline void xs_cow(xs *x)
{
    *(x->ptr + x->size + 1) -= 1;
    xs *dup_x = xs_tmp(x->ptr);
    for (int i = 0; i < 16; ++i)
        x->data[i] = dup_x->data[i];
}

xs *xs_concat(xs *string, const xs *prefix, const xs *suffix)
{
    if (xs_is_ptr(string) && xs_ref_counter(string) != 1)
        xs_cow(string);

    size_t pres = xs_size(prefix), sufs = xs_size(suffix),
           size = xs_size(string), capacity = xs_capacity(string);

    char *pre = xs_data(prefix), *suf = xs_data(suffix),
         *data = xs_data(string);

    if (size + pres + sufs <= capacity) {
        memmove(data + pres, data, size);
        memcpy(data, pre, pres);
        memcpy(data + pres + size, suf, sufs + 1);
        if (xs_is_ptr(string))
            string->size = size + pres + sufs;
        else
            string->space_left = 15 - (size + pres + sufs);
    } else {
        xs tmps = xs_literal_empty();
        xs_grow(&tmps, size + pres + sufs);
        char *tmpdata = xs_data(&tmps);
        memcpy(tmpdata + pres, data, size);
        memcpy(tmpdata, pre, pres);
        memcpy(tmpdata + pres + size, suf, sufs + 1);
        xs_free(string);
        *string = tmps;
        string->size = size + pres + sufs;
    }
    if (xs_is_ptr(string))
        xs_ref_counter(string) = 1;
    return string;
}

xs *xs_trim(xs *x, const char *trimset)
{
    if (!trimset[0])
        return x;

    if (xs_is_ptr(x) && xs_ref_counter(x) != 1)
        xs_cow(x);

    char *dataptr = xs_data(x), *orig = dataptr;

    /* similar to strspn/strpbrk but it operates on binary data */
    uint8_t mask[32] = {0};

#define check_bit(byte) (mask[(uint8_t) byte / 8] & 1 << (uint8_t) byte % 8)
#define set_bit(byte) (mask[(uint8_t) byte / 8] |= 1 << (uint8_t) byte % 8)

    size_t i, slen = xs_size(x), trimlen = strlen(trimset);

    for (i = 0; i < trimlen; i++)
        set_bit(trimset[i]);
    for (i = 0; i < slen; i++)
        if (!check_bit(dataptr[i]))
            break;
    for (; slen > 0; slen--)
        if (!check_bit(dataptr[slen - 1]))
            break;
    dataptr += i;
    slen -= i;

    /* reserved space as a buffer on the heap.
     * Do not reallocate immediately. Instead, reuse it as possible.
     * Do not shrink to in place if < 16 bytes.
     */
    memmove(orig, dataptr, slen);
    /* do not dirty memory unless it is needed */
    if (orig[slen])
        orig[slen] = 0;

    if (xs_is_ptr(x)) {
        x->size = slen;
        xs_ref_counter(x) = 1;
    } else
        x->space_left = 15 - slen;
    return x;
#undef check_bit
#undef set_bit
}

xs *xs_copy(xs *dest, xs *src)
{
    if (!dest || !src)
        return dest;
    if (xs_is_ptr(dest)) {
        xs_ref_counter(dest) -= 1;
        if (!xs_ref_counter(dest))
            free(xs_data(dest));
    }

    if (xs_is_ptr(src) && (unsigned char) xs_ref_counter(src) == MAX_REF_COUNTER) {
        xs *dup_x = xs_tmp(src->ptr);
        for (int i = 0; i < 16; ++i)
            dest->data[i] = dup_x->data[i];
        return dest;
    }

    for (int i = 0; i < 16; ++i)
        dest->data[i] = src->data[i];
    if (xs_is_ptr(src))
        xs_ref_counter(src) += 1;
    return dest;
}

xs *xs_tok(xs *x, const char *delim)
{
    static xs *old;

    if (x == NULL)
        x = xs_copy(xs_tmp(""), old);
    if (xs_data(x) == '\0') {
        xs_copy(old, x);
        return NULL;
    }
    if (!delim[0])
        return x;

    if (xs_is_ptr(x) && xs_ref_counter(x) != 1)
        xs_cow(x);

    char *dataptr = xs_data(x), *orig = dataptr;
    uint8_t mask[32] = {0};

#define check_bit(byte) (mask[(uint8_t) byte / 8] & 1 << (uint8_t) byte % 8)
#define set_bit(byte) (mask[(uint8_t) byte / 8] |= 1 << (uint8_t) byte % 8)

    size_t i, xlen = xs_size(x), dlen = strlen(delim);

    for (i = 0; i < dlen; i++)
        set_bit(delim[i]);
    for (i = 0; i < xlen; i++)
        if (!check_bit(dataptr[i]))
            break;

    dataptr += i;
    xlen -= i;

    memmove(orig, dataptr, xlen);
    orig[xlen] = '\0';

    if (xs_is_ptr(x))
        x->size = xlen;
    else
        x->space_left = 15 - xlen;

    if (*xs_data(x) == '\0') {
        xs_copy(old, x);
        if (xs_is_ptr(x))
            xs_ref_counter(x) = 1;
        return NULL;
    }

    dataptr = xs_data(x);
    for (i = 0; i < xlen; i++)
        if (check_bit(dataptr[i]))
            break;
    xlen = i;

    if (*(xs_data(x) + xlen) == '\0') {
        xs_copy(old, xs_tmp(""));
        if (xs_is_ptr(x))
            xs_ref_counter(x) = 1;
        return x;
    }

    orig[xlen] = '\0';
    old = xs_tmp(xs_data(x) + xlen + 1);
    if (xs_is_ptr(x)) {
        x->size = xlen;
        xs_ref_counter(x) = 1;
    } else
        x->space_left = 15 - xlen;

    return x;

#undef check_bit
#undef set_bit
}

#include <stdio.h>

int main()
{
    return 0;
}
