#include <stdint.h>

#if defined(__PCLMUL__)

#include <x86intrin.h>

void bitvec_f2polymul(size_t lenA, const uint64_t * restrict a, size_t lenB, const uint64_t * restrict b, uint64_t * restrict result)
{
    for (size_t i = 0; i < lenA; ++i) {
        __m128i av = _mm_loadl_epi64((const __m128i *)&a[i]);
        for (size_t j = 0; j < lenB; ++j) {
            __m128i bv = _mm_loadl_epi64((const __m128i *)&b[j]);
            union {
                uint64_t a[2];
                __m128i v;
            } c;
            c.v = _mm_clmulepi64_si128(av, bv, 0);
            result[i + j] ^= c.a[0];
            result[i + j + 1] ^= c.a[1];
        }
    }
}

#else
#error "CLMUL instruction not available; Compile with -mpclmul"
#endif
