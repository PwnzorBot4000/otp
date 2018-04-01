/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */


#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "erl_binary.h"

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */
#include "hipe_bif0.h"

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    register unsigned long long beg __asm__("x0") = (unsigned long long)address;
    register unsigned long long end __asm__("x1") = (unsigned long long)address + nbytes;
    register unsigned long long flg __asm__("x2") = 0;
    register unsigned long long scno __asm__("x7") = 0xf0002;
    __asm__ __volatile__("svc #0"	/* sys_cacheflush() */
			 : "=r"(beg)
			 : "0"(beg), "r"(end), "r"(flg), "r"(scno));
}

void hipe_flush_icache_word(void *address)
{
    hipe_flush_icache_range(address, 8);
}


static int check_callees(Eterm callees)
{
    Eterm *tuple;
    Uint arity;
    Uint i;

    if (is_not_tuple(callees))
	return -1;
    tuple = tuple_val(callees);
    arity = arityval(tuple[0]);
    for (i = 1; i <= arity; ++i) {
	Eterm mfa = tuple[i];
	if (is_atom(mfa))
	    continue;
	if (is_not_tuple(mfa) ||
	    tuple_val(mfa)[0] != make_arityval(3) ||
	    is_not_atom(tuple_val(mfa)[1]) ||
	    is_not_atom(tuple_val(mfa)[2]) ||
	    is_not_small(tuple_val(mfa)[3]) ||
	    unsigned_val(tuple_val(mfa)[3]) > 255)
	    return -1;
    }
    return arity;
}

#define TRAMPOLINE_WORDS 4

static void generate_trampolines(Uint32* address,
                                 int nrcallees, Eterm callees,
                                 Uint32** trampvec)
{
    Uint32* trampoline = address;
    int i;

    for (i = 0; i < nrcallees; ++i) {
        trampoline[0] = 0x58000050;     /* ldr x16, .+8 */
        trampoline[1] = 0xD61F0200;		/* br x16 */
        trampoline[2] = 0;		/* callee's address (part 1) */
        trampoline[3] = 0;		/* callee's address (part 2)*/
    	trampvec[i] = trampoline;
        trampoline += TRAMPOLINE_WORDS;
    }
    hipe_flush_icache_range(address, nrcallees*TRAMPOLINE_WORDS*sizeof(Uint32));
}

void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p)
{
    Uint code_words;
    int nrcallees;
    Eterm trampvecbin;
    Uint32 **trampvec;
    Uint32 *address;

    if (nrbytes & 0x3)
	return NULL;
    code_words = nrbytes / sizeof(Uint32);

    nrcallees = check_callees(callees);
    if (nrcallees < 0)
	return NULL;
    trampvecbin = new_binary(p, NULL, nrcallees*sizeof(Uint32*));
    trampvec = (Uint32**)binary_bytes(trampvecbin);

    address = erts_alloc(ERTS_ALC_T_HIPE_EXEC,
                         (code_words + nrcallees*TRAMPOLINE_WORDS)*sizeof(Uint32));

    generate_trampolines(address + code_words, nrcallees, callees, trampvec);
    *trampolines = trampvecbin;
    return address;
}

void  hipe_free_code(void* code, unsigned int bytes)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, code);
}

/*
 * ARMv8's support for 64-bit immediates is effectively non-existent.
 * Hence, every 64-bit immediate is stored in memory and loaded via
 * a PC-relative addressing mode. Relocation entries refer to those
 * data words, NOT the load instructions, so patching is trivial.
 */
static void patch_imm64(Uint64 *address, unsigned long long imm64)
{
    *address = imm64;
    hipe_flush_icache_word(address);
}

void hipe_patch_load_fe(Uint64 *address, Uint64 value)
{
    patch_imm64(address, value);
}

int hipe_patch_insn(void *address, Uint64 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	break;
      default:
	return -1;
    }
    patch_imm64((Uint64*)address, value);
    return 0;
}

/* Make stub for native code calling exported beam function
*/
void *hipe_make_native_stub(void *callee_exp, unsigned int beamArity)
{
    Uint32 *code;
    Sint64 callemu_offset;
    int is_short_jmp;

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * mov x0, #beamArity
     * ldr x8, .+12 // callee_exp
     * b nbif_callemu
     * dd ?
     * .quad callee_exp
     *
     * or if nbif_callemu is too far away (+-128MB):
     *
     * mov x0, #beamArity
     * ldr x8, .+12 // callee_exp
     * ldr x16, .+16 // nbif_callemu
     * br x16
     * .quad callee_exp
     * .quad nbif_callemu
     *
     * I'm using x0 and x8 since they aren't used for
     * parameter passing in native code.
     */

    code = erts_alloc(ERTS_ALC_T_HIPE_EXEC, 8*sizeof(Uint32));
    if (!code)
	return NULL;
    callemu_offset = (Sint64)((Uint64)(&nbif_callemu) >> 2) - (Sint64)((Uint64)(&code[2]) >> 2);
    is_short_jmp = (callemu_offset >= -0x02000000 &&
                    callemu_offset <= 0x01FFFFFF);
#ifdef DEBUG
    if (is_short_jmp && (callemu_offset % 3)==0) {
        is_short_jmp = 0;
    }
#endif

    /* mov x0, #beamArity */
    code[0] = 0xD2800000 | ((beamArity & 0xFF) << 5);
    /* ldr x8, .+12 // callee_exp */
    code[1] = 0x58000000 | (3 << 5) | 8;
    if (is_short_jmp) {
        /* b nbif_callemu */
        code[2] = 0x14000000 | (callemu_offset & 0x03FFFFFF);
    }
    else {
        /* ldr x16, .+16 // nbif_callemu */
        code[2] = 0x58000000 | (4 << 5) | 16;
        /* br x16 */
        code[3] = 0xD61F0200;
    }
    /* .quad callee_exp */
    *((Uint64*)&(code[4])) = (Uint64) callee_exp;
    if (!is_short_jmp) {
        /* .quad nbif_callemu */
        *((Uint64*)&(code[6])) = (Uint64) &nbif_callemu;
    }

    hipe_flush_icache_range(code, 8*sizeof(Uint32));

    return code;
}

void hipe_free_native_stub(void* stub)
{
    erts_free(ERTS_ALC_T_HIPE_EXEC, stub);
}

static void patch_b(Uint32 *address, Sint64 offset, Uint32 AA)
{
    Uint32 oldI = *address;
    Uint32 newI = (oldI & 0xFC000000) | (Uint32)(offset & 0x03FFFFFF);
    *address = newI;
    hipe_flush_icache_word(address);
}

int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline)
{
    Sint64 destOffset = (Sint64)((Uint64)destAddress >> 2) - (Sint64)((Uint64)callAddress >> 2);
    if (destOffset >= -0x02000000 && destOffset <= 0x01FFFFFF) {
	/* The destination is within a [-128MB,+128MB] range from us.
	   We can reach it with a b/bl instruction.
	   This is typical for nearby Erlang code. */
	patch_b((Uint32*)callAddress, destOffset, 0);
    } else {
	/* The destination is too distant for b/bl.
	   Must do a b/bl to the trampoline. */
	Sint64 trampOffset = (Sint64)((Uint64)trampoline >> 2) - (Sint64)((Uint64)callAddress >> 2);
	if (trampOffset >= -0x02000000 && trampOffset <= 0x01FFFFFF) {
	    /* Update the trampoline's address computation.
	       (May be redundant, but we can't tell.) */
	    patch_imm64((Uint64*)trampoline+1, (Uint64)destAddress);
	    /* Update this call site. */
	    patch_b((Uint32*)callAddress, trampOffset, 0);
	} else
	    return -1;
    }
    return 0;
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx | %*s |\r\n", (int)offsetof(struct hipe_process_state,x), n, 2*(int)sizeof(long), (unsigned long)p->x, 2+2*(int)sizeof(long), "")
    U("nra        ", nra);
    U("narity     ", narity);
#undef U
}
