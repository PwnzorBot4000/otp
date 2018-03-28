

#ifndef HIPE_AARCH64_H
#define HIPE_AARCH64_H

extern void hipe_flush_icache_word(void *address);
extern void hipe_flush_icache_range(void *address, unsigned int nbytes);

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	3	/* low 3 bits are always zero */

/* for hipe_bifs_{read,write}_{s,u}32 -- XXX: most likely, NOT needed */
static __inline__ int hipe_word32_address_ok(void *address)
{
    return ((unsigned long)address & 0x3) == 0;
}

/* for hipe_bifs_{read,write}_{s,u}64 */
static __inline__ int hipe_word64_address_ok(void *address)
{
    return 1;
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#define hipe_arch_name	am_aarch64

extern void hipe_aarch64_inc_stack(void);

#endif /* HIPE_AARCH_H */
