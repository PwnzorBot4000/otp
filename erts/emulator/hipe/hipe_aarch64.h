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


#ifndef HIPE_AARCH64_H
#define HIPE_AARCH64_H

#include "hipe_arm.h"
#undef hipe_arch_name

/* for hipe_bifs_{read,write}_{s,u}64 */
static __inline__ int hipe_word64_address_ok(void *address)
{
    return ((unsigned long long)address & 0x3) == 0;
}

#define hipe_arch_name	am_aarch64

#endif /* HIPE_AARCH64_H */
