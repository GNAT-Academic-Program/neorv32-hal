/*****************************************************************************
 *                                                                           *
 *                         Copyright (C) 2023 AdaCore                        *
 *                                                                           *
 *  GNAT is free software;  you can  redistribute it  and/or modify it under *
 *  terms of the  GNU General Public License as published  by the Free Soft- *
 *  ware  Foundation;  either version 3,  or (at your option) any later ver- *
 *  sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 *  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 *  or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                           *
 *  As a special exception under Section 7 of GPL version 3, you are granted *
 *  additional permissions described in the GCC Runtime Library Exception,   *
 *  version 3.1, as published by the Free Software Foundation.               *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License and    *
 *  a copy of the GCC Runtime Library Exception along with this program;     *
 *  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 *  <http://www.gnu.org/licenses/>.                                          *
 *                                                                           *
 *****************************************************************************/

        /*********/
        /* .data */
        /*********/
         .section .data.argv
argv_str:
        .ascii  "main\0"

        .align 4
argv:
        .word argv_str
        .word 0

        /**********/
        /* _start */
        /**********/

        .section .start
        .globl _start
        .type _start,@function

_start:
.option push
.option norelax
        la gp, __global_pointer$
.option pop
        la sp, __stack_end


        /* Load data section */
        .type _startup_copy_data,@function
_startup_copy_data:
        la a0, __data_load
        la a1, __data_start
        la a2, __data_end
        bgeu a1, a2, 2f
1:
        lw t0, (a0)
        sw t0, (a1)
        addi a0, a0, 4
        addi a1, a1, 4
        bltu a1, a2, 1b
2:
        .size _startup_copy_data, . - _startup_copy_data


        /* Clear bss section */
        .type _startup_clear_bss,@function
_startup_clear_bss:
        la a0, __bss_start
        la a1, __bss_end
        bgeu a0, a1, 2f
1:
        sw zero, (a0)
        addi a0, a0, 4
        bltu a0, a1, 1b
2:
        .size _startup_clear_bss, . - _startup_clear_bss


        /* Call static constructors */
.weak __libc_init_array
        la t0, __libc_init_array
        beq t0, zero, .skip_libc_init
        jalr t0
.skip_libc_init:

        /* Call main, with argc, argv  */
        la a0, 1
        la a1, argv
        call main

        /* Save main's return value */
        mv t0, a0

        /* static destructors */
.weak __libc_fini_array
        la t0, __libc_fini_array
        beq t0, zero, .skip_libc_fini
        jalr t0
.skip_libc_fini:

        /* Restore main's return value */
        mv a0, t0

        call __gnat_exit
2:      j 2b






