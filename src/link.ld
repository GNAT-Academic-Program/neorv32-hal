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


/* This is a RISC-V specific version of this file */

_DEFAULT_STACK_SIZE = 0x1000;

OUTPUT_ARCH("riscv")

ENTRY(_start);

MEMORY
{ /* As an uploadable app: */
  rom (rx) : ORIGIN = 0x0, LENGTH = 0x10000

  /* As a bootloader app: */
  /*rom (rx) : ORIGIN = 0xFFE00000, LENGTH = 0x10000 */
  ram (rwx) : ORIGIN = 0x80000000, LENGTH = 0x4000
}

SECTIONS
{
  /*******/
  /* rom */
  /*******/
  .text :
  {
    KEEP (*(SORT_NONE(.start)))
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
    KEEP (*(.init))
    KEEP (*(.fini))
  } >  rom

  .eh_frame_hdr :
  {
      *(.eh_frame_hdr)
  } > rom

  .eh_frame :
  {
      __EH_FRAME__ = .;
      KEEP(*(.eh_frame))
      LONG(0);
  } > rom

  .gcc_except_table :
  {
      *(.gcc_except_table .gcc_except_table.*)
  } > rom

  .init_array :
  {
    PROVIDE_HIDDEN (__init_array_start = .);
    KEEP (*(SORT(.init_array.*)))
    KEEP (*(.init_array*))
    PROVIDE_HIDDEN (__init_array_end = .);
  } > rom

  .fini_array :
  {
    PROVIDE_HIDDEN (__fini_array_start = .);
    KEEP (*(SORT(.fini_array.*)))
    KEEP (*(.fini_array*))
    PROVIDE_HIDDEN (__fini_array_end = .);
  } > rom

  .rodata :
  {
    *(.rdata)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rom_end = .;
  } > rom
  /**************/
  /* End of rom */
  /**************/

  /*******/
  /* ram */
  /*******/
  .data :
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
  } > ram AT > rom

  .srodata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    PROVIDE( __global_pointer$ = . + 0x800 );
    *(.srodata.cst16)
    *(.srodata.cst8)
    *(.srodata.cst4)
    *(.srodata.cst2)
    *(.srodata .srodata.*)
  } > ram  AT > rom

  .sdata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    *(.sdata .sdata.*)
    *(.gnu.linkonce.s.*)
  } > ram  AT > rom

  __data_end = .;

  /* Size of all data sections (.data, .srodata, .sdata) in number of 32bit
   * words.
   */
  __data_words = (__data_end - __data_start) >> 2;

  /* Base address of all data sections in ROM. The startup code copies these
   * sections from __data_load (in ROM) to __data_start (in RAM).
   */
  __data_load = LOADADDR(.data);

  .bss (NOLOAD): {
    . = ALIGN(0x8);
    __bss_start = .;

    *(.sbss*)
    *(.gnu.linkonce.sb.*)
    *(.bss .bss.*)
    *(.gnu.linkonce.b.*)
    *(COMMON)

    /* Interrupt stacks. Statically allocated in System.BB.Interrupts */
    __interrupt_stack_start = .;
    *(.interrupt_stacks)
    . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
    __interrupt_stack_end = .;

    __stack_start = .;
    . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
    . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
    __stack_end = .;

    _end = .;
    __heap_start = .;
    __heap_end = ORIGIN(ram) + LENGTH(ram);
    __bss_end = .;
  } > ram
  __bss_words = (__bss_end - __bss_start) >> 2;
  /**************/
  /* End of ram */
  /**************/

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
