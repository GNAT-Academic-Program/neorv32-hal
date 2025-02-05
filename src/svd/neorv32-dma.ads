pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Direct memory access controller
package neorv32.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_DMA_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_AUTO_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_FENCE_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_ERROR_RD_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_ERROR_WR_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_BUSY_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_DONE_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_FIRQ_TYPE_Field is neorv32.Bit;
   subtype CTRL_DMA_CTRL_FIRQ_SEL_Field is neorv32.UInt4;

   --  Control register
   type CTRL_Register is record
      --  DMA enable flag
      DMA_CTRL_EN        : CTRL_DMA_CTRL_EN_Field := 16#0#;
      --  Enable automatic transfer trigger (FIRQ-triggered)
      DMA_CTRL_AUTO      : CTRL_DMA_CTRL_AUTO_Field := 16#0#;
      --  Issue a downstream FENCE operation when DMA transfer completes
      --  (without errors)
      DMA_CTRL_FENCE     : CTRL_DMA_CTRL_FENCE_Field := 16#0#;
      --  unspecified
      Reserved_3_7       : neorv32.UInt5 := 16#0#;
      --  Read-only. Error during last read access
      DMA_CTRL_ERROR_RD  : CTRL_DMA_CTRL_ERROR_RD_Field := 16#0#;
      --  Read-only. Error during last write access
      DMA_CTRL_ERROR_WR  : CTRL_DMA_CTRL_ERROR_WR_Field := 16#0#;
      --  Read-only. DMA transfer in progress
      DMA_CTRL_BUSY      : CTRL_DMA_CTRL_BUSY_Field := 16#0#;
      --  DMA transfer done; auto-clears on write access
      DMA_CTRL_DONE      : CTRL_DMA_CTRL_DONE_Field := 16#0#;
      --  unspecified
      Reserved_12_14     : neorv32.UInt3 := 16#0#;
      --  Trigger on rising-edge (0) or high-level (1) or selected FIRQ channel
      DMA_CTRL_FIRQ_TYPE : CTRL_DMA_CTRL_FIRQ_TYPE_Field := 16#0#;
      --  FIRQ trigger select
      DMA_CTRL_FIRQ_SEL  : CTRL_DMA_CTRL_FIRQ_SEL_Field := 16#0#;
      --  unspecified
      Reserved_20_31     : neorv32.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      DMA_CTRL_EN        at 0 range 0 .. 0;
      DMA_CTRL_AUTO      at 0 range 1 .. 1;
      DMA_CTRL_FENCE     at 0 range 2 .. 2;
      Reserved_3_7       at 0 range 3 .. 7;
      DMA_CTRL_ERROR_RD  at 0 range 8 .. 8;
      DMA_CTRL_ERROR_WR  at 0 range 9 .. 9;
      DMA_CTRL_BUSY      at 0 range 10 .. 10;
      DMA_CTRL_DONE      at 0 range 11 .. 11;
      Reserved_12_14     at 0 range 12 .. 14;
      DMA_CTRL_FIRQ_TYPE at 0 range 15 .. 15;
      DMA_CTRL_FIRQ_SEL  at 0 range 16 .. 19;
      Reserved_20_31     at 0 range 20 .. 31;
   end record;

   subtype TTYPE_DMA_TTYPE_NUM_Field is neorv32.UInt24;
   subtype TTYPE_DMA_TTYPE_QSEL_Field is neorv32.UInt2;
   subtype TTYPE_DMA_TTYPE_SRC_INC_Field is neorv32.Bit;
   subtype TTYPE_DMA_TTYPE_DST_INC_Field is neorv32.Bit;
   subtype TTYPE_DMA_TTYPE_ENDIAN_Field is neorv32.Bit;

   --  Destination base address; shows the last accessed write address on read
   --  access
   type TTYPE_Register is record
      --  Number of elements to transfer
      DMA_TTYPE_NUM     : TTYPE_DMA_TTYPE_NUM_Field := 16#0#;
      --  unspecified
      Reserved_24_26    : neorv32.UInt3 := 16#0#;
      --  Data quantity select
      DMA_TTYPE_QSEL    : TTYPE_DMA_TTYPE_QSEL_Field := 16#0#;
      --  Source constant (0) or incrementing (1) address
      DMA_TTYPE_SRC_INC : TTYPE_DMA_TTYPE_SRC_INC_Field := 16#0#;
      --  Destination constant (0) or incrementing (1) address
      DMA_TTYPE_DST_INC : TTYPE_DMA_TTYPE_DST_INC_Field := 16#0#;
      --  Convert Endianness when set
      DMA_TTYPE_ENDIAN  : TTYPE_DMA_TTYPE_ENDIAN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TTYPE_Register use record
      DMA_TTYPE_NUM     at 0 range 0 .. 23;
      Reserved_24_26    at 0 range 24 .. 26;
      DMA_TTYPE_QSEL    at 0 range 27 .. 28;
      DMA_TTYPE_SRC_INC at 0 range 29 .. 29;
      DMA_TTYPE_DST_INC at 0 range 30 .. 30;
      DMA_TTYPE_ENDIAN  at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Direct memory access controller
   type DMA_Peripheral is record
      --  Control register
      CTRL     : aliased CTRL_Register;
      --  Source base address; shows the last accessed read address on read
      --  access
      SRC_BASE : aliased neorv32.UInt32;
      --  Destination base address; shows the last accessed write address on
      --  read access
      DST_BASE : aliased neorv32.UInt32;
      --  Destination base address; shows the last accessed write address on
      --  read access
      TTYPE    : aliased TTYPE_Register;
   end record
     with Volatile;

   for DMA_Peripheral use record
      CTRL     at 16#0# range 0 .. 31;
      SRC_BASE at 16#4# range 0 .. 31;
      DST_BASE at 16#8# range 0 .. 31;
      TTYPE    at 16#C# range 0 .. 31;
   end record;

   --  Direct memory access controller
   DMA_Periph : aliased DMA_Peripheral
     with Import, Address => DMA_Base;

end neorv32.DMA;
