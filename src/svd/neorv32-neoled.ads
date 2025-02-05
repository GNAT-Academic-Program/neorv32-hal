pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Smart LED hardware interface
package neorv32.NEOLED is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_NEOLED_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_MODE_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_STROBE_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_NEOLED_CTRL_BUFS_Field is neorv32.UInt4;
   subtype CTRL_NEOLED_CTRL_T_TOT_Field is neorv32.UInt5;
   subtype CTRL_NEOLED_CTRL_T_ZERO_H_Field is neorv32.UInt5;
   subtype CTRL_NEOLED_CTRL_T_ONE_H_Field is neorv32.UInt5;
   subtype CTRL_NEOLED_CTRL_IRQ_CONF_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_TX_HALF_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_NEOLED_CTRL_TX_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  NEOLED enable flag
      NEOLED_CTRL_EN       : CTRL_NEOLED_CTRL_EN_Field := 16#0#;
      --  TX mode (0=24-bit, 1=32-bit)
      NEOLED_CTRL_MODE     : CTRL_NEOLED_CTRL_MODE_Field := 16#0#;
      --  Strobe (0=send normal data, 1=send RESET command on data write)
      NEOLED_CTRL_STROBE   : CTRL_NEOLED_CTRL_STROBE_Field := 16#0#;
      --  Clock prescaler select
      NEOLED_CTRL_PRSC     : CTRL_NEOLED_CTRL_PRSC_Field := 16#0#;
      --  Read-only. log2(tx buffer size)
      NEOLED_CTRL_BUFS     : CTRL_NEOLED_CTRL_BUFS_Field := 16#0#;
      --  pulse-clock ticks per total period bit
      NEOLED_CTRL_T_TOT    : CTRL_NEOLED_CTRL_T_TOT_Field := 16#0#;
      --  pulse-clock ticks per ZERO high-time
      NEOLED_CTRL_T_ZERO_H : CTRL_NEOLED_CTRL_T_ZERO_H_Field := 16#0#;
      --  pulse-clock ticks per ONE high-time
      NEOLED_CTRL_T_ONE_H  : CTRL_NEOLED_CTRL_T_ONE_H_Field := 16#0#;
      --  unspecified
      Reserved_25_26       : neorv32.UInt2 := 16#0#;
      --  TX FIFO interrupt: 0=IRQ if FIFO is less than half-full, 1=IRQ if
      --  FIFO is empty
      NEOLED_CTRL_IRQ_CONF : CTRL_NEOLED_CTRL_IRQ_CONF_Field := 16#0#;
      --  Read-only. TX FIFO is empty
      NEOLED_CTRL_TX_EMPTY : CTRL_NEOLED_CTRL_TX_EMPTY_Field := 16#0#;
      --  Read-only. TX FIFO is at least half-full
      NEOLED_CTRL_TX_HALF  : CTRL_NEOLED_CTRL_TX_HALF_Field := 16#0#;
      --  Read-only. TX FIFO is full
      NEOLED_CTRL_TX_FULL  : CTRL_NEOLED_CTRL_TX_FULL_Field := 16#0#;
      --  Read-only. busy flag
      NEOLED_CTRL_TX_BUSY  : CTRL_NEOLED_CTRL_TX_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      NEOLED_CTRL_EN       at 0 range 0 .. 0;
      NEOLED_CTRL_MODE     at 0 range 1 .. 1;
      NEOLED_CTRL_STROBE   at 0 range 2 .. 2;
      NEOLED_CTRL_PRSC     at 0 range 3 .. 5;
      NEOLED_CTRL_BUFS     at 0 range 6 .. 9;
      NEOLED_CTRL_T_TOT    at 0 range 10 .. 14;
      NEOLED_CTRL_T_ZERO_H at 0 range 15 .. 19;
      NEOLED_CTRL_T_ONE_H  at 0 range 20 .. 24;
      Reserved_25_26       at 0 range 25 .. 26;
      NEOLED_CTRL_IRQ_CONF at 0 range 27 .. 27;
      NEOLED_CTRL_TX_EMPTY at 0 range 28 .. 28;
      NEOLED_CTRL_TX_HALF  at 0 range 29 .. 29;
      NEOLED_CTRL_TX_FULL  at 0 range 30 .. 30;
      NEOLED_CTRL_TX_BUSY  at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Smart LED hardware interface
   type NEOLED_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  Data register
      DATA : aliased neorv32.UInt32;
   end record
     with Volatile;

   for NEOLED_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  Smart LED hardware interface
   NEOLED_Periph : aliased NEOLED_Peripheral
     with Import, Address => NEOLED_Base;

end neorv32.NEOLED;
