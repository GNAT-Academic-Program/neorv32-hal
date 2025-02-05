pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  1-Wire Interface Controller
package neorv32.ONEWIRE is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_ONEWIRE_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_CLEAR_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_PRSC_Field is neorv32.UInt2;
   subtype CTRL_ONEWIRE_CTRL_CLKDIV_Field is neorv32.Byte;
   subtype CTRL_ONEWIRE_CTRL_FIFO_Field is neorv32.UInt4;
   subtype CTRL_ONEWIRE_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_SENSE_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  ONEWIRE controller enable
      ONEWIRE_CTRL_EN       : CTRL_ONEWIRE_CTRL_EN_Field := 16#0#;
      --  Clear RXT FIFO, auto-clears
      ONEWIRE_CTRL_CLEAR    : CTRL_ONEWIRE_CTRL_CLEAR_Field := 16#0#;
      --  Clock prescaler select
      ONEWIRE_CTRL_PRSC     : CTRL_ONEWIRE_CTRL_PRSC_Field := 16#0#;
      --  Clock divider
      ONEWIRE_CTRL_CLKDIV   : CTRL_ONEWIRE_CTRL_CLKDIV_Field := 16#0#;
      --  unspecified
      Reserved_12_14        : neorv32.UInt3 := 16#0#;
      --  Read-only. log2(ONEWIRE FIFO size)
      ONEWIRE_CTRL_FIFO     : CTRL_ONEWIRE_CTRL_FIFO_Field := 16#0#;
      --  unspecified
      Reserved_19_27        : neorv32.UInt9 := 16#0#;
      --  Read-only. TX FIFO full
      ONEWIRE_CTRL_TX_FULL  : CTRL_ONEWIRE_CTRL_TX_FULL_Field := 16#0#;
      --  Read-only. RX FIFO data available
      ONEWIRE_CTRL_RX_AVAIL : CTRL_ONEWIRE_CTRL_RX_AVAIL_Field := 16#0#;
      --  Read-only. Current state of the 1-wire bus line
      ONEWIRE_CTRL_SENSE    : CTRL_ONEWIRE_CTRL_SENSE_Field := 16#0#;
      --  Read-only. Operation in progress when set
      ONEWIRE_CTRL_BUSY     : CTRL_ONEWIRE_CTRL_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      ONEWIRE_CTRL_EN       at 0 range 0 .. 0;
      ONEWIRE_CTRL_CLEAR    at 0 range 1 .. 1;
      ONEWIRE_CTRL_PRSC     at 0 range 2 .. 3;
      ONEWIRE_CTRL_CLKDIV   at 0 range 4 .. 11;
      Reserved_12_14        at 0 range 12 .. 14;
      ONEWIRE_CTRL_FIFO     at 0 range 15 .. 18;
      Reserved_19_27        at 0 range 19 .. 27;
      ONEWIRE_CTRL_TX_FULL  at 0 range 28 .. 28;
      ONEWIRE_CTRL_RX_AVAIL at 0 range 29 .. 29;
      ONEWIRE_CTRL_SENSE    at 0 range 30 .. 30;
      ONEWIRE_CTRL_BUSY     at 0 range 31 .. 31;
   end record;

   subtype DCMD_ONEWIRE_DCMD_DATA_Field is neorv32.Byte;
   subtype DCMD_ONEWIRE_DCMD_CMD_Field is neorv32.UInt2;
   subtype DCMD_ONEWIRE_DCMD_PRESENCE_Field is neorv32.Bit;

   --  Read/write transmission data/command register
   type DCMD_Register is record
      --  *** This field is modified following a read operation ***. RTX data,
      --  transmitted LSB-first
      ONEWIRE_DCMD_DATA     : DCMD_ONEWIRE_DCMD_DATA_Field := 16#0#;
      --  *** This field is modified following a read operation ***. Operation
      --  command
      ONEWIRE_DCMD_CMD      : DCMD_ONEWIRE_DCMD_CMD_Field := 16#0#;
      --  Read-only. *** This field is modified following a read operation ***.
      --  Bus presence detected
      ONEWIRE_DCMD_PRESENCE : DCMD_ONEWIRE_DCMD_PRESENCE_Field := 16#0#;
      --  unspecified
      Reserved_11_31        : neorv32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCMD_Register use record
      ONEWIRE_DCMD_DATA     at 0 range 0 .. 7;
      ONEWIRE_DCMD_CMD      at 0 range 8 .. 9;
      ONEWIRE_DCMD_PRESENCE at 0 range 10 .. 10;
      Reserved_11_31        at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  1-Wire Interface Controller
   type ONEWIRE_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  Read/write transmission data/command register
      DCMD : aliased DCMD_Register;
   end record
     with Volatile;

   for ONEWIRE_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DCMD at 16#4# range 0 .. 31;
   end record;

   --  1-Wire Interface Controller
   ONEWIRE_Periph : aliased ONEWIRE_Peripheral
     with Import, Address => ONEWIRE_Base;

end neorv32.ONEWIRE;
