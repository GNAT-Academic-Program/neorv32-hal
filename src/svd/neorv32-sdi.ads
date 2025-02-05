pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Serial data interface controller
package neorv32.SDI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_SDI_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_FIFO_Field is neorv32.UInt4;
   subtype CTRL_SDI_CTRL_IRQ_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_IRQ_RX_HALF_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_IRQ_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_IRQ_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_IRQ_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_RX_HALF_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_SDI_CTRL_CS_ACTIVE_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  SDI enable flag
      SDI_CTRL_EN           : CTRL_SDI_CTRL_EN_Field := 16#0#;
      --  unspecified
      Reserved_1_3          : neorv32.UInt3 := 16#0#;
      --  Log2 of SDI FIFO size
      SDI_CTRL_FIFO         : CTRL_SDI_CTRL_FIFO_Field := 16#0#;
      --  unspecified
      Reserved_8_14         : neorv32.UInt7 := 16#0#;
      --  Fire interrupt if RX FIFO is not empty
      SDI_CTRL_IRQ_RX_AVAIL : CTRL_SDI_CTRL_IRQ_RX_AVAIL_Field := 16#0#;
      --  Fire interrupt if RX FIFO is at least half full
      SDI_CTRL_IRQ_RX_HALF  : CTRL_SDI_CTRL_IRQ_RX_HALF_Field := 16#0#;
      --  Fire interrupt if RX FIFO is full
      SDI_CTRL_IRQ_RX_FULL  : CTRL_SDI_CTRL_IRQ_RX_FULL_Field := 16#0#;
      --  Fire interrupt if TX FIFO is empty
      SDI_CTRL_IRQ_TX_EMPTY : CTRL_SDI_CTRL_IRQ_TX_EMPTY_Field := 16#0#;
      --  Fire interrupt if TX FIFO is not at least half full
      SDI_CTRL_IRQ_TX_NHALF : CTRL_SDI_CTRL_IRQ_TX_NHALF_Field := 16#0#;
      --  unspecified
      Reserved_20_22        : neorv32.UInt3 := 16#0#;
      --  Read-only. RX FIFO not empty (data available)
      SDI_CTRL_RX_AVAIL     : CTRL_SDI_CTRL_RX_AVAIL_Field := 16#0#;
      --  Read-only. RX FIFO at least half full
      SDI_CTRL_RX_HALF      : CTRL_SDI_CTRL_RX_HALF_Field := 16#0#;
      --  Read-only. RX FIFO full
      SDI_CTRL_RX_FULL      : CTRL_SDI_CTRL_RX_FULL_Field := 16#0#;
      --  Read-only. TX FIFO empty
      SDI_CTRL_TX_EMPTY     : CTRL_SDI_CTRL_TX_EMPTY_Field := 16#0#;
      --  Read-only. TX FIFO not at least half full
      SDI_CTRL_TX_NHALF     : CTRL_SDI_CTRL_TX_NHALF_Field := 16#0#;
      --  Read-only. TX FIFO full
      SDI_CTRL_TX_FULL      : CTRL_SDI_CTRL_TX_FULL_Field := 16#0#;
      --  unspecified
      Reserved_29_30        : neorv32.UInt2 := 16#0#;
      --  Read-only. Chip-select line is active when set
      SDI_CTRL_CS_ACTIVE    : CTRL_SDI_CTRL_CS_ACTIVE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      SDI_CTRL_EN           at 0 range 0 .. 0;
      Reserved_1_3          at 0 range 1 .. 3;
      SDI_CTRL_FIFO         at 0 range 4 .. 7;
      Reserved_8_14         at 0 range 8 .. 14;
      SDI_CTRL_IRQ_RX_AVAIL at 0 range 15 .. 15;
      SDI_CTRL_IRQ_RX_HALF  at 0 range 16 .. 16;
      SDI_CTRL_IRQ_RX_FULL  at 0 range 17 .. 17;
      SDI_CTRL_IRQ_TX_EMPTY at 0 range 18 .. 18;
      SDI_CTRL_IRQ_TX_NHALF at 0 range 19 .. 19;
      Reserved_20_22        at 0 range 20 .. 22;
      SDI_CTRL_RX_AVAIL     at 0 range 23 .. 23;
      SDI_CTRL_RX_HALF      at 0 range 24 .. 24;
      SDI_CTRL_RX_FULL      at 0 range 25 .. 25;
      SDI_CTRL_TX_EMPTY     at 0 range 26 .. 26;
      SDI_CTRL_TX_NHALF     at 0 range 27 .. 27;
      SDI_CTRL_TX_FULL      at 0 range 28 .. 28;
      Reserved_29_30        at 0 range 29 .. 30;
      SDI_CTRL_CS_ACTIVE    at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial data interface controller
   type SDI_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  RX/TX data register (lowest 8 bit)
      DATA : aliased neorv32.UInt32;
   end record
     with Volatile;

   for SDI_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  Serial data interface controller
   SDI_Periph : aliased SDI_Peripheral
     with Import, Address => SDI_Base;

end neorv32.SDI;
