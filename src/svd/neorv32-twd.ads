pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Two-wire device controller
package neorv32.TWD is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_TWD_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_CLR_RX_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_CLR_TX_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_FSEL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_DEV_ADDR_Field is neorv32.UInt7;
   subtype CTRL_TWD_CTRL_IRQ_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_IRQ_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_IRQ_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_FIFO_Field is neorv32.UInt4;
   subtype CTRL_TWD_CTRL_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_SENSE_SCL_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_SENSE_SDA_Field is neorv32.Bit;
   subtype CTRL_TWD_CTRL_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  TWD enable flag
      TWD_CTRL_EN           : CTRL_TWD_CTRL_EN_Field := 16#0#;
      --  Clear RX FIFO, flag auto-clears
      TWD_CTRL_CLR_RX       : CTRL_TWD_CTRL_CLR_RX_Field := 16#0#;
      --  Clear TX FIFO, flag auto-clears
      TWD_CTRL_CLR_TX       : CTRL_TWD_CTRL_CLR_TX_Field := 16#0#;
      --  Bus sample clock / filter select
      TWD_CTRL_FSEL         : CTRL_TWD_CTRL_FSEL_Field := 16#0#;
      --  Device address (7-bit)
      TWD_CTRL_DEV_ADDR     : CTRL_TWD_CTRL_DEV_ADDR_Field := 16#0#;
      --  IRQ if RX FIFO data available
      TWD_CTRL_IRQ_RX_AVAIL : CTRL_TWD_CTRL_IRQ_RX_AVAIL_Field := 16#0#;
      --  IRQ if RX FIFO full
      TWD_CTRL_IRQ_RX_FULL  : CTRL_TWD_CTRL_IRQ_RX_FULL_Field := 16#0#;
      --  IRQ if TX FIFO empty
      TWD_CTRL_IRQ_TX_EMPTY : CTRL_TWD_CTRL_IRQ_TX_EMPTY_Field := 16#0#;
      --  unspecified
      Reserved_14_14        : neorv32.Bit := 16#0#;
      --  Read-only. log2(TWD FIFO size)
      TWD_CTRL_FIFO         : CTRL_TWD_CTRL_FIFO_Field := 16#0#;
      --  unspecified
      Reserved_19_24        : neorv32.UInt6 := 16#0#;
      --  Read-only. RX FIFO data available
      TWD_CTRL_RX_AVAIL     : CTRL_TWD_CTRL_RX_AVAIL_Field := 16#0#;
      --  Read-only. RX FIFO full
      TWD_CTRL_RX_FULL      : CTRL_TWD_CTRL_RX_FULL_Field := 16#0#;
      --  Read-only. TX FIFO empty
      TWD_CTRL_TX_EMPTY     : CTRL_TWD_CTRL_TX_EMPTY_Field := 16#0#;
      --  Read-only. TX FIFO full
      TWD_CTRL_TX_FULL      : CTRL_TWD_CTRL_TX_FULL_Field := 16#0#;
      --  Read-only. current state of the SCL bus line
      TWD_CTRL_SENSE_SCL    : CTRL_TWD_CTRL_SENSE_SCL_Field := 16#0#;
      --  Read-only. current state of the SDA bus line
      TWD_CTRL_SENSE_SDA    : CTRL_TWD_CTRL_SENSE_SDA_Field := 16#0#;
      --  Read-only. bus engine is busy (transaction in progress)
      TWD_CTRL_BUSY         : CTRL_TWD_CTRL_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      TWD_CTRL_EN           at 0 range 0 .. 0;
      TWD_CTRL_CLR_RX       at 0 range 1 .. 1;
      TWD_CTRL_CLR_TX       at 0 range 2 .. 2;
      TWD_CTRL_FSEL         at 0 range 3 .. 3;
      TWD_CTRL_DEV_ADDR     at 0 range 4 .. 10;
      TWD_CTRL_IRQ_RX_AVAIL at 0 range 11 .. 11;
      TWD_CTRL_IRQ_RX_FULL  at 0 range 12 .. 12;
      TWD_CTRL_IRQ_TX_EMPTY at 0 range 13 .. 13;
      Reserved_14_14        at 0 range 14 .. 14;
      TWD_CTRL_FIFO         at 0 range 15 .. 18;
      Reserved_19_24        at 0 range 19 .. 24;
      TWD_CTRL_RX_AVAIL     at 0 range 25 .. 25;
      TWD_CTRL_RX_FULL      at 0 range 26 .. 26;
      TWD_CTRL_TX_EMPTY     at 0 range 27 .. 27;
      TWD_CTRL_TX_FULL      at 0 range 28 .. 28;
      TWD_CTRL_SENSE_SCL    at 0 range 29 .. 29;
      TWD_CTRL_SENSE_SDA    at 0 range 30 .. 30;
      TWD_CTRL_BUSY         at 0 range 31 .. 31;
   end record;

   subtype DATA_TWI_DATA_Field is neorv32.Byte;

   --  RX/TX data register
   type DATA_Register is record
      --  *** This field is modified following a read operation ***. RX/TX data
      --  FIFO access
      TWI_DATA      : DATA_TWI_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : neorv32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATA_Register use record
      TWI_DATA      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Two-wire device controller
   type TWD_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  RX/TX data register
      DATA : aliased DATA_Register;
   end record
     with Volatile;

   for TWD_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  Two-wire device controller
   TWD_Periph : aliased TWD_Peripheral
     with Import, Address => TWD_Base;

end neorv32.TWD;
