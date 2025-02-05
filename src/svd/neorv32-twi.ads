pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Two-wire interface controller
package neorv32.TWI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_TWI_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_TWI_CTRL_CDIV_Field is neorv32.UInt4;
   subtype CTRL_TWI_CTRL_CLKSTR_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_FIFO_Field is neorv32.UInt4;
   subtype CTRL_TWI_CTRL_SENSE_SCL_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_SENSE_SDA_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_TWI_CTRL_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  TWI enable flag
      TWI_CTRL_EN        : CTRL_TWI_CTRL_EN_Field := 16#0#;
      --  Clock prescaler select
      TWI_CTRL_PRSC      : CTRL_TWI_CTRL_PRSC_Field := 16#0#;
      --  TWI clock divider
      TWI_CTRL_CDIV      : CTRL_TWI_CTRL_CDIV_Field := 16#0#;
      --  Enable (allow) clock stretching
      TWI_CTRL_CLKSTR    : CTRL_TWI_CTRL_CLKSTR_Field := 16#0#;
      --  unspecified
      Reserved_9_14      : neorv32.UInt6 := 16#0#;
      --  Read-only. log2(TWI FIFO size)
      TWI_CTRL_FIFO      : CTRL_TWI_CTRL_FIFO_Field := 16#0#;
      --  unspecified
      Reserved_19_26     : neorv32.Byte := 16#0#;
      --  Read-only. current state of the SCL bus line
      TWI_CTRL_SENSE_SCL : CTRL_TWI_CTRL_SENSE_SCL_Field := 16#0#;
      --  Read-only. current state of the SDA bus line
      TWI_CTRL_SENSE_SDA : CTRL_TWI_CTRL_SENSE_SDA_Field := 16#0#;
      --  Read-only. TX FIFO full
      TWI_CTRL_TX_FULL   : CTRL_TWI_CTRL_TX_FULL_Field := 16#0#;
      --  Read-only. RX FIFO not empty (data available)
      TWI_CTRL_RX_AVAIL  : CTRL_TWI_CTRL_RX_AVAIL_Field := 16#0#;
      --  Read-only. Bus engine busy of TX FIFO not empty
      TWI_CTRL_BUSY      : CTRL_TWI_CTRL_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      TWI_CTRL_EN        at 0 range 0 .. 0;
      TWI_CTRL_PRSC      at 0 range 1 .. 3;
      TWI_CTRL_CDIV      at 0 range 4 .. 7;
      TWI_CTRL_CLKSTR    at 0 range 8 .. 8;
      Reserved_9_14      at 0 range 9 .. 14;
      TWI_CTRL_FIFO      at 0 range 15 .. 18;
      Reserved_19_26     at 0 range 19 .. 26;
      TWI_CTRL_SENSE_SCL at 0 range 27 .. 27;
      TWI_CTRL_SENSE_SDA at 0 range 28 .. 28;
      TWI_CTRL_TX_FULL   at 0 range 29 .. 29;
      TWI_CTRL_RX_AVAIL  at 0 range 30 .. 30;
      TWI_CTRL_BUSY      at 0 range 31 .. 31;
   end record;

   subtype DCMD_TWI_DCMD_Field is neorv32.Byte;
   subtype DCMD_TWI_DCMD_ACK_Field is neorv32.Bit;
   subtype DCMD_TWI_DCMD_CMD_Field is neorv32.UInt2;

   --  RX/TX data/command register
   type DCMD_Register is record
      --  *** This field is modified following a read operation ***. RX/TX data
      TWI_DCMD       : DCMD_TWI_DCMD_Field := 16#0#;
      --  *** This field is modified following a read operation ***. RX =
      --  ACK/NACK, TX = MACK
      TWI_DCMD_ACK   : DCMD_TWI_DCMD_ACK_Field := 16#0#;
      --  *** This field is modified following a read operation ***. Operation
      --  command
      TWI_DCMD_CMD   : DCMD_TWI_DCMD_CMD_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : neorv32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCMD_Register use record
      TWI_DCMD       at 0 range 0 .. 7;
      TWI_DCMD_ACK   at 0 range 8 .. 8;
      TWI_DCMD_CMD   at 0 range 9 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Two-wire interface controller
   type TWI_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  RX/TX data/command register
      DCMD : aliased DCMD_Register;
   end record
     with Volatile;

   for TWI_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DCMD at 16#4# range 0 .. 31;
   end record;

   --  Two-wire interface controller
   TWI_Periph : aliased TWI_Peripheral
     with Import, Address => TWI_Base;

end neorv32.TWI;
