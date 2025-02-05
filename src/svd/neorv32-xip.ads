pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Execute In Place Module
package neorv32.XIP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_XIP_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_XIP_CTRL_CPOL_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_CPHA_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_SPI_NBYTES_Field is neorv32.UInt4;
   subtype CTRL_XIP_CTRL_XIP_EN_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_XIP_ABYTES_Field is neorv32.UInt2;
   subtype CTRL_XIP_CTRL_RD_CMD_Field is neorv32.Byte;
   subtype CTRL_XIP_CTRL_SPI_CSEN_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_HIGHSPEED_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_CDIV_Field is neorv32.UInt4;
   subtype CTRL_XIP_CTRL_PHY_BUSY_Field is neorv32.Bit;
   subtype CTRL_XIP_CTRL_XIP_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  XIP module enable flag
      XIP_CTRL_EN         : CTRL_XIP_CTRL_EN_Field := 16#0#;
      --  SPI clock prescaler select
      XIP_CTRL_PRSC       : CTRL_XIP_CTRL_PRSC_Field := 16#0#;
      --  SPI clock (idle) polarity
      XIP_CTRL_CPOL       : CTRL_XIP_CTRL_CPOL_Field := 16#0#;
      --  SPI clock phase
      XIP_CTRL_CPHA       : CTRL_XIP_CTRL_CPHA_Field := 16#0#;
      --  Number of bytes in SPI transmission
      XIP_CTRL_SPI_NBYTES : CTRL_XIP_CTRL_SPI_NBYTES_Field := 16#0#;
      --  XIP mode enable
      XIP_CTRL_XIP_EN     : CTRL_XIP_CTRL_XIP_EN_Field := 16#0#;
      --  Number of XIP address bytes (minus 1)
      XIP_CTRL_XIP_ABYTES : CTRL_XIP_CTRL_XIP_ABYTES_Field := 16#0#;
      --  SPI flash read command
      XIP_CTRL_RD_CMD     : CTRL_XIP_CTRL_RD_CMD_Field := 16#0#;
      --  SPI chip-select enable
      XIP_CTRL_SPI_CSEN   : CTRL_XIP_CTRL_SPI_CSEN_Field := 16#0#;
      --  SPI high-speed mode enable (ignoring XIP_CTRL_PRSC)
      XIP_CTRL_HIGHSPEED  : CTRL_XIP_CTRL_HIGHSPEED_Field := 16#0#;
      --  SPI clock divider
      XIP_CTRL_CDIV       : CTRL_XIP_CTRL_CDIV_Field := 16#0#;
      --  unspecified
      Reserved_27_29      : neorv32.UInt3 := 16#0#;
      --  Read-only. SPI PHY busy
      XIP_CTRL_PHY_BUSY   : CTRL_XIP_CTRL_PHY_BUSY_Field := 16#0#;
      --  Read-only. XIP access in progress
      XIP_CTRL_XIP_BUSY   : CTRL_XIP_CTRL_XIP_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      XIP_CTRL_EN         at 0 range 0 .. 0;
      XIP_CTRL_PRSC       at 0 range 1 .. 3;
      XIP_CTRL_CPOL       at 0 range 4 .. 4;
      XIP_CTRL_CPHA       at 0 range 5 .. 5;
      XIP_CTRL_SPI_NBYTES at 0 range 6 .. 9;
      XIP_CTRL_XIP_EN     at 0 range 10 .. 10;
      XIP_CTRL_XIP_ABYTES at 0 range 11 .. 12;
      XIP_CTRL_RD_CMD     at 0 range 13 .. 20;
      XIP_CTRL_SPI_CSEN   at 0 range 21 .. 21;
      XIP_CTRL_HIGHSPEED  at 0 range 22 .. 22;
      XIP_CTRL_CDIV       at 0 range 23 .. 26;
      Reserved_27_29      at 0 range 27 .. 29;
      XIP_CTRL_PHY_BUSY   at 0 range 30 .. 30;
      XIP_CTRL_XIP_BUSY   at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Execute In Place Module
   type XIP_Peripheral is record
      --  Control register
      CTRL    : aliased CTRL_Register;
      --  Direct SPI access - data register low
      DATA_LO : aliased neorv32.UInt32;
      --  Direct SPI access - data register high
      DATA_HI : aliased neorv32.UInt32;
   end record
     with Volatile;

   for XIP_Peripheral use record
      CTRL    at 16#0# range 0 .. 31;
      DATA_LO at 16#8# range 0 .. 31;
      DATA_HI at 16#C# range 0 .. 31;
   end record;

   --  Execute In Place Module
   XIP_Periph : aliased XIP_Peripheral
     with Import, Address => XIP_Base;

end neorv32.XIP;
