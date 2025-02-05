pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Serial peripheral interface controller
package neorv32.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_SPI_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_CPHA_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_CPOL_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_SPI_CTRL_CDIV_Field is neorv32.UInt4;
   subtype CTRL_SPI_CTRL_HIGHSPEED_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_IRQ_RX_AVAIL_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_IRQ_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_IRQ_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_IRQ_IDLE_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_FIFO_Field is neorv32.UInt4;
   subtype CTRL_SPI_CS_ACTIVE_Field is neorv32.Bit;
   subtype CTRL_SPI_CTRL_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  SPI enable flag
      SPI_CTRL_EN           : CTRL_SPI_CTRL_EN_Field := 16#0#;
      --  Clock phase
      SPI_CTRL_CPHA         : CTRL_SPI_CTRL_CPHA_Field := 16#0#;
      --  Clock polarity
      SPI_CTRL_CPOL         : CTRL_SPI_CTRL_CPOL_Field := 16#0#;
      --  Clock prescaler select
      SPI_CTRL_PRSC         : CTRL_SPI_CTRL_PRSC_Field := 16#0#;
      --  SPI clock divider
      SPI_CTRL_CDIV         : CTRL_SPI_CTRL_CDIV_Field := 16#0#;
      --  SPI high-speed mode
      SPI_CTRL_HIGHSPEED    : CTRL_SPI_CTRL_HIGHSPEED_Field := 16#0#;
      --  unspecified
      Reserved_11_15        : neorv32.UInt5 := 16#0#;
      --  Read-only. RX FIFO data available (RX FIFO not empty)
      SPI_CTRL_RX_AVAIL     : CTRL_SPI_CTRL_RX_AVAIL_Field := 16#0#;
      --  Read-only. TX FIFO is empty
      SPI_CTRL_TX_EMPTY     : CTRL_SPI_CTRL_TX_EMPTY_Field := 16#0#;
      --  Read-only. TX FIFO not at least half full
      SPI_CTRL_TX_NHALF     : CTRL_SPI_CTRL_TX_NHALF_Field := 16#0#;
      --  Read-only. TX FIFO is full
      SPI_CTRL_TX_FULL      : CTRL_SPI_CTRL_TX_FULL_Field := 16#0#;
      --  Fire interrupt if RX FIFO is not empty
      SPI_CTRL_IRQ_RX_AVAIL : CTRL_SPI_CTRL_IRQ_RX_AVAIL_Field := 16#0#;
      --  Fire interrupt if TX FIFO is empty
      SPI_CTRL_IRQ_TX_EMPTY : CTRL_SPI_CTRL_IRQ_TX_EMPTY_Field := 16#0#;
      --  Fire interrupt if TX FIFO is not at least half full
      SPI_CTRL_IRQ_TX_NHALF : CTRL_SPI_CTRL_IRQ_TX_NHALF_Field := 16#0#;
      --  Fire interrupt if TX FIFO is empty and SPI bus engine is idle
      SPI_CTRL_IRQ_IDLE     : CTRL_SPI_CTRL_IRQ_IDLE_Field := 16#0#;
      --  Read-only. log2(FIFO size)
      SPI_CTRL_FIFO         : CTRL_SPI_CTRL_FIFO_Field := 16#0#;
      --  unspecified
      Reserved_28_29        : neorv32.UInt2 := 16#0#;
      --  Read-only. Set if any chip-select line is active
      SPI_CS_ACTIVE         : CTRL_SPI_CS_ACTIVE_Field := 16#0#;
      --  Read-only. SPI busy flag (transmission in progress and TX FIFO not
      --  empty yet)
      SPI_CTRL_BUSY         : CTRL_SPI_CTRL_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      SPI_CTRL_EN           at 0 range 0 .. 0;
      SPI_CTRL_CPHA         at 0 range 1 .. 1;
      SPI_CTRL_CPOL         at 0 range 2 .. 2;
      SPI_CTRL_PRSC         at 0 range 3 .. 5;
      SPI_CTRL_CDIV         at 0 range 6 .. 9;
      SPI_CTRL_HIGHSPEED    at 0 range 10 .. 10;
      Reserved_11_15        at 0 range 11 .. 15;
      SPI_CTRL_RX_AVAIL     at 0 range 16 .. 16;
      SPI_CTRL_TX_EMPTY     at 0 range 17 .. 17;
      SPI_CTRL_TX_NHALF     at 0 range 18 .. 18;
      SPI_CTRL_TX_FULL      at 0 range 19 .. 19;
      SPI_CTRL_IRQ_RX_AVAIL at 0 range 20 .. 20;
      SPI_CTRL_IRQ_TX_EMPTY at 0 range 21 .. 21;
      SPI_CTRL_IRQ_TX_NHALF at 0 range 22 .. 22;
      SPI_CTRL_IRQ_IDLE     at 0 range 23 .. 23;
      SPI_CTRL_FIFO         at 0 range 24 .. 27;
      Reserved_28_29        at 0 range 28 .. 29;
      SPI_CS_ACTIVE         at 0 range 30 .. 30;
      SPI_CTRL_BUSY         at 0 range 31 .. 31;
   end record;

   subtype DATA_SPI_DATA_Field is neorv32.Byte;
   subtype DATA_SPI_DATA_CMD_Field is neorv32.Bit;

   --  Data register
   type DATA_Register is record
      --  *** This field is modified following a read operation ***. RX/TX data
      --  (via FIFOs)
      SPI_DATA      : DATA_SPI_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_30 : neorv32.UInt23 := 16#0#;
      --  *** This field is modified following a read operation ***. SPI TX
      --  data (0) / command (1) select
      SPI_DATA_CMD  : DATA_SPI_DATA_CMD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATA_Register use record
      SPI_DATA      at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      SPI_DATA_CMD  at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial peripheral interface controller
   type SPI_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  Data register
      DATA : aliased DATA_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  Serial peripheral interface controller
   SPI_Periph : aliased SPI_Peripheral
     with Import, Address => SPI_Base;

end neorv32.SPI;
