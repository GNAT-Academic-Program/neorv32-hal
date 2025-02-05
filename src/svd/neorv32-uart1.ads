pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Secondary universal asynchronous receiver and transmitter
package neorv32.UART1 is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_UART_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_SIM_MODE_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_HWFC_EN_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_UART_CTRL_BAUD_Field is neorv32.UInt10;
   subtype CTRL_UART_CTRL_RX_NEMPTY_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_RX_HALF_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_TX_FULL_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_IRQ_RX_NEMPTY_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_IRQ_RX_HALF_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_IRQ_RX_FULL_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_IRQ_TX_EMPTY_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_IRQ_TX_NHALF_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_RX_CLR_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_TX_CLR_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_RX_OVER_Field is neorv32.Bit;
   subtype CTRL_UART_CTRL_TX_BUSY_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  UART enable flag
      UART_CTRL_EN            : CTRL_UART_CTRL_EN_Field := 16#0#;
      --  Simulation output override enable, for use in simulation only
      UART_CTRL_SIM_MODE      : CTRL_UART_CTRL_SIM_MODE_Field := 16#0#;
      --  Enable RTS/CTS hardware flow-control
      UART_CTRL_HWFC_EN       : CTRL_UART_CTRL_HWFC_EN_Field := 16#0#;
      --  CLock prescaler select
      UART_CTRL_PRSC          : CTRL_UART_CTRL_PRSC_Field := 16#0#;
      --  BAUD rate divisor
      UART_CTRL_BAUD          : CTRL_UART_CTRL_BAUD_Field := 16#0#;
      --  Read-only. RX FIFO not empty
      UART_CTRL_RX_NEMPTY     : CTRL_UART_CTRL_RX_NEMPTY_Field := 16#0#;
      --  Read-only. RX FIFO at least half full
      UART_CTRL_RX_HALF       : CTRL_UART_CTRL_RX_HALF_Field := 16#0#;
      --  Read-only. RX FIFO full
      UART_CTRL_RX_FULL       : CTRL_UART_CTRL_RX_FULL_Field := 16#0#;
      --  Read-only. TX FIFO empty
      UART_CTRL_TX_EMPTY      : CTRL_UART_CTRL_TX_EMPTY_Field := 16#0#;
      --  Read-only. TX FIFO not at least half full
      UART_CTRL_TX_NHALF      : CTRL_UART_CTRL_TX_NHALF_Field := 16#0#;
      --  Read-only. TX FIFO full
      UART_CTRL_TX_FULL       : CTRL_UART_CTRL_TX_FULL_Field := 16#0#;
      --  Fire IRQ if RX FIFO not empty
      UART_CTRL_IRQ_RX_NEMPTY : CTRL_UART_CTRL_IRQ_RX_NEMPTY_Field := 16#0#;
      --  Fire IRQ if RX FIFO at least half-full
      UART_CTRL_IRQ_RX_HALF   : CTRL_UART_CTRL_IRQ_RX_HALF_Field := 16#0#;
      --  Fire IRQ if RX FIFO full
      UART_CTRL_IRQ_RX_FULL   : CTRL_UART_CTRL_IRQ_RX_FULL_Field := 16#0#;
      --  Fire IRQ if TX FIFO empty
      UART_CTRL_IRQ_TX_EMPTY  : CTRL_UART_CTRL_IRQ_TX_EMPTY_Field := 16#0#;
      --  Fire IRQ if TX FIFO not at least half-full
      UART_CTRL_IRQ_TX_NHALF  : CTRL_UART_CTRL_IRQ_TX_NHALF_Field := 16#0#;
      --  unspecified
      Reserved_27_27          : neorv32.Bit := 16#0#;
      --  Clear RX FIFO, flag auto-clears
      UART_CTRL_RX_CLR        : CTRL_UART_CTRL_RX_CLR_Field := 16#0#;
      --  Clear TX FIFO, flag auto-clears
      UART_CTRL_TX_CLR        : CTRL_UART_CTRL_TX_CLR_Field := 16#0#;
      --  Read-only. RX FIFO overflow
      UART_CTRL_RX_OVER       : CTRL_UART_CTRL_RX_OVER_Field := 16#0#;
      --  Read-only. Transmitter busy or TX FIFO not empty
      UART_CTRL_TX_BUSY       : CTRL_UART_CTRL_TX_BUSY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      UART_CTRL_EN            at 0 range 0 .. 0;
      UART_CTRL_SIM_MODE      at 0 range 1 .. 1;
      UART_CTRL_HWFC_EN       at 0 range 2 .. 2;
      UART_CTRL_PRSC          at 0 range 3 .. 5;
      UART_CTRL_BAUD          at 0 range 6 .. 15;
      UART_CTRL_RX_NEMPTY     at 0 range 16 .. 16;
      UART_CTRL_RX_HALF       at 0 range 17 .. 17;
      UART_CTRL_RX_FULL       at 0 range 18 .. 18;
      UART_CTRL_TX_EMPTY      at 0 range 19 .. 19;
      UART_CTRL_TX_NHALF      at 0 range 20 .. 20;
      UART_CTRL_TX_FULL       at 0 range 21 .. 21;
      UART_CTRL_IRQ_RX_NEMPTY at 0 range 22 .. 22;
      UART_CTRL_IRQ_RX_HALF   at 0 range 23 .. 23;
      UART_CTRL_IRQ_RX_FULL   at 0 range 24 .. 24;
      UART_CTRL_IRQ_TX_EMPTY  at 0 range 25 .. 25;
      UART_CTRL_IRQ_TX_NHALF  at 0 range 26 .. 26;
      Reserved_27_27          at 0 range 27 .. 27;
      UART_CTRL_RX_CLR        at 0 range 28 .. 28;
      UART_CTRL_TX_CLR        at 0 range 29 .. 29;
      UART_CTRL_RX_OVER       at 0 range 30 .. 30;
      UART_CTRL_TX_BUSY       at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Secondary universal asynchronous receiver and transmitter
   type UART1_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  RTX data register
      DATA : aliased neorv32.UInt32;
   end record
     with Volatile;

   for UART1_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  Secondary universal asynchronous receiver and transmitter
   UART1_Periph : aliased UART1_Peripheral
     with Import, Address => UART1_Base;

end neorv32.UART1;
