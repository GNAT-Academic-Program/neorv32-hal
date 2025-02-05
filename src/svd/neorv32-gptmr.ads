pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  General purpose timer
package neorv32.GPTMR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_GPTMR_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_GPTMR_CTRL_PRSC_Field is neorv32.UInt3;
   subtype CTRL_GPTMR_CTRL_MODE_Field is neorv32.Bit;
   subtype CTRL_GPTMR_CTRL_IRQ_CLR_Field is neorv32.Bit;
   subtype CTRL_GPTMR_CTRL_IRQ_PND_Field is neorv32.Bit;

   --  Control register
   type CTRL_Register is record
      --  Timer enable flag
      GPTMR_CTRL_EN      : CTRL_GPTMR_CTRL_EN_Field := 16#0#;
      --  Clock prescaler select
      GPTMR_CTRL_PRSC    : CTRL_GPTMR_CTRL_PRSC_Field := 16#0#;
      --  Operation mode (0=single-shot, 1=continuous)
      GPTMR_CTRL_MODE    : CTRL_GPTMR_CTRL_MODE_Field := 16#0#;
      --  unspecified
      Reserved_5_29      : neorv32.UInt25 := 16#0#;
      --  Set to clear timer-match interrupt
      GPTMR_CTRL_IRQ_CLR : CTRL_GPTMR_CTRL_IRQ_CLR_Field := 16#0#;
      --  Read-only. Timer-match interrupt pending
      GPTMR_CTRL_IRQ_PND : CTRL_GPTMR_CTRL_IRQ_PND_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      GPTMR_CTRL_EN      at 0 range 0 .. 0;
      GPTMR_CTRL_PRSC    at 0 range 1 .. 3;
      GPTMR_CTRL_MODE    at 0 range 4 .. 4;
      Reserved_5_29      at 0 range 5 .. 29;
      GPTMR_CTRL_IRQ_CLR at 0 range 30 .. 30;
      GPTMR_CTRL_IRQ_PND at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose timer
   type GPTMR_Peripheral is record
      --  Control register
      CTRL  : aliased CTRL_Register;
      --  Threshold register
      THRES : aliased neorv32.UInt32;
      --  Counter register
      COUNT : aliased neorv32.UInt32;
   end record
     with Volatile;

   for GPTMR_Peripheral use record
      CTRL  at 16#0# range 0 .. 31;
      THRES at 16#4# range 0 .. 31;
      COUNT at 16#8# range 0 .. 31;
   end record;

   --  General purpose timer
   GPTMR_Periph : aliased GPTMR_Peripheral
     with Import, Address => GPTMR_Base;

end neorv32.GPTMR;
