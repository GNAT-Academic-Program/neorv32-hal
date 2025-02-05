pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  Pulse-width modulation controller
package neorv32.PWM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_PWM_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_PWM_CTRL_PRSCx_Field is neorv32.UInt3;

   --  Control register
   type CTRL_Register is record
      --  PWM controller enable flag
      PWM_CTRL_EN    : CTRL_PWM_CTRL_EN_Field := 16#0#;
      --  Clock prescaler select
      PWM_CTRL_PRSCx : CTRL_PWM_CTRL_PRSCx_Field := 16#0#;
      --  unspecified
      Reserved_4_31  : neorv32.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      PWM_CTRL_EN    at 0 range 0 .. 0;
      PWM_CTRL_PRSCx at 0 range 1 .. 3;
      Reserved_4_31  at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Pulse-width modulation controller
   type PWM_Peripheral is record
      --  Control register
      CTRL : aliased CTRL_Register;
      --  Duty cycle register 0
      DC0  : aliased neorv32.UInt32;
      --  Duty cycle register 1
      DC1  : aliased neorv32.UInt32;
      --  Duty cycle register 2
      DC2  : aliased neorv32.UInt32;
   end record
     with Volatile;

   for PWM_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DC0  at 16#4# range 0 .. 31;
      DC1  at 16#8# range 0 .. 31;
      DC2  at 16#C# range 0 .. 31;
   end record;

   --  Pulse-width modulation controller
   PWM_Periph : aliased PWM_Peripheral
     with Import, Address => PWM_Base;

end neorv32.PWM;
