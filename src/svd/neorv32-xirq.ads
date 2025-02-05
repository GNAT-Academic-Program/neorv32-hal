pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

--  External interrupts controller
package neorv32.XIRQ is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupts controller
   type XIRQ_Peripheral is record
      --  External IRQ channel enable register
      EIE  : aliased neorv32.UInt32;
      --  External IRQ source register
      ESC  : aliased neorv32.UInt32;
      --  External IRQ trigger type (level/edge)
      TTYP : aliased neorv32.UInt32;
      --  External IRQ trigger polarity (high/low, rising/falling)
      TPOL : aliased neorv32.UInt32;
   end record
     with Volatile;

   for XIRQ_Peripheral use record
      EIE  at 16#0# range 0 .. 31;
      ESC  at 16#4# range 0 .. 31;
      TTYP at 16#8# range 0 .. 31;
      TPOL at 16#C# range 0 .. 31;
   end record;

   --  External interrupts controller
   XIRQ_Periph : aliased XIRQ_Peripheral
     with Import, Address => XIRQ_Base;

end neorv32.XIRQ;
