pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

--  General purpose input/output port
package neorv32.GPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose input/output port
   type GPIO_Peripheral is record
      --  Parallel input register - low
      INPUT0  : aliased neorv32.UInt32;
      --  Parallel input register - high
      INPUT1  : aliased neorv32.UInt32;
      --  Parallel output register - low
      OUTPUT0 : aliased neorv32.UInt32;
      --  Parallel output register - high
      OUTPUT1 : aliased neorv32.UInt32;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      INPUT0  at 16#0# range 0 .. 31;
      INPUT1  at 16#4# range 0 .. 31;
      OUTPUT0 at 16#8# range 0 .. 31;
      OUTPUT1 at 16#C# range 0 .. 31;
   end record;

   --  General purpose input/output port
   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIO_Base;

end neorv32.GPIO;
