pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

--  Cyclic redundancy check unit
package neorv32.CRC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- Peripherals --
   -----------------

   --  Cyclic redundancy check unit
   type CRC_Peripheral is record
      --  CRC mode control (CRC8, CRC16, CRC32)
      MODE : aliased neorv32.UInt32;
      --  CRC polynomial
      POLY : aliased neorv32.UInt32;
      --  LSB-aligned data input (bytes)
      DATA : aliased neorv32.UInt32;
      --  CRC shift register
      SREG : aliased neorv32.UInt32;
   end record
     with Volatile;

   for CRC_Peripheral use record
      MODE at 16#0# range 0 .. 31;
      POLY at 16#4# range 0 .. 31;
      DATA at 16#8# range 0 .. 31;
      SREG at 16#C# range 0 .. 31;
   end record;

   --  Cyclic redundancy check unit
   CRC_Periph : aliased CRC_Peripheral
     with Import, Address => CRC_Base;

end neorv32.CRC;
