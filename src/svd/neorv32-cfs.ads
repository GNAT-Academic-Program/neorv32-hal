pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

--  Custom functions subsystem
package neorv32.CFS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- Peripherals --
   -----------------

   --  Custom functions subsystem
   type CFS_Peripheral is record
      --  User-defined
      REG0  : aliased neorv32.UInt32;
      --  User-defined
      REG1  : aliased neorv32.UInt32;
      --  User-defined
      REG2  : aliased neorv32.UInt32;
      --  User-defined
      REG3  : aliased neorv32.UInt32;
      --  User-defined
      REG4  : aliased neorv32.UInt32;
      --  User-defined
      REG5  : aliased neorv32.UInt32;
      --  User-defined
      REG6  : aliased neorv32.UInt32;
      --  User-defined
      REG7  : aliased neorv32.UInt32;
      --  User-defined
      REG8  : aliased neorv32.UInt32;
      --  User-defined
      REG9  : aliased neorv32.UInt32;
      --  User-defined
      REG10 : aliased neorv32.UInt32;
      --  User-defined
      REG11 : aliased neorv32.UInt32;
      --  User-defined
      REG12 : aliased neorv32.UInt32;
      --  User-defined
      REG13 : aliased neorv32.UInt32;
      --  User-defined
      REG14 : aliased neorv32.UInt32;
      --  User-defined
      REG15 : aliased neorv32.UInt32;
      --  User-defined
      REG16 : aliased neorv32.UInt32;
      --  User-defined
      REG17 : aliased neorv32.UInt32;
      --  User-defined
      REG18 : aliased neorv32.UInt32;
      --  User-defined
      REG19 : aliased neorv32.UInt32;
      --  User-defined
      REG20 : aliased neorv32.UInt32;
      --  User-defined
      REG21 : aliased neorv32.UInt32;
      --  User-defined
      REG22 : aliased neorv32.UInt32;
      --  User-defined
      REG23 : aliased neorv32.UInt32;
      --  User-defined
      REG24 : aliased neorv32.UInt32;
      --  User-defined
      REG25 : aliased neorv32.UInt32;
      --  User-defined
      REG26 : aliased neorv32.UInt32;
      --  User-defined
      REG27 : aliased neorv32.UInt32;
      --  User-defined
      REG28 : aliased neorv32.UInt32;
      --  User-defined
      REG29 : aliased neorv32.UInt32;
      --  User-defined
      REG30 : aliased neorv32.UInt32;
      --  User-defined
      REG31 : aliased neorv32.UInt32;
      --  User-defined
      REG32 : aliased neorv32.UInt32;
      --  User-defined
      REG33 : aliased neorv32.UInt32;
      --  User-defined
      REG34 : aliased neorv32.UInt32;
      --  User-defined
      REG35 : aliased neorv32.UInt32;
      --  User-defined
      REG36 : aliased neorv32.UInt32;
      --  User-defined
      REG37 : aliased neorv32.UInt32;
      --  User-defined
      REG38 : aliased neorv32.UInt32;
      --  User-defined
      REG39 : aliased neorv32.UInt32;
      --  User-defined
      REG40 : aliased neorv32.UInt32;
      --  User-defined
      REG41 : aliased neorv32.UInt32;
      --  User-defined
      REG42 : aliased neorv32.UInt32;
      --  User-defined
      REG43 : aliased neorv32.UInt32;
      --  User-defined
      REG44 : aliased neorv32.UInt32;
      --  User-defined
      REG45 : aliased neorv32.UInt32;
      --  User-defined
      REG46 : aliased neorv32.UInt32;
      --  User-defined
      REG47 : aliased neorv32.UInt32;
      --  User-defined
      REG48 : aliased neorv32.UInt32;
      --  User-defined
      REG49 : aliased neorv32.UInt32;
      --  User-defined
      REG50 : aliased neorv32.UInt32;
      --  User-defined
      REG51 : aliased neorv32.UInt32;
      --  User-defined
      REG52 : aliased neorv32.UInt32;
      --  User-defined
      REG53 : aliased neorv32.UInt32;
      --  User-defined
      REG54 : aliased neorv32.UInt32;
      --  User-defined
      REG55 : aliased neorv32.UInt32;
      --  User-defined
      REG56 : aliased neorv32.UInt32;
      --  User-defined
      REG57 : aliased neorv32.UInt32;
      --  User-defined
      REG58 : aliased neorv32.UInt32;
      --  User-defined
      REG59 : aliased neorv32.UInt32;
      --  User-defined
      REG60 : aliased neorv32.UInt32;
      --  User-defined
      REG61 : aliased neorv32.UInt32;
      --  User-defined
      REG62 : aliased neorv32.UInt32;
      --  User-defined
      REG63 : aliased neorv32.UInt32;
   end record
     with Volatile;

   for CFS_Peripheral use record
      REG0  at 16#0# range 0 .. 31;
      REG1  at 16#4# range 0 .. 31;
      REG2  at 16#8# range 0 .. 31;
      REG3  at 16#C# range 0 .. 31;
      REG4  at 16#10# range 0 .. 31;
      REG5  at 16#14# range 0 .. 31;
      REG6  at 16#18# range 0 .. 31;
      REG7  at 16#1C# range 0 .. 31;
      REG8  at 16#20# range 0 .. 31;
      REG9  at 16#24# range 0 .. 31;
      REG10 at 16#28# range 0 .. 31;
      REG11 at 16#2C# range 0 .. 31;
      REG12 at 16#30# range 0 .. 31;
      REG13 at 16#34# range 0 .. 31;
      REG14 at 16#38# range 0 .. 31;
      REG15 at 16#3C# range 0 .. 31;
      REG16 at 16#40# range 0 .. 31;
      REG17 at 16#44# range 0 .. 31;
      REG18 at 16#48# range 0 .. 31;
      REG19 at 16#4C# range 0 .. 31;
      REG20 at 16#50# range 0 .. 31;
      REG21 at 16#54# range 0 .. 31;
      REG22 at 16#58# range 0 .. 31;
      REG23 at 16#5C# range 0 .. 31;
      REG24 at 16#60# range 0 .. 31;
      REG25 at 16#64# range 0 .. 31;
      REG26 at 16#68# range 0 .. 31;
      REG27 at 16#6C# range 0 .. 31;
      REG28 at 16#70# range 0 .. 31;
      REG29 at 16#74# range 0 .. 31;
      REG30 at 16#78# range 0 .. 31;
      REG31 at 16#7C# range 0 .. 31;
      REG32 at 16#80# range 0 .. 31;
      REG33 at 16#84# range 0 .. 31;
      REG34 at 16#88# range 0 .. 31;
      REG35 at 16#8C# range 0 .. 31;
      REG36 at 16#90# range 0 .. 31;
      REG37 at 16#94# range 0 .. 31;
      REG38 at 16#98# range 0 .. 31;
      REG39 at 16#9C# range 0 .. 31;
      REG40 at 16#A0# range 0 .. 31;
      REG41 at 16#A4# range 0 .. 31;
      REG42 at 16#A8# range 0 .. 31;
      REG43 at 16#AC# range 0 .. 31;
      REG44 at 16#B0# range 0 .. 31;
      REG45 at 16#B4# range 0 .. 31;
      REG46 at 16#B8# range 0 .. 31;
      REG47 at 16#BC# range 0 .. 31;
      REG48 at 16#C0# range 0 .. 31;
      REG49 at 16#C4# range 0 .. 31;
      REG50 at 16#C8# range 0 .. 31;
      REG51 at 16#CC# range 0 .. 31;
      REG52 at 16#D0# range 0 .. 31;
      REG53 at 16#D4# range 0 .. 31;
      REG54 at 16#D8# range 0 .. 31;
      REG55 at 16#DC# range 0 .. 31;
      REG56 at 16#E0# range 0 .. 31;
      REG57 at 16#E4# range 0 .. 31;
      REG58 at 16#E8# range 0 .. 31;
      REG59 at 16#EC# range 0 .. 31;
      REG60 at 16#F0# range 0 .. 31;
      REG61 at 16#F4# range 0 .. 31;
      REG62 at 16#F8# range 0 .. 31;
      REG63 at 16#FC# range 0 .. 31;
   end record;

   --  Custom functions subsystem
   CFS_Periph : aliased CFS_Peripheral
     with Import, Address => CFS_Base;

end neorv32.CFS;
