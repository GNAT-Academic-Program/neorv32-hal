pragma Style_Checks (Off);

--  This spec has been automatically generated from neorv32.svd

pragma Restrictions (No_Elaboration_Code);

with System;

--  True random number generator
package neorv32.TRNG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_TRNG_CTRL_EN_Field is neorv32.Bit;
   subtype CTRL_TRNG_CTRL_FIFO_CLR_Field is neorv32.Bit;
   subtype CTRL_ONEWIRE_CTRL_FIFO_SIZE_Field is neorv32.UInt4;
   subtype CTRL_TRNG_CTRL_SIM_MODE_Field is neorv32.Bit;
   subtype CTRL_TRNG_CTRL_AVAIL_Field is neorv32.Bit;

   --  Control and data register
   type CTRL_Register is record
      --  TRNG enable flag
      TRNG_CTRL_EN           : CTRL_TRNG_CTRL_EN_Field := 16#0#;
      --  Clear data FIFO when set (flag auto clears)
      TRNG_CTRL_FIFO_CLR     : CTRL_TRNG_CTRL_FIFO_CLR_Field := 16#0#;
      --  Read-only. log2(TRNG FIFO size)
      ONEWIRE_CTRL_FIFO_SIZE : CTRL_ONEWIRE_CTRL_FIFO_SIZE_Field := 16#0#;
      --  Read-only. TRNG simulation mode (PRNG!) active
      TRNG_CTRL_SIM_MODE     : CTRL_TRNG_CTRL_SIM_MODE_Field := 16#0#;
      --  Read-only. Random data available
      TRNG_CTRL_AVAIL        : CTRL_TRNG_CTRL_AVAIL_Field := 16#0#;
      --  unspecified
      Reserved_8_31          : neorv32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      TRNG_CTRL_EN           at 0 range 0 .. 0;
      TRNG_CTRL_FIFO_CLR     at 0 range 1 .. 1;
      ONEWIRE_CTRL_FIFO_SIZE at 0 range 2 .. 5;
      TRNG_CTRL_SIM_MODE     at 0 range 6 .. 6;
      TRNG_CTRL_AVAIL        at 0 range 7 .. 7;
      Reserved_8_31          at 0 range 8 .. 31;
   end record;

   subtype DATA_TRNG_DATA_Field is neorv32.Byte;

   --  Random data
   type DATA_Register is record
      --  Read-only. *** This field is modified following a read operation ***.
      --  Random data
      TRNG_DATA     : DATA_TRNG_DATA_Field;
      --  unspecified
      Reserved_8_31 : neorv32.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATA_Register use record
      TRNG_DATA     at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  True random number generator
   type TRNG_Peripheral is record
      --  Control and data register
      CTRL : aliased CTRL_Register;
      --  Random data
      DATA : aliased DATA_Register;
   end record
     with Volatile;

   for TRNG_Peripheral use record
      CTRL at 16#0# range 0 .. 31;
      DATA at 16#4# range 0 .. 31;
   end record;

   --  True random number generator
   TRNG_Periph : aliased TRNG_Peripheral
     with Import, Address => TRNG_Base;

end neorv32.TRNG;
