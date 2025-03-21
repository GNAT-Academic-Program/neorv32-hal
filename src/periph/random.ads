with neorv32; use neorv32;

package Random is
   procedure Init;
   function Random return UInt8;
end Random;