with neorv32; use neorv32;

package Spi is
   procedure Init (Clock_Frequency : Natural; 
   Clock_Polarity : Boolean := False;
   Clock_Phase : Boolean := False);

   type Clk_Prescaler_Cdiv is record
      Prescaler : UInt3;
      Cdiv : UInt4;
   end record;
   function Get_Clk_Prescaler(Clock_Frequency : Natural) return Clk_Prescaler_Cdiv;
end Spi;