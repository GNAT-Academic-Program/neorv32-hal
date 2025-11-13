
with neorv32; use neorv32;

package twi is

procedure Init(Clock_Frequency : Natural);

type Clk_Prescaler_Cdiv is record
   Prescaler : UInt3;
   Cdiv : UInt4;
end record;

function Get_Clk_Prescaler_I2C(Clock_Frequency : Natural) return Clk_Prescaler_Cdiv;

end twi;