with neorv32; use neorv32;
with neorv32.TWI; use neorv32.TWI;

with Sysinfo; use Sysinfo;

package body twi is

procedure Init(Clock_Frequency : Natural) is
CLK_Pre_Cdiv : Clk_Prescaler_Cdiv;
Sys_clk : constant Natural := Clk;
begin
   TWI_Periph.CTRL.TWI_CTRL_EN := 0;

   if (Sys_clk / 8 < Clock_Frequency) then
      raise Constraint_Error with "I2C clock frequency too high";
   end if;

   if (Sys_clk / 262144 > Clock_Frequency) then
      raise Constraint_Error with "I2C clock frequency too low";
   end if;

   CLK_Pre_Cdiv := Get_Clk_Prescaler_I2C(Clock_Frequency);

   TWI_Periph.CTRL.TWI_CTRL_PRSC := CLK_Pre_Cdiv.Prescaler;
   TWI_Periph.CTRL.TWI_CTRL_CDIV := CLK_Pre_Cdiv.Cdiv;

   TWI_Periph.CTRL.TWI_CTRL_EN := 1;

end Init;

function Get_Clk_Prescaler_I2C (Clock_Frequency : Natural) return Clk_Prescaler_Cdiv is
   -- The SPI clock frequency is based off two variables, the prescaler and the clock divider
   -- To find the best prescaler and clock divider for a given frequency, we do a search
   error : Integer := Integer'last;
   best_result : Clk_Prescaler_Cdiv;
   begin
   for Prescaler in 0 .. 7 loop
      for Cdiv in 0 .. 15 loop
         declare
            actual_freq : Natural := Clk / (4*(2**(Prescaler + 1) * ((Cdiv + 1))));
            freq_error : Integer := Integer(Clock_Frequency) - Integer(actual_freq);
         begin
            if freq_error < 0 then
               freq_error := -freq_error;
            end if;
            if freq_error < error then
               error := freq_error;
               best_result.Prescaler := UInt3(Prescaler);
               best_result.Cdiv := UInt4(Cdiv);
            end if;
         end;
      end loop;
   end loop;

   return best_result;
end Get_Clk_Prescaler_I2C;

end twi;