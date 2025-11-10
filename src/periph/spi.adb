with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

with neorv32; use neorv32;
with neorv32.SPI; use neorv32.SPI;

with Sysinfo; use Sysinfo;

with RISCV.CSR; use RISCV.CSR;

package body Spi is

   procedure Init
      (Clock_Frequency : Natural; 
      Clock_Polarity : Boolean := False;
      Clock_Phase : Boolean := False)
   is
   Reset : UInt32 with Volatile, Address => SPI_Periph.CTRL'Address;
   CLK_Pre_Cdiv : Clk_Prescaler_Cdiv;
   begin
      if(Clock_Frequency > Clk / 4) then
         raise Constraint_Error with "SPI clock frequency too high";
      end if;
      if(Clock_Frequency < Clk / 131072) then
         raise Constraint_Error with "SPI clock frequency too low";
      end if;

      Reset := 0;
      SPI_Periph.CTRL.SPI_CTRL_EN := 0;

      if (Clock_Polarity) then
         SPI_Periph.CTRL.SPI_CTRL_CPOL := 1;
      else
         SPI_Periph.CTRL.SPI_CTRL_CPOL := 0;
      end if;

      if (Clock_Phase) then
         SPI_Periph.CTRL.SPI_CTRL_CPHA := 1;
      else
         SPI_Periph.CTRL.SPI_CTRL_CPHA := 0;
      end if;

      CLK_Pre_Cdiv := Get_Clk_Prescaler(Clock_Frequency);
      SPI_Periph.CTRL.SPI_CTRL_PRSC := CLK_Pre_Cdiv.Prescaler;
      SPI_Periph.CTRL.SPI_CTRL_CDIV := CLK_Pre_Cdiv.Cdiv;

      SPI_Periph.CTRL.SPI_CTRL_EN := 1;

   end Init;

   function Transfer (Send : Byte) return Byte is
      TX, RX : UInt32;
   begin
      -- Combine command=0 (bit31=0) with data byte
      TX := UInt32(Send) and 16#FF#;

      -- Write full 32-bit word to trigger transfer
      SPI_Periph.DATA := (SPI_DATA => Send,
                        Reserved_8_30 => 0,
                        SPI_DATA_CMD  => 0);

      -- Wait until transfer finishes
      while SPI_Periph.CTRL.SPI_CTRL_BUSY = 1 loop
         null;
      end loop;

      -- Read back RX data
      RX := UInt32(SPI_Periph.DATA.SPI_DATA);
      return Byte(RX and 16#FF#);
   end Transfer;

   function Check_Enabled return Bit is
   begin
      return SPI_Periph.CTRL.SPI_CTRL_EN;
   end Check_Enabled;

   procedure CS_Enable_Only (enable : UInt3) is 
      begin
      SPI_Periph.DATA := (SPI_DATA => 16#08#,
                        Reserved_8_30 => 0,
                        SPI_DATA_CMD  => 1);
      end CS_Enable_Only;
   
   procedure CS_Disable_All is 
      begin
      SPI_Periph.DATA.SPI_DATA_CMD := 1;
      SPI_Periph.DATA.SPI_DATA := Byte(SPI_Periph.DATA.SPI_DATA and 16#F7#);
      end CS_Disable_All;

   function Get_Clk_Prescaler (Clock_Frequency : Natural) return Clk_Prescaler_Cdiv is
   error : Integer := Integer'last;
   best_result : Clk_Prescaler_Cdiv;
   begin
   for Prescaler in 0 .. 7 loop
      for Cdiv in 0 .. 15 loop
         declare
            actual_freq : Natural := Clk / (2*(2 ** (Prescaler) * ((2**Cdiv) + 1)));
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
   end Get_Clk_Prescaler;

end Spi;