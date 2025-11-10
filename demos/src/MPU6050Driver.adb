with SPI; use SPI;

package body MPU6050Driver is

   procedure Init_MPU6050 is
   begin
      SPI.Init (Clock_Frequency => 500_000, Clock_Polarity => True, Clock_Phase => True);
   end Init_MPU6050;

   function Read_MPU6050 (Reg_Address : Byte) return Byte is
   no_data : Byte;
   Data : Byte;
   begin
      SPI.CS_Enable_Only (2#000#);  -- Enable CS for MPU6050 (CS0)
      no_data := SPI.Transfer (Reg_Address);  -- Set MSB for read operation
      Data := SPI.Transfer (0);
      SPI.CS_Disable_All;
      return Data;
   end Read_MPU6050;

end MPU6050Driver;