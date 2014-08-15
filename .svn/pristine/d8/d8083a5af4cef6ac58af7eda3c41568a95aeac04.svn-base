--Locomotivation
--Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Port_IO; use Port_IO;
--Digital to Analog Converter
package body DAC is
   Base : constant Port_IO.Address_Range := 16#240#;
   type Address_Array is array (Channel_Number) of Port_IO.Address_Range;
   Address_Of : constant Address_Array := (Base,
                                           Base + 2,
                                           Base + 4,
                                           Base + 6,
                                           Base + 8,
                                           Base + 10);

   procedure Write (Channel : in Channel_Number;
                    Value   : in Output_Volts) is
      Digital : Common_Units.Bit_Value;

   begin

      --Performe conversion to Volts
      Digital := Common_Units.Bit_Value ((Float (Value) + 5.0) * 409.5);
      -- writes to board
      Port_IO.Out_Word (Address => Address_Of (Channel),
                        Data    => Port_IO.Word (Digital));
   end Write;
begin
   for Channel in Channel_Number loop
      Port_IO.Out_Word (Address => Address_Of (Channel),
                        Data    => 4095/2);
   end loop;
end DAC;
