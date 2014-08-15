--  Locomotivation
--  Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with DAC;
procedure DAC_test is
   package Channel_IO is new Ada.Text_IO.Integer_IO (DAC.Channel_Number);
   package Volts_IO is new Ada.Text_IO.Fixed_IO (DAC.Output_Volts);

   Channel : DAC.Channel_Number;
   Volts   : DAC.Output_Volts;
begin
   loop
      Ada.Text_IO.Put ("which channel do you want to test : ");
      Channel_IO.Get (Channel);
      Ada.Text_IO.Put ("enter amount of volts between -5 to 5 : ");
      Volts_IO.Get (Volts);
      DAC.Write (Channel => Channel,
                 Value   => Volts);
   end loop;
end DAC_test;

