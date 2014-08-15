--  Locomotivation
--  Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with ADC;

procedure adc_test is
   package Channel_IO is new Ada.Text_IO.Integer_IO (ADC.Channel_Number);
   package Volts_IO is new Ada.Text_IO.Fixed_IO (ADC.Output_Volts);

   Channel : ADC.Channel_Number;
   Volts   : ADC.Output_Volts;
begin

   --Each iteration checks the output volts on supplied channel
   loop
      Ada.Text_IO.Put ("which channel do you want to test : ");
      Channel_IO.Get (Channel);
      ADC.Read (Channel => Channel,
                Value   => Volts);
      Ada.Text_IO.Put ("the output volts for selected channel : ");
      Volts_IO.Put (Volts);
      Ada.Text_IO.New_Line;
   end loop;

end adc_test;
