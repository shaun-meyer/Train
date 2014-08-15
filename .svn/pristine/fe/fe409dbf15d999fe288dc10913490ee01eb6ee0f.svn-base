-- Locomotivation
-- Shaun Meyer

with Common_Units; use type Common_Units.Volts;
package DAC is
-- Specification the CIO-DDA06/Jr board

   type    Channel_Number is range 0 .. 5;
   subtype Output_Volts is Common_Units.Volts range -5.0 .. 5.0;

   procedure Write (Channel : in Channel_Number;
                    Value   : in Output_Volts);
   -- This procedure writes a specified bit value on the specified channel
   -- Precondition : None
   -- Postcondition : Data is written to the board

end DAC;
