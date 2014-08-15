-- Locomotivation
-- Mike Miller

with Common_Units;
use type Common_Units.Volts;
package ADC is
-- Specification for CIO_DAS08/Jr board

   type    Channel_Number    is range 0 .. 7;
   subtype Output_Volts is Common_Units.Volts range -5.0 .. 5.0;


   procedure Read (Channel     : in  Channel_Number;
                   Value       : out Output_Volts);
   -- This procedure reads the bit from the specified channel
   -- Precondition : There must be bit values at the specified channel
   -- Postcondition : Bit values are returned

private
   for Channel_Number'Size use 3;
end ADC;
