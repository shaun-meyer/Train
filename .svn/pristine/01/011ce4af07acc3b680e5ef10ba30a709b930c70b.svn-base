--  Locomotivation
--  James Vannordstrand

with DAC;
with Common_Units;
use Common_Units;
package body Cabs is

   type Cab_Array_Type is array (Cab_ID) of Percent;

   Set_Values : Cab_Array_Type := (0, 0, 0, 0, 0, 0, 0, 0);
   Limit_Values  : Cab_Array_Type := (0, 0, 0, 0, 0, 0, 0, 0);
------------------------------------------------------------------------------
   procedure Set (Cab   : in Control_Cab_ID;
                  Value : in Percent) is
   begin
      if Value < Limit_Values (Cab) then
         Set_Values (Cab) := Value;
         DAC.Write (Channel => DAC.Channel_Number (Cab - 1),
                   Value   => Common_Units.Volts (5.0 * Float (Value) / 100.0));
      else
         Set_Values (Cab) := Limit_Values (Cab);
         DAC.Write
           (Channel => DAC.Channel_Number (Cab - 1),
            Value   =>
               (Common_Units.Volts (Limit_Values (Cab) * 5.0) / 100.0));
      end if;
   end Set;
------------------------------------------------------------------------------
   procedure Get (Cab   : in  Cab_ID;
                  Value : out Percent) is
   begin
      Value := Set_Values (Cab);
   end Get;
------------------------------------------------------------------------------

   procedure Set_Limit (Cab   : in Control_Cab_ID;
			Value : in Percent) is
   begin
      Limit_Values (Cab) := Value;
      if Set_Values (Cab) > Value then
	 Set (Cab   => Cab,
              Value => Value);
      end if;
   end Set_Limit;
------------------------------------------------------------------------------
   procedure Get_Limit (Cab   : in  Cab_ID;
                        Value : out Percent) is
   begin
      Value := Limit_Values (Cab);
   end Get_Limit;
------------------------------------------------------------------------------
end Cabs;
