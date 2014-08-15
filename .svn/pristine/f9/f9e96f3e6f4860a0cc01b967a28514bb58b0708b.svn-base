-- Locomotivation
-- Mike Miller
package Dallee is
   type State is (Off, On);
   type Sound_Type is (Horn, Bell);
   type Sound_Units is range 1 .. 4;
   subtype Dallee_Sound_Units is Sound_Units range 1 .. 3;

   procedure Set_Sound (Changed_State : in State;
                        Sound_Unit    : in Dallee_Sound_Units;
                        Sound         : in Sound_Type);
   --this procedure turns the horn or Bell on or off for a specific sound unit
   --Precondition  : None
   --Postcondition : the Horn or Bell makes noise or stops making noise

private
   for State use (Off => 2#0#,
                  On  => 2#1#);
end Dallee;

