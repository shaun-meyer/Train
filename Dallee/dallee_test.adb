-- locomotivation
-- James Vannordstrand
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Dallee;

procedure Dallee_Test is
   package Dallee_State_IO is new Ada.Text_IO.Enumeration_IO (Dallee.State);

   package Dallee_Sound_Units_IO is new Ada.Text_IO.Integer_IO
     (Dallee.Dallee_Sound_Units);

   package Dallee_Sound_Type_IO is new Ada.Text_IO.Enumeration_IO
     (Dallee.Sound_Type);

-------------------------------------------------------------------------------
   procedure Test_Sound (Number : Dallee.Dallee_Sound_Units;
                         Sound  : Dallee.Sound_Type) is
      State : Dallee.State;
   begin
      Ada.Text_IO.Put ("select (On) or (Off) : ");
      Dallee_State_IO.Get (State);
      Dallee.Set_Sound (Changed_State => State,
                        Sound_Unit    => Number,
                        Sound         => Sound);

   end Test_Sound;
-------------------------------------------------------------------------------
   Sound_Choice : Dallee.Sound_Type;
   Device       : Dallee.Dallee_Sound_Units;

begin

   --This loop keeps taking horn or bell commands from user
   --Each iteration tests either the horn or bell for a device
   loop
      Ada.Text_IO.Put ("Test Horn or Bell - ");
      Dallee_Sound_Type_IO.Get (Sound_Choice);
      Ada.Text_IO.Put ("Which device (1,2,3) - ");
      Dallee_Sound_Units_IO.Get (Device);
      Test_Sound (Number => Device,
                  Sound  => Sound_Choice);
   end loop;

end Dallee_Test;

-------------------------------------------------------------------------------
