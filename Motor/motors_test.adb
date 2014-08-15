-- Locomotivation
-- Shaun Meyer

with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Layout;
with Motors;

procedure Motors_Test is
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Turnout_Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Motors.State);

   Motor : Layout.Turnout_ID;
   Direction : Motors.State;

begin
   loop
      Ada.Text_IO.Put ("Enter a Turnout id: ");
      Turnout_IO.Get (Motor);
      Ada.Text_IO.Put ("Enter a direction: ");
      Turnout_Direction_IO.Get (Direction);

      Motors.Set (Motor     => Motor,
                  Direction => Direction);
      Ada.Text_IO.Put ("Check Turnout_ID In_Position Enter ID: ");
      Turnout_IO.Get (Motor);
      if Motors.In_Position (Motor => Motor) then
         Ada.Text_IO.Put_Line ("TRUE");
      else
         Ada.Text_IO.Put_Line ("FALSE");
      end if;
   end loop;
end Motors_Test;
