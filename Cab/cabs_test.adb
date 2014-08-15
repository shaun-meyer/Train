-- Locomotivation
-- Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Cabs;
with Common_Units;
procedure Cabs_Test is
   package Control_Cab_IO is new Ada.Text_IO.Integer_IO (Cabs.Control_Cab_ID);
   package Percent_IO is new Ada.Text_IO.Integer_IO (Common_Units.Percent);

   Percent        : Common_Units.Percent;
   Control_Cab_ID : Cabs.Control_Cab_ID;
begin
   loop
      Ada.Text_IO.Put ("(Set_Limit)enter Control_Cab_ID : ");
      Control_Cab_IO.Get (Control_Cab_ID);
      Ada.Text_IO.Put ("(Set_Limit)enter Percent : ");
      Percent_IO.Get (Percent);
      Cabs.Set_Limit (Cab   => Control_Cab_ID,
                      Value => Percent);


      Cabs.Get_Limit (Cab   => Control_Cab_ID,
                      Value => Percent);
      Ada.Text_IO.Put ("(Get_Limit)percent = ");
      Percent_IO.Put (Percent);
      Ada.Text_IO.New_Line;

      Cabs.Get (Cab   => Control_Cab_ID,
                Value => Percent);
      Ada.Text_IO.Put ("(Get)percent = ");
      Percent_IO.Put (Percent);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("(Set)enter Control_Cab_ID : ");
      Control_Cab_IO.Get (Control_Cab_ID);
      Ada.Text_IO.Put ("(Set)enter Percent : ");
      Percent_IO.Get (Percent);
      Cabs.Set (Cab   => Control_Cab_ID,
                Value => Percent);

      Cabs.Get (Cab   => Control_Cab_ID,
                Value => Percent);
      Ada.Text_IO.Put ("(Get)percent = ");
      Percent_IO.Put (Percent);
      Ada.Text_IO.New_Line;
   end loop;
end Cabs_Test;
