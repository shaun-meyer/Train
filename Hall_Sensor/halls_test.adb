--Locomotivation
--Shaun Meyer

with Ada.Text_IO;
with Halls;
use Halls;
with Ada.Integer_Text_IO;
with MaRTE_OS;
with Interruptor;
pragma Warnings (Off, MaRTE_OS);

procedure Halls_Test is
   Call_Back : Callback_Ptr;

   procedure Is_Triggered_Test is
      Hall   : Integer;
      Result : Boolean;
   begin
      loop
         Ada.Text_IO.Put ("Enter a Hall Id : ");
         Ada.Integer_Text_IO.Get (Hall);
         exit when Hall = 0;
         Result := Halls.Is_Triggered (Halls.Hall_ID (Hall));

         Ada.Text_IO.Put_Line (Boolean'Image (Result));
         Ada.Text_IO.New_Line;
      end loop;
   end Is_Triggered_Test;

   procedure Disable_Test is
   begin
      Halls.Disable;

   end Disable_Test;

   procedure Enable_Test (Call : in Callback_Ptr) is
   begin
      Halls.Enable (Callback => Call);

   end Enable_Test;

begin
   Call_Back := Interruptor.Print_Interrupt'Access;

   Ada.Text_IO.Put_Line ("INITIALIZING . . . ");
   Halls.Initialize;

   Is_Triggered_Test;

   Ada.Text_IO.Put_Line ("ENABLING INTERRUPTS . . . ");
   Enable_Test (Call => Call_Back);
   Is_Triggered_Test;

   Ada.Text_IO.Put_Line ("DISABLING INTERRUPTS . . . ");
   Disable_Test;
   Is_Triggered_Test;
end Halls_Test;
