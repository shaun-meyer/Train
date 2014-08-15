-- Locomotivation
-- Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Layout;
with Command;
with Trains;
with Engineers;
use Command;
with Console_Management;
procedure Command_Test is
   Returned_Command : Command.Command_Rec;
   package Train_IO is new Ada.Text_IO.Integer_IO (Trains.Train_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Engineer_IO is new Ada.Text_IO.Integer_IO (Engineers.Engineer_ID);

begin
   Console_Management.Set_Raw_Mode;
   loop
      Command.Get (Command => Returned_Command);
      if Returned_Command.Which = Stop_All then
         Ada.Text_IO.Put_Line ("Stop_All was called");
      elsif Returned_Command.Which = Restart then
         Ada.Text_IO.Put_Line ("Restart was called");
      elsif Returned_Command.Which = Quit then
         Ada.Text_IO.Put_Line ("Quit was called");
      elsif Returned_Command.Which = Error then
         Ada.Text_IO.Put_Line ("Error was called");
      elsif Returned_Command.Which = Stop then
         Ada.Text_IO.Put ("Stop was called, Train_ID  : ");
         Train_IO.Put (Returned_Command.Train);
         Ada.Text_IO.New_Line;
      elsif Returned_Command.Which = Go then
         Ada.Text_IO.Put ("Go was called, Train_ID  : ");
         Train_IO.Put (Returned_Command.Train);
         Ada.Text_IO.New_Line;
      elsif Returned_Command.Which = Left then
         Ada.Text_IO.Put ("Left was called, Turnout_ID : ");
         Turnout_IO.Put (Returned_Command.Turnout);
         Ada.Text_IO.New_Line;
      elsif Returned_Command.Which = Right then
         Ada.Text_IO.Put ("Right was called, Turnout_ID : ");
         Turnout_IO.Put (Returned_Command.Turnout);
         Ada.Text_IO.New_Line;
      elsif Returned_Command.Which = Free then
         Ada.Text_IO.Put ("Free was called, Block_id  : ");
         Block_IO.Put (Returned_Command.Block);
         Ada.Text_IO.New_Line;
      elsif Returned_Command.Which = Skill then
         Ada.Text_IO.Put ("Skill was called, Engineer_ID  : ");
         Engineer_IO.Put (Returned_Command.Engineer);
         Ada.Text_IO.New_Line;
      else
         Ada.Text_IO.Put_Line ("Unrecognizable Error");
      end if;
   end loop;

end Command_Test;
