--  Locomotivation
--  James Vannordstrand
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Turnouts;
with Trains;
with Motors;
with Layout;
with Callback_Test;

procedure Turnouts_Test is

   package Train_IO is new Ada.Text_IO.Integer_IO (Trains.Train_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO
     (Layout.Turnout_Direction);

   Char      : Character;
   Train     : Trains.Train_ID;
   Turnout   : Layout.Turnout_ID;
   Direction : Layout.Turnout_Direction;
   Status    : Turnouts.Status_Rec;

   --Setting callbacks
   Change    : Turnouts.Change_Ptr;
   Failure   : Turnouts.Failure_Ptr;
   Recovery  : Turnouts.Recover_Ptr;

begin
   Change := Callback_Test.Change_Callback'Access;
   Failure := Callback_Test.Failure_Callback'Access;
   Recovery := Callback_Test.Recovery_Callback'Access;

   Turnouts.Set_Change_Callback (Change);
   Turnouts.Set_Failure_Callback (Failure);
   Turnouts.Set_Recovery_Callback (Recovery);

   loop
      Ada.Text_IO.Put ("(S)et, (T)erminate, stat(U)s, or (D)irection_of - ");
      Ada.Text_IO.Get (Item => Char);
      Ada.Text_IO.Put ("Enter Turnout ID - ");
      Turnout_IO.Get (Turnout);
      if Char = 'S' then
         Ada.Text_IO.Put ("Enter the train - ");
         Train_IO.Get (Train);
         Ada.Text_IO.Put ("Enter the direction - ");
         Direction_IO.Get (Direction);
         Turnouts.Set (Requestor => Train,
                       Turnout   => Turnout,
                       Direction => Direction);
      elsif Char = 'D' then
         Direction := Turnouts.Direction_Of (Turnout);
         Direction_IO.Put (Direction);
         Ada.Text_IO.New_Line;
      elsif Char = 'U' then
         Status := Turnouts.Status (Turnout);
         Ada.Text_IO.Put ("Desired - ");
         Direction_IO.Put (Status.Desired);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Current - ");
         Direction_IO.Put (Status.Current);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Moving - ");
         Ada.Text_IO.Put_Line (Boolean'Image (Status.Moving));
         Ada.Text_IO.New_Line (2);
      end if;

      exit when Char = 'T';
   end loop;

   Turnouts.Shut_Down;
   delay 30.0;

   for Turnout_ID in Layout.Turnout_ID loop
      Ada.Text_IO.Put
        (Boolean'Image (Motors.In_Position (Turnout_ID)));
   end loop;

end Turnouts_Test;
