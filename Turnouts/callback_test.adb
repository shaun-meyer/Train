-- locomotivation
-- James Vannordstrand
with Ada.Text_IO;

package body Callback_Test is

   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Request_IO is new Ada.Text_IO.Integer_IO (Trains.Request_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO
     (Layout.Turnout_Direction);

   -----------------------------------------------
   procedure Change_Callback (Turnout   : in Layout.Turnout_ID;
                              Direction : in Layout.Turnout_Direction;
                              Moving    : in Boolean) is
   begin
      Ada.Text_IO.Put_Line ("Status of Turnout.");
      Ada.Text_IO.Put ("Turnout - ");
      Turnout_IO.Put (Turnout);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Direction - ");
      Direction_IO.Put (Direction);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Moving - ");
      Ada.Text_IO.Put_Line (Boolean'Image (Moving));
      Ada.Text_IO.New_Line (2);
   end Change_Callback;
   -----------------------------------------------
   procedure Failure_Callback (Requestor : in Trains.Request_ID;
                               Turnout   : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("TURNOUT FAILED");
      Ada.Text_IO.Put ("Request - ");
      Request_IO.Put (Requestor);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Turnout - ");
      Turnout_IO.Put (Turnout);
      Ada.Text_IO.New_Line (3);
   end Failure_Callback;
   -----------------------------------------------
   procedure Recovery_Callback (Turnout : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("TURNOUT RECOVERED");
      Ada.Text_IO.Put ("Turnout - ");
      Turnout_IO.Put (Turnout);
      Ada.Text_IO.New_Line (3);
   end Recovery_Callback;
   -----------------------------------------------

end Callback_Test;
