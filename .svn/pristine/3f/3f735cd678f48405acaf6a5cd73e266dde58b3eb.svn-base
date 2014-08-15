with Display;
package body Callback is

   procedure Change_Callback (Turnout   : in Layout.Turnout_ID;
                              Direction : in Layout.Turnout_Direction;
                              Moving    : in Boolean) is
   begin
      Display.Put (Turnout  => Turnout,
                  Direction => Direction,
                  Moving    => Moving);
   end Change_Callback;
   -----------------------------------------------
   procedure Failure_Callback (Requestor : in Trains.Request_ID;
                               Turnout   : in Layout.Turnout_ID) is
   begin
      null;
   end Failure_Callback;
   -----------------------------------------------
   procedure Recovery_Callback (Turnout : in Layout.Turnout_ID) is
   begin
      null;
   end Recovery_Callback;
   -----------------------------------------------

end Callback;
