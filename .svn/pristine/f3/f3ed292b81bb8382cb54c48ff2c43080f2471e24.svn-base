with Trains.functions;
with Layout;

package Callback is

   procedure Failure_Callback (Requestor : in Trains.Request_ID;
                               Turnout   : in Layout.Turnout_ID);

   procedure Recovery_Callback (Turnout : in Layout.Turnout_ID);

   procedure Change_Callback (Turnout   : in Layout.Turnout_ID;
                              Direction : in Layout.Turnout_Direction;
                              Moving    : in Boolean);

end Callback;
