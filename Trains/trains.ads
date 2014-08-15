with Layout;

package Trains is

   -- This package provides the basic types for trains.
   --
   -- Written by John McCormick, April 2002 as a single package.
   -- Broken into parent and child packages to separate the basic types
   -- from the operations in February 2008.

   -- IDs for the trains.
   -- Zero is reserved for dispatcher making requests for an engineer
   Max_Trains : constant := 3;
   type    Request_ID is            range 0 .. Max_Trains;
   subtype Train_ID   is Request_ID range 1 .. Max_Trains;

   Dispatcher : constant Request_ID := 0;

   type Direction_Type is (Forward, Backward);   -- Direction of train travel

   Max_Length_Start   : constant := 3;   -- Max number of blocks beneath a train
   Max_Length_Running : constant := 5;   -- at startup and running

   -----------------------------------------------------------------------------

   -- Types for keeping track of train stops

   type Stop_Reason is (Dispatcher_Request,  Turnout_Failure,
                        Reservation_Failure, Lost_Caboose);
   type Stop_Set    is array (Stop_Reason) of Boolean;
   type Turnout_Set is array (Layout.Turnout_ID) of Boolean;
   type Stop_Rec is
      record
         Reasons   : Stop_Set := (False, False, False, False);
         Block     : Layout.Block_ID;
         Turnouts  : Turnout_Set := (others => False);
      end record;

end Trains;

