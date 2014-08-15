--locomotivation
-- James Vannordstrand

with Layout.Search;
with Halls;
with Cabs; use Cabs;
with Common_Units;
with Locomotives;

package Trains.Functions is
   procedure Enable_Train (ID       : in Train_ID;
                           Cab      : in Cabs.Cab_ID;
                           Blocks   : in Layout.Search.Block_List;
                           Starting_Turnouts : in Layout.Search.Turnout_List;
                           Info     : in Locomotives.Loco_Rec);
   -- Enables a train
   -- Preconditions  : The train is disabled
   -- Postconditions : The train is enabled

   procedure Is_Triggered (Hall   : Halls.Hall_ID);

   procedure Throttle (ID      : in Train_ID;
                       Percent : in Common_Units.Percent);
   -- Changes speed of train
   -- Preconditions  : None
   -- Postconditions : the blocks powered to percent given

   procedure Set_Turnout (ID        : in Train_ID;
                          Direction : in Layout.Turnout_Direction);
   -- Set next turnout Direction
   -- Preconditions  : None
   -- Postconditions : the turnout is set to the direction given

   procedure Set_Direction (ID        : in Train_ID;
                            Direction : in Direction_Type);
   -- Set Trains Direction
   -- Preconditions  : None
   -- Postconditions : direction is set in either normal or reverse direction

   procedure Horn (ID : in Train_ID);
   -- Sound the Horn
   -- Preconditions  : None
   -- Postconditions : The horn sounds

   procedure Engineer_Stop (ID : in Train_ID);
   -- Stop a train, sent by engineer
   -- Preconditions  : None
   -- Postconditions : The train is stopped

   procedure Dispatcher_Request_Stop (ID : in Train_ID);
   -- Stop a Train, sent by dispatcher
   -- Preconditions  : None
   -- Postconditions : the train is stopped

   procedure Dispatcher_Request_Go (ID : in Train_ID);
   -- Start a Train
   -- Preconditions  : None
   -- Postconditions : the Dispatcher_Request is false

   procedure Failure_Callback (Requestor : in Trains.Request_ID;
                               Turnout   : in Layout.Turnout_ID);
   -- Stop a Train because of a failed turnout
   -- Preconditions : None
   -- Postconditions : The train is stopped, waiting for turnout

   procedure Recovery_Callback (Turnout : in Layout.Turnout_ID);
   -- Set Turnout failure to false and cap percent to 100
   -- Preconditions : None
   -- Postconditions : Turnout_Failure is set to false
   procedure Disable;
   -- Reset The_Trains to Default
   -- Preconditions : None
   -- Postconditions : The_Trains are set to default
end Trains.Functions;
