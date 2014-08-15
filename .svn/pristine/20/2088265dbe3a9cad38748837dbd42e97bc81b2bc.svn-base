--Locomotivation
--James Vannordstrand
with Layout;
with Cabs;
with Trains;
with Port_IO; use type Port_IO.Address_Range;

package Block is
   procedure Connect_Cab (Cab      : in Cabs.Cab_ID;
                          Polarity : in Layout.Block_Polarity;
                          Block    : in Layout.Block_ID);
   --This procedure connects a cab to a block and sets it polarity
   --Precondition  : Train only calls if the block is reserved.
   --Postcondition : The cab is connected to the block along with polarity.

   procedure Reserve (Train_ID : in Trains.Train_ID;
                      Block    : in Layout.Block_ID;
                      Success  : out Boolean);
   --This procedure attempts to reserve a block
   --Precondition  : none
   --Postcondition : either the block is reserved or failed to reserve because
   		     --  it is already reserved by another train
   procedure UnReserve (Block      : in Layout.Block_ID;
                        Requestor  : in Trains.Request_ID);
   --This procedure UnReserves a block
   --Precondition  : none
   --Postcondition : Either the block is unreserved and the cab is set to the
   --   null cab, or it decrements the reserved counter

   procedure Get_Polarity (Block    : in Layout.Block_ID;
                           Polarity : out Layout.Block_Polarity;
                           Cab      : out Cabs.Cab_ID);
   --This Procedure returns the polarity of a given block
   --Precondition  : none
   --Postcondition : Returns polarity of a given block

   procedure Get_Reserve_Count (Block : in Layout.Block_ID;
                                ID    : in Trains.Train_ID;
                                Count : out Natural);
   --This Procedure returns the Reservation_Count of a given block
   --Precondition  : none
   --Postcondition : Returns Reservation_Count of a given block

   procedure UnReserve_All;
   --This Procedure unreserves all blocks
   --Precondition  : none
   --Postcondition : no Blocks are Reserved
end Block;
