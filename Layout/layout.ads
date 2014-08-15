--Locomotivation
--James Vannordstrand

package Layout is
   pragma Pure (Layout);

   type Block_Polarity is (Normal, Reversed);

   type Turnout_ID is range 1 .. 26;

   type Block_ID is range 1 .. 40;

   type Hall_ID is range 1 .. 51;

   type Terminator_Type is (Block, Deadend, Turnout);

   type Turnout_Direction is (Left, Right, Common);

   type Block_Array is array (Positive range <>) of Block_ID;

   type Block_List (Max_Size : Positive) is
      record
         Size : Natural := 0;
         Items : Block_Array (1 .. Max_Size);
      end record;

   function Retrieve_Terminator  (Block      : in Block_ID;
                                  Direction  : in Block_Polarity)
                                  return Terminator_Type;
   --This procedure gets the type of track componant that the block leads to
   --in the specified direction
   --Precondition  : None
   --Postcondition : Terminator is the Terminator_Type at the end of a block

   function Retrieve_Turnout_ID (Block     : in Block_ID;
                                 Direction : in Block_Polarity)
                                 return Turnout_ID;
   --This procedure gets the number of the turnout given a block and a polarity
   --Precondition  : Must be a turnout on the block in the given direction
   --Postcondition : Get turnout number

   function Retrieve_Next_Block_ID (Block      : in Block_ID;
                                    Direction  : in Block_Polarity)
                                    return Block_ID;
   --This procedure gets the next block number given a block
   --precondition  : Must be a block on the end of current block given direction
   --Postcondition : Retrieves next block id

   function Retrieve_Turnout_Block_ID (Direction : in Turnout_Direction;
                                       Turnout   : in Turnout_ID)
                                       return Block_ID;
   --This procedure gets the specified block number of the direction specified
   --on a turnout
   --Precondition  : Must be a turnout
   --Postcondition : Returned block id of specified direction on turnout

   function Retrieve_Hall_ID (Current_Block   : in Block_ID;
                              Next_Block      : in Block_ID) return Hall_ID;
   --This procedure gets the sensor number and the block boundary
   --Precondition  : Blocks must be connected
   --Postcondition : Returns Hall Id of specified blocks

   function Is_Reversing_Point (Sensor : in Hall_ID) return Boolean;
   --Given a sensor we will get back a boolean stating whether or not a
   --reversing point
   --Precondition  : None
   --Postcondition : Return if the sensor is a reversing point.

   function Opposite (Direction : in Block_Polarity) return Block_Polarity;
   --Given a block polarity will return the opposite (Normal, Reversed)
   --Precondition  : None
   --Postcondition : Returns opposite block polarity

   function Opposite (Direction : in Turnout_Direction)
                      return Turnout_Direction;
   --Give a turnout direction returns the opposite (Left, Right)
   --Precondition  : None
   --Postcondition : Returns opposite turnout direction

   procedure Get_Block_IDs_With_Hall_Sensor (ID        : in Hall_ID;
                                             First_ID  : out Block_ID;
                                             Second_ID : out Block_ID);
   --Returns block IDs that surround and hall ID
   --Precondition  : None
   --Postcondition : returns block ids given a hall id

   procedure Is_Forced_Turnout (ID       : in Block_ID;
                                Polarity : in Block_Polarity;
                                Forced   : out Boolean);
   --Checks if there is a forced turnut on a given end of a block
   --Precondition  : None
   --Postcondition : Returns true or false depending if there's a forced turnout

   procedure Set_Forced_Turnout (Block     : in  Block_ID;
                                 Polarity  : in  Block_Polarity;
                                 Turnout   : out Turnout_ID;
                                 Direction : out Turnout_Direction);
   --Given a block and polarity returns forced turnout's id and direction must
   --be set
   --Precondition  : Must be a forced turnout on the end of the block
   --Postcondition : returns forced turnout id and direction to be set

   function Is_Joint_Turnout (Direction    : in Turnout_Direction;
                              In_Turnout   : in Turnout_ID)
                              return Boolean;
   --Checkes if a turnout is a joint turnout
   --Precondition  : None
   --Postcondition : returns true or false depending if it is a joint turnout

   procedure Retrieve_Joint_Turnout (Direction    : in Turnout_Direction;
                                     In_Turnout   : in Turnout_ID;
                                     Joint        : out Turnout_ID);
   --Returns the joint turnout id of the turnout passed in
   --Precondition  : turnout passed in must be a joint turnout
   --Postcondition : returns joint turnout id

   procedure Retrieve_Cross_Blocks (Block        : in Block_ID;
                                    Cross_Blocks : out Block_List);
   --Given a block id returns a list of blocks ids that cross
   --Precondition  : None
   --Postcondition : returns block ids of blocks that cross block id passed in

   procedure Next_Choice_Turnout (Block        : in Block_ID;
                                  Polarity     : in Block_Polarity;
                                  Out_Turnout  : out Turnout_ID);
   --Returns turnout id of the next choice turnout
   --Precondition  : None
   --Postcondition : returns next turnout id of the next choice turnout



end Layout;
