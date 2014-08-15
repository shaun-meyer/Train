--Locomotivation
--James Vannordstrand

package body Layout is

   type Terminator_Block_ID_Rec (Terminator : Terminator_Type := Block) is
      record
         case Terminator is
            when Block =>
               Block : Block_ID;
            when Turnout =>
               Turnout : Turnout_ID;
            when Deadend =>
               null;
         end case;
      end record;

   type Terminator_Array_Type is array (Block_ID, Block_Polarity) of
      Terminator_Block_ID_Rec;

   type Index_Type is (First, Second);
   type Hall_ID_Array_Type is array (Hall_ID, Index_Type) of Block_ID;

   type Reversing_Point_Array_Type is array (Hall_ID) of Boolean;

   type Turnout_Block_ID_Array_Type is array (Turnout_ID, Turnout_Direction) of
     Block_ID;

   Terminator_Array : constant Terminator_Array_Type :=
   -- Terminator type, terminator ID
   -- Normal
   -- Reversed
   (((Terminator => Block, Block => 2),
    (Terminator => Block, Block => 11)),
   ((Terminator => Turnout, Turnout => 6),
    (Terminator => Turnout, Turnout => 3)),
   ((Terminator => Block, Block => 4),
    (Terminator => Block, Block => 2)),
   ((Terminator => Block, Block => 5),
    (Terminator => Turnout, Turnout => 18)),
   ((Terminator => Block, Block => 6),
    (Terminator => Block, Block => 4)),
   ((Terminator => Block, Block => 7),
    (Terminator => Turnout, Turnout => 22)),
   ((Terminator => Block, Block => 8),
    (Terminator => Turnout, Turnout => 23)),
   ((Terminator => Turnout, Turnout => 12),
    (Terminator => Turnout, Turnout => 10)),
   ((Terminator => Block, Block => 10),
    (Terminator => Block, Block => 8)),
   ((Terminator => Turnout, Turnout => 15),
    (Terminator => Turnout, Turnout => 13)),
   ((Terminator => Block, Block => 1),
    (Terminator => Block, Block => 10)),
   ((Terminator => Block, Block => 31),
    (Terminator => Block, Block => 10)),
   ((Terminator => Turnout, Turnout => 2),
    (Terminator => Block, Block => 31)),
   ((Terminator => Block, Block => 15),
    (Terminator => Block, Block => 13)),
   ((Terminator => Turnout, Turnout => 5),
    (Terminator => Turnout, Turnout => 4)),
   ((Terminator => Block, Block => 17),
    (Terminator => Block, Block => 15)),
   ((Terminator => Block, Block => 18),
    (Terminator => Turnout, Turnout => 7)),
   ((Terminator => Turnout, Turnout => 17),
    (Terminator => Turnout, Turnout => 8)),
   ((Terminator => Block, Block => 20),
    (Terminator => Block, Block => 18)),
   ((Terminator => Block, Block => 21),
    (Terminator => Turnout, Turnout => 19)),
   ((Terminator => Turnout, Turnout => 21),
    (Terminator => Turnout, Turnout => 20)),
   ((Terminator => Block, Block => 6),
    (Terminator => Block, Block => 21)),
   ((Terminator => Block, Block => 24),
    (Terminator => Block, Block => 21)),
   ((Terminator => Turnout, Turnout => 11),
    (Terminator => Block, Block => 23)),
   ((Terminator => Block, Block => 26),
    (Terminator => Block, Block => 24)),
   ((Terminator => Block, Block => 15),
    (Terminator => Turnout, Turnout => 14)),
   ((Terminator => Block, Block => 26),
    (Terminator => Block, Block => 8)),
   ((Terminator => Block, Block => 24),
    (Terminator => Block, Block => 15)),
   ((Terminator => Block, Block => 10),
    (Terminator => Block, Block => 30)),
   ((Terminator => Block, Block => 18),
    (Terminator => Block, Block => 29)),
   ((Terminator => Turnout, Turnout => 1),
    (Terminator => Turnout, Turnout => 16)),
   ((Terminator => Block, Block => 33),
    (Terminator => Block, Block => 31)),
   ((Terminator => Block, Block => 34),
    (Terminator => Block, Block => 32)),
   ((Terminator => Block, Block => 35),
    (Terminator => Block, Block => 33)),
   ((Terminator => Block, Block => 20),
    (Terminator => Turnout, Turnout => 9)),
   ((Terminator => Block, Block => 37),
    (Terminator => Block, Block => 31)),
   ((Terminator => Block, Block => 38),
    (Terminator => Block, Block => 36)),
   ((Terminator => Block, Block => 35),
    (Terminator => Block, Block => 37)),
   ((Terminator => Block, Block => 21),
    (Terminator => Block, Block => 8)),
   ((Terminator => Block, Block => 7),
    (Terminator => Deadend)));


   Hall_ID_Array : constant Hall_ID_Array_Type :=
   -- Lower block, Higher block
      ((1, 11), (13, 31), (12, 31), (31, 36), (31, 32), (10, 11),
      (10, 12), (13, 14), (2, 13), (1, 2), (14, 15), (15, 26),
      (36, 37), (32, 33), (15, 16), (15, 28), (25, 26), (26, 27),
      (9, 10), (10, 29), (16, 17), (2, 17), (2, 3), (33, 34),
      (37, 38), (24, 28), (24, 25), (8, 27), (8, 9), (29, 30),
      (8, 39), (7, 8), (17, 18), (18, 30), (35, 38), (34, 35),
      (23, 24), (3, 4), (4, 18), (18, 19), (19, 20), (20, 35),
      (20, 21), (21, 39), (4, 5), (21, 23), (21, 22), (6, 22),
       (5, 6), (6, 7), (7, 40));

   Reversing_Point_Array  : constant Reversing_Point_Array_Type :=
      (False, False, True, True, True, False, False, False, False, False,
       False, False, False, False, False, False, False, False, False, False,
       False, False, False, False, False, True, False, False, False, True,
       True, False, False, False, False, False, False, False, False, False,
       False, False, False, False, False, False, False, False, False, False,
       False);

   Turnout_Block_ID_Array : constant Turnout_Block_ID_Array_Type :=
   -- Left, Right, Common
      ((13, 12, 31), (2, 14, 13), (13, 1, 2), (26, 14, 15), (16, 28, 15),
       (3, 17, 2), (16, 2, 17), (30, 17, 18), (34, 38, 35), (39, 7, 8),
       (25, 28, 24), (9, 27, 8), (9, 29, 10), (25, 27, 26), (11, 12, 10),
       (36, 32, 31), (4, 19, 18), (18, 3, 4), (35, 19, 20), (39, 20, 21),
       (22, 23, 21), (22, 5, 6), (6, 40, 7), (40, 40, 40), (40, 40, 40),
       (40, 40, 40));

--------------------------------------------------------------------------------
   function Retrieve_Terminator  (Block      : in Block_ID;
                                  Direction  : in Block_Polarity)
                                  return Terminator_Type is
   begin
      return Terminator_Array (Block, Direction).Terminator;
   end Retrieve_Terminator;
--------------------------------------------------------------------------------
   function Retrieve_Turnout_ID (Block     : in Block_ID;
                                 Direction : in Block_Polarity)
                                 return Turnout_ID is
   begin
      return Terminator_Array (Block, Direction).Turnout;
   end Retrieve_Turnout_ID;
--------------------------------------------------------------------------------
   function Retrieve_Next_Block_ID (Block      : in Block_ID;
                                    Direction  : in Block_Polarity)
                                    return Block_ID is
   begin
      return Terminator_Array (Block, Direction).Block;
   end Retrieve_Next_Block_ID;
--------------------------------------------------------------------------------
   function Retrieve_Turnout_Block_ID (Direction : in Turnout_Direction;
                                       Turnout   : in Turnout_ID)
                                       return Block_ID is
   begin
      return Turnout_Block_ID_Array (Turnout, Direction);
   end Retrieve_Turnout_Block_ID;
--------------------------------------------------------------------------------
   function Retrieve_Hall_ID (Current_Block   : in Block_ID;
                              Next_Block      : in Block_ID) return Hall_ID is
      ID : Hall_ID;
   begin
      ID := Hall_ID'First;
      loop
         exit when
            (Hall_ID_Array (ID, First) = Current_Block and
             Hall_ID_Array (ID, Second) = Next_Block) or
            (Hall_ID_Array (ID, First) = Next_Block and
             Hall_ID_Array (ID, Second) = Current_Block);
         ID := Hall_ID'Succ (ID);
      end loop;
      return ID;
   end Retrieve_Hall_ID;
--------------------------------------------------------------------------------
   function Is_Reversing_Point (Sensor : in Hall_ID) return Boolean is
   begin
      return Reversing_Point_Array (Sensor);
   end Is_Reversing_Point;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--ADDITIONAL LAYOUT FUNCTIONS---------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
   function Opposite (Direction : Block_Polarity) return Block_Polarity is
   begin
      if Direction = Normal then
         return Reversed;
      else
         return Normal;
      end if;
   end Opposite;
--------------------------------------------------------------------------------
   function Opposite (Direction : Turnout_Direction) return Turnout_Direction is
   begin
      if Direction = Left then
         return Right;
      else
         return Left;
      end if;
   end Opposite;
--------------------------------------------------------------------------------
   procedure Get_Block_IDs_With_Hall_Sensor (ID        : in Hall_ID;
                                             First_ID  : out Block_ID;
                                             Second_ID : out Block_ID) is
   begin
      First_ID := Hall_ID_Array (ID, First);
      Second_ID := Hall_ID_Array (ID, Second);
   end Get_Block_IDs_With_Hall_Sensor;
--------------------------------------------------------------------------------
   procedure Is_Forced_Turnout (ID        : in Block_ID;
                                Polarity  : in Block_Polarity;
                                Forced    : out Boolean) is

      Current_Hall_ID : Hall_ID;
      Next_Terminator : constant Terminator_Type := Retrieve_Terminator
         (Block     => ID,
          Direction => Polarity);
   begin
      if Next_Terminator = Block then

         Current_Hall_ID := Retrieve_Hall_ID
           (Current_Block => ID,
            Next_Block    => Retrieve_Next_Block_ID (Block     => ID,
                                                     Direction => Polarity));

         if Is_Reversing_Point (Current_Hall_ID) then
            if Retrieve_Terminator
              (Block     => Retrieve_Next_Block_ID (Block     => ID,
                                                    Direction => Polarity),
               Direction => Polarity) = Turnout then
               Forced := True;
            else
               Forced := False;
            end if;
         else
            if Retrieve_Terminator
              (Block     => Retrieve_Next_Block_ID (Block     => ID,
                                                    Direction => Polarity),
               Direction => Opposite (Polarity)) = Turnout then
               Forced := True;
            else
               Forced := False;
            end if;
         end if;
      else
         Forced := False;
      end if;
   end Is_Forced_Turnout;
--------------------------------------------------------------------------------
   procedure Set_Forced_Turnout (Block     : in  Block_ID;
                                 Polarity  : in  Block_Polarity;
                                 Turnout   : out Turnout_ID;
                                 Direction : out Turnout_Direction) is

      Next_Block : Block_ID;
      Current_Hall_ID : Hall_ID;

   begin
      Next_Block := Retrieve_Next_Block_ID (Block     => Block,
                                            Direction => Polarity);

      Current_Hall_ID := Retrieve_Hall_ID (Current_Block => Block,
                                           Next_Block    => Next_Block);

      if Is_Reversing_Point (Current_Hall_ID) then
         Turnout := Retrieve_Turnout_ID (Block     => Next_Block,
                                         Direction => Polarity);
         if Retrieve_Turnout_Block_ID (Direction => Left,
                                       Turnout   => Turnout) = Block then
            Direction := Left;
         else
            Direction := Right;
         end if;
      else
         Turnout := Retrieve_Turnout_ID (Block     => Next_Block,
                                         Direction => Opposite (Polarity));
         if Retrieve_Turnout_Block_ID (Direction => Left,
                                       Turnout   => Turnout) = Block then
            Direction := Left;
         else
            Direction := Right;
         end if;
      end if;
   end Set_Forced_Turnout;
--------------------------------------------------------------------------------
   function Is_Joint_Turnout (Direction    : in Turnout_Direction;
                              In_Turnout   : in Turnout_ID)
                              return Boolean is

      Current_Block_ID : Block_ID;
      Next_Turnout     : Turnout_ID;
      Left_Limb        : Block_ID;
      Right_Limb       : Block_ID;
      Next_Block_ID    : Block_ID;
      Is_Joint         : Boolean := False;

   begin
      Current_Block_ID := Retrieve_Turnout_Block_ID (Direction => Common,
                                                     Turnout   => In_Turnout);
      Next_Block_ID := Retrieve_Turnout_Block_ID (Direction => Direction,
                                                  Turnout   => In_Turnout);

      if Retrieve_Terminator (Block     => Next_Block_ID,
                              Direction => Normal) = Turnout then

         Next_Turnout := Retrieve_Turnout_ID (Block     => Next_Block_ID,
                                              Direction => Normal);
         Left_Limb := Retrieve_Turnout_Block_ID (Direction => Left,
                                                 Turnout   => Next_Turnout);
         Right_Limb := Retrieve_Turnout_Block_ID (Direction => Right,
                                                  Turnout   => Next_Turnout);

         if Left_Limb = Current_Block_ID or Right_Limb = Current_Block_ID then
            Is_Joint := True;
         end if;

      end if;

      if Retrieve_Terminator (Block     => Next_Block_ID,
                              Direction => Reversed) = Turnout
        and not Is_Joint then

         Next_Turnout := Retrieve_Turnout_ID (Block     => Next_Block_ID,
                                              Direction => Reversed);
         Left_Limb := Retrieve_Turnout_Block_ID (Direction => Left,
                                                 Turnout   => Next_Turnout);
         Right_Limb := Retrieve_Turnout_Block_ID (Direction => Right,
                                                  Turnout   => Next_Turnout);

         if Left_Limb = Current_Block_ID or Right_Limb = Current_Block_ID then
            Is_Joint := True;
         end if;

      end if;

      return Is_Joint;
   end Is_Joint_Turnout;
--------------------------------------------------------------------------------
   procedure Retrieve_Joint_Turnout (Direction    : in Turnout_Direction;
                                     In_Turnout   : in Turnout_ID;
                                     Joint        : out Turnout_ID) is

      Current_Block_ID : Block_ID;
      Left_Limb        : Block_ID;
      Right_Limb       : Block_ID;
      Next_Block_ID    : Block_ID;
      Is_Joint         : Boolean := False;

   begin
      Current_Block_ID := Retrieve_Turnout_Block_ID (Direction => Common,
                                                     Turnout   => In_Turnout);
      Next_Block_ID := Retrieve_Turnout_Block_ID (Direction => Direction,
                                                  Turnout   => In_Turnout);

      if Retrieve_Terminator (Block     => Next_Block_ID,
                              Direction => Normal) = Turnout then

         Joint := Retrieve_Turnout_ID (Block     => Next_Block_ID,
                                       Direction => Normal);
         Left_Limb := Retrieve_Turnout_Block_ID (Direction => Left,
                                                 Turnout   => Joint);
         Right_Limb := Retrieve_Turnout_Block_ID (Direction => Right,
                                                  Turnout   => Joint);

         if Left_Limb = Current_Block_ID or Right_Limb = Current_Block_ID then
            Is_Joint := True;
         end if;

      end if;

      if not Is_Joint then
         Joint := Retrieve_Turnout_ID (Block     => Next_Block_ID,
                                       Direction => Reversed);
      end if;

   end Retrieve_Joint_Turnout;
--------------------------------------------------------------------------------
   procedure Retrieve_Cross_Blocks (Block        : in Block_ID;
                                    Cross_Blocks : out Block_List) is
      Cross : Block_List (1);
   begin
      if Block = 23 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 39;
      elsif Block = 39 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 23;
      elsif Block = 8 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 29;
      elsif Block = 29 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 8;
      elsif Block = 30 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 24;
      elsif Block = 24 then
         Cross.Size := 1;
         Cross.Items (Cross.Size) := 30;
      else
         Cross.Size := 0;
      end if;
      Cross_Blocks := Cross;
   end Retrieve_Cross_Blocks;
--------------------------------------------------------------------------------
   procedure Next_Choice_Turnout (Block        : in Block_ID;
                                  Polarity     : in Block_Polarity;
                                  Out_Turnout  : out Turnout_ID) is

      Last_Temp_Block : Block_ID;
      Temp_Block      : Block_ID := Block;
      Temp_Polarity   : Block_Polarity := Polarity;
      Next_Terminator : Terminator_Type;
      Sensor          : Hall_ID;

   begin

      Next_Terminator := Retrieve_Terminator (Block     => Temp_Block,
                                              Direction => Temp_Polarity);
      loop
         exit when Next_Terminator = Turnout;

         Last_Temp_Block := Temp_Block;
         Temp_Block := Retrieve_Next_Block_ID (Block     => Temp_Block,
                                               Direction => Temp_Polarity);
         Sensor := Retrieve_Hall_ID (Current_Block => Last_Temp_Block,
                                     Next_Block    => Temp_Block);

         if Is_Reversing_Point (Sensor) then
            Temp_Polarity := Opposite (Temp_Polarity);
         end if;

         Next_Terminator := Retrieve_Terminator (Block     => Temp_Block,
                                                 Direction => Temp_Polarity);
      end loop;

      Out_Turnout := Retrieve_Turnout_ID (Block     => Temp_Block,
                                          Direction => Temp_Polarity);

   end Next_Choice_Turnout;
end Layout;
