with Layout;
package Motors is

   pragma Elaborate_Body;
   -- Written by John McCormick, February 2002
   -- Additional documentation, May 2008

   -- Assumption for Layout.Turn_Choice
   --     Left is stored as 0
   --     Right is stored as 1
   type State is (Left, Right);

   ----------------------------------------------------------------------------
   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in State);
   -- Set the direction of the motor for the given turnout ID
   --
   -- Preconditions  : None
   --
   -- Postconditions : Motor is set to the given direction


   ----------------------------------------------------------------------------
   function In_Position (Motor : in Layout.Turnout_ID) return Boolean;
   -- Checks the status of the turnout
   --
   -- Preconditions  : None
   --
   -- Postconditions : Returns True if the motor has reached the position
   --                  requested in the last call to Set
private
   for State use (Left => 2#0#,
                  Right  => 2#1#);
end Motors;
