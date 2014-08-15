--Locomotivation
--James Vannordstrand
with Common_Units;
package Controller is

   type Controller_ID is (A, B, C);
   type FB_Toggle_Type is (Backward, Forward);
   type LRC_Toggle_Type is (Left, Right, Center);
   type Button_Type is (Down, Up);

   procedure Get (Controller       : in Controller_ID;
                  Red_Button       : out Button_Type;
                  Black_Button     : out Button_Type;
                  Direction_Switch : out FB_Toggle_Type;
                  Turn_Switch      : out LRC_Toggle_Type);
   --This returns all buttons and switches
   --Preconditions  : None
   --Postconditions : Returns all buttons and switches

   procedure Get (Controller : in Controller_ID;
                  Knob       : out Common_Units.Percent);
   --This procedure gets the position of the knob
   --Preconditions  : None
   --Postconditions : Position of the knob is returned

private
   for FB_Toggle_Type use (Forward   => 2#1#,
                           Backward  => 2#0#);

   for LRC_Toggle_Type use (Left     => 2#01#,
                            Right    => 2#10#,
                            Center   => 2#11#);

   for Button_Type use (Down  => 2#0#,
                        Up    => 2#1#);

end Controller;
