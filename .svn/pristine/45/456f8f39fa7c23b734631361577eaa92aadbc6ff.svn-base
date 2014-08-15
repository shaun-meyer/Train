-- Locomotivation
-- Shaun Meyer

with Controller;
with Trains;
with Common_Units;
package Engineers.Functions is
   procedure Enable_Engineer (Engineer     : in Engineer_ID;
                              Train        : in Trains.Train_ID;
                              Min_Throttle : in Common_Units.Percent;
                              Control_ID   : in Controller.Controller_ID);
   -- This procedure will enable an engineer task, and assign it a Train and
   --   a Controller
   -- Precondition  : Engineer is currently disabled
   -- Postcondition : Engineer is enabled

   procedure Disable_Engineer (Engineer  : in Engineer_ID);
   -- This procedure will disable an engineer task
   -- Precondition  : Engineer is enabled
   -- Postcondition : Engineer is disabled

   procedure Toggle_Skill (Engineer : in Engineer_ID);
   -- This procedure will toggle skill level between expert and novice
   -- Precondition  : None
   -- Postcondition : Skill level is the opposite of what it was before

   function Get_Skill (Engineer : in Engineer_ID) return Skill_Level;
   -- This procedure will return an engineer's skill level
   -- Precondition  : None
   -- Postcondition : Returns skill level

end Engineers.Functions;
