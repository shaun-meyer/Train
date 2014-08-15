--Locomotivation
--Shaun Meyer

package Common_Units is

   type Bit_Value is range 0 .. 4095;
   type Volts is delta 1.0 / 2.0**12 range -45_000.0 .. 45_000.0;

   subtype Percent is Integer range 0 .. 100;
end Common_Units;
