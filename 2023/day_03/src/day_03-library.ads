with Ada.Command_Line;

package Day_03.Library is

   type Result_Type is record

      Part_Numbers_Sum,
      Gear_Ratios_Sum : Natural;

   end record;

   procedure Check_Argument_Count (Success : out Boolean)
     with Post => Success xor Ada.Command_Line.Argument_Count /= 1;

   function File_Result (Schematic_File_Name : String) return Result_Type;

   procedure Output (Result : Result_Type);

end Day_03.Library;
