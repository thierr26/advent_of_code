with Ada.Command_Line,
     Ada.Text_IO;

package Day_02.Library is

   type Result_Type is record

      Possible_Games_ID_Sum,
      Minimum_Set_Powers_Sum : Natural;

   end record;

   procedure Check_Argument_Count (Success : out Boolean)
     with Post => Success xor Ada.Command_Line.Argument_Count /= 1;

   function File_Result (File : Ada.Text_IO.File_Type) return Result_Type
     with Pre => Ada.Text_IO.Is_Open (File);

   procedure Output (Result : Result_Type);

end Day_02.Library;
