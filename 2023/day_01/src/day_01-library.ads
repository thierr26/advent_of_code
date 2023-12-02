with Ada.Command_Line,
     Ada.Text_IO;

package Day_01.Library is

   procedure Check_Argument_Count (Success : out Boolean)
     with Post => Success xor Ada.Command_Line.Argument_Count /= 2;

   procedure Check_Argument_1 (Success,
                               Ignore_Spelled : out Boolean)
     with Pre => Ada.Command_Line.Argument_Count >= 1;

   function Literal_Calibration_Values_Sum
     (File           : Ada.Text_IO.File_Type;
      Ignore_Spelled : Boolean) return String
     with Pre => Ada.Text_IO.Is_Open (File);

end Day_01.Library;
