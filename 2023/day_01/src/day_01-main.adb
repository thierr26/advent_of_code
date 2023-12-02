with Ada.Command_Line,
     Ada.Text_IO,
     Day_01.Library;

procedure Day_01.Main is

   Success : Boolean;

   Input_File : Ada.Text_IO.File_Type;

   Ignore_Spelled : Boolean;

begin

   Library.Check_Argument_Count (Success);
   if not Success then
      goto Main_Exit;
   end if;

   Library.Check_Argument_1 (Success,
                             Ignore_Spelled);
   if not Success then
      goto Main_Exit;
   end if;

   Ada.Text_IO.Open (File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (2));

   Ada.Text_IO.Put_Line (Library.Literal_Calibration_Values_Sum
                           (Input_File,
                            Ignore_Spelled));

   Ada.Text_IO.Close (File => Input_File);

   <<Main_Exit>>
   Ada.Command_Line.Set_Exit_Status (if Success then
                                        Ada.Command_Line.Success
                                     else
                                        Ada.Command_Line.Failure);

end Day_01.Main;
