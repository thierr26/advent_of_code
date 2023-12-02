with Ada.Command_Line,
     Ada.Text_IO,
     Day_02.Library;

procedure Day_02.Main is

   Success : Boolean;

   Input_File : Ada.Text_IO.File_Type;

   Result : Day_02.Library.Result_Type;

begin

   Library.Check_Argument_Count (Success);
   if not Success then
      goto Main_Exit;
   end if;

   Ada.Text_IO.Open (File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (1));

   Result := Library.File_Result (Input_File);

   Ada.Text_IO.Close (File => Input_File);

   Library.Output (Result);

   <<Main_Exit>>
   Ada.Command_Line.Set_Exit_Status (if Success then
                                        Ada.Command_Line.Success
                                     else
                                        Ada.Command_Line.Failure);

end Day_02.Main;
