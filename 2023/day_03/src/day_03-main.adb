with Ada.Command_Line,
     Day_03.Library;

procedure Day_03.Main is

   Success : Boolean;

   Result : Library.Result_Type;

begin

   Library.Check_Argument_Count (Success);
   if not Success then
      goto Main_Exit;
   end if;

   Result := Library.File_Result (Ada.Command_Line.Argument (1));

   Library.Output (Result);

   <<Main_Exit>>
   Ada.Command_Line.Set_Exit_Status (if Success then
                                        Ada.Command_Line.Success
                                     else
                                        Ada.Command_Line.Failure);

end Day_03.Main;
