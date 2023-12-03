with Ada.Assertions,
     Ada.Text_IO,
     Ada.Directories,
     Ada.Direct_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed;

package body Day_03.Library is

   use type Ada.Directories.File_Size;

   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   type File_Structure_Type is record

      Byte_Length,
      Text_Width,
      Line_Count,
      EOL_Width   : Ada.Directories.File_Size;

   end record;

   ----------------------------------------------------------------------------

   procedure Error (Message : String) is

   begin

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: " & Message);

   end Error;

   ----------------------------------------------------------------------------

   procedure Check_Argument_Count (Success : out Boolean) is

   begin

      Success := True;

      if Ada.Command_Line.Argument_Count /= 1 then
         Error ("Exactly one command-line argument expected");
         Success := False;
      end if;

   end Check_Argument_Count;

   ----------------------------------------------------------------------------

   function Literal (N : Natural) return String is

      Raw_Literal : String (1 .. Natural'Width);

   begin

      Natural_IO.Put (To   => Raw_Literal,
                      Item => N);

      return Ada.Strings.Fixed.Trim (Source => Raw_Literal,
                                     Side   => Ada.Strings.Left);

   end Literal;

   ----------------------------------------------------------------------------

   function File_Structure (File_Name : String) return File_Structure_Type is

      Text_File : Ada.Text_IO.File_Type;

      Ret : File_Structure_Type
        := (Byte_Length => Ada.Directories.Size (File_Name),
            others      => <>);

      Line_Byte_Length,
      Remainder        : Ada.Directories.File_Size;

   begin

      Ada.Text_IO.Open (File => Text_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);

      declare

         function Is_Graphic (Item : Character) return Boolean
           renames Ada.Characters.Handling.Is_Graphic;

         First_Line : constant String := Ada.Text_IO.Get_Line (Text_File);

         type Buffer_Type
           is array (Ada.Directories.File_Size'(1) .. First_Line'Length + 3)
           of Character;

         package IO is new Ada.Direct_IO (Element_Type => Buffer_Type);

         File : IO.File_Type;

         Buffer : Buffer_Type;

      begin

         Ada.Text_IO.Close (File => Text_File);

         Ret.Text_Width := First_Line'Length;

         IO.Open (File => File,
                  Mode => IO.In_File,
                  Name => File_Name);

         IO.Read (File => File,
                  Item => Buffer);

         IO.Close (File => File);

         Ada.Assertions.Assert (not Is_Graphic (Buffer (Ret.Text_Width + 1)));
         Ada.Assertions.Assert (Is_Graphic (Buffer (Ret.Text_Width + 3)));

         Ret.EOL_Width := (if Is_Graphic (Buffer (Ret.Text_Width + 2)) then
                              1
                           else
                              2);

      end;

      Line_Byte_Length := Ret.Text_Width + Ret.EOL_Width;
      Remainder        := Ret.Byte_Length
                            mod
                          Line_Byte_Length;

      Ada.Assertions.Assert (Remainder = 0);

      Ret.Line_Count := Ret.Byte_Length / Line_Byte_Length;

      return Ret;

   end File_Structure;

   ----------------------------------------------------------------------------

   function File_Result (Schematic_File_Name : String) return Result_Type is

      type Offset is (Minus_One, Zero, Plus_One);

      S : constant File_Structure_Type := File_Structure (Schematic_File_Name);

      subtype L_Index is Ada.Directories.File_Size range 1 .. S.Line_Count;

      subtype C_Index
        is Ada.Directories.File_Size range 1 .. S.Text_Width + S.EOL_Width;

      type Table is array (L_Index, C_Index) of Character;

      type Gear_1 is array (L_Index, C_Index) of Integer;

      package IO is new Ada.Direct_IO (Element_Type => Table);

      File : IO.File_Type;

      T : Table;

      G_1 : Gear_1 := (others => (others => -1));

      In_Number,
      In_Part_Number,
      G_1_Assignment_Required,
      Gear_Ration_Computation_Required : Boolean := False;

      G_1_K_L,
      G_1_K_C : Ada.Directories.File_Size;

      function Is_Graphic (Item : Character) return Boolean
        renames Ada.Characters.Handling.Is_Graphic;

      function Is_Digit (Item : Character) return Boolean
        renames Ada.Characters.Handling.Is_Decimal_Digit;

      ------------------------------------------------

      function Is_Symbol (Item : Character) return Boolean
        is (Is_Graphic (Item)
              and then
            not Is_Digit (Item)
              and then
            Item /= '.');

      ------------------------------------------------

      procedure Check_Symbol_At (K_L,
                                 K_C : Ada.Directories.File_Size) is

      begin

         In_Part_Number := In_Part_Number
                             or else
                           Is_Symbol (T (K_L, K_C));

         if T (K_L, K_C) = '*' then
            if G_1 (K_L, K_C) = -1 then
               G_1_Assignment_Required := True;
            else
               Gear_Ration_Computation_Required := True;
            end if;
            G_1_K_L := K_L;
            G_1_K_C := K_C;
         end if;

      end Check_Symbol_At;

      ------------------------------------------------

      procedure Check_Symbol_At (K_L,
                                 K_C      : Ada.Directories.File_Size;
                                 L_Offset,
                                 C_Offset : Offset) is

         K_L_Offsetted : Ada.Directories.File_Size;

      begin

         case L_Offset is

            when Minus_One =>

               if K_L = T'First (1) then
                  return;
               end if;
               K_L_Offsetted := K_L - 1;

            when Zero      =>

               K_L_Offsetted := K_L;

            when Plus_One  =>

               if K_L = T'Last (1) then
                  return;
               end if;
               K_L_Offsetted := K_L + 1;

         end case;

         case C_Offset is

            when Minus_One =>

               if K_C > T'First (2) then
                  Check_Symbol_At (K_L_Offsetted, K_C - 1);
               end if;

            when Zero      =>

               Check_Symbol_At (K_L_Offsetted, K_C);

            when Plus_One  =>

               if K_C < T'Last (2) then
                  Check_Symbol_At (K_L_Offsetted, K_C + 1);
               end if;

         end case;

      end Check_Symbol_At;

      ------------------------------------------------

      procedure Check_Previous_Column (K_L,
                                       K_C : Ada.Directories.File_Size) is

      begin

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Minus_One,
                          C_Offset => Minus_One);

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Zero,
                          C_Offset => Minus_One);

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Plus_One,
                          C_Offset => Minus_One);

      end Check_Previous_Column;

      ------------------------------------------------

      procedure Check_Current_Column (K_L,
                                      K_C : Ada.Directories.File_Size) is

      begin

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Minus_One,
                          C_Offset => Zero);

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Zero,
                          C_Offset => Zero);

         Check_Symbol_At (K_L      => K_L,
                          K_C      => K_C,
                          L_Offset => Plus_One,
                          C_Offset => Zero);

      end Check_Current_Column;

      ------------------------------------------------

      Part_Number_K_C_First,
      Part_Number_K_C_Last  : Positive;

      Part_Number : Natural;

      Ret : Result_Type := (Part_Numbers_Sum => 0,
                            Gear_Ratios_Sum  => 0);

   begin

      Ada.Assertions.Assert (Table'Size = 8 * S.Byte_Length);

      IO.Open (File => File,
               Mode => IO.In_File,
               Name => Schematic_File_Name);

      IO.Read (File => File,
               Item => T);

      IO.Close (File => File);

      for K_L in T'Range (1) loop

         for K_C in T'Range (2) loop

            if Is_Digit (T (K_L, K_C)) then

               if In_Number then

                  Check_Current_Column (K_L, K_C);

               else

                  In_Number := True;

                  Part_Number_K_C_First := Positive (K_C);

                  Check_Previous_Column (K_L, K_C);
                  Check_Current_Column (K_L, K_C);

               end if;

            else

               if In_Number then

                  Check_Current_Column (K_L, K_C);

                  if In_Part_Number then

                     declare
                        Part_Number_Literal : String (Part_Number_K_C_First
                                                        ..
                                                      Positive (K_C) - 1);
                     begin
                        for K in Part_Number_Literal'Range loop
                           Part_Number_Literal (K)
                             := T (K_L, Ada.Directories.File_Size (K));
                        end loop;
                        Natural_IO.Get (From => Part_Number_Literal,
                                        Item => Part_Number,
                                        Last => Part_Number_K_C_Last);
                     end;

                     Ret.Part_Numbers_Sum := Ret.Part_Numbers_Sum
                                               +
                                             Part_Number;

                     if G_1_Assignment_Required then

                        G_1 (G_1_K_L, G_1_K_C)  := Part_Number;
                        G_1_Assignment_Required := False;

                     elsif Gear_Ration_Computation_Required then

                        Ret.Gear_Ratios_Sum := Ret.Gear_Ratios_Sum
                                                 +
                                               G_1 (G_1_K_L, G_1_K_C)
                                                 *
                                               Part_Number;
                        Gear_Ration_Computation_Required := False;

                     end if;

                  end if;

                  In_Part_Number := False;
                  In_Number      := False;

               end if;

            end if;

         end loop;

      end loop;

      return Ret;

   end File_Result;

   ----------------------------------------------------------------------------

   procedure Output (Result : Result_Type) is

   begin

      Ada.Text_IO.Put_Line (Literal (Result.Part_Numbers_Sum));
      Ada.Text_IO.Put_Line (Literal (Result.Gear_Ratios_Sum));

   end Output;

   ----------------------------------------------------------------------------

end Day_03.Library;
