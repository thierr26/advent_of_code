with Ada.Assertions,
     Ada.Strings.Maps,
     Ada.Strings.Fixed;

package body Day_02.Library is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   type Gama_Data_Type is record

      Possible : Boolean;

      Minimum_Set_Power : Natural;

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

   function Game_Data (Line : String;
                       ID   : Positive) return Gama_Data_Type is

      type Cube_Kind is (Red, Green, Blue);

      type Cube_Set is array (Cube_Kind) of Natural;

      Set,
      Minimum_Set : Cube_Set := (others => 0);

      package Cube_Kind_IO
        is new Ada.Text_IO.Enumeration_IO (Enum => Cube_Kind);

      Separators : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set (",;");

      Letters : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set (Span => (Low  => 'a',
                                             High => 'z'));

      Literal_ID : constant String := Literal (ID);

      Line_Head : constant String := "Game " & Literal_ID & ':';

      From_For_Separator_Search : Positive := Line'First + Line_Head'Length;
      From_For_Letter_Search    : Positive;

      ------------------------------------------------

      function Separator_Index return Natural
        is (Ada.Strings.Fixed.Index (Source => Line,
                                     Set    => Separators,
                                     From   => From_For_Separator_Search));

      ------------------------------------------------

      function Letter_Index return Natural
        is (Ada.Strings.Fixed.Index (Source => Line,
                                     Set    => Letters,
                                     From   => From_For_Letter_Search));

      ------------------------------------------------

      function Power (Set : Cube_Set) return Natural is

         Ret : Natural := 1;

      begin

         for E of Set loop

            Ret := Ret * E;

         end loop;

         return Ret;

      end Power;

      ------------------------------------------------

      Max_Set : constant Cube_Set := (Red   => 12,
                                      Green => 13,
                                      Blue  => 14);

      Ret : Gama_Data_Type := (Possible => True,
                               others   => <>);

   begin

      Ada.Assertions.Assert
        (Ada.Strings.Fixed.Head (Source => Line,
                                 Count  => Line_Head'Length) = Line_Head);

      loop

         declare

            SI : constant Natural := Separator_Index;

            Last_Iteration : constant Boolean := SI = 0;

            Separator_Is_Kind_Separator : constant Boolean
              := not Last_Iteration
                   and then
                 Line (SI) = ',';

            Count : Natural;

            Kind : Cube_Kind;

            Kind_Last : Positive;

         begin

            Natural_IO.Get (From => Line (From_For_Separator_Search
                                            ..
                                          Line'Last),
                            Item => Count,
                            Last => From_For_Letter_Search);

            Cube_Kind_IO.Get (From => Line (Letter_Index .. Line'Last),
                              Item => Kind,
                              Last => Kind_Last);

            Set (Kind) := Count;

            if not Separator_Is_Kind_Separator then

               for K in Cube_Kind loop
                  if Set (K) > Minimum_Set (K) then
                     Minimum_Set (K) := Set (K);
                  end if;
               end loop;

               Ret.Possible
                 := Ret.Possible
                      and then
                    (for all K in Cube_Kind => Set (K) <= Max_Set (K));

               Set         := (others => 0);

            end if;

            exit when Last_Iteration;

            From_For_Separator_Search := SI + 1;

         end;

      end loop;

      Ret.Minimum_Set_Power := Power (Minimum_Set);

      return Ret;

   end Game_Data;

   ----------------------------------------------------------------------------

   function File_Result (File : Ada.Text_IO.File_Type) return Result_Type is

      Game_ID : Natural := 0;

      Game_Data : Gama_Data_Type;

      Ret : Result_Type := (Possible_Games_ID_Sum  => 0,
                            Minimum_Set_Powers_Sum => 0);

   begin

      while not Ada.Text_IO.End_Of_File (File) loop

         Game_ID := Game_ID + 1;

         Game_Data := Library.Game_Data (Ada.Text_IO.Get_Line (File), Game_ID);

         if Game_Data.Possible then
            Ret.Possible_Games_ID_Sum := Ret.Possible_Games_ID_Sum + Game_ID;
         end if;

         Ret.Minimum_Set_Powers_Sum := Ret.Minimum_Set_Powers_Sum
                                         +
                                       Game_Data.Minimum_Set_Power;

      end loop;

      return Ret;

   end File_Result;

   ----------------------------------------------------------------------------

   procedure Output (Result : Result_Type) is

   begin

      Ada.Text_IO.Put_Line (Literal (Result.Possible_Games_ID_Sum));
      Ada.Text_IO.Put_Line (Literal (Result.Minimum_Set_Powers_Sum));

   end Output;

   ----------------------------------------------------------------------------

end Day_02.Library;
