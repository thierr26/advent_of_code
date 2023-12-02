with Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Strings.Bounded;

package body Day_01.Library is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   subtype Character_1_To_9 is Character range '1' .. '9';

   package B is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 5);

   type Spelled_Digit_Array is array (Character_1_To_9) of B.Bounded_String;

   A : constant Spelled_Digit_Array
     := ('1' => B.To_Bounded_String ("one"),
         '2' => B.To_Bounded_String ("two"),
         '3' => B.To_Bounded_String ("three"),
         '4' => B.To_Bounded_String ("four"),
         '5' => B.To_Bounded_String ("five"),
         '6' => B.To_Bounded_String ("six"),
         '7' => B.To_Bounded_String ("seven"),
         '8' => B.To_Bounded_String ("eight"),
         '9' => B.To_Bounded_String ("nine"));

   ----------------------------------------------------------------------------

   procedure Error (Message : String) is

   begin

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: " & Message);

   end Error;

   ----------------------------------------------------------------------------

   procedure Check_Argument_Count (Success : out Boolean) is

   begin

      Success := True;

      if Ada.Command_Line.Argument_Count /= 2 then
         Error ("Exactly two command-line arguments expected");
         Success := False;
      end if;

   end Check_Argument_Count;

   ----------------------------------------------------------------------------

   procedure Check_Argument_1 (Success,
                               Ignore_Spelled : out Boolean) is

   begin

      if Ada.Command_Line.Argument (1) = "part_1" then
         Success        := True;
         Ignore_Spelled := True;
      elsif Ada.Command_Line.Argument (1) = "part_2" then
         Success        := True;
         Ignore_Spelled := False;
      else
         Success := False;
         Error ("Argument 1 must be ""part_1"" or ""part_2""");
      end if;

   end Check_Argument_1;

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

   function Is_Spelled_Digit (S : String) return Boolean
     is (for some E of A => B.To_String (E) = S);

   ----------------------------------------------------------------------------

   function To_Character (Spelled_Digit : String) return Character_1_To_9 is

   begin

      for C in A'Range loop

         if B.To_String (A (C)) = Spelled_Digit then
            return C;
         end if;

      end loop;

      return Character_1_To_9'First; --  Won't happen.

   end To_Character;

   ----------------------------------------------------------------------------

   function To_Character
     (Line                   : String;
      Index,
      Digit_Character_Length : Positive) return Character_1_To_9 is

      S : constant String := Line (Index
                                     ..
                                   Index - 1 + Digit_Character_Length);

   begin

      return (if Is_Spelled_Digit (S) then
                 To_Character (S)
              else
                 S (S'First));

   end To_Character;

   ----------------------------------------------------------------------------

   procedure Check_Digit_Presence_At_Index
     (Line                   :     String;
      Index                  :     Positive;
      Ignore_Spelled         :     Boolean;
      Digit_Presence         : out Boolean;
      Digit_Character_Length : out Positive) is

   begin

      if Ada.Characters.Handling.Is_Decimal_Digit (Line (Index)) then
         Digit_Presence         := True;
         Digit_Character_Length := 1;
         return;
      end if;

      if not Ignore_Spelled then

         for C in Character_1_To_9 loop

            declare

               Spelled : constant String   := B.To_String (A (C));
               Length  : constant Positive := B.Length (A (C));
               LM1     : constant Natural  := Length - 1;

            begin

               if Line'Last - Index >= LM1
                    and then
                  Line (Index .. Index + LM1) = Spelled
               then
                  Digit_Presence         := True;
                  Digit_Character_Length := Length;
                  return;
               end if;

            end;

         end loop;

      end if;

      Digit_Presence         := False;
      Digit_Character_Length := 1;

   end Check_Digit_Presence_At_Index;

   ----------------------------------------------------------------------------

   function Calibration_Value (Line           : String;
                               Ignore_Spelled : Boolean) return Natural is

      K : Positive := Line'First;

      Digit_Presence : Boolean;

      Digit_Character_Length : Positive;

      First_Digit,
      Last_Digit  : Character_1_To_9;

      Last : Positive;

      Ret : Natural;

   begin

      loop

         Check_Digit_Presence_At_Index
           (Line                   => Line,
            Index                  => K,
            Ignore_Spelled         => Ignore_Spelled,
            Digit_Presence         => Digit_Presence,
            Digit_Character_Length => Digit_Character_Length);

         exit when Digit_Presence;

         K := K + 1;

      end loop;

      First_Digit := To_Character (Line,
                                   K,
                                   Digit_Character_Length);
      Last_Digit  := First_Digit;
      K           := K + Digit_Character_Length;

      while K <= Line'Last loop

         Check_Digit_Presence_At_Index
           (Line                   => Line,
            Index                  => K,
            Ignore_Spelled         => Ignore_Spelled,
            Digit_Presence         => Digit_Presence,
            Digit_Character_Length => Digit_Character_Length);

         if Digit_Presence then
            Last_Digit := To_Character (Line,
                                        K,
                                        Digit_Character_Length);
         end if;

         K := K + 1;

      end loop;

      Natural_IO.Get (From => First_Digit & Last_Digit,
                      Item => Ret,
                      Last => Last);

      return Ret;

   end Calibration_Value;

   ----------------------------------------------------------------------------

   function Calibration_Values_Sum (File           : Ada.Text_IO.File_Type;
                                    Ignore_Spelled : Boolean) return Natural is

      Ret : Natural := 0;

   begin

      while not Ada.Text_IO.End_Of_File (File) loop

         Ret := Ret
                  +
                Calibration_Value (Ada.Text_IO.Get_Line (File),
                                   Ignore_Spelled);

      end loop;

      return Ret;

   end Calibration_Values_Sum;

   ----------------------------------------------------------------------------

   function Literal_Calibration_Values_Sum
     (File           : Ada.Text_IO.File_Type;
      Ignore_Spelled : Boolean) return String
     is (Literal (Calibration_Values_Sum (File, Ignore_Spelled)));

   ----------------------------------------------------------------------------

end Day_01.Library;
