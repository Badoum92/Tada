with Ada.Numerics.Discrete_Random;

with Ada.Text_IO; use Ada.Text_IO;

package body Game is

   function Game_Get_Random_Piece return Tetromino_Base is
      package Rand is new Ada.Numerics.Discrete_Random (Tetrominos_Index);
      use Rand;
      G : Generator;
   begin
      Reset (G);
      return Tetromino_Bases (Random (G));
   end Game_Get_Random_Piece;

   procedure Game_Spawn_Piece is
   begin
      Cur_Piece := Next_Piece;
      Cur_Piece.X := Grid_Width / 2 - 2;
      Cur_Piece.Y := 0;

      Next_Piece := (Base => Game_Get_Random_Piece,
                    X => Grid_Width + 2,
                    Y => 2,
                    Rot => 1);
   end Game_Spawn_Piece;

   procedure Game_Reset is
   begin
      Next_Piece := (Base => Game_Get_Random_Piece,
                    X => Grid_Width + 2,
                    Y => 2,
                    Rot => 1);
      Game_Init;
   end Game_Reset;

   procedure Game_Init is
   begin
      Grid_Init;
      Game_Spawn_Piece;
      Level := 1;
      Score := 0;
      Cur_Lines := 0;
      Total_Delay := 1000;
      Current_Delay := 0;
   end Game_Init;

   procedure Game_Handle_Input (Key : Scan_Codes) is
   begin
      if Key = Scan_Code_Left then
         Game_Move_Piece_Proc (Left);
      elsif Key = Scan_Code_Right then
         Game_Move_Piece_Proc (Right);
      elsif Key = Scan_Code_Down then
         Game_Move_Piece_Proc (Down);
      elsif Key = Scan_Code_Up then
         Game_Move_Piece_Proc (Rotate);
      elsif Key = Scan_Code_Space then
         while Game_Move_Piece (Down) loop
            null;
         end loop;
      end if;
   end Game_Handle_Input;

   function Game_Move_Piece (action : Piece_Action) return Boolean is
      Old_X : constant Integer := Cur_Piece.X;
      Old_Y : constant Integer := Cur_Piece.Y;
      Old_Rot : constant Rotation_Index := Cur_Piece.Rot;
      Nb_Lines : Natural := 0;
   begin
      if action = Left then
         Cur_Piece.X := Cur_Piece.X - 1;
      elsif action = Right then
         Cur_Piece.X := Cur_Piece.X + 1;
      elsif action = Down then
         Cur_Piece.Y := Cur_Piece.Y + 1;
      elsif action = Rotate then
         Tetromino_Rotate (Cur_Piece);
      end if;

      if not Grid_Piece_Fits (Cur_Piece) then
         Cur_Piece.X := Old_X;
         Cur_Piece.Y := Old_Y;
         Cur_Piece.Rot := Old_Rot;

         if action = Down then
            Grid_Lock_Piece (Cur_Piece);
            Nb_Lines := Grid_Remove_Full_Lines;
            Cur_Lines := Cur_Lines + Nb_Lines;
            Game_Update_Score (Nb_Lines);
            Game_Spawn_Piece;
            return False;
         end if;
      end if;

      return True;
   end Game_Move_Piece;

   procedure Game_Move_Piece_Proc (action : Piece_Action) is
      Status : Boolean := False;
   begin
      Status := Game_Move_Piece (action);
      pragma Unreferenced (Status);
   end Game_Move_Piece_Proc;

   procedure Game_Update_Score (Nb_Lines : Natural) is
   begin
      if Nb_Lines = 1 then
         Score := Score + 100 * Level;
      elsif Nb_Lines = 2 then
         Score := Score + 300 * Level;
      elsif Nb_Lines = 3 then
         Score := Score + 500 * Level;
      elsif Nb_Lines = 4 then
         Score := Score + 800 * Level;
      end if;
   end Game_Update_Score;

   procedure Game_Display (R : in out Renderer) is
   begin
      Grid_Display (R);
      Tetromino_Display (R, Cur_Piece);
      Tetromino_Display (R, Next_Piece);
   end Game_Display;

   procedure Game_Speed_Up is
      Speed_Up_Factor : Uint64 := 275;
   begin
      if Cur_Lines >= Lines_To_Next_Level then
         Cur_Lines := Cur_Lines - Lines_To_Next_Level;
         Speed_Up_Factor := Speed_Up_Factor / (Level + 1);
         if Total_Delay > Speed_Up_Factor then
            Level := Level + 1;
            Total_Delay := Total_Delay - Speed_Up_Factor;
            Put_Line (Uint64'Image (Total_Delay));
         end if;
      end if;
   end Game_Speed_Up;

   procedure Game_Update is
   begin
      Current_Delay := Current_Delay + Delta_Time;
      if Current_Delay >= Total_Delay then
         Game_Move_Piece_Proc (Down);
         if Current_Delay /= 0 then
            Current_Delay := Current_Delay - Total_Delay;
         end if;
         Game_Speed_Up;
      end if;
   end Game_Update;

end Game;
