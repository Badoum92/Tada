with Ada.Numerics.Discrete_Random;

package body Game is

   function Get_Random_Piece return Tetromino_Base is
      package Rand is new Ada.Numerics.Discrete_Random (Tetrominos_Index);
      use Rand;
      G : Generator;
   begin
      Reset (G);
      return Tetromino_Bases (Random (G));
   end Get_Random_Piece;

   procedure Game_Spawn_Piece is
   begin
      Cur_Piece := (Base => Get_Random_Piece,
                    X => Grid_Width / 2 - 2,
                    Y => 0,
                    Rot => 1);
   end Game_Spawn_Piece;

   procedure Game_Init is
   begin
      Grid_Init;
      Game_Spawn_Piece;
      Total_Delay := 1000;
      Current_Delay := 0;
   end Game_Init;

   procedure Game_Handle_Input (Key : Scan_Codes) is
   begin
      if Key = Scan_Code_Left then
         Game_Move_Piece (Left);
      elsif Key = Scan_Code_Right then
         Game_Move_Piece (Right);
      elsif Key = Scan_Code_Down then
         Game_Move_Piece (Down);
         Current_Delay := 0;
      elsif Key = Scan_Code_Up then
         Game_Move_Piece (Rotate);
      end if;
   end Game_Handle_Input;

   procedure Game_Move_Piece (action : Piece_Action) is
      Old_X : constant Integer := Cur_Piece.X;
      Old_Y : constant Integer := Cur_Piece.Y;
      Old_Rot : constant Rotation_Index := Cur_Piece.Rot;
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
            Game_Spawn_Piece;
         end if;
      end if;
   end Game_Move_Piece;


   procedure Game_Display (R : in out Renderer) is
   begin
      Grid_Display (R);
      Tetromino_Display (R, Cur_Piece);
   end Game_Display;

   procedure Game_Update is
   begin
      Current_Delay := Current_Delay + Delta_Time;
      if Current_Delay >= Total_Delay then
         Game_Move_Piece (Down);
         if Current_Delay /= 0 then
            Current_Delay := Current_Delay - Total_Delay;
         end if;
      end if;
   end Game_Update;

end Game;
