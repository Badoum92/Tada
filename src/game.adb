with Ada.Numerics.Discrete_Random;

package body Game is

   procedure Game_Init is
      package Rand is new Ada.Numerics.Discrete_Random (Tetrominos_Index);
      use Rand;
      G : Generator;
   begin
      Grid_Init;
      Reset (G);
      Cur_Piece := (Base => Tetromino_Bases (Random (G)),
                    X => Grid_Width / 2 - 2,
                    Y => 0,
                    Rot => 1);
   end Game_Init;

   procedure Game_Handle_Input (Key : Scan_Codes) is
      Old_X : constant Integer := Cur_Piece.X;
      Old_Y : constant Integer := Cur_Piece.Y;
   begin
      if Key = Scan_Code_Left then
         Cur_Piece.X := Cur_Piece.X - 1;
      elsif Key = Scan_Code_Right then
         Cur_Piece.X := Cur_Piece.X + 1;
      end if;

      if not Grid_Piece_Fits (Cur_Piece) then
         Cur_Piece.X := Old_X;
         Cur_Piece.Y := Old_Y;
      end if;
   end Game_Handle_Input;


   procedure Game_Display (R : in out Renderer) is
   begin
      Grid_Display (R);
      Tetromino_Display (R, Cur_Piece);
   end Game_Display;

end Game;
