with Ada.Numerics.Discrete_Random;
with Interfaces.C; use Interfaces.C;
with Grid;

package body Tetromino is

   function Get_Random_Piece return Tetromino_Base is
      package Rand is new Ada.Numerics.Discrete_Random (Tetrominos_Index);
      use Rand;
      G : Generator;
   begin
      Reset (G);
      return Tetromino_Bases (Random (G));
   end Get_Random_Piece;

   procedure Display (T : Tetromino_T; R : in out Renderer) is
   begin
      R.Set_Draw_Colour (T.Base.Col);
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            if T.Base.Rot (T.Rot) (Y * 4 + X + 1) = 'X' then
               R.Fill (Rectangle => (
                  int((T.X + X) * Grid.Block_Size + Grid.Outline_Size),
                  int((T.Y + Y) * Grid.Block_Size + Grid.Outline_Size),
                  Grid.Block_Size - Grid.Outline_Size * 2,
                  Grid.Block_Size - Grid.Outline_Size * 2
               ));
            end if;
         end loop;
      end loop;
   end Display;

   procedure Rotate (T : in out Tetromino_T) is
   begin
      if T.Rot = 4 then
         T.Rot := 1;
      else
         T.Rot := T.Rot + 1;
      end if;
   end Rotate;

end Tetromino;
