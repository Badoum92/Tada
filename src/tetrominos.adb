with Tetris; use Tetris;
with Interfaces.C; use Interfaces.C;

package body Tetrominos is

   procedure Tetromino_Display (R : in out Renderer; T : Tetromino) is
   begin
      R.Set_Draw_Colour (T.Base.Col);
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            if T.Base.Rot (T.Rot) (Y * 4 + X + 1) = 'X' then
               R.Fill (Rectangle => (
                  int((T.X + X) * Block_Size + Outline_Size),
                  int((T.Y + Y) * Block_Size + Outline_Size),
                  Block_Size - Outline_Size * 2,
                  Block_Size - Outline_Size * 2
               ));
            end if;
         end loop;
      end loop;
   end Tetromino_Display;

   procedure Tetromino_Rotate (T : in out Tetromino) is
   begin
      if T.Rot = 4 then
         T.Rot := 1;
      else
         T.Rot := T.Rot + 1;
      end if;
   end Tetromino_Rotate;

end Tetrominos;
