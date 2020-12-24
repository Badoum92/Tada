with Tetris;
with Interfaces.C; use Interfaces.C;

package body Tetrominos is

   procedure Tetromino_Display (R : in out Renderer; T : Tetromino) is
   begin
      R.Set_Draw_Colour (T.Base.Col);
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            if T.Base.Str (Y * 4 + X + 1) = 'X' then
               R.Fill (Rectangle => (
                  int((T.X + X) * Tetris.Block_Size),
                  int((T.Y + Y) * Tetris.Block_Size),
                  Tetris.Block_Size,
                  Tetris.Block_Size
               ));
            end if;
         end loop;
      end loop;
   end Tetromino_Display;

end Tetrominos;
