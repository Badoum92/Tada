with Interfaces.C; use Interfaces.C;

package body Grid is

   function Grid_Block_Fits (Block : Character; X, Y : Integer) return Boolean is
   begin
      if Block = '.' then
         return True;
      end if;

      if X < 0 or else Y < 0 then
         return False;
      elsif X >= Grid_Width or else Y >= Grid_Height then
         return False;
      elsif Grid (Y + 1) (X + 1).Set then
         return False;
      end if;

      return True;
   end Grid_Block_Fits;

   function Grid_Piece_Fits (T : Tetromino) return Boolean is
      Block : Character;
   begin
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            Block := T.Base.Rot (T.Rot) (Y * 4 + X + 1);
            if not Grid_Block_Fits (Block, T.X + X, T.Y + Y) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Grid_Piece_Fits;


   procedure Grid_Init is
   begin
      Grid := (others => (others => (Set => False, Col => (0, 0, 0, 255))));
   end Grid_Init;

   procedure Grid_Lock_Piece (T : Tetromino) is
      Block : Character;
   begin
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            Block := T.Base.Rot (T.Rot) (Y * 4 + X + 1);
            if Block = 'X' then
               Grid (T.Y + Y + 1) (T.X + X + 1) := (True, T.Base.Col);
            end if;
         end loop;
      end loop;
   end Grid_Lock_Piece;

   procedure Grid_Display (R : in out Renderer) is
   begin
      for Y in Grid'Range loop
         for X in Grid (Y)'Range loop
            if Grid (Y) (X).Set then
               R.Set_Draw_Colour (Grid (Y) (X).Col);
               R.Fill (Rectangle => (
                  int((X - 1) * Block_Size + Outline_Size),
                  int((Y - 1) * Block_Size + Outline_Size),
                  Block_Size - Outline_Size * 2,
                  Block_Size - Outline_Size * 2
               ));
            end if;
         end loop;
         R.Set_Draw_Colour ((100, 100, 100, 255));
         R.Fill (Rectangle => (
            int(Grid (Y)'Last * Block_Size + Outline_Size),
            int((Y - 1) * Block_Size + Outline_Size),
            Block_Size - Outline_Size * 2,
            Block_Size - Outline_Size * 2
         ));
      end loop;
   end Grid_Display;

end Grid;
