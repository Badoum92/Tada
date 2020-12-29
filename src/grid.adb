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

   function Grid_Is_Line_Full (Line_Idx : Positive) return Boolean is
      Line : constant Grid_Line := Grid (Line_Idx);
   begin
      for I in Line'Range loop
         if not Line (I).Set then
            return False;
         end if;
      end loop;
      return True;
   end Grid_Is_Line_Full;

   procedure Grid_Remove_Line (Line_Idx : Positive) is
      First_Line : Grid_Line := Grid (Grid'First);
   begin
      if Line_Idx > 1 then
         for L in reverse 2 .. Line_Idx loop
            for I in Grid (L)'Range loop
               Grid (L) (I) := Grid (L - 1) (I);
            end loop;
         end loop;
      end if;

      for I in First_Line'Range loop
         First_Line (I).Set := False;
      end loop;
   end Grid_Remove_Line;

   function Grid_Remove_Full_Lines return Natural is
      Nb_Lines : Natural := 0;
   begin
      for L in Grid'Range loop
         if Grid_Is_Line_Full (L) then
            Grid_Remove_Line (L);
            Nb_Lines := Nb_Lines + 1;
         end if;
      end loop;
      return Nb_Lines;
   end Grid_Remove_Full_Lines;

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
