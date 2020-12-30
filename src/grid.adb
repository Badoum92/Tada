with Interfaces.C; use Interfaces.C;

package body Grid is

   function Block_Fits (G : Grid_T; Block : Character; X, Y : Integer)
      return Boolean is
   begin
      if Block = '.' then
         return True;
      end if;

      if X < 0 or else Y < 0 then
         return False;
      elsif X >= Grid_Width or else Y >= Grid_Height then
         return False;
      elsif G (Y + 1) (X + 1).Set then
         return False;
      end if;

      return True;
   end Block_Fits;

   function Piece_Fits (G : Grid_T; T : Tetromino.Tetromino_T) return Boolean is
      Block : Character;
   begin
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            Block := T.Base.Rot (T.Rot) (Y * 4 + X + 1);
            if not Block_Fits (G, Block, T.X + X, T.Y + Y) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Piece_Fits;

   function Is_Line_Full (G : Grid_T; Line_Idx : Positive) return Boolean is
      Line : constant Grid_Line := G (Line_Idx);
   begin
      for I in Line'Range loop
         if not Line (I).Set then
            return False;
         end if;
      end loop;
      return True;
   end Is_Line_Full;

   procedure Remove_Line (G : in out Grid_T; Line_Idx : Positive) is
      First_Line : Grid_Line := G (G'First);
   begin
      if Line_Idx > 1 then
         for L in reverse 2 .. Line_Idx loop
            for I in G (L)'Range loop
               G (L) (I) := G (L - 1) (I);
            end loop;
         end loop;
      end if;

      for I in First_Line'Range loop
         First_Line (I).Set := False;
      end loop;
   end Remove_Line;

   function Remove_Full_Lines (G : in out Grid_T) return Natural is
      Nb_Lines : Natural := 0;
   begin
      for L in G'Range loop
         if Is_Line_Full (G, L) then
            Remove_Line (G, L);
            Nb_Lines := Nb_Lines + 1;
         end if;
      end loop;
      return Nb_Lines;
   end Remove_Full_Lines;

   procedure Init (G : in out Grid_T) is
   begin
      G := (others => (others => (Set => False, Col => (0, 0, 0, 255))));
   end Init;

   procedure Lock_Piece (G : in out Grid_T; T : Tetromino.Tetromino_T) is
      Block : Character;
   begin
      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            Block := T.Base.Rot (T.Rot) (Y * 4 + X + 1);
            if Block = 'X' then
               G (T.Y + Y + 1) (T.X + X + 1) := (True, T.Base.Col);
            end if;
         end loop;
      end loop;
   end Lock_Piece;

   procedure Display (G : Grid_T; R : in out Renderer) is
   begin
      for Y in G'Range loop
         for X in G (Y)'Range loop
            if G (Y) (X).Set then
               R.Set_Draw_Colour (G (Y) (X).Col);
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
            int(G (Y)'Last * Block_Size + Outline_Size),
            int((Y - 1) * Block_Size + Outline_Size),
            Block_Size - Outline_Size * 2,
            Block_Size - Outline_Size * 2
         ));
      end loop;
   end Display;

end Grid;
