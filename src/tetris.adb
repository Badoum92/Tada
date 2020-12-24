with Interfaces.C; use Interfaces.C;

package body Tetris is

   procedure Grid_Init is
   begin
      Grid := (others => (
         others => (
            Set => False,
            Col => (0, 0, 0, 255)
         )
      ));

      for Y in Grid'Range loop
         for X in Grid (Y)'Range loop
            if X = 1 or else X = Grid_Width or else Y = Grid_Height then
               Grid (Y) (X) := (True, (100, 100, 100, 255));
            end if;
         end loop;
      end loop;
   end Grid_Init;

   procedure Grid_Preview_Display (R : in out Renderer) is
   begin
      R.Set_Draw_Colour ((100, 100, 100, 255));
      for Y in 0 .. 5 loop
         for X in 0 .. 5 loop
            if X = 0 or else X = 5 or else Y = 0 or else Y = 5 then
               R.Fill (Rectangle => (
                  int((Grid_Offset_X + Grid_Width + 2 + X) * Block_Size + Outline_Size),
                  int((Grid_Offset_Y + Y) * Block_Size + Outline_Size),
                  Block_Size - Outline_Size * 2,
                  Block_Size - Outline_Size * 2
               ));
            end if;
         end loop;
      end loop;
   end Grid_Preview_Display;

   procedure Grid_Display (R : in out Renderer) is
   begin
      Grid_Preview_Display (R);
      for Y in Grid'Range loop
         for X in Grid (Y)'Range loop
            if Grid (Y) (X).Set then
               R.Set_Draw_Colour (Grid (Y) (X).Col);
               R.Fill (Rectangle => (
                  int((Grid_Offset_X + (X - 1)) * Block_Size + Outline_Size),
                  int((Grid_Offset_Y + (Y - 1)) * Block_Size + Outline_Size),
                  Block_Size - Outline_Size * 2,
                  Block_Size - Outline_Size * 2
               ));
            end if;
         end loop;
      end loop;
   end Grid_Display;

end Tetris;
