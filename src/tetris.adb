with Interfaces.C; use Interfaces.C;

package body Tetris is

   procedure Grid_Display (R : in out Renderer) is
   begin
      R.Set_Draw_Colour ((100, 100, 100, 255));
      -- Left Wall
      R.Fill (Rectangle => (
         int(Grid_Offset_X * Block_Size),
         int(Grid_Offset_Y * Block_Size),
         Block_Size,
         (Block_Size * Grid_Height)
      ));
      -- Right Wall
      R.Fill (Rectangle => (
         int((Grid_Offset_X + Grid_Width - 1) * Block_Size),
         int(Grid_Offset_Y * Block_Size),
         Block_Size,
         (Block_Size * Grid_Height)
      ));
      -- Floor
      R.Fill (Rectangle => (
         int(Grid_Offset_X * Block_Size),
         int((Grid_Offset_Y + Grid_Height) * Block_Size),
         (Block_Size * Grid_Width),
         Block_Size
      ));
      -- Piece Preview
      R.Fill (Rectangle => (
         int((Grid_Offset_X + Grid_Width + 2) * Block_Size),
         int(Grid_Offset_Y * Block_Size),
         Block_Size * 6,
         Block_Size * 6
      ));
      R.Set_Draw_Colour ((0, 0, 0, 255));
      R.Fill (Rectangle => (
         int((Grid_Offset_X + Grid_Width + 3) * Block_Size),
         int((Grid_Offset_Y + 1) * Block_Size),
         Block_Size * 4,
         Block_Size * 4
      ));
   end Grid_Display;

end Tetris;
