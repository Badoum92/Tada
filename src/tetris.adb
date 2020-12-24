with Interfaces.C; use Interfaces.C;

package body Tetris is

   procedure Display_Grid (R : in out Renderer) is
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
   end Display_Grid;

end Tetris;
