with SDL.Video.Renderers; use SDL.Video.Renderers;

package Tetris is

   Block_Size : constant := 25;
   Outline_Size : constant := 1;

   Grid_Width : constant := 12;
   Grid_Height : constant := 18;
   Grid_Offset_X : constant := 2;
   Grid_Offset_Y : constant := 2;

   procedure Display_Grid (R : in out Renderer);

end Tetris;
