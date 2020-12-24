with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Palettes; use SDL.Video.Palettes;

package Tetris is

   Block_Size : constant := 25;
   Outline_Size : constant := 1;

   Grid_Width : constant := 12;
   Grid_Height : constant := 18;
   Grid_Offset_X : constant := 2;
   Grid_Offset_Y : constant := 2;

   type Block is record
      Set : Boolean := False;
      Col : Colour := (0, 0, 0, 255);
   end record;

   type Grid_Line is array (1 .. Grid_Width) of Block;
   type Grid_Array is array (1 .. Grid_Height) of Grid_Line;
   Grid : Grid_Array;

   procedure Grid_Init;
   procedure Grid_Preview_Display (R : in out Renderer);
   procedure Grid_Display (R : in out Renderer);

end Tetris;
