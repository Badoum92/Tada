with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Palettes; use SDL.Video.Palettes;
with Tetrominos; use Tetrominos;

package Grid is

   Block_Size : constant := 25;
   Outline_Size : constant := 1;

   Grid_Width : constant := 10;
   Grid_Height : constant := 20;

   type Block is record
      Set : Boolean := False;
      Col : Colour := (0, 0, 0, 255);
   end record;

   type Grid_Line is array (1 .. Grid_Width) of Block;
   type Grid_Array is array (1 .. Grid_Height) of Grid_Line;
   Grid : Grid_Array;

   function Grid_Block_Fits (Block : Character; X, Y : Integer) return Boolean;
   function Grid_Piece_Fits (T : Tetromino) return Boolean;
   procedure Grid_Lock_Piece (T : Tetromino);

   function Grid_Is_Line_Full (Line_Idx : Positive) return Boolean;
   procedure Grid_Remove_Line (Line_Idx : Positive);
   function Grid_Remove_Full_Lines return Natural;

   procedure Grid_Init;
   procedure Grid_Display (R : in out Renderer);

end Grid;
