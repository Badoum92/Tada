with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Palettes; use SDL.Video.Palettes;
with Tetromino;

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
   type Grid_T is array (1 .. Grid_Height) of Grid_Line;

   function Block_Fits (G : Grid_T; Block : Character; X, Y : Integer)
      return Boolean;
   function Piece_Fits (G : Grid_T; T : Tetromino.Tetromino_T) return Boolean;
   procedure Lock_Piece (G : in out Grid_T; T : Tetromino.Tetromino_T);

   function Is_Line_Full (G : Grid_T; Line_Idx : Positive) return Boolean;
   procedure Remove_Line (G : in out Grid_T; Line_Idx : Positive);
   function Remove_Full_Lines (G : in out Grid_T) return Natural;

   procedure Init (G : in out Grid_T);
   procedure Display (G : Grid_T; R : in out Renderer);

end Grid;
