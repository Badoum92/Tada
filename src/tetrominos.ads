with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Palettes; use SDL.Video.Palettes;

package Tetrominos is

   type Tetromino_Base is record
      Str : String (1 .. 16);
      Col : Colour;
   end record;

   type Tetromino is record
      Base : Tetromino_Base;
      X : Integer;
      Y : Integer;
   end record;

   procedure Tetromino_Display (R : in out SDL.Video.Renderers.Renderer; T : Tetromino);

   Tetromino_I : constant Tetromino_Base := (
      Str => "..X." &
             "..X." &
             "..X." &
             "..X.",
      Col => (
         Red => 250,
         Green => 206,
         Blue => 140,
         Alpha => 255
      )
   );

   Tetromino_T : constant Tetromino_Base := (
      Str => "..X." &
             ".XX." &
             "..X." &
             "....",
      Col => (
         Red => 130,
         Green => 255,
         Blue => 205,
         Alpha => 255
      )
   );

   Tetromino_O : constant Tetromino_Base := (
      Str => ".XX." &
             ".XX." &
             "...." &
             "....",
      Col => (
         Red => 255,
         Green => 80,
         Blue => 80,
         Alpha => 255
      )
   );

   Tetromino_Z : constant Tetromino_Base := (
      Str => "..X." &
             ".XX." &
             ".X.." &
             "....",
      Col => (
         Red => 72,
         Green => 255,
         Blue => 31,
         Alpha => 255
      )
   );

   Tetromino_S : constant Tetromino_Base := (
      Str => ".X.." &
             ".XX." &
             "..X." &
             "....",
      Col => (
         Red => 250,
         Green => 224,
         Blue => 27,
         Alpha => 255
      )
   );

   Tetromino_L : constant Tetromino_Base := (
      Str => ".X.." &
             ".X.." &
             ".XX." &
             "....",
      Col => (
         Red => 27,
         Green => 120,
         Blue => 250,
         Alpha => 255
      )
   );

   Tetromino_J : constant Tetromino_Base := (
      Str => "..X." &
             "..X." &
             ".XX." &
             "....",
      Col => (
         Red => 187,
         Green => 27,
         Blue => 250,
         Alpha => 255
      )
   );

end Tetrominos;
