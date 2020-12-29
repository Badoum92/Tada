with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Palettes; use SDL.Video.Palettes;

package Tetrominos is

   type Rotation_Index is range 1 .. 4;
   type Rotation_Array is array (Rotation_Index) of String (1 .. 16);

   type Tetromino_Base is record
      Rot : Rotation_Array;
      Col : Colour;
   end record;

   type Tetromino is record
      Base : Tetromino_Base;
      X : Integer;
      Y : Integer;
      Rot : Rotation_Index := 1;
   end record;

   procedure Tetromino_Display (R : in out Renderer; T : Tetromino);
   procedure Tetromino_Rotate (T : in out Tetromino);

   Tetromino_I : constant Tetromino_Base := (
      Rot => ("..X." &
              "..X." &
              "..X." &
              "..X.",

              "...." &
              "...." &
              "XXXX" &
              "....",

              "..X." &
              "..X." &
              "..X." &
              "..X.",

              "...." &
              "...." &
              "XXXX" &
              "...."),
      Col => (
         Red => 250,
         Green => 206,
         Blue => 140,
         Alpha => 255)
   );

   Tetromino_T : constant Tetromino_Base := (
      Rot => ("...." &
              ".XXX" &
              "..X." &
              "....",

              "..X." &
              ".XX." &
              "..X." &
              "....",

              "..X." &
              ".XXX" &
              "...." &
              "....",

              "..X." &
              "..XX" &
              "..X." &
              "...."),
      Col => (
         Red => 130,
         Green => 255,
         Blue => 205,
         Alpha => 255
      )
   );

   Tetromino_O : constant Tetromino_Base := (
      Rot => ("...." &
              ".XX." &
              ".XX." &
              "....",

              "...." &
              ".XX." &
              ".XX." &
              "....",

              "...." &
              ".XX." &
              ".XX." &
              "....",

              "...." &
              ".XX." &
              ".XX." &
              "...."),
      Col => (
         Red => 255,
         Green => 80,
         Blue => 80,
         Alpha => 255
      )
   );

   Tetromino_Z : constant Tetromino_Base := (
      Rot => ("...X" &
              "..XX" &
              "..X." &
              "....",

              "...." &
              ".XX." &
              "..XX" &
              "....",

              "...X" &
              "..XX" &
              "..X." &
              "....",

              "...." &
              ".XX." &
              "..XX" &
              "...."),
      Col => (
         Red => 72,
         Green => 255,
         Blue => 31,
         Alpha => 255
      )
   );

   Tetromino_S : constant Tetromino_Base := (
      Rot => ("..X." &
              "..XX" &
              "...X" &
              "....",

              "...." &
              "..XX" &
              ".XX." &
              "....",

              "..X." &
              "..XX" &
              "...X" &
              "....",

              "...." &
              "..XX" &
              ".XX." &
              "...."),
      Col => (
         Red => 250,
         Green => 224,
         Blue => 27,
         Alpha => 255
      )
   );

   Tetromino_L : constant Tetromino_Base := (
      Rot => ("..X." &
              "..X." &
              "..XX" &
              "....",

              "...." &
              ".XXX" &
              ".X.." &
              "....",

              ".XX." &
              "..X." &
              "..X." &
              "....",

              "...X" &
              ".XXX" &
              "...." &
              "...."),
      Col => (
         Red => 27,
         Green => 120,
         Blue => 250,
         Alpha => 255
      )
   );

   Tetromino_J : constant Tetromino_Base := (
      Rot => ("...X" &
              "...X" &
              "..XX" &
              "....",

              ".X.." &
              ".XXX" &
              "...." &
              "....",

              "..XX" &
              "..X." &
              "..X." &
              "....",

              "...." &
              ".XXX" &
              "...X" &
              "...."),
      Col => (
         Red => 187,
         Green => 27,
         Blue => 250,
         Alpha => 255
      )
   );

   type Tetrominos_Index is range 1 .. 7;
   type Tetrominos_Array is array (Tetrominos_Index) of Tetromino_Base;
   Tetromino_Bases : Tetrominos_Array := (
      Tetromino_I, Tetromino_T, Tetromino_O, Tetromino_Z,
      Tetromino_S, Tetromino_L, Tetromino_J
   );


end Tetrominos;
