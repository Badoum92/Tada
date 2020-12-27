with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Tetrominos; use Tetrominos;
with Grid; use Grid;
with Time; use Time;

package Game is

   Cur_Piece : Tetromino;

   procedure Game_Init;
   procedure Game_Handle_Input (Key : Scan_Codes);
   procedure Game_Display (R : in out Renderer);

end Game;
