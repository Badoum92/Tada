with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Tetrominos; use Tetrominos;
with Grid; use Grid;
with Time; use Time;
with C_Types; use C_Types;

package Game is

   type Piece_Action is (Left, Right, Down, Rotate);

   function Get_Random_Piece return Tetromino_Base;
   procedure Game_Spawn_Piece;

   procedure Game_Handle_Input (Key : Scan_Codes);
   procedure Game_Move_Piece (action : Piece_Action);

   procedure Game_Update_Score (Nb_Lines : Natural);

   procedure Game_Init;
   procedure Game_Display (R : in out Renderer);
   procedure Game_Update;

private

   Cur_Piece : Tetromino;
   Level : Uint64;
   Score : Uint64;
   Total_Delay : Uint64;
   Current_Delay : Uint64;

end Game;
