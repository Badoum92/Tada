with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Tetromino;
with Grid;
with Time;
with C_Types; use C_Types;

package Game is

   type Game_T is private;
   type Piece_Action is (Left, Right, Down, Rotate);

   procedure Handle_Input (G : in out Game_T; Key : Scan_Codes);
   function Move_Piece (G : in out Game_T; action : Piece_Action)
      return Boolean;
   procedure Move_Piece_Proc (G : in out Game_T; action : Piece_Action);

   procedure Spawn_Piece (G : in out Game_T);
   function Is_Game_Over (G : Game_T) return Boolean;
   procedure Reset (G : in out Game_T);
   procedure Init (G : in out Game_T);
   procedure Display_Terminal (G : Game_T);
   procedure Display (G : Game_T; R : in out Renderer);
   procedure Update_Score (G : in out Game_T; Nb_Lines : Natural);
   procedure Speed_Up (G : in out Game_T);
   procedure Update (G : in out Game_T);

private

   Lines_To_Next_Level : constant := 12;

   type Game_T is record
      Game_Grid : Grid.Grid_T;
      Game_Over : Boolean;
      Cur_Piece : Tetromino.Tetromino_T;
      Next_Piece : Tetromino.Tetromino_T;
      Cur_Lines : Natural;
      Level : Uint64;
      Score : Uint64;
      Total_Delay : Uint64;
      Current_Delay : Uint64;
   end record;

end Game;
