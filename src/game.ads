with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Tetromino;
with Grid;
with Time;
with C_Types; use C_Types;

package Game is

   Lines_To_Next_Level : constant := 12;

   type Game_T is record
      Game_Grid : Grid.Grid_T;
      Game_Over : Boolean;
      Cur_Piece : Tetromino.Tetromino_T;
      Next_Piece : Tetromino.Tetromino_T;
      Cur_Lines : Natural := 0;
      Level : Uint64 := 1;
      Score : Uint64 := 0;
      Total_Delay : Uint64 := 1000;
      Current_Delay : Uint64 := 0;
   end record
   with Dynamic_Predicate => Game_T.Current_Delay < Game_T.Total_Delay and
                             Game_T.Total_Delay > 0;

   type Piece_Action is (Left, Right, Down, Rotate);

   procedure Handle_Input (G : in out Game_T; Key : Scan_Codes);
   function Move_Piece (G : in out Game_T; action : Piece_Action)
      return Boolean;
   procedure Move_Piece_Proc (G : in out Game_T; action : Piece_Action);

   procedure Spawn_Piece (G : in out Game_T)
      with
      Post => Tetromino.Equals (G.Cur_Piece, G.Next_Piece'Old);

   function Is_Game_Over (G : Game_T) return Boolean;
   procedure Reset (G : in out Game_T);
   procedure Init (G : in out Game_T);
   procedure Display_Terminal (G : Game_T);
   procedure Display (G : Game_T; R : in out Renderer);
   procedure Update_Score (G : in out Game_T; Nb_Lines : Natural)
      with
      Pre => Nb_Lines <= 4,
      Post => (if Nb_Lines > 0 then
               G.Score > G.Score'Old);

   procedure Speed_Up (G : in out Game_T)
      with
      Post => (if G.Cur_Lines'Old >= Lines_To_Next_Level then
               G.Level = G.Level'Old + 1 and
               G.Total_Delay < G.Total_Delay'Old and
               G.Cur_Lines = G.Cur_Lines'Old - Lines_To_Next_Level);

   procedure Update (G : in out Game_T)
      with
      Pre => not G.Game_Over,
      Post => G.Current_Delay = G.Current_Delay'Old + Time.Get_Delta_Time or
              G.Current_Delay = G.Current_Delay'Old + Time.Get_Delta_Time - G.Total_Delay'Old;

end Game;
