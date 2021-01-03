with Ada.Text_IO; use Ada.Text_IO;

package body Game is

   function Is_Game_Over (G : Game_T) return Boolean is
   begin
      return G.Game_Over;
   end Is_Game_Over;

   procedure Spawn_Piece (G : in out Game_T) is
   begin
      G.Cur_Piece := G.Next_Piece;
      G.Cur_Piece.X := Grid.Grid_Width / 2 - 2;
      G.Cur_Piece.Y := 0;

      G.Next_Piece := (Base => Tetromino.Get_Random_Piece,
                    X => Grid.Grid_Width + 2,
                    Y => 2,
                    Rot => 1);

      if not Grid.Piece_Fits (G.Game_Grid, G.Cur_Piece) then
         G.Game_Over := True;
      end if;
   end Spawn_Piece;

   procedure Reset (G : in out Game_T) is
   begin
      G.Next_Piece := (Base => Tetromino.Get_Random_Piece,
                    X => Grid.Grid_Width + 2,
                    Y => 2,
                    Rot => 1);
      Init (G);
   end Reset;

   procedure Init (G : in out Game_T) is
   begin
      Grid.Init (G.Game_Grid);
      Spawn_Piece (G);
      G.Game_Over := False;
      G.Level := 1;
      G.Score := 0;
      G.Cur_Lines := 0;
      G.Total_Delay := 1000;
      G.Current_Delay := 0;
   end Init;

   procedure Handle_Input (G : in out Game_T; Key : Scan_Codes) is
   begin
      if Key = Scan_Code_R then
         Reset (G);
         return;
      end if;

      if G.Game_Over then
         return;
      end if;

      if Key = Scan_Code_Left then
         Move_Piece_Proc (G, Left);
      elsif Key = Scan_Code_Right then
         Move_Piece_Proc (G, Right);
      elsif Key = Scan_Code_Down then
         Move_Piece_Proc (G, Down);
      elsif Key = Scan_Code_Up then
         Move_Piece_Proc (G, Rotate);
      elsif Key = Scan_Code_Space then
         while Move_Piece (G, Down) loop
            null;
         end loop;
      end if;
   end Handle_Input;

   function Move_Piece (G : in out Game_T; action : Piece_Action)
      return Boolean is
      Old_X : constant Integer := G.Cur_Piece.X;
      Old_Y : constant Integer := G.Cur_Piece.Y;
      Old_Rot : constant Tetromino.Rotation_Index := G.Cur_Piece.Rot;
      Nb_Lines : Natural := 0;
   begin
      if action = Left then
         G.Cur_Piece.X := G.Cur_Piece.X - 1;
      elsif action = Right then
         G.Cur_Piece.X := G.Cur_Piece.X + 1;
      elsif action = Down then
         G.Cur_Piece.Y := G.Cur_Piece.Y + 1;
      elsif action = Rotate then
         Tetromino.Rotate (G.Cur_Piece);
      end if;

      if not Grid.Piece_Fits (G.Game_Grid, G.Cur_Piece) then
         G.Cur_Piece.X := Old_X;
         G.Cur_Piece.Y := Old_Y;
         G.Cur_Piece.Rot := Old_Rot;

         if action = Down then
            Grid.Lock_Piece (G.Game_Grid, G.Cur_Piece);
            Nb_Lines := Grid.Remove_Full_Lines (G.Game_Grid);
            G.Cur_Lines := G.Cur_Lines + Nb_Lines;
            Update_Score (G, Nb_Lines);
            Spawn_Piece (G);
            return False;
         end if;
      end if;

      return True;
   end Move_Piece;

   procedure Move_Piece_Proc (G : in out Game_T; action : Piece_Action) is
      Status : Boolean := False;
   begin
      Status := Move_Piece (G, action);
      pragma Unreferenced (Status);
   end Move_Piece_Proc;

   procedure Update_Score (G : in out Game_T; Nb_Lines : Natural) is
   begin
      if Nb_Lines = 1 then
         G.Score := G.Score + 100 * G.Level;
      elsif Nb_Lines = 2 then
         G.Score := G.Score + 300 * G.Level;
      elsif Nb_Lines = 3 then
         G.Score := G.Score + 500 * G.Level;
      elsif Nb_Lines = 4 then
         G.Score := G.Score + 800 * G.Level;
      end if;
   end Update_Score;

   procedure Display_Terminal (G : Game_T) is
   begin
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[1;1H");
      Put_Line ("--- Tada ---");
      Put_Line ("Left arrow: move left");
      Put_Line ("Right arrow: move right");
      Put_Line ("Down arrow: move down");
      Put_Line ("Up arrow: rotate");
      Put_Line ("Space bar: fast down");
      Put_Line ("R: restart");
      Put_Line ("------------");
      Put_Line ("Score: " & Uint64'Image (G.Score));
      Put_Line ("Level: " & Uint64'Image (G.Level));

      if G.Game_Over then
         Put_Line ("GAME OVER");
         Put_Line ("Press R to start a new game");
      end if;
   end Display_Terminal;

   procedure Display (G : Game_T; R : in out Renderer) is
   begin
      Grid.Display (G.Game_Grid, R);
      Tetromino.Display (G.Cur_Piece, R);
      Tetromino.Display (G.Next_Piece, R);
   end Display;

   procedure Speed_Up (G : in out Game_T) is
      Speed_Up_Factor : Uint64 := 500;
   begin
      if G.Cur_Lines >= Lines_To_Next_Level then
         G.Cur_Lines := G.Cur_Lines - Lines_To_Next_Level;
         Speed_Up_Factor := Speed_Up_Factor / (G.Level + 1);
         if G.Total_Delay > Speed_Up_Factor then
            G.Level := G.Level + 1;
            G.Total_Delay := G.Total_Delay - Speed_Up_Factor;
         end if;
      end if;
   end Speed_Up;

   procedure Update (G : in out Game_T) is
   begin
      G.Current_Delay := G.Current_Delay + Time.Get_Delta_Time;
      if G.Current_Delay >= G.Total_Delay then
         G.Current_Delay := G.Current_Delay - G.Total_Delay;
         Move_Piece_Proc (G, Down);
         Speed_Up (G);
      end if;
   end Update;

end Game;
