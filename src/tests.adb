with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Source_Info; use GNAT.Source_Info;
with C_Types; use C_Types;
with SDL_Wrapper;
with Game;
with Grid;
with Tetromino;
with Time;

with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;

package body Tests is

   procedure Test_Error (Test_Name, Msg : String) is
   begin
      Put (Test_Name);
      Put (": " & ASCII.ESC & "[31mERROR" & ASCII.ESC & "[0m: ");
      Put_Line (Msg);
   end Test_Error;

   procedure Test_Success (Test_Name : String) is
   begin
      Put (Test_Name);
      Put_Line (": " & ASCII.ESC & "[32mSUCCESS" & ASCII.ESC & "[0m");
   end Test_Success;

   function TC_1_1_1_Grid_Empty return Boolean is
      G : Game.Game_T;
   begin
      Game.Reset (G);

      if Grid.Count_Blocks (G.Game_Grid) /= 0 then
         Test_Error (Enclosing_Entity, "Grid is not empty");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_1_1_1_Grid_Empty;

   function TC_1_2_1_Reset return Boolean is
      G : Game.Game_T;
   begin
      Game.Reset (G);
      G.Game_Grid (1) (1).Set := True;
      Game.Handle_Input (G, Scan_Code_R);

      if Grid.Count_Blocks (G.Game_Grid) /= 0 then
         Test_Error (Enclosing_Entity, "Grid is not empty");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_1_2_1_Reset;

   function TC_2_1_1_Piece_Move_Left return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
   begin
      Game.Reset (G);
      Old_X := G.Cur_Piece.X;
      Old_Y := G.Cur_Piece.Y;
      Game.Handle_Input (G, Scan_Code_Left);

      if G.Cur_Piece.X /= Old_X - 1 or else G.Cur_Piece.Y /= Old_Y then
         Test_Error (Enclosing_Entity, "Piece did not move left");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_2_1_1_Piece_Move_Left;

   function TC_2_1_2_Piece_Move_Right return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
   begin
      Game.Reset (G);
      Old_X := G.Cur_Piece.X;
      Old_Y := G.Cur_Piece.Y;
      Game.Handle_Input (G, Scan_Code_Right);

      if G.Cur_Piece.X /= Old_X + 1 or else G.Cur_Piece.Y /= Old_Y then
         Test_Error (Enclosing_Entity, "Piece did not move right");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_2_1_2_Piece_Move_Right;

   function TC_2_2_1_Piece_Blocked_Left return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
   begin
      Game.Reset (G);
      G.Cur_Piece.X := -4;
      Old_X := G.Cur_Piece.X;
      Old_Y := G.Cur_Piece.Y;
      Game.Handle_Input (G, Scan_Code_Left);

      if G.Cur_Piece.X /= Old_X or else G.Cur_Piece.Y /= Old_Y then
         Test_Error (Enclosing_Entity, "Piece moved");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_2_2_1_Piece_Blocked_Left;

   function TC_2_2_2_Piece_Blocked_Right return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
   begin
      Game.Reset (G);
      G.Cur_Piece.X := -4;
      Old_X := G.Cur_Piece.X;
      Old_Y := G.Cur_Piece.Y;
      Game.Handle_Input (G, Scan_Code_Right);

      if G.Cur_Piece.X /= Old_X or else G.Cur_Piece.Y /= Old_Y then
         Test_Error (Enclosing_Entity, "Piece moved");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_2_2_2_Piece_Blocked_Right;

   function TC_3_1_1_Full_Line_Deleted return Boolean is
      G : Game.Game_T;
      Deleted_Lines_Nb : Natural;
      Total_Blocks : Natural;
   begin
      Game.Reset (G);
      G.Game_Grid (Grid.Grid_Height) := (others => (True, (0, 0, 0, 0)));
      Total_Blocks := Grid.Count_Blocks (G.Game_Grid);
      Deleted_Lines_Nb := Grid.Remove_Full_Lines (G.Game_Grid);

      if Grid.Count_Blocks (G.Game_Grid) /= Total_Blocks - Grid.Grid_Width then
         Test_Error (Enclosing_Entity, "Full line was not removed");
         return False;
      end if;

      if Deleted_Lines_Nb /= 1 then
         Test_Error (Enclosing_Entity, "Expected 1 line deleted got" &
                     Natural'Image (Deleted_Lines_Nb));
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_3_1_1_Full_Line_Deleted;

   function TC_3_2_1_Lowered_Lines_On_Delete return Boolean is
      G : Game.Game_T;
      Deleted_Lines_Nb : Natural;
   begin
      Game.Reset (G);
      G.Game_Grid (Grid.Grid_Height) := (others => (True, (0, 0, 0, 0)));
      G.Game_Grid (Grid.Grid_Height - 1) (1) := (True, (0, 0, 0, 0));
      Deleted_Lines_Nb := Grid.Remove_Full_Lines (G.Game_Grid);

      if Deleted_Lines_Nb /= 1 then
         Test_Error (Enclosing_Entity, "Expected 1 line deleted got" &
                     Natural'Image (Deleted_Lines_Nb));
         return False;
      end if;

      if not G.Game_Grid (Grid.Grid_Height) (1).Set then
         Test_Error (Enclosing_Entity, "Above lines were not lowered");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_3_2_1_Lowered_Lines_On_Delete;

   function TC_4_1_1_Piece_Falls_Period return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
      Steps : Natural := 0;
      Total_Steps : constant Natural := 2;
      Cur_Delay : Uint64 := 0;
      Period : constant Uint64 := 1000;
   begin
      Game.Reset (G);

      while Steps < Total_Steps loop
         Time.Update;
         Cur_Delay := Cur_Delay + Time.Get_Delta_Time;
         Old_X := G.Cur_Piece.X;
         Old_Y := G.Cur_Piece.Y;
         Game.Update (G);

         if Cur_Delay >= Period then
            if G.Cur_Piece.X /= Old_X or else G.Cur_Piece.Y /= Old_Y + 1 then
               Test_Error (Enclosing_Entity, "Piece didn't move");
               return False;
            end if;
            Cur_Delay := Cur_Delay - Period;
            Steps := Steps + 1;
         else
            if G.Cur_Piece.X /= Old_X or else G.Cur_Piece.Y /= Old_Y then
               Test_Error (Enclosing_Entity, "Piece moved when it shouldn't");
               return False;
            end if;
         end if;

         delay 0.1;
      end loop;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_4_1_1_Piece_Falls_Period;

   function TC_4_2_1_Piece_Blocked_Down return Boolean is
      G : Game.Game_T;
   begin
      Game.Reset (G);
      G.Cur_Piece := (Base => Tetromino.I_Tetromino,
                    X => Grid.Grid_Width / 2 - 2,
                    Y => 2,
                    Rot => 1);
      G.Game_Grid (G.Cur_Piece.Y + 3) (G.Cur_Piece.X + 1).Set := True;
      Time.Update;
      delay 1.0;
      Time.Update;
      Game.Update (G);

      if Grid.Count_Blocks (G.Game_Grid) /= 5 then
         Test_Error (Enclosing_Entity, "The piece wasn't added to the grid");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_4_2_1_Piece_Blocked_Down;

   function TC_4_3_1_New_Piece_Selected return Boolean is
      G : Game.Game_T;
      Old_X, Old_Y : Integer;
   begin
      Game.Reset (G);
      G.Cur_Piece := (Base => Tetromino.I_Tetromino,
                    X => Grid.Grid_Width / 2 - 2,
                    Y => 2,
                    Rot => 1);
      G.Game_Grid (G.Cur_Piece.Y + 4) (G.Cur_Piece.X + 1).Set := True;
      Time.Update;
      delay 1.0;
      Time.Update;
      Game.Update (G);
      Old_X := G.Cur_Piece.X;
      Old_Y := G.Cur_Piece.Y;
      delay 1.0;
      Time.Update;
      Game.Update (G);

      if G.Cur_Piece.X = Old_X and then G.Cur_Piece.Y = Old_Y  then
         Test_Error (Enclosing_Entity, "A new piece wasn't selected");
         return False;
      end if;

      Test_Success (Enclosing_Entity);
      return True;
   end TC_4_3_1_New_Piece_Selected;

   function TC_5_1_Display return Boolean is
      Size : constant := Grid.Block_Size * 6;
      Window : SDL.Video.Windows.Window;
      Renderer : SDL.Video.Renderers.Renderer;
      Piece : Tetromino.Tetromino_T;
      Event : SDL.Events.Events.Events;
      use type SDL.Events.Event_Types;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         Test_Error (Enclosing_Entity, "Could not initialize SDL");
         return False;
      end if;

      SDL_Wrapper.Create_Window (Window, Size, Size, "Press any key");
      SDL_Wrapper.Create_Renderer (Renderer, Window);
      Piece := (Tetromino.O_Tetromino, 1, 1, 1);

      while True loop
         while SDL.Events.Events.Poll (Event) loop
            if Event.Common.Event_Type = Key_Down then
               Window.Finalize;
               SDL.Finalise;
               Test_Success (Enclosing_Entity);
               return True;
            end if;
         end loop;

         Renderer.Set_Draw_Colour ((0, 0, 0, 255));
         Renderer.Fill (Rectangle => (0, 0, Size, Size));
         Tetromino.Display (Piece, Renderer);
         Window.Update_Surface;
      end loop;

      Test_Error (Enclosing_Entity, "Something went wrong");
      return False;
   end TC_5_1_Display;

   procedure Run_Test (Test : Test_Func_T; Total, Success : in out Natural) is
   begin
      Total := Total + 1;
      if Test.all then
         Success := Success + 1;
      end if;
   end Run_Test;

   procedure Exec is
      Total : Natural := 0;
      Success : Natural := 0;
   begin
      Run_Test (TC_1_1_1_Grid_Empty'Access, Total, Success);
      Run_Test (TC_1_2_1_Reset'Access, Total, Success);

      Run_Test (TC_2_1_1_Piece_Move_Left'Access, Total, Success);
      Run_Test (TC_2_1_2_Piece_Move_Right'Access, Total, Success);
      Run_Test (TC_2_2_1_Piece_Blocked_Left'Access, Total, Success);
      Run_Test (TC_2_2_2_Piece_Blocked_Right'Access, Total, Success);

      Run_Test (TC_3_1_1_Full_Line_Deleted'Access, Total, Success);
      Run_Test (TC_3_2_1_Lowered_Lines_On_Delete'Access, Total, Success);

      Run_Test (TC_4_1_1_Piece_Falls_Period'Access, Total, Success);
      Run_Test (TC_4_2_1_Piece_Blocked_Down'Access, Total, Success);
      Run_Test (TC_4_3_1_New_Piece_Selected'Access, Total, Success);

      Run_Test (TC_5_1_Display'Access, Total, Success);

      Put_Line ("Tests: " & Natural'Image (Success) &
                " /" & Natural'Image (Total));
   end Exec;

end Tests;
