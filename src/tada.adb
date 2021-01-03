with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with C_Types; use C_Types;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL_Wrapper;
with Game;
with Grid;
with Time;
with Tests;

procedure Tada is

   Width   : constant := Grid.Block_Size * (Grid.Grid_Width + 8);
   Height  : constant := Grid.Block_Size * Grid.Grid_Height;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;

   Current_Game : Game.Game_T;
   Term_Update_Delay : constant Uint64 := 500;
   Term_Update_Time : Uint64 := 0;

begin
   if Argument_Count = 1 and then Argument (1) = "check" then
      Tests.Exec;
      return;
   end if;

   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      Put_Line ("Could not initialise SDL");
      return;
   end if;

   SDL_Wrapper.Create_Window (Window, Width, Height, "Tada - Tetris in Ada");
   SDL_Wrapper.Create_Renderer (Renderer, Window);

   Game.Reset (Current_Game);
   Game.Display_Terminal (Current_Game);

   loop
      Time.Update;

      if SDL_Wrapper.Poll_Events (Current_Game) then
         Window.Finalize;
         SDL.Finalise;
         return;
      end if;

      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill (Rectangle => (0, 0, Width, Height));

      if not Game.Is_Game_Over (Current_Game) then
         Game.Update (Current_Game);
      end if;
      Game.Display (Current_Game, Renderer);

      Term_Update_Time := Term_Update_Time + Time.Get_Delta_Time;
      if Term_Update_Time >= Term_Update_Delay then
         Game.Display_Terminal (Current_Game);
         Term_Update_Time := Term_Update_Time - Term_Update_Delay;
      end if;

      Window.Update_Surface;
      delay 0.01;
   end loop;
end Tada;
