with Ada.Text_IO; use Ada.Text_IO;
with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;

with Interfaces.C; use Interfaces.C;
with Tetrominos; use Tetrominos;
with Grid; use Grid;
with Game; use Game;
with C_Types; use C_Types;
with Time;

procedure Tada is

   Width   : constant := Block_Size * (Grid_Width + 10);
   Height  : constant := Block_Size * Grid_Height;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;

   function Poll_Events return Boolean is
      use type SDL.Events.Event_Types;
      Event : SDL.Events.Events.Events;
   begin
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            return True;
         elsif Event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
            Game_Handle_Input (Event.Keyboard.Key_Sym.Scan_Code);
         end if;
      end loop;
      return False;
   end Poll_Events;

begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      Put_Line ("Could not initialise SDL");
      return;
   end if;

   SDL.Video.Windows.Makers.Create (Win    => Window,
                                    Title  => "Tada - Tetris in Ada",
                                    X      => 0,
                                    Y      => 0,
                                    Width  => Width,
                                    Height => Height,
                                    Flags  => 0);
   SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);

   Game_Reset;

   loop
      Time.Update;

      if Poll_Events then
         Window.Finalize;
         SDL.Finalise;
         return;
      end if;

      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill (Rectangle => (0, 0, Width, Height));

      Game_Update;
      Game_Display (Renderer);

      Window.Update_Surface;
      delay 0.05;
   end loop;
end Tada;
