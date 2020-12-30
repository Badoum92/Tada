package body SDL_Wrapper is

   function Poll_Events (G : in out Game.Game_T) return Boolean is
      use type SDL.Events.Event_Types;
      Event : SDL.Events.Events.Events;
   begin
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            return True;
         elsif Event.Common.Event_Type = Key_Down then
            if Event.Keyboard.Key_Sym.Scan_Code = Scan_Code_R then
               Game.Reset (G);
            end if;
            if not Game.Is_Game_Over (G) then
               Game.Handle_Input (G, Event.Keyboard.Key_Sym.Scan_Code);
            end if;
         end if;
      end loop;
   return False;
   end Poll_Events;

   procedure Create_Window (Window : in out SDL.Video.Windows.Window;
      Width, Height : Positive) is
   begin
      SDL.Video.Windows.Makers.Create (Win    => Window,
                                       Title  => "Tada - Tetris in Ada",
                                       X      => 0,
                                       Y      => 0,
                                       Width  => Width,
                                       Height => Height,
                                       Flags  => 0);
   end Create_Window;

   procedure Create_Renderer (Renderer : in out SDL.Video.Renderers.Renderer;
      Window : in out SDL.Video.Windows.Window) is
   begin
      SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);
   end Create_Renderer;

end SDL_Wrapper;
