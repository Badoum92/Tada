with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Interfaces.C; use Interfaces.C;
with Game;

package SDL_Wrapper is

   function Poll_Events (G : in out Game.Game_T) return Boolean;
   procedure Create_Window (Window : in out SDL.Video.Windows.Window;
      Width, Height : Positive);
   procedure Create_Renderer (Renderer : in out SDL.Video.Renderers.Renderer;
      Window : in out SDL.Video.Windows.Window);

end SDL_Wrapper;
