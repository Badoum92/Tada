with Ada.Text_IO; use Ada.Text_IO;
with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Interfaces.C; use Interfaces.C;

procedure Tada is

   Should_Quit : Boolean := False;

   Width   : constant := 320;
   Height  : constant := 200;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;

   Position_X : int := 0;
   Position_Y : int := 0;

   Square_Width   : constant := 10;
   Square_Height  : constant := 10;

   function Poll_Events return Boolean is
      use type SDL.Events.Event_Types;
      Event : SDL.Events.Events.Events;
   begin
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            return True;
         elsif Event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
            if Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
               Position_X := Position_X + 10;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
               Position_X := Position_X - 10;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Down then
               Position_Y := Position_Y + 10;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Up then
               Position_Y := Position_Y - 10;
            end if;
         end if;
      end loop;
      return False;
   end Poll_Events;

begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      Put_Line ("Could not initialise SDL");
      return;
   end if;

   SDL.Video.Windows.Makers.Create (Win      => Window,
                                    Title    => "Draw a pixel",
                                    X        => 0,
                                    Y        => 0,
                                    Width    => Width,
                                    Height   => Height,
                                    Flags    => 0);
   SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);

   loop
      Should_Quit := Poll_Events;
      if Should_Quit then
         Window.Finalize;
         SDL.Finalise;
         return;
      end if;
      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill (Rectangle => (0, 0, Width, Height));
      Renderer.Set_Draw_Colour ((255, 0, 0, 255));
      Renderer.Fill (Rectangle => (Position_X, Position_Y, Square_Width, Square_Height));
      Window.Update_Surface;
   end loop;
end Tada;
