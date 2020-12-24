with Ada.Text_IO; use Ada.Text_IO;
with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with Interfaces.C; use Interfaces.C;
with Tetrominos; use Tetrominos;
with Tetris; use Tetris;

procedure Tada is

   Should_Quit : Boolean := False;

   Width   : constant := 1280;
   Height  : constant := 720;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;

   Position_X : Integer := 0;
   Position_Y : Integer := 0;

   TI : Tetromino;
   TT : Tetromino;
   TO : Tetromino;
   TZ : Tetromino;
   TS : Tetromino;
   TL : Tetromino;
   TJ : Tetromino;

   function Poll_Events return Boolean is
      use type SDL.Events.Event_Types;
      Event : SDL.Events.Events.Events;
   begin
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            return True;
         elsif Event.Common.Event_Type = SDL.Events.Keyboards.Key_Down then
            if Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
               Position_X := Position_X + Block_Size;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
               Position_X := Position_X - Block_Size;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Down then
               Position_Y := Position_Y + Block_Size;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Up then
               Position_Y := Position_Y - Block_Size;
            elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_R then
               Tetromino_Rotate (TI);
               Tetromino_Rotate (TT);
               Tetromino_Rotate (TO);
               Tetromino_Rotate (TZ);
               Tetromino_Rotate (TS);
               Tetromino_Rotate (TL);
               Tetromino_Rotate (TJ);
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

   TI := (Tetromino_I, 12 + 0, 0, 1);
   TT := (Tetromino_T, 12 + 4, 0, 1);
   TO := (Tetromino_O, 12 + 8, 0, 1);
   TZ := (Tetromino_Z, 12 + 12, 0, 1);
   TS := (Tetromino_S, 12 + 16, 0, 1);
   TL := (Tetromino_L, 12 + 20, 0, 1);
   TJ := (Tetromino_J, 12 + 24, 0, 1);

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
      Renderer.Fill (Rectangle => (int(Position_X), int(Position_Y), Block_Size, Block_Size));

      Tetromino_Display (Renderer, TI);
      Tetromino_Display (Renderer, TT);
      Tetromino_Display (Renderer, TO);
      Tetromino_Display (Renderer, TZ);
      Tetromino_Display (Renderer, TS);
      Tetromino_Display (Renderer, TL);
      Tetromino_Display (Renderer, TJ);
      Display_Grid (Renderer);
      Window.Update_Surface;
   end loop;
end Tada;
