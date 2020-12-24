package body Time is

   procedure Update is
   begin
      Last := Now;
      Now := SDL_GetPerformanceCounter;
      if not Init then
         Init := True;
         return;
      end if;
      Delta_Time := ((Now - Last) * 1000) / SDL_GetPerformanceFrequency;
   end Update;

end Time;
