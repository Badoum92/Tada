with C_Types; use C_Types;

package Time is


   function SDL_GetPerformanceCounter return Uint64
      with
         Import => True,
         Convention => C,
         External_Name => "SDL_GetPerformanceCounter";

   function SDL_GetPerformanceFrequency return Uint64
      with
         Import => True,
         Convention => C,
         External_Name => "SDL_GetPerformanceFrequency";

   procedure Update;
   function Get_Delta_Time return Uint64;

private

   Delta_Time : Uint64 := 0;
   Now : Uint64 := 0;
   Last : Uint64;
   Init : Boolean := False;

end Time;
