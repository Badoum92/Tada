with C_Types; use C_Types;

package Time is

   Delta_Time : Uint64 := 0;

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

private

   Now : Uint64 := 0;
   Last : Uint64;

end Time;
