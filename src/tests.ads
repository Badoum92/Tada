package Tests is

   type Test_Func_T is access function return Boolean;

   procedure Test_Error (Test_Name, Msg : String);
   procedure Test_Success (Test_Name : String);

   function TC_1_1_1_Grid_Empty return Boolean;
   function TC_1_2_1_Reset return Boolean;

   function TC_2_1_1_Piece_Move_Left return Boolean;
   function TC_2_1_2_Piece_Move_Right return Boolean;
   function TC_2_2_1_Piece_Blocked_Left return Boolean;
   function TC_2_2_2_Piece_Blocked_Right return Boolean;

   function TC_3_1_1_Full_Line_Deleted return Boolean;
   function TC_3_2_1_Lowered_Lines_On_Delete return Boolean;

   function TC_4_1_1_Piece_Falls_Period return Boolean;
   function TC_4_2_1_Piece_Blocked_Down return Boolean;
   function TC_4_3_1_New_Piece_Selected return Boolean;

   function TC_5_1_Display return Boolean;

   procedure Run_Test (Test : Test_Func_T; Total, Success : in out Natural);
   procedure Exec;

end Tests;
