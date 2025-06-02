with Ada.Text_IO;
with GNAT.OS_Lib;

package body Utils is

   procedure Run_Python(Script : String) is
      Command : String := "python " & Script;
      Result  : Integer;
   begin
      Result := GNAT.OS_Lib.Spawn(Command);
      Ada.Text_IO.Put_Line("Python exited with: " & Integer'Image(Result));
   end Run_Python;

   procedure Read_Log(Path : String) is
      File : Ada.Text_IO.File_Type;
      Line : String(1 .. 200);
      Last : Natural;
   begin
      Ada.Text_IO.Open(File, Ada.Text_IO.In_File, Path);
      while not Ada.Text_IO.End_Of_File(File) loop
         Ada.Text_IO.Get_Line(File, Line, Last);
         Ada.Text_IO.Put_Line(Line(1 .. Last));
      end loop;
      Ada.Text_IO.Close(File);
   end Read_Log;

end Utils;
