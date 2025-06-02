with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

procedure Main is
   File   : Ada.Text_IO.File_Type;
   Line   : Unbounded_String;
begin
   Put_Line("Ada Logic - Reading Log Events");
   Open(File, In_File, "./shared/output_log.txt");

   while not End_Of_File(File) loop
      Ada.Strings.Unbounded.Text_IO.Get_Line(File, Line);
      if Index(Line, "Bright screen region detected", 1) > 0 then
         Put_Line("⚠️  Alert: Bright region detected by Python.");
      end if;
   end loop;

   Close(File);
end Main;
