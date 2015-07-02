program RoomReader;

uses
  Vcl.Forms,
  FormRoomReaderMain in '..\..\src\test\FormRoomReaderMain.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
