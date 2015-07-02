program CostumeAnim;

uses
  Vcl.Forms,
  FormCostumeAnimMain in '..\..\src\test\FormCostumeAnimMain.pas' {Form1},
  SCUMMCostumeV2 in '..\..\src\test\SCUMMCostumeV2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
