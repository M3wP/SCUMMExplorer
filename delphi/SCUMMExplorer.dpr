program SCUMMExplorer;

uses
  Vcl.Forms,
  FormSCUMMExpMain in '..\src\FormSCUMMExpMain.pas' {SCUMMExpMainForm},
  FormSCUMMExpConfig in '..\src\FormSCUMMExpConfig.pas' {SCUMMExpConfigForm},
  SCUMMExpTypes in '..\src\SCUMMExpTypes.pas',
  FormSCUMMExpLog in '..\src\FormSCUMMExpLog.pas' {SCUMMExpLogForm},
  DModSCUMMExpMain in '..\src\DModSCUMMExpMain.pas' {SCUMMExpMainDMod: TDataModule},
  FrameSCUMMExpMain in '..\src\FrameSCUMMExpMain.pas' {SCUMMExpMainFrame: TFrame},
  SCUMMExpStrs in '..\src\SCUMMExpStrs.pas',
  SCUMMExpClasses in '..\src\SCUMMExpClasses.pas',
  FormSCUMMExpAbout in '..\src\FormSCUMMExpAbout.pas' {SCUMMExpAboutForm};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSCUMMExpMainForm, SCUMMExpMainForm);
  Application.Run;
end.
