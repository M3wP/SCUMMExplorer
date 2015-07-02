unit FormSCUMMExpMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ImgList, System.Actions,
	Vcl.ActnList, Vcl.ToolWin, Vcl.AppEvnts, FrameSCUMMExpMain;

const
	MSG_SCUMMEXP_MAINPREP = WM_USER + $0D01;

type
	TSCUMMExpMainForm = class(TForm)
		CoolBar1: TCoolBar;
		ToolBar1: TToolBar;
		ToolBar2: TToolBar;
		StatusBar1: TStatusBar;
		SCUMMExpMainFrame1: TSCUMMExpMainFrame;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);

	private
		FFirstTime: Boolean;

	protected
		procedure MsgPrepare(var AMsg: TMessage); message MSG_SCUMMEXP_MAINPREP;

	public
		{ Public declarations }
	end;

var
	SCUMMExpMainForm: TSCUMMExpMainForm;

implementation

uses
	SCUMMLogTypes, DModSCUMMExpMain;

{$R *.dfm}

procedure TSCUMMExpMainForm.FormCreate(Sender: TObject);
	begin
	FFirstTime:= True;

	Application.CreateForm(TSCUMMExpMainDMod, SCUMMExpMainDMod);
	SCUMMExpMainDMod.MainFrame:= SCUMMExpMainFrame1;
	end;

procedure TSCUMMExpMainForm.FormDestroy(Sender: TObject);
	begin
//
	end;

procedure TSCUMMExpMainForm.FormShow(Sender: TObject);
	begin
	if  FFirstTime then
		begin
		FFirstTime:= False;
		PostMessage(Self.Handle, MSG_SCUMMEXP_MAINPREP, 0, 0);
		end;
	end;

procedure TSCUMMExpMainForm.MsgPrepare(var AMsg: TMessage);
	begin
	SCUMMExpMainDMod.Prepare;
	end;

end.
