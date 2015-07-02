unit FormSCUMMExpSplash;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  	Vcl.StdCtrls, Vcl.ComCtrls;

type
	TSCUMMExpSplashForm = class(TForm)
		prgbrProgress: TProgressBar;
		Label1: TLabel;
		lblProgress: TLabel;
    Label2: TLabel;
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
  	SCUMMExpSplashForm: TSCUMMExpSplashForm;

implementation

{$R *.dfm}

end.
