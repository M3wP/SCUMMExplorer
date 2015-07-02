unit FormSCUMMExpAbout;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ExtCtrls, Vcl.StdCtrls;

type
	TSCUMMExpAboutForm = class(TForm)
		Image1: TImage;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		LinkLabel1: TLinkLabel;
		Label9: TLabel;
		Label10: TLabel;
		Label8: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClick(Sender: TObject);
	private
		procedure ReadIconFromApp;

	public
		{ Public declarations }
	end;

var
	SCUMMExpAboutForm: TSCUMMExpAboutForm;

implementation

uses
	SCUMMConsts;

{$R *.dfm}

procedure TSCUMMExpAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
	begin
	Close;
	end;

procedure TSCUMMExpAboutForm.ReadIconFromApp;
	var
	i: TIcon;

	begin
	i:= TIcon.Create;
	try
		i.Handle:= LoadImage(HInstance, MakeIntResource('MAINICON'),
				IMAGE_ICON, 128, 128, LR_DEFAULTCOLOR);

		if  i.Handle > 0 then
			Image1.Picture.Icon:= i
		else
			Image1.Picture.Icon:= Application.Icon;

		finally
		i.Free;
		end;
	end;

procedure TSCUMMExpAboutForm.FormClick(Sender: TObject);
	begin
	Close;
	end;

procedure TSCUMMExpAboutForm.FormCreate(Sender: TObject);
	begin
	ReadIconFromApp;

	label6.Caption:= Format('%2.2d.%2.2d.%4.4d - %s - %s', [
			VAL_SCUMMEXP_EXCORE_VMAJ, VAL_SCUMMEXP_EXCORE_VMIN,
			VAL_SCUMMEXP_EXCORE_VBLD, LIT_SCUMMEXP_EXCORE_VTYP,
			LIT_SCUMMEXP_EXCORE_VSTG]);
	end;

end.
