unit Unit2;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ExtCtrls, Vcl.StdCtrls, PNGImage;

type
  TForm2 = class(TForm)
	Button1: TButton;
	Image1: TImage;
	OpenDialog1: TOpenDialog;
	procedure Button1Click(Sender: TObject);
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
	const
	p: array[0..15] of TColor = (
		TColor($000000), TColor($0000BB), TColor($00BB00), TColor($00BBBB),
		TColor($BB0000), TColor($BB00BB), TColor($BB7700), TColor($BBBBBB),
		TColor($777777), TColor($7777FF), TColor($00FF00), TColor($00FFFF),
		TColor($FF8888), TColor($FF00FF), TColor($FFFF00), TColor($FFFFFF));


	var
	f: TFileStream;
	i: TPNGImage;
	w,
	h: Word;
	b: Byte;
	x,
	y: Integer;
	bsl: PByte;
	asl: PByteArray;
	c: TColor;

	begin
	if  OpenDialog1.Execute then
		begin
		f:= TFileStream.Create(OpenDialog1.FileName, fmOpenRead);

		f.Read(w, 2);
		f.Read(h, 2);

		i:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, w, h);

		for x:= 0 to w - 1 do
			for y:= 0 to h - 1 do
				begin
				bsl:= PByte(i.Scanline[y]);
				Inc(bsl,  x * 3);
				asl:= i.AlphaScanline[y];

				f.Read(b, 1);
				asl[x]:= b;

				f.Read(b, 1);
				c:= p[b];

				bsl^:= (c and $0000FF);
				Inc(bsl);
				bsl^:= (c and $00FF00) shr 8;
				Inc(bsl);
				bsl^:= (c and $FF0000) shr 16;
//				Inc(bsl);
				end;

		Image1.Picture.Assign(i);
		i.Free;
		f.Free;
        end;
	end;

end.
