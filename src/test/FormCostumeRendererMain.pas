unit FormCostumeRendererMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, SCUMMActor, SCUMMCostume,
	PNGImage;

type
	TForm1 = class(TForm)
		Panel1: TPanel;
		Button1: TButton;
		Label1: TLabel;
		OpenDialog1: TOpenDialog;
		Panel2: TPanel;
		trkbrAnim: TTrackBar;
		Label4: TLabel;
		trkbrFacing: TTrackBar;
		Label3: TLabel;
		Label2: TLabel;
		trkbrFrame: TTrackBar;
		ScrollBox1: TScrollBox;
		Panel5: TPanel;
		Label6: TLabel;
		Image15: TImage;
		Panel3: TPanel;
		Label5: TLabel;
		Image01: TImage;
		Panel4: TPanel;
		Label7: TLabel;
		Image00: TImage;
		Panel6: TPanel;
		Label8: TLabel;
		Image14: TImage;
		Panel7: TPanel;
		Label9: TLabel;
		Image13: TImage;
		Panel8: TPanel;
		Label10: TLabel;
		Image12: TImage;
		Panel9: TPanel;
		Label11: TLabel;
		Image11: TImage;
		Panel10: TPanel;
		Label12: TLabel;
		Image10: TImage;
		Panel11: TPanel;
		Label13: TLabel;
		Image09: TImage;
		Panel12: TPanel;
		Label14: TLabel;
		Image08: TImage;
		Panel13: TPanel;
		Label15: TLabel;
		Image07: TImage;
		Panel14: TPanel;
		Label16: TLabel;
		Image06: TImage;
		Panel15: TPanel;
		Label17: TLabel;
		Image05: TImage;
		Panel16: TPanel;
		Label18: TLabel;
		Image04: TImage;
		Panel17: TPanel;
		Label19: TLabel;
		Image03: TImage;
		Panel18: TPanel;
		Label20: TLabel;
		Image02: TImage;
		procedure Button1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);

	private
		FResource: TMemoryStream;
		FLoader: TClassicCostumeLoader;
		FActor: TActorV2;
		FBuffer: TSimpleDrawBuffer;
		FTracking: Boolean;
		FImages: array[0..15] of TImage;

		procedure ResetControls;
		procedure ReleaseResource;
		procedure AquireResource(AFileName: string);
		procedure InitResources;
		procedure UpdateControls;
		procedure PerformDecodeV2;
		procedure DrawCellBitmaps;

	public
		{ Public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.dfm}

const
	AMIGA_PALETTE: array[0..15] of TColor = (
		TColor($000000), TColor($0000BB), TColor($00BB00), TColor($00BBBB),
		TColor($BB0000), TColor($BB00BB), TColor($BB7700), TColor($BBBBBB),
		TColor($777777), TColor($7777FF), TColor($00FF00), TColor($00FFFF),
		TColor($FF8888), TColor($FF00FF), TColor($FFFF00), TColor($FFFFFF));


procedure TForm1.AquireResource(AFileName: string);
	var
	d: Cardinal;
	f: TFileStream;

	begin
	f:= TFileStream.Create(AFileName, fmOpenRead);
	try
		FResource:= TMemoryStream.Create;

		d:= 0;
		FResource.Write(d, 4);

		FResource.CopyFrom(f, f.Size);
		FResource.Seek(0, soFromBeginning);

		finally
		f.Free;
		end;

	FLoader:= TClassicCostumeLoader.Create(nil);
	FBuffer.renderer:= TClassicCostumeRenderer.Create(nil);
	FActor:= TActorV2.Create(nil, 1);
	end;

procedure TForm1.Button1Click(Sender: TObject);
	begin
	if OpenDialog1.Execute then
		begin
		ResetControls;
		ReleaseResource;
		AquireResource(OpenDialog1.FileName);
		InitResources;
		UpdateControls;
		PerformDecodeV2;
		end;
	end;

procedure TForm1.DrawCellBitmaps;
	var
	i,
	j,
	x,
	y: Integer;
	img: TPNGImage;
	bsl: PByte;
	asl: PByteArray;
	c: TColor;
	b: Byte;
	cx,
	cy: Integer;

	begin
	for i:= 0 to 15 do
		begin
		j:= 0;
		cx:= FBuffer.cells[i].width;
		cy:= FBuffer.cells[i].height;

		if  (cx > 0)
		and (cy > 0) then
			begin
			img:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, cx, cy);

			for x:= 0 to cx - 1 do
				for y:= 0 to cy - 1 do
					begin
					bsl:= PByte(img.Scanline[y]);
					Inc(bsl,  x * 3);
					asl:= img.AlphaScanline[y];

					b:= FBuffer.cells[i].image[j];
					Inc(j);
					asl[x]:= b;

					b:= FBuffer.cells[i].image[j];
					Inc(j);
					c:= AMIGA_PALETTE[b];

					bsl^:= (c and $0000FF);
					Inc(bsl);
					bsl^:= (c and $00FF00) shr 8;
					Inc(bsl);
					bsl^:= (c and $FF0000) shr 16;
					end;

			FImages[i].Picture.Assign(img);
			img.Free;
			end
		else
			begin
			FImages[i].Width:= 0;
			FImages[i].Height:= 0;
			end;
		end;
	end;

procedure TForm1.FormCreate(Sender: TObject);
	var
	i: Integer;
	b: Byte;

	begin
	FImages[0]:= Image00;
	FImages[1]:= Image01;
	FImages[2]:= Image02;
	FImages[3]:= Image03;
	FImages[4]:= Image04;
	FImages[5]:= Image05;
	FImages[6]:= Image06;
	FImages[7]:= Image07;
	FImages[8]:= Image08;
	FImages[9]:= Image09;
	FImages[10]:= Image10;
	FImages[11]:= Image11;
	FImages[12]:= Image12;
	FImages[13]:= Image13;
	FImages[14]:= Image14;
	FImages[15]:= Image15;
	end;

procedure TForm1.InitResources;
	begin
	FActor.InitActor(-1);
	FActor.SetActorCostume(1);
	FActor.ShowActor(FLoader, FResource);
	end;

procedure TForm1.PerformDecodeV2;
	begin
	FActor.DrawActorCostume(@FBuffer, FResource, False);

	DrawCellBitmaps;
	end;

procedure TForm1.ReleaseResource;
	begin
	if  Assigned(FResource) then
		FreeAndNil(FResource);

	if  Assigned(FActor) then
		FreeAndNil(FActor);

	if  Assigned(FBuffer.renderer) then
		FreeAndNil(FBuffer.renderer);

	if Assigned(FLoader) then
		FreeAndNil(FLoader);
	end;

procedure TForm1.ResetControls;
	begin
	FTracking:= False;
	try
		trkbrFrame.Position:= 0;
		trkbrAnim.Position:= 0;
		trkbrFacing.Position:= 0;

		finally
		FTracking:= True;
		end;
	end;

procedure TForm1.UpdateControls;
	begin

	end;

end.
