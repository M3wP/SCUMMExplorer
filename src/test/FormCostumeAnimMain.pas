unit FormCostumeAnimMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, PNGImage, SCUMMCostumeV2;

type
	TForm1 = class(TForm)
		Panel1: TPanel;
		Panel2: TPanel;
		TrackBar2: TTrackBar;
		TrackBar3: TTrackBar;
		TrackBar4: TTrackBar;
		TrackBar5: TTrackBar;
		TrackBar6: TTrackBar;
		TrackBar1: TTrackBar;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label1: TLabel;
		Button1: TButton;
		Button2: TButton;
		Label9: TLabel;
		Button3: TButton;
		Timer1: TTimer;
		PaintBox1: TPaintBox;
		TrackBar8: TTrackBar;
		Label10: TLabel;
		Label11: TLabel;
		CheckBox2: TCheckBox;
		CheckBox3: TCheckBox;
		CheckBox4: TCheckBox;
		CheckBox5: TCheckBox;
		CheckBox6: TCheckBox;
		CheckBox7: TCheckBox;
		Label8: TLabel;
		CheckBox8: TCheckBox;
		OpenDialog1: TOpenDialog;
		CheckBox9: TCheckBox;
		CheckBox10: TCheckBox;
		CheckBox11: TCheckBox;
		CheckBox12: TCheckBox;
		CheckBox13: TCheckBox;
		CheckBox14: TCheckBox;
		Label12: TLabel;
		CheckBox15: TCheckBox;
		CheckBox16: TCheckBox;
		ListBox1: TListBox;
		Label14: TLabel;
		Button4: TButton;
		SaveDialog1: TSaveDialog;
		Button5: TButton;
		PageControl1: TPageControl;
		TabSheet1: TTabSheet;
		TabSheet2: TTabSheet;
		RadioGroup2: TRadioGroup;
		GroupBox1: TGroupBox;
		Label13: TLabel;
		RadioButton1: TRadioButton;
		RadioButton2: TRadioButton;
		CheckBox1: TCheckBox;
		ComboBox1: TComboBox;
		CheckBox17: TCheckBox;
		CheckBox18: TCheckBox;
		CheckBox19: TCheckBox;
		Label15: TLabel;
		ComboBox2: TComboBox;
		ComboBox3: TComboBox;
		ComboBox4: TComboBox;
		ComboBox5: TComboBox;
		Label16: TLabel;
		Label17: TLabel;
		Label18: TLabel;
    CheckBox20: TCheckBox;
		procedure Button1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure TrackBar8Change(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);
		procedure PaintBox1Paint(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure CheckBox17Click(Sender: TObject);
		procedure Button4Click(Sender: TObject);
		procedure Button5Click(Sender: TObject);

	private
		FName: string;
		FCostume: TSCUMMCostumeV2;
		FAnimations: TSCUMMCostAnimations;
		FMemStream: TMemoryStream;
		FUtilised: array[9..14] of TCheckBox;
		FSequences: array[9..14] of TTrackBar;
		FState: array[9..14] of TCheckBox;
		FDrawBuf: TBitmap;
		FMirror: Boolean;

		procedure IncludeAnimation(var AAnimations: TSCUMMCostAnimations;
				AAnim: Integer; ACells: Byte; const AMerge: Boolean = True);

		procedure FetchFromControls(var ADirect: TSCUMMAnimDir;
				var AType: TSCUMMAnimType;
				var AUseNearFar, ANear, ANoBody, AUseHead: Boolean;
				var AMouth: TSCUMMAnimMouth);

		procedure PrepareAnimations(var AAnimations: TSCUMMCostAnimations;
				var AMirror: Boolean; ADirect: TSCUMMAnimDir;
				AType: TSCUMMAnimType; AUseNearFar: Boolean; ANear: Boolean;
				ANoBody: Boolean; AUseHead: Boolean; AMouth: TSCUMMAnimMouth);

		procedure PrepareInformation;
		procedure InitialiseAnimations(var AAnimations: TSCUMMCostAnimations);
		procedure PrepareControls(AAnimations: TSCUMMCostAnimations);
		procedure ProgressAnimations(var AAnimations: TSCUMMCostAnimations);

		procedure PrepareFrame(AAnims: TSCUMMCostAnimations;
				AMirror: Boolean; APoint: TPoint; AFrame: TPNGImage); overload;
		procedure PrepareFrame(AAnims: TSCUMMCostAnimations;
				AMirror: Boolean; APoint: TPoint; AFrame: TBitmap); overload;

		procedure SaveFrames(AFilePath: string;
				var AAnims: TSCUMMCostAnimations; AMirror: Boolean;
				ADirect: TSCUMMAnimDir; AType: TSCUMMAnimType;
				AUseNearFar, ANear, ANoBody, AUseHead: Boolean;
				AMouth: TSCUMMAnimMouth);
		procedure LoadCostume(AFileName: string);
	public
		{ Public declarations }
	end;

var
	Form1: TForm1;


implementation

uses
	Types, IOUtils, Math;

{$R *.dfm}

const
	AMIGA_PALETTE: array[0..15] of TColor = (
		TColor($000000),	//black
		TColor($BD0000),	//blue
		TColor($00BA00),	//green
		TColor($BDBA00),	//dark cyan
		TColor($0000BD),	//red
		TColor($BD00BD),	//purple
		TColor($0075BD),	//brown
		TColor($BDBABD),	//light gray
		TColor($737573),	//dark gray
		TColor($FF7573),	//light blue
		TColor($00FF00),	//light green
		TColor($FFFF00),	//light cyan
		TColor($8C8AFF),	//pink
		TColor($FF00FF),	//light purple
		TColor($00FFFF),	//*yellow*
		TColor($FFFFFF));	//white


procedure DrawCellBitmap(ASource: TMemoryStream; ACX, ACY: Integer;
		AMirror: Boolean; out AImg: TPNGImage);
	var
	x,
	y,
	w: Integer;
	bsl: PByte;
	asl: PByteArray;
	c: TColor;
	b: Byte;

	begin
	AImg:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, ACX, ACY);

//dengland Have to be able to handle this for Zak.
	if  ASource.Size = 0 then
		Exit;

	if  AMirror then
		begin
		x:= ACX - 1;
		w:= 0;
		end
	else
		begin
		x:= 0;
		w:= ACX - 1;
		end;

//	for x:= 0 to ACX - 1 do
	repeat
		for y:= 0 to ACY - 1 do
			begin
			bsl:= PByte(AImg.Scanline[y]);
			Inc(bsl,  x * 3);
			asl:= AImg.AlphaScanline[y];

			ASource.Read(b, 1);
			asl[x]:= b;

			ASource.Read(b, 1);
			c:= AMIGA_PALETTE[b];

			bsl^:= (c and $FF0000) shr 16;
			Inc(bsl);
			bsl^:= (c and $00FF00) shr 8;
			Inc(bsl);
			bsl^:= (c and $0000FF);
			end;

		if AMirror then
			begin
			Dec(x);
			if  x < 0 then
				Break;
			end
		else
			begin
			Inc(x);
			if  x > w then
				Break;
			end;
		until False;
	end;

procedure DrawPngWithAlpha(Src, Dest: TPNGImage; const R: TRect);
	var
	X,
	Y: Integer;
	Alpha: PByte;

	begin
	Src.Draw(Dest.Canvas, R);

//	I have no idea why standard implementation of TPNGImage.Draw doesn't
//			apply transparency even to its own kind.
	for Y:= R.Top to R.Bottom - 1 do
		for X:= R.Left to R.Right - 1 do
			begin
			Alpha:= @Dest.AlphaScanline[Y]^[X];
			Alpha^:= Min(255, Alpha^ +
					Src.AlphaScanline[Y - R.Top]^[X - R.Left]);
			end;
	end;

type
	TCellDrawFunc = procedure(ACell: TPNGImage; ADest: Pointer; ARect: TRect);

procedure PNGDrawFunc(ACell: TPNGImage; ADest: Pointer; ARect: TRect);
	begin
	DrawPngWithAlpha(ACell, TPNGImage(ADest), ARect);
	end;

procedure BitmapDrawFunc(ACell: TPNGImage; ADest: Pointer; ARect: TRect);
	begin
	ACell.DrawUsingPixelInformation(TBitmap(ADest).Canvas,
			Point(ARect.Left, ARect.Top));
	end;

procedure DrawCellToFrame(ACostume: TSCUMMCostumeV2;
		AAnims: TSCUMMCostAnimations; AMirror: Boolean; AStream: TMemoryStream;
		AAnim: Byte; APoint: TPoint; AFunc: TCellDrawFunc; AData: Pointer);
	var
	p: TPoint;
	v,
	q: Byte;
	cell: TPNGImage;

	begin
	p.X:= APoint.X;
	p.Y:= APoint.Y;

	if  (AAnims[AAnim].state = sasRunning) then
//	or  ((FAnimations[i].state = sasEnded)
//	and  (FAnimations[i].currCell < $FF)) then
		begin
		v:= ACostume.layerInfos[AAnim].cmdCellListIdx;
		q:= ACostume.cmdCellIndex[v].cmdCellIdxs[AAnims[AAnim].currCmd and $7F];

//FIXME dengland HACK!!!  This hack is for Zak since in it we are getting an image
//      with no data that seems to have to be there for everything except where it is
//		referred to.  If I remove it, all of the other animations are incorrect.  So,
//		if we are going to try to use an empty image, look to the next one instead.
		if Length(ACostume.cellInfos[q].data) = 0 then
			Inc(q);

		if  AMirror then
			Dec(p.X, ACostume.cellInfos[q].relativeX +
					ACostume.cellInfos[q].width - 72)
		else
			p.X:= p.X - 72 + ACostume.cellInfos[q].relativeX;

		Inc(p.Y, ACostume.cellInfos[q].relativeY +
				ACostume.cellInfos[q].moveY);

		AStream.Clear;

		ACostume.cellInfos[q].DecodeProcV2(ACostume.resInfo.colour,
				AStream);

		AStream.Seek(0, soFromBeginning);
		DrawCellBitmap(AStream, ACostume.cellInfos[q].width,
				ACostume.cellInfos[q].height, AMirror, cell);
		try
// 			cell.DrawUsingPixelInformation(frame.Canvas, p);
//			DrawPngWithAlpha(cell, AFrame, Rect(p.X, p.Y,
//					p.X + cell.Width, p.Y + cell.Height));
			AFunc(cell, AData, Rect(p.X, p.Y, p.X + cell.Width,
					p.Y + cell.Height));

			finally
			cell.Free;
			end;
		end;
	end;



{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
	begin
	if  OpenDialog1.Execute then
		begin
		LoadCostume(OpenDialog1.FileName);

		Button3.Enabled:= True;
		Button3Click(Sender);
		end;
	end;

procedure TForm1.Button2Click(Sender: TObject);
	begin
	Timer1.Enabled:= not Timer1.Enabled;
	if  Timer1.Enabled then
		Button2.Caption:= 'Stop'
	else
		Button2.Caption:= 'Run';
	end;

procedure TForm1.Button3Click(Sender: TObject);
	var
	dir: TSCUMMAnimDir;
	atype: TSCUMMAnimType;
	useNearFar,
	useNear,
	noBody,
	useHead: Boolean;
	mouth: TSCUMMAnimMouth;

	begin
	InitialiseAnimations(FAnimations);

	if  PageControl1.ActivePageIndex = 0 then
		begin
		FetchFromControls(dir, atype, useNearFar, useNear, noBody, useHead, mouth);
		PrepareAnimations(FAnimations, FMirror, dir, atype, useNearFar, useNear,
				noBody, useHead, mouth);
		end
	else
		begin
		FMirror:= CheckBox20.Checked and not FCostume.resInfo.NoMirror;

		if  ComboBox2.ItemIndex > 0 then
			IncludeAnimation(FAnimations, ComboBox2.ItemIndex - 1, $FF);
		if  ComboBox3.ItemIndex > 0 then
			IncludeAnimation(FAnimations, ComboBox3.ItemIndex - 1, $FF);
		if  ComboBox4.ItemIndex > 0 then
			IncludeAnimation(FAnimations, ComboBox4.ItemIndex - 1, $FF);
		if  ComboBox5.ItemIndex > 0 then
			IncludeAnimation(FAnimations, ComboBox5.ItemIndex - 1, $FF);
		end;

	PrepareControls(FAnimations);
	end;

procedure TForm1.Button4Click(Sender: TObject);
	var
	m: Boolean;
	dir: TSCUMMAnimDir;
	atype: TSCUMMAnimType;
	useNearFar,
	useNear,
	noBody,
	useHead: Boolean;
	mouth: TSCUMMAnimMouth;
	anims: TSCUMMCostAnimations;
	f: string;

	begin
	if  SaveDialog1.Execute then
		begin
		f:= TPath.GetFileNameWithoutExtension(SaveDialog1.FileName);
		f:= IncludeTrailingPathDelimiter(
				ExtractFilePath(SaveDialog1.FileName))+ f;

		InitialiseAnimations(anims);
		FetchFromControls(dir, atype, useNearFar, useNear, noBody, useHead,
				mouth);
		PrepareAnimations(anims, m, dir, atype, useNearFar, useNear, noBody,
				useHead, mouth);

		SaveFrames(f, anims, m, dir, atype, useNearFar, useNear, noBody,
				useHead, mouth);
		end;
	end;

procedure TForm1.Button5Click(Sender: TObject);
	var
	t: TextFile;
	s: string;
	anims: TSCUMMCostAnimations;
	m: Boolean;
	dir: TSCUMMAnimDir;
	atype: TSCUMMAnimType;
	useNearFar,
	useNear,
	noBody,
	useHead: Boolean;
	mouth: TSCUMMAnimMouth;

	procedure DoLoad;
		var
		f: string;

		begin
		ReadLn(t, f);
		f:= Trim(f);
		LoadCostume(f);
		end;

	procedure DoSelect;
		var
		i1,
		i2,
		i3,
		i4,
		i5,
		i6,
		i7: Integer;

		begin
		InitialiseAnimations(anims);

		ReadLn(t, i1, i2, i3, i4, i5, i6, i7);
		dir:= TSCUMMAnimDir(i1);
		atype:= TSCUMMAnimType(i2);
		useNearFar:= i3 <> 0;
		useNear:= i4 <> 0;
		noBody:= i5 <> 0;
		useHead:= i6 <> 0;
		mouth:= TSCUMMAnimMouth(i7);

		PrepareAnimations(anims, m, dir, atype, useNearFar, useNear, noBody,
				useHead, mouth);
		end;

	procedure DoSave;
		var
		f: string;

		begin
		ReadLn(t, f);
		f:= Trim(f);

		SaveFrames(f, anims, m, dir, atype, useNearFar, useNear, noBody,
				useHead, mouth);
		end;

	begin
	if  OpenDialog1.Execute then
		begin
		AssignFile(t, OpenDialog1.FileName);
		try
			FileMode:= fmOpenRead;
			Reset(t);

			while not Eof(t) do
				begin
//				Read instruction
				Readln(t, s);
				s:= Trim(s);

				if  (Length(s) > 0)
				and (s[1] = '#') then
					Continue;

				if  CompareText(s, 'PATH') = 0 then
					begin
					Readln(t, s);
					s:= Trim(s);
					SetCurrentDir(s);
					end
				else if CompareText(s, 'LOAD') = 0 then
					DoLoad
				else if CompareText(s, 'SELECT') = 0 then
					DoSelect
				else if CompareText(s, 'SAVE') = 0 then
					DoSave
				else
					Assert(false, 'Invalid command in batch file');
				end;

			finally
			CloseFile(t);
			end;
		end;
	end;

procedure TForm1.CheckBox17Click(Sender: TObject);
	begin
	CheckBox18.Enabled:= CheckBox17.Checked;
	end;

procedure TForm1.FetchFromControls(var ADirect: TSCUMMAnimDir;
		var AType: TSCUMMAnimType;
		var AUseNearFar, ANear, ANoBody, AUseHead: Boolean;
		var AMouth: TSCUMMAnimMouth);
	begin
	ADirect:= TSCUMMAnimDir(RadioGroup2.ItemIndex);
	if  RadioButton1.Checked then
		AType:= satWalk
	else
		AType:= satStand;
	AUseNearFar:= CheckBox17.Checked;
	ANear:= CheckBox18.Checked;
	ANoBody:= CheckBox19.Checked;
	AUseHead:= CheckBox1.Checked;
	AMouth:= TSCUMMAnimMouth(ComboBox1.ItemIndex);
	end;

procedure TForm1.FormCreate(Sender: TObject);
	begin
	TrackBar8Change(Self);
	FMemStream:= TMemoryStream.Create;
	FDrawBuf:= TBitmap.Create;
	FDrawBuf.Width:= 160;
	FDrawBuf.Height:= 100;

	FMirror:= False;

	InitialiseAnimations(FAnimations);

	FUtilised[9]:= CheckBox2;
	FUtilised[10]:= CheckBox3;
	FUtilised[11]:= CheckBox4;
	FUtilised[12]:= CheckBox5;
	FUtilised[13]:= CheckBox6;
	FUtilised[14]:= CheckBox7;

	FState[9]:= CheckBox9;
	FState[10]:= CheckBox10;
	FState[11]:= CheckBox11;
	FState[12]:= CheckBox12;
	FState[13]:= CheckBox13;
	FState[14]:= CheckBox14;

	FSequences[9]:= TrackBar2;
	FSequences[10]:= TrackBar1;
	FSequences[11]:= TrackBar3;
	FSequences[12]:= TrackBar4;
	FSequences[13]:= TrackBar5;
	FSequences[14]:= TrackBar6;
	end;

procedure TForm1.IncludeAnimation(var AAnimations: TSCUMMCostAnimations;
		AAnim: Integer; ACells: Byte; const AMerge: Boolean);
	var
	i: Integer;
	a: Integer;

	begin
	a:= FCostume.resInfo.animIndex[AAnim];

	for i:= Low(AAnimations) to High(AAnimations) do
		begin
		if  (ACells = $FF)
		or  (ACells = i) then
			if  (High(FCostume.animInfos) >= a)
			and FCostume.animInfos[a].IsCellsUsed(i) then
				begin
				if  FCostume.animInfos[a].layerCmds[i].IsAvail then
					AAnimations[i].state:= sasEnded
				else
					AAnimations[i].state:= sasRunning;

				if  FCostume.animInfos[a].layerCmds[i].IsPause then
					AAnimations[i].mode:= samPaused
				else
					AAnimations[i].mode:= samRunning;

				AAnimations[i].startIdx:=
						FCostume.animInfos[a].layerCmds[i].cmdIdx;
				AAnimations[i].currCmd:=
						FCostume.animInfos[a].layerCmds[i].cmd;
				AAnimations[i].steps:=
						FCostume.animInfos[a].layerCmds[i].Steps;
				AAnimations[i].currStep:= 0;
				AAnimations[i].loop:=
						FCostume.animInfos[a].layerCmds[i].IsLoop;
				end
			else if not AMerge then
				AAnimations[i].state:= sasNotVisible;

		if  (AAnimations[i].state = sasRunning)
		and  (AAnimations[i].mode = samRunning) then
			AAnimations[i].included:= True;
		end;
	end;

procedure TForm1.InitialiseAnimations(var AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	begin
	for i:= High(AAnimations) downto Low(AAnimations) do
		begin
		AAnimations[i].state:= sasNotVisible;
		AAnimations[i].mode:= samPaused;
		AAnimations[i].startIdx:= $FF;
		AAnimations[i].currCmd:= $FF;
		AAnimations[i].steps:= $FF;
		AAnimations[i].currStep:= $FF;
		AAnimations[i].loop:= False;
		AAnimations[i].included:= False;
		end;
	end;

procedure TForm1.LoadCostume(AFileName: string);
	var
	f: TFileStream;
	m: TMemoryStream;
	w: Cardinal;

	begin
	InitialiseAnimations(FAnimations);

	FName:= TPath.GetFileNameWithoutExtension(AFileName);

	w:= 0;
	m:= TMemoryStream.Create;
	try
//		need to inject 4 bytes because the dumping program removed them.
		m.Write(w, 4);

		f:= TFileStream.Create(AFileName, fmOpenRead);
		try
//			Use copy from because injecting bytes
			m.CopyFrom(f, f.Size);

			finally
			f.Free;
			end;

		m.Seek(0, soFromBeginning);
		FCostume.LoadFromStream(m);

		finally
		m.Free;
		end;

	PrepareInformation;
	end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
	var
	p: TPoint;

	begin
	FDrawBuf.Canvas.Brush.Color:= clBtnFace;
	FDrawBuf.Canvas.Brush.Style:= bsSolid;
	FDrawBuf.Canvas.FillRect(Rect(0, 0, 319, 199));

	p.X:= 80;
	p.Y:= -10;
	PrepareFrame(FAnimations, FMirror, p, FDrawBuf);

//	PaintBox1.Canvas.Draw(0, 0, FDrawBuf);
	PaintBox1.Canvas.StretchDraw(Rect(0, 0, 320, 240), FDrawBuf);
	end;

procedure TForm1.PrepareAnimations(var AAnimations: TSCUMMCostAnimations;
		var AMirror: Boolean; ADirect: TSCUMMAnimDir; AType: TSCUMMAnimType;
		AUseNearFar: Boolean; ANear: Boolean; ANoBody: Boolean;
		AUseHead: Boolean; AMouth: TSCUMMAnimMouth);
	var
	a: Integer;

	begin
	AMirror:= (ADirect = sadRight) and not FCostume.resInfo.NoMirror;

//	This logic needs a clean up
	if  not ANoBody then
		if  not AUseNearFar then
			begin
			if  AType = satWalk then
				a:= 0
			else
				a:= 1;

			IncludeAnimation(AAnimations, a * 4 + Ord(ADirect), $FF);
			end
		else
			begin
			if  AType = satWalk then
				begin
				a:= 1;
				if  ANear then
					begin
					if (ADirect > sadLeft) then
						a:= Ord(ADirect);
					end
				else
					begin
					if (ADirect > sadLeft) then
						a:= Ord(ADirect)
					else
						a:= 0;
					end;
				end
			else
				a:= 4 + Ord(ADirect);

			IncludeAnimation(AAnimations, a, $FF);
			end;

	if  AUseHead then
		begin
		a:= 8 + Ord(ADirect);

//		if  not ANoBody then
//			IncludeAnimation(AAnimations, a, 12)
//		else
			IncludeAnimation(AAnimations, a, $FF)
		end;

	if  AMouth > smoNone then
		begin
		a:= 8 + (4 * Ord(AMouth)) + Ord(ADirect);
//		IncludeAnimation(AAnimations, a, 11);
		IncludeAnimation(AAnimations, a, $FF);
		end;
	end;

procedure TForm1.PrepareControls(AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	begin
	for i:= 14 downto 9 do
		begin
		FUtilised[i].Checked:= AAnimations[i].state <> sasNotVisible;

		FState[i].State:= cbChecked;
		if  AAnimations[i].mode = samPaused then
			FState[i].State:= cbGrayed
		else if AAnimations[i].state = sasEnded then
			FState[i].State:= cbUnchecked;

		FSequences[i].Min:= 1;
		if  AAnimations[i].steps < $FF then
			FSequences[i].Max:= AAnimations[i].steps + 1
		else
			FSequences[i].Max:= 2;

		FSequences[i].Position:= 1;
		if  AAnimations[i].steps < $FF then
			FSequences[i].Position:= AAnimations[i].currStep + 1;
		end;


	ListBox1.Clear;
	for i:= 0 to 8 do
		if  AAnimations[i].included then
			ListBox1.Items.Add('Layer ' + IntToStr(i));

	if  AAnimations[15].included then
			ListBox1.Items.Add('Layer 15');
	end;

procedure TForm1.PrepareFrame(AAnims: TSCUMMCostAnimations;
		AMirror: Boolean; APoint: TPoint; AFrame: TPNGImage);
	var
	i: Byte;

	begin
	for i:= High(AAnims) downto Low(AAnims) do
		DrawCellToFrame(FCostume, AAnims, AMirror, FMemStream, i, APoint,
			PNGDrawFunc, AFrame);
	end;

procedure TForm1.PrepareFrame(AAnims: TSCUMMCostAnimations;
		AMirror: Boolean; APoint: TPoint; AFrame: TBitmap);
	var
	i: Byte;

	begin
	for i:= High(AAnims) downto Low(AAnims) do
		DrawCellToFrame(FCostume, AAnims, AMirror, FMemStream, i, APoint,
				BitmapDrawFunc, AFrame);
	end;

procedure TForm1.PrepareInformation;
	var
	i,
	j: Byte;
	a: set of 0..15;

	begin
	Label9.Caption:= FName;
	CheckBox8.Checked:= FCostume.resInfo.NoMirror;

	CheckBox15.Checked:= Length(FCostume.animInfos) = 0;

	a:= [];
	if Length(FCostume.animInfos) > 0 then
		for i:= Low(FCostume.animInfos) to High(FCostume.animInfos) do
			for j:= Low(FCostume.animInfos[i].layerCmds) to
					High(FCostume.animInfos[i].layerCmds) do
				if  FCostume.animInfos[i].layerCmds[j].isUsed then
					Include(a, j);
	a:= a - [9..14];
	CheckBox16.Checked:= a <> [];

	ComboBox2.Items.Clear;
	ComboBox3.Items.Clear;
	ComboBox4.Items.Clear;
	ComboBox5.Items.Clear;

	ComboBox2.Items.Add('Not Used');
	ComboBox3.Items.Add('Not Used');
	ComboBox4.Items.Add('Not Used');
	ComboBox5.Items.Add('Not Used');

	for i:= 0 to High(FCostume.resInfo.animIndex) do
		begin
		ComboBox2.Items.Add(IntToStr(i));
		ComboBox3.Items.Add(IntToStr(i));
		ComboBox4.Items.Add(IntToStr(i));
		ComboBox5.Items.Add(IntToStr(i));
		end;

	ComboBox2.ItemIndex:= 0;
	ComboBox3.ItemIndex:= 0;
	ComboBox4.ItemIndex:= 0;
	ComboBox5.ItemIndex:= 0;
	end;

procedure TForm1.ProgressAnimations(var AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	procedure ProgressInner(AI: Integer);
		begin
		if  AAnimations[AI].currStep <= AAnimations[AI].steps then
			Inc(AAnimations[AI].currStep);

		if  (AAnimations[AI].currStep > AAnimations[AI].steps) then
			if (AAnimations[AI].loop) then
				AAnimations[AI].currStep:= 0
			else
				AAnimations[AI].currStep:= AAnimations[AI].steps;
		end;

	begin
	for i:= Low(AAnimations) to High(AAnimations) do
		if  (AAnimations[i].state = sasRunning)
		and (AAnimations[i].mode = samRunning) then
			begin
			ProgressInner(i);

//			if  (FCostume.animCmds[AAnimations[i].startIdx +
//					AAnimations[i].currStep].cmd and $80) <> 0 then
//				ProgressInner(i);

			AAnimations[i].currCmd:= FCostume.animCmds[
					AAnimations[i].startIdx + AAnimations[i].currStep].cmd;
			end;
	end;

procedure TForm1.SaveFrames(AFilePath: string; var AAnims: TSCUMMCostAnimations;
		AMirror: Boolean; ADirect: TSCUMMAnimDir; AType: TSCUMMAnimType;
		AUseNearFar, ANear, ANoBody, AUseHead: Boolean; AMouth: TSCUMMAnimMouth);
	var
	s: Byte;
	frame: TPNGImage;
	i: Byte;

	function ScanAnimsForHighSteps: Byte;
		var
		i: Integer;

		begin
		Result:= 0;

		for i:= Low(AAnims) to High(AAnims) do
			if  AAnims[i].state <> sasNotVisible then
				if  (AAnims[i].steps <> $FF)
				and (AAnims[i].steps > Result) then
					Result:= AAnims[i].steps;
		end;

	begin
	s:= ScanAnimsForHighSteps;

	for i:= 0 to s do
		begin
		frame:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, 36, 88);

		try
			PrepareFrame(AAnims, AMirror, Point(18, -14), frame);

			frame.SaveToFile(AFilePath + IntToHex(i, 2) + '.png');

			finally
			frame.Free;
			end;

		ProgressAnimations(AAnims);
		end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
	begin
	PrepareControls(FAnimations);
	PaintBox1.Invalidate;
	ProgressAnimations(FAnimations);
	end;

procedure TForm1.TrackBar8Change(Sender: TObject);
	var
	v: Integer;

	begin
	v:= TrackBar8.Max - TrackBar8.Position + TrackBar8.Min;
	Timer1.Interval:= v;
	TrackBar8.Hint:= IntToStr(v) + 'ms';
	end;

end.
