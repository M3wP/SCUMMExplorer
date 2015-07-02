unit FormObjectReaderMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
	Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
	Vcl.ToolWin, PNGImage, System.Generics.Collections, Vcl.ExtCtrls;

type
	TSCUMMObjResource = record
		roomNo: Byte;
		OBIMoffset: Word;
		OBCDoffset: Word;

		unk1: Word;
		size: Word;
		unk2: Word;
	end;

	PSCUMMObjectData = ^TSCUMMObjectData;
	TSCUMMObjectData = record
		resInfo: TSCUMMObjResource;
		obj_nr: Word;
		walk_x,
		walk_y: Word;
		x_pos,
		y_pos: SmallInt;
		width,
		height: Word;
		actorDir: Byte;
		parent: Byte;
		parentstate: Byte;

		byte17: Byte;

		state: Byte;
		fl_object_index: Byte;
		flags: Byte;

		img: TPNGImage;
	end;


	TForm4 = class(TForm)
		ToolBar1: TToolBar;
		StatusBar1: TStatusBar;
		TreeView1: TTreeView;
		ToolButton1: TToolButton;
		FileOpenDialog1: TFileOpenDialog;
		Panel1: TPanel;
		RichEdit1: TRichEdit;
		Image1: TImage;
		Splitter1: TSplitter;
		Splitter2: TSplitter;
		procedure ToolButton1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
	private
		FObjects: TList<PSCUMMObjectData>;

	protected
		procedure Clear;

		procedure ReadFiles(const APath: string);
		procedure ReadFile(const AIndex: Integer; const AFileName: string);
		procedure LoadFileToMemory(const AFileName: string; out AMem: TMemoryStream);

		procedure UpdateDisplay;

	public
		{ Public declarations }
	end;

var
	Form4: TForm4;

implementation

{$R *.dfm}

uses
	System.Generics.Defaults, IOUtils;

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

type
	TObjCompare = class(TInterfacedObject, IComparer<PSCUMMObjectData>)
	public
		function Compare(const Left, Right: PSCUMMObjectData): Integer;
	end;

procedure DecodeObjectImage(AData: TMemoryStream; AWidth, AHeight: Word;
		out ADest: TPNGImage);
	var
	dstTmp: array of Byte;
	dst: PByte;

//	byte *mask_ptr;
	src: PByte;
	color,
	data: Byte;
	run: Integer;
	dither: Boolean;
	dither_table: array[0..127] of Byte;
	ptr_dither_table: PByte;
	theX,
	theY,
	maxX: Integer;
//	dstX,
//	dstY: Integer;

	begin
	Assert(AHeight <= 128, 'Height is ' + IntToStr(AHeight));

	SetLength(dstTmp, AWidth * AHeight);
	dst:= @dstTmp[0];

//	data:= 0;
	dither:= False;
	FillChar(dither_table[0], SizeOf(dither_table), 0);

	run:= 1;
	color:= 0;
	src:= PByte(AData.Memory) + AData.Position;
	theX:= 0;
	maxX:= AWidth;

	while theX < maxX do
		begin
		ptr_dither_table:= @dither_table[0];

		for theY:= 0 to AHeight - 1 do
			begin
			Dec(run);
			if  run = 0 then
				begin
				data:= src^;
				Inc(src);

				if  (data and $80) <> 0 then
					begin
					run:= data and $7F;
					dither:= True;
					end
				else
					begin
					run:= data shr 4;
					dither:= False;
					end;

//				color:= AMIGA_PALETTE[data and $0F];
				color:= data and $0F;
				if  run = 0 then
					begin
					run:= src^;
					Inc(src);
					end;
				end;

			if not dither then
				ptr_dither_table^:= color;

			if  (0 <= theX)
			and (theX < AWidth) then
				begin
				dst^:= ptr_dither_table^;
				Inc(ptr_dither_table);
				Inc(dst, AWidth);
				end;
			end;

		if  (0 <= theX)
		and (theX < AWidth) then
			begin
//			height * vs->pitch - 1 * vs->format.bytesPerPixel
			Dec(dst, AHeight * AWidth - 1);
			end;

		Inc(theX);
		end;

	ADest:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, AWidth, AHeight);

	dst:= @dstTmp[0];
	for theY:= 0 to AHeight - 1 do
		for theX:= 0 to AWidth - 1 do
			begin
			Assert(dst^ < 16, 'pixel colour out of range');

			ADest.Pixels[theX, theY]:= AMIGA_PALETTE[dst^];
			ADest.AlphaScanline[theY]^[theX]:= $FF;

			Inc(dst);
			end;
	end;

{ TObjCompare }

function TObjCompare.Compare(const Left, Right: PSCUMMObjectData): Integer;
	begin
	Result:= Left^.obj_nr - Right^.obj_nr;
	end;

procedure TForm4.Clear;
	var
	i: Integer;

	begin
	TreeView1.Items.Clear;
	RichEdit1.Lines.Clear;

	for i:= FObjects.Count - 1 downto 0 do
		Dispose(FObjects[i]);

	FObjects.Clear;
	end;

procedure TForm4.FormCreate(Sender: TObject);
	begin
	FObjects:= TList<PSCUMMObjectData>.Create;
	end;

procedure TForm4.LoadFileToMemory(const AFileName: string; out AMem: TMemoryStream);
	var
	b: Byte;
	f: TFileStream;

	begin
	AMem:= TMemoryStream.Create;

	f:= TFileStream.Create(AFileName, fmOpenRead);
	try
		while not (f.Position = f.Size) do
			begin
			f.Read(b, 1);
			b:= b xor $FF;
			AMem.Write(b, 1);
			end;

		finally
		f.Free;
		end;

	AMem.Position:= 0;
	end;

procedure TForm4.ReadFile(const AIndex: Integer; const AFileName: string);
	var
	ptr,
	optr: PByte;
	ipos: Word;
	cnt: Byte;
	obj: PSCUMMObjectData;
//	i: Integer;
	m: TMemoryStream;

	begin
	LoadFileToMemory(AFileName, m);
	try
		ptr:= m.Memory;

		cnt:= (ptr + 20)^;
		ipos:= 28;
		ptr:= ptr + 28 + cnt * 2;

		while cnt > 0 do
			begin
			New(obj);

			obj^.resInfo.roomNo:= AIndex;
			obj^.resInfo.OBCDoffset:= PWord(ptr)^;

			optr:= PByte(m.Memory) + obj^.resInfo.OBCDoffset - 2;

			obj^.resInfo.unk1:= PWord(optr)^;
			obj^.resInfo.size:= PWord(optr + 2)^;
			obj^.resInfo.unk2:= PWord(optr + 4)^;
			obj^.obj_nr:= PWord(optr + 6)^;

			obj^.x_pos:= (optr + 9)^;
			obj^.parentstate:= (((optr + 10)^ and $80) shr 7) * 8;
			obj^.y_pos:= (optr + 10)^ and $7F;
			obj^.width:= (optr + 11)^ * 8;
			obj^.parent:= (optr + 12)^;
			obj^.walk_x:= (optr + 13)^ * 8;
			obj^.walk_y:= ((optr + 14)^ and $1F) * 8;
			obj^.height:= ((optr + 15)^ and $F8);
			obj^.actorDir:= (optr + 15)^ and $07;

			obj^.byte17:= (optr + 17)^;

			m.Position:= PWord(PByte(m.Memory) + iPos)^;
			DecodeObjectImage(m, obj^.width, obj^.height, obj^.img);

			FObjects.Add(obj);
			Dec(cnt);
			Inc(ptr, 2);
			Inc(ipos, 2);
			end;

		finally
		m.Free;
		end;
	end;

procedure TForm4.ReadFiles(const APath: string);
	const
	LIT_FMT_LFLFILE = '%2.2d.LFL';

	var
	i: Byte;
	s: string;

	begin
	try
		i:= 1;
		s:= TPath.Combine(APath, Format(LIT_FMT_LFLFILE, [i]));
		while TFile.Exists(s) do
			begin
			ReadFile(i, s);

			Inc(i);
			s:= TPath.Combine(APath, Format(LIT_FMT_LFLFILE, [i]));
			end;
		except
		end;
	end;

procedure TForm4.ToolButton1Click(Sender: TObject);
	var
	cr: TCursor;
	srt: TObjCompare;

	begin
	if  FileOpenDialog1.Execute then
		begin
		cr:= Screen.Cursor;
		Screen.Cursor:= crHourGlass;
		try
			Application.ProcessMessages;

			Clear;
			ReadFiles(FileOpenDialog1.FileName);

			srt:= TObjCompare.Create;
			FObjects.Sort(srt);

			UpdateDisplay;
			finally
			Screen.Cursor:= cr;
			end;
		end;
	end;

procedure TForm4.TreeView1Change(Sender: TObject; Node: TTreeNode);
	var
	o: PSCUMMObjectData;

	begin
	if  Assigned(Node) then
		begin
		o:= PSCUMMObjectData(Node.Data);

		RichEdit1.Lines.Clear;
		RichEdit1.Lines.Add('RoomNo     : ' + IntToStr(o^.resInfo.roomNo));
		RichEdit1.Lines.Add('OBCD Offset: $' + IntToHex(o^.resInfo.OBCDoffset, 4));
		RichEdit1.Lines.Add('Size       : ' + IntToStr(o^.resInfo.size));
		RichEdit1.Lines.Add('Width      : ' + IntToStr(o^.width));
		RichEdit1.Lines.Add('Height     : ' + IntToStr(o^.height));
		RichEdit1.Lines.Add('Byte 17    : $' + IntToHex(o^.byte17, 2));

		if  (o^.width > 0)
		and (o^.height > 0) then
			Image1.Picture.Assign(o^.img)
		else
			Image1.Picture.Assign(nil);
		end;
	end;

procedure TForm4.UpdateDisplay;
	var
	i: Integer;

	begin
	TreeView1.Items.BeginUpdate;
	try
		for i:= 0 to FObjects.Count - 1 do
			TreeView1.Items.AddObject(nil, IntToStr(FObjects[i]^.obj_nr) + ' ('+
					IntToStr(FObjects[i]^.resInfo.roomNo) + ')', FObjects[i]);

		finally
		TreeView1.Items.EndUpdate;
		end;
	end;

end.
