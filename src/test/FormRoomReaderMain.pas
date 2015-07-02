unit FormRoomReaderMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
	Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
	Vcl.ComCtrls, Vcl.ToolWin, PNGImage, System.Generics.Collections;

type
	TSCUMMRoomResource = record
		roomNo: Byte;
		unk1,
		unk2: Word;
	end;

	PSCUMMRoomData = ^TSCUMMRoomData;
	TSCUMMRoomData = record
		resInfo: TSCUMMRoomResource;
		width,
		height: Word;

		img: TPNGImage;
	end;

	TForm5 = class(TForm)
		TreeView1: TTreeView;
		ToolBar1: TToolBar;
		ToolButton1: TToolButton;
		FileOpenDialog1: TFileOpenDialog;
		Panel1: TPanel;
		RichEdit1: TRichEdit;
		ScrollBox1: TScrollBox;
		Image1: TImage;
		Splitter1: TSplitter;
		Splitter2: TSplitter;
		procedure ToolButton1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
	private
		FRooms: TList<PSCUMMRoomData>;

	protected
		procedure Clear;

		procedure ReadFiles(const APath: string);
		procedure ReadFile(const AIndex: Integer; const AFileName: string);
		procedure LoadFileToMemory(const AFileName: string; out AMem: TMemoryStream);

		procedure UpdateDisplay;

	public

	end;

var
	Form5: TForm5;

implementation

{$R *.dfm}

uses
	IOUtils;

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
	TStripTable = record
		offsets: array[0..159] of Integer;
		run: array[0..159] of Integer;
		color: array[0..159] of Integer;
		zoffsets: array[0..119] of Integer;
		zrun: array[0..119] of Integer;
	end;

procedure GenerateStripTable(AData: TMemoryStream; AWidth, AHeight: Word;
		var ATable: TStripTable);
	var
	src,
	bitmapStart: PByte;
	color,
	data: Byte;
	x,
	y,
	len: Integer;
	run: Byte;
	runFlag: Byte;

	begin
	src:= PByte(AData.Memory) + AData.Position;
	bitmapStart:= src;

	color:= 0;
//	data:= 0;
//	len:= 0;
	run:= 1;

//	Decode the graphics strips, and memorize the run/color values as well as the byte
//			offset.
	for x:= 0 to AWidth - 1 do
		begin
		if  (x mod 8) = 0 then
			begin
			Assert(x div 8 < 160);

			ATable.run[x div 8]:= run;
			ATable.color[x div 8]:= color;
			ATable.offsets[x div 8]:= src - bitmapStart;
			end;

		for y:= 0 to AHeight - 1 do
			begin
			Dec(run);

			if  run = 0 then
				begin
				data:= src^;
				Inc(src);

				if  (data and $80) <> 0 then
					run:= data and $7F
				else
					run:= data shr 4;

				if  run = 0 then
					begin
					run:= src^;
					Inc(src);
					end;

				color:= data and $0F;
				end;
			end;
		end;

// The mask data follows immediately after the graphics.
	x:= 0;
	y:= AHeight;
	AWidth:= AWidth div 8;

	while True do
		begin
		len:= src^;
		Inc(src);
		runFlag:= len and $80;
		if  runFlag <> 0 then
			begin
			len:= len and $7F;
//			data:= src^;
			Inc(src);
			end;

		repeat
			if  runFlag = 0 then
				begin
//				data:= src^;
				Inc(src);
				end;

			if  y = AHeight then
				begin
				Assert(x < 120);

				ATable.zoffsets[x]:= src - bitmapStart - 1;
				ATable.zrun[x]:= len or runFlag;
				end;

			Dec(y);
			if  y = 0 then
				begin
				Dec(AWidth);
				if  AWidth = 0 then
					Exit;
				Inc(x);
				y:= AHeight;
				end;

			Dec(len);
			until len = 0;
		end;
	end;


procedure DecodeRoomImage(AData: TMemoryStream; AWidth, AHeight: Word;
		out ADest: TPNGImage);
	var
	dstTmp: array of Byte;
	dst: PByte;

	table: TStripTable;
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

//	GenerateStripTable(AData, AWidth, AHeight, table);

	SetLength(dstTmp, AWidth * AHeight);
	dst:= @dstTmp[0];

//	data:= 0;
	dither:= False;
	FillChar(dither_table[0], SizeOf(dither_table), 0);

//	run:= table.run[0];
//	color:= table.color[0];
//	src:= PByte(AData.Memory) + AData.Position + table.offsets[0];
//	theX:= 0;
//	maxX:= AWidth;
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


procedure TForm5.Clear;
	var
	i: Integer;

	begin
	TreeView1.Items.Clear;
	RichEdit1.Lines.Clear;

	for i:= FRooms.Count - 1 downto 0 do
		Dispose(FRooms[i]);

	FRooms.Clear;
	end;

procedure TForm5.FormCreate(Sender: TObject);
	begin
	FRooms:= TList<PSCUMMRoomData>.Create;
	end;

procedure TForm5.LoadFileToMemory(const AFileName: string; out AMem: TMemoryStream);
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

procedure TForm5.ReadFile(const AIndex: Integer; const AFileName: string);
	var
	ptr: PByte;
	ipos: Word;
	room: PSCUMMRoomData;
//	i: Integer;
	m: TMemoryStream;

	begin
	LoadFileToMemory(AFileName, m);
	try
		ptr:= m.Memory;

		New(room);
		room^.resInfo.roomNo:= AIndex;

		room^.resInfo.unk1:= PWord(ptr)^;
		room^.resInfo.unk2:= PWord(ptr + 2)^;
		room^.width:= PWord(ptr + 4)^;
		room^.height:= PWord(ptr + 6)^;

		iPos:= PWord(ptr + 10)^;

		m.Position:= iPos;
		DecodeRoomImage(m, room^.width, room^.height, room^.img);

		FRooms.Add(room);

		finally
		m.Free;
		end;
	end;

procedure TForm5.ReadFiles(const APath: string);
	const
	LIT_FMT_LFLFILE = '%2.2d.LFL';

	var
	i: Byte;
	s: string;

	begin
	i:= 1;
	try
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

procedure TForm5.ToolButton1Click(Sender: TObject);
	var
	cr: TCursor;

	begin
	if  FileOpenDialog1.Execute then
		begin
		cr:= Screen.Cursor;
		Screen.Cursor:= crHourGlass;
		try
			Clear;
			ReadFiles(FileOpenDialog1.FileName);

			UpdateDisplay;
			finally
			Screen.Cursor:= cr;
			end;
		end;
	end;

procedure TForm5.TreeView1Change(Sender: TObject; Node: TTreeNode);
	var
	r: PSCUMMRoomData;

	begin
	if  Assigned(Node) then
		begin
		r:= PSCUMMRoomData(Node.Data);

		RichEdit1.Lines.Clear;
		RichEdit1.Lines.Add('RoomNo: ' + IntToStr(r^.resInfo.roomNo));

		Image1.Picture.Assign(r^.img);
		end;
	end;

procedure TForm5.UpdateDisplay;
	var
	i: Integer;

	begin
	TreeView1.Items.BeginUpdate;
	try
		for i:= 0 to FRooms.Count - 1 do
			TreeView1.Items.AddObject(nil, IntToStr(FRooms[i]^.resInfo.roomNo),
					FRooms[i]);

		finally
		TreeView1.Items.EndUpdate;
		end;
	end;

end.
