unit FormCostumeReaderMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.StdCtrls, Vcl.ComCtrls, PNGImage;

type
	TSCUMMCostResInfoV2 = record
	public
		unkId: Word;
		unkHdr1: Word;
		size: Word;
		unkHdr2: Word;
		numAnims: Byte;
		fmtByte: Byte;
		colour: Byte;
//		This is redundant after reading?  I still want it.
//		cellIndex: array of Byte;
		animIndex: array of Byte;

		function NoMirror: Boolean;
		function Format: Byte;
	end;

	TSCUMMCostAnimCmdsInfoV2 = record
		isUsed: Boolean;
		startIdx: Byte;
		flags: Byte;

		hasCmds: Boolean;
//		This is actually an array from steps
		cmds: array of Byte;
		hasSteps: Boolean;

		function HasCell: Boolean;			//cmd
		function IsOpen: Boolean;			//cmdIdx
		function IsPlay: Boolean;			//cmd

//		Not used in V2... V3 maybe?
//		function IsPause: Boolean;			//cmd
//		function IsUnPause: Boolean;		//cmd
//		function IsSound: Boolean;			//cmd

		function IsLoop: Boolean;			//flags
		function Steps: Byte;				//flags
	end;

	TSCUMMCostAnimInfoV2 = record
		cellsCmds: array[0..15] of TSCUMMCostAnimCmdsInfoV2;

		function IsCellsUsed(const ACells: Byte): Boolean;
	end;

	TSCUMMCostAnimCmdV2 = record
		cmd: Byte;
	end;

	TSCUMMCostCellInfoV2 = record
		cmdImgListIdx: Byte;
	end;

	TSCUMMCostCmdImageInfoV2 = record
//		debugging
		e: Word;
		cmdData: array of Word;

		cmdImgIdxs: array of Byte;
	end;

	TSCUMMCostImageInfoV2 = record
		width,
		height: Word;
		relativeX,
		relativeY: SmallInt;
		moveX,
		moveY: SmallInt;
		data: array of Byte;

		procedure DecodeProcV2(AObjColour: Byte; ADest: TMemoryStream);
	end;

	TSCUMMCostumeV2 = record
		resInfo: TSCUMMCostResInfoV2;
		animInfos: array of TSCUMMCostAnimInfoV2;
		animCmds: array of TSCUMMCostAnimCmdV2;
		cellInfos: array of TSCUMMCostCellInfoV2;
		imageCmdIndex: array of TSCUMMCostCmdImageInfoV2;
		imageInfos: array of TSCUMMCostImageInfoV2;

		procedure Clear;
		procedure LoadFromStream(AStream: TStream);
	end;


	TForm3 = class(TForm)
		RichEdit1: TRichEdit;
		Button1: TButton;
		OpenDialog1: TOpenDialog;
		procedure Button1Click(Sender: TObject);
	private
		FCostume: TSCUMMCostumeV2;

		procedure DumpCostumeInfo;
		procedure DrawCellBitmap(ASource: TMemoryStream; ACX, ACY: Integer;
				out AImg: TPNGImage);
		procedure DrawTestImage(AColor: TColor; out AImg: TPNGImage);

	public
		{ Public declarations }
	end;

var
	Form3: TForm3;

implementation

uses
	Types, IOUtils;

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


{ TSCUMMCostResInfoV2 }

function TSCUMMCostResInfoV2.Format: Byte;
	begin
	Result:= fmtByte and $7F;
	end;

function TSCUMMCostResInfoV2.NoMirror: Boolean;
	begin
	Result:= (fmtByte and $80) <> 0;
	end;

{ TSCUMMCostAnimInfoV2 }

function TSCUMMCostAnimCmdsInfoV2.HasCell: Boolean;
	begin
//	The special commands don't occur in V2
	Result:= isUsed and hasCmds; (*  and (cmd < $7B);*)
	end;

function TSCUMMCostAnimCmdsInfoV2.IsOpen: Boolean;
	begin
	Result:= isUsed and (startIdx = $FF);
	end;

function TSCUMMCostAnimCmdsInfoV2.IsLoop: Boolean;
	begin
	Result:= isUsed and hasCmds and hasSteps and ((flags and $80) = 0);
	end;

//function TSCUMMCostAnimCmdsInfoV2.IsPause: Boolean;
//	begin
//	Result:= isUsed and hasCmds and (cmd = $79);
//	end;

function TSCUMMCostAnimCmdsInfoV2.IsPlay: Boolean;
	begin
	Result:= isUsed and hasCmds and hasSteps;
	end;

//function TSCUMMCostAnimCmdsInfoV2.IsSound: Boolean;
//	begin
//	Result:= isUsed and hasCmds and ((cmd and $80) <> 0);
//	end;
//
//function TSCUMMCostAnimCmdsInfoV2.IsUnPause: Boolean;
//	begin
//	Result:= isUsed and hasCmds and (cmd = $7A);
//	end;

function TSCUMMCostAnimCmdsInfoV2.Steps: Byte;
	begin
	if  isUsed and hasCmds and hasSteps then
		Result:= (flags and $7F)
	else
		Result:= $FF;
	end;

{ TSCUMMCostAnimInfoV2 }

function TSCUMMCostAnimInfoV2.IsCellsUsed(const ACells: Byte): Boolean;
	begin
	Result:= cellsCmds[ACells].isUsed;
	end;

{ TSCUMMCostumeV2 }

procedure TSCUMMCostumeV2.Clear;
	begin
	resInfo.unkId:= 0;
	resInfo.unkHdr1:= 0;
	resInfo.size:= 0;
	resInfo.unkHdr2:= 0;
	resInfo.numAnims:= 0;
	resInfo.fmtByte:= 0;
	resInfo.colour:= 0;

//	SetLength(resInfo.cellIndex, 0);
	SetLength(resInfo.animIndex, 0);

	SetLength(animInfos, 0);
	SetLength(animCmds, 0);
	SetLength(cellInfos, 0);
	SetLength(imageInfos, 0);
	end;

function DoSortCmdList(Item1, Item2: Pointer): Integer;
	begin
	Result:= Integer(Item1) - Integer(Item2);
	end;

procedure TSCUMMCostumeV2.LoadFromStream(AStream: TStream);
	var
	animCmdsOff: Word;
	animOffsIdx: array of Word;
	animAddrs: array of Word;
	animCount: Integer;
	cellOffs: array[0..15] of Word;
	t: array[0..15, 0..$78] of Word;
	l: array[0..15] of Byte;
	q: TList;
	e: Word;

	procedure LoadResInfoFromStream;
		var
		i: Integer;

		begin
		Clear;

		AStream.Read(resInfo.unkId, 2);
		AStream.Read(resInfo.unkHdr1, 2);
		AStream.Read(resInfo.size, 2);
		AStream.Read(resInfo.unkHdr2, 2);
		AStream.Read(resInfo.numAnims, 1);
		AStream.Read(resInfo.fmtByte, 1);
		AStream.Read(resInfo.colour, 1);
		AStream.Read(animCmdsOff, 2);

		for i:= 0 to 15 do
			AStream.Read(cellOffs[i], 2);

//		We aren't handling these so just be sure.
		Assert(not (resInfo.Format in [$60..$63]));

//		There seems to be a bug/oddity.  See below.
		SetLength(resInfo.animIndex, resInfo.numAnims + 1);
		end;

	procedure LoadAnimsFromStream;
		var
		i: Integer;
		n: Byte;
		m: Word;
		c: Cardinal;

		begin
//dengland As far as I can tell, the code below will always fail to give the
//		correct result.  Firstly, the offsets used by the anim offs are off by 2.
//		Secondly, the entire costume data block should be closed (one section
//		immediately following the previous).  I

//		I believe that the problem is actually that the original (v2/v3) SCUMM
//		code did't use numanims correctly.  There was probably an earlier version
//		(perhaps SPUTM) that simply read through the list here until the address
//		read was the same one reached.  I'm not really sure what to do.  The
//		SCUMMVM code doesn't use this "dummy anim" so I'm not really inclined to
//		use it.

//		The documentation on the SCUMMVM tech pages suggests that there is
//		actually one more anim when the size is given as non zero.  It also
//		fails to mention that the offsets are incorrect by 2 but this may be
//		due to it being only in V2 (which SCUMMVM suggests in a comment) and the
//		documentation itself is for V4 or so and above.
//
//		I think I'll split the difference and read in the extra one.  I should
//		quickly find any exceptions to the rule anyway.
//
//		SetLength(animOffsIdx, resInfo.numAnims);
//		for i:= 0 to resInfo.numAnims - 1 do
//			AStream.Read(animOffsIdx[i], 2);
//
//		Assert((Length(animOffsIdx) > 0) and ((AStream.Position - 2) = animOffsIdx[0]),
//				'Error reading costume res info header');

//		Oh dear, we do have a problem.  The man eating plant costume doesn't
//		work with the replacement logic.  So far, the only way to tell its him
//		is to try to do something different given the costume/actor id when
//		loading or by checking the unkHdr2 value in the resInfo block.  The
//		later would work because they all have unique numbers there (except for
//		the tentacles).  I think that field might be a version/revision number.

		c:= resInfo.numAnims + 1;

//		The SCUMM compiler seems to output this data incorrectly.
		SetLength(animOffsIdx, c);
		for i:= 0 to c - 1 do
			AStream.Read(animOffsIdx[i], 2);

//		Another problem appears here.  Must be an older compiler or some bug?
//		I think someone put the offset in by hand and forgot to encode them as
//		hexadecimal so we've ended up with binary coded decimal.  It also has a
//		problem given that it has no standard animations.  I guess I should
//		handle that case separately but no other costume has the problem.
		if resInfo.unkHdr2 = $1600 then
			begin
			animCmdsOff:= 89;
			c:= animCmdsOff;
			end
		else
			c:= animOffsIdx[0];

//		ShowMessage('The numbers are:  ' + IntToStr(AStream.Position - 4) +
//				', ' + IntToStr(animOffsIdx[0]) + ', ' + IntToStr(animCmdsOff));

//		We need to use -4 because the offsets are wrong by 2.
		Assert((Length(animOffsIdx) > 0)
				and ((AStream.Position - 4) = c),
				'Error reading costume res info header');

//		Initially expect to get as many anim infos as there are numAnims.
		SetLength(animInfos, resInfo.numAnims);
		SetLength(animAddrs, resInfo.numAnims);

		animCount:= 0;
		while (AStream.Position - 4) < animCmdsOff do
			begin
//			This must be a good way to fragment memory but the structures aren't
//				"clean enough" to allow me to do it any other way than reading
//				twice.  Hopefully there aren't any (or many) "unreleased"
//				animation sequences nor repeated ones.

			if  animCount = Length(animAddrs) then
//				Grow by 4
				SetLength(animAddrs, Length(animAddrs) + 4);
			if  animCount = Length(animInfos) then
//				Grow by 4
				SetLength(animInfos, Length(animInfos) + 4);

			if  animCount < Length(animOffsIdx) then
				if  animOffsIdx[animCount] = 4 then
					Continue;

			animAddrs[animCount]:= AStream.Position - 4;

			AStream.Read(m, 2);
			for n:= 15 downto 0 do
				begin
				with animInfos[animCount].cellsCmds[n] do
					begin
					isUsed:= False;
					startIdx:= $FF;
					flags:= $FF;
					hasCmds:= False;
					SetLength(cmds, 0);
					hasSteps:= False;
					end;

				if  ((m and $FFFF) <> 0)
				and ((m and $8000) <> 0) then
					begin
					animInfos[animCount].cellsCmds[n].isUsed:= True;
					AStream.Read(animInfos[animCount].cellsCmds[n].startIdx, 1);

					if  animInfos[animCount].cellsCmds[n].startIdx < $FF then
						begin
						AStream.Read(animInfos[animCount].cellsCmds[n].flags, 1);
						animInfos[animCount].cellsCmds[n].hasCmds:= True;

//						Will do the rest in fixups after getting the cmd list
						end;
					end;

				m:= m shl 1;
				end;

			Inc(animCount);
			end;

		Assert((AStream.Position - 4) = animCmdsOff,
				'Error reading anim sequence list.');
		end;

	procedure FixUpAnimsIndex;
		var
		i,
		j: Integer;

		begin
		for i:= 0 to animCount - 1 do
			for j:= 0 to resInfo.numAnims do
				if  animOffsIdx[j] = animAddrs[i] then
					 resInfo.animIndex[j]:= i;

		SetLength(animOffsIdx, 0);
		SetLength(animAddrs, 0);

		if Length(animInfos) > animCount then
			SetLength(animInfos, animCount);
		end;

	procedure LoadAnimCmdsFromStream;
		var
		c: Cardinal;

		begin
//		This should actually represent "limbs" 15 down to 0?

//		Calculate how many we'll have.  The problem with being two off seems to
//			continue here.
		c:= cellOffs[0] - (AStream.Position - 4);
		SetLength(animCmds, c);
		for c:= 0 to c - 1 do
			AStream.Read(animCmds[c].cmd, 1);

		Assert((AStream.Position - 4) = cellOffs[0],
				'Error reading anim cmds list.');
		end;

	procedure FixUpAnimsCmdIndex;
		var
		i,
		j,
		k: Integer;
		n,
		x: Integer;
		c: Byte;

		begin
		for i:= 0 to High(animInfos) do
			for j:= 0 to 15 do
				if  animInfos[i].cellsCmds[j].isUsed
				and animInfos[i].cellsCmds[j].hasCmds then
					with animInfos[i].cellsCmds[j] do
						begin
						hasSteps:= (flags and $7F) <> 0;

						x:= startIdx;
						if  hasSteps then
							n:= Steps + 1
						else
							n:= 1;

						SetLength(cmds, n);

						for k:= 0 to n - 1 do
							begin
							c:= animCmds[x].cmd;
							cmds[k]:= c;
							Inc(x);

//							These don't occur in V2
//							hasSteps:= not (c in [$7A, $79]);
							end;
						end;
		end;

	procedure LoadCellsFromStream;
		var
		i,
		j,
		c,
		x: Integer;
		b,
		s: Word;
		p: array of Word;
		d: Boolean;
		f: Word;

		function FindNextEnd: Word;
			var
			j: Integer;
//			k: Integer;

			begin
			Result:= s;
			for j:= i + 1 to High(cellOffs) do
				if  cellOffs[j] > Result then
					begin
					Result:= cellOffs[j];
					Exit;
					end;

			d:= True;
			Result:= $FFFF;

//			There is something amiss.  The table will end up with an image
//				at this point in the stream but also that there should be more
//				index data.  From what I can tell, those index entries should be
//				patched to say they aren't valid because certianly, valid data
//				stops appearing right now.

{			for j:= 0 to 15 do
				for k:= 0 to l[j] - 1 do
					if  (t[j, k] > s)
					and (t[j, k] < Result) then
						Result:= t[j, k];}
			end;

		begin
//		This should actually represent "limbs" 15 down to 0?

		i:= 0;
		FillChar(l[0], 15, 0);
		FillChar(t[0,0], SizeOf(t), 0);
		SetLength(p, Length(cellOffs));

		d:= False;
		b:= cellOffs[0];
		s:= b;
		f:= s;
		e:= FindNextEnd;
		repeat
			p[i]:= s;
			while (AStream.Position - 4) < e do
				begin
				AStream.Read(t[i, l[i]], 2);

				c:= t[i, l[i]];
				Assert(c < (AStream.Size - 4),
						'Invalid data found reading cell image index');

				if  q.IndexOf(Pointer(c)) = -1  then
					q.Add(Pointer(c));

				Inc(l[i]);
				end;

			Inc(i);
			if  (not d)
			and (i < Length(cellOffs)) then
				begin
				s:= e;
				f:= e;
				e:= FindNextEnd;
				end
			until d {and ((AStream.Position - 4) >= e)};

		q.SortList(DoSortCmdList);

//dengland In Zak, we are getting 0's here and it seems we need to skip them.
//		s:= Integer(q.Items[0]);
		j:= 0;
		repeat
			s:= Integer(q.Items[j]);
			Inc(j);
			until (s <> 0) or (j = q.Count);

		Assert(((AStream.Position - 4) = s),
				'Error reading cell cmd to image indexes');

		SetLength(cellInfos, Length(cellOffs));
		for c:= 0 to i - 1 do
			for x:= 0 to High(cellInfos) do
				if  p[c] = cellOffs[x] then
					cellInfos[15 - x].cmdImgListIdx:= c;

//		As above, there is a problem with the list and the actually invalid
//			references should be patched out.

//		while Integer(q[0]) < e do
//			q.Delete(0);
//		s:= Integer(q.Items[0]);
//		Assert(((AStream.Position - 4) = s) and (e = s),
//				'Error reading cell cmd to image indexes');

		e:= f;

//dengland Errm...  I'm not sure what to do in Zak since we are getting 0's and I
//          can't recall at this point why I have to delete this first entry.  Let's
//          try just culling the first non zero value, as above.  In
//			LoadImagesFromStream, we can just skip 0's.
//		q.Delete(0);
		Dec(j);
		q.Delete(j);
		end;

	procedure LoadImagesFromStream;
		var
		i,
		j,
		l: Integer;

		begin
		SetLength(imageInfos, q.Count + 1);
		for i:= 0 to q.Count do
			begin
//dengland  In Zak, getting 0's in q list so I'm going to just try skipping them
			if  (i < q.Count)
			and (Integer(q[i]) = 0) then
				Continue;

			AStream.Read(imageInfos[i].width, 2);
			AStream.Read(imageInfos[i].height, 2);
			AStream.Read(imageInfos[i].relativeX, 2);
			AStream.Read(imageInfos[i].relativeY, 2);
			AStream.Read(imageInfos[i].moveX, 2);
			AStream.Read(imageInfos[i].moveY, 2);

			if  i < q.Count then
				l:= Integer(q[i])
			else
				l:= AStream.Size - 4;

			Dec(l, (AStream.Position - 4));

			Assert((l > 0) and ((AStream.Size - AStream.Position) >= l),
					'Error reading image info header');

			SetLength(imageInfos[i].data, l);
			for j:= 0 to l - 1 do
				AStream.Read(imageInfos[i].data[j], 1);
			end;
		end;

	procedure FixUpImageIndex;
		var
		i,
		j: Integer;

		begin
		q.Insert(0, Pointer(e));
		SetLength(imageCmdIndex, 16);
		for i:= 0 to 15 do
			begin
			imageCmdIndex[i].e:= e;

			SetLength(imageCmdIndex[i].cmdImgIdxs, l[i]);

//			Grr...  I need to check this...
			SetLength(imageCmdIndex[i].cmdData, l[i]);

			for j:= 0 to l[i] - 1 do
//				if t[i, j] < e then
				if  q.IndexOf(Pointer(t[i, j])) = -1 then
					begin
					Assert(False, 'There appears to be an error in the imageCmdIndex');
					imageCmdIndex[i].cmdData[j]:= t[i, j];
					imageCmdIndex[i].cmdImgIdxs[j]:= $FF;
					end
				else
					begin
					imageCmdIndex[i].cmdImgIdxs[j]:= q.IndexOf(Pointer(t[i, j]));
					imageCmdIndex[i].cmdData[j]:= $FF;
					end;
			end;
		end;

	begin
	Clear;
	LoadResInfoFromStream;

	LoadAnimsFromStream;
	FixUpAnimsIndex;

	LoadAnimCmdsFromStream;

	FixUpAnimsCmdIndex;

	q:= TList.Create;
	try
		LoadCellsFromStream;

		LoadImagesFromStream;
		FixUpImageIndex;

		finally
		q.Free;
		end;
	end;

{ TSCUMMCostImageInfoV2 }

procedure TSCUMMCostImageInfoV2.DecodeProcV2(AObjColour: Byte;
		ADest: TMemoryStream);
	const
	v1mask = 15;
	v1_shr = 4;

	var
	idx: Integer;
	b: Byte;
	len,
	color,
	pcolor: Cardinal;
	w,
	h: Integer;

	begin
//dengland Need this for Zak.
	if  Length(data) = 0 then
		Exit;

	w:= width;
	h:= height;

	idx:= 0;
	repeat
		len:= data[idx];
		Inc(idx);

		color:= len shr v1_shr;
		len:= len and v1mask;
		if  len = 0 then
			begin
			len:= data[idx];
			Inc(idx);
			end;

		repeat
			if  (color <> 0) then
				begin
				if  color = AObjColour then
					pcolor:= 0
				else
					pcolor:= color;

				b:= $FF;
				ADest.Write(b, 1);
				ADest.Write(Byte(pcolor), 1);
				end
			else
				begin
				b:= $00;
				ADest.Write(b, 1);
				ADest.Write(b, 1);
				end;

			Dec(h);
			if  h = 0 then
				begin
				Dec(w);

				if  w = 0 then
					Exit;

				h:= height;
				end;

			Dec(len);
			until len = 0;
		until False;
	end;

procedure TForm3.Button1Click(Sender: TObject);
	var
	f: TFileStream;
	m: TMemoryStream;
	w: Cardinal;
	i: Integer;
	p: TPoint;
	g: TPNGImage;
	s,
	n: string;

	begin
	if  OpenDialog1.Execute then
		begin
		s:= TPath.GetFileNameWithoutExtension(OpenDialog1.FileName);

		w:= 0;
		m:= TMemoryStream.Create;
		try
//			need to inject 4 bytes because the dumping program removed them.
			m.Write(w, 4);

			f:= TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
			try
//				Use copy from because injecting bytes
				m.CopyFrom(f, f.Size);

				finally
				f.Free;
				end;

			m.Seek(0, soFromBeginning);
			FCostume.LoadFromStream(m);

			finally
			m.Free;
			end;

		DumpCostumeInfo;

		p.X:= Width - 200;
		p.Y:= 0;
		Canvas.Brush.Color:= clBtnFace;
		Canvas.Brush.Style:= bsSolid;
		Canvas.FillRect(Rect(P.X - 50, 0, Width, Height));

		for i:= 0 to 15 do
			begin
			Canvas.Brush.Color:= AMIGA_PALETTE[i];
			Canvas.Brush.Style:= bsSolid;
			Canvas.FillRect(Rect(P.X - 50, 4 + i * 15, p.X - 35, 19 + i * 15));
			end;

		for i:= 0 to 15 do
			begin
			DrawTestImage(AMIGA_PALETTE[i], g);
			try
				g.DrawUsingPixelInformation(Canvas, Point(P.X - 35, 4 + i * 15));

				finally
				g.Free;
				end;
			end;

		m:= TMemoryStream.Create;
		try
			s:= IncludeTrailingPathDelimiter(ExtractFilePath(
					OpenDialog1.FileName)) + s + '_CELLS';
			CreateDir(s);
			s:= IncludeTrailingPathDelimiter(s);

			for i:= 0 to High(FCostume.imageInfos) do
				begin
				m.Clear;

				n:= Format('%3.3d.png', [i]);

				if  (FCostume.imageInfos[i].width > 0)
				and (FCostume.imageInfos[i].height > 0) then
					begin
					FCostume.imageInfos[i].DecodeProcV2(FCostume.resInfo.colour,
							m);

					m.Seek(0, soFromBeginning);
					DrawCellBitmap(m, FCostume.imageInfos[i].width,
							FCostume.imageInfos[i].height, g);
					try
						g.SaveToFile(s + n);
						g.DrawUsingPixelInformation(Canvas, p);

						Inc(p.Y, FCostume.imageInfos[i].height + 4);

						finally
						g.Free;
						end;
					end;

				if  (i > 0)
				and (i mod 10 = 0) then
					begin
					Inc(p.X, 30);
					p.Y:= 0;
					end;

				end;
			finally
			m.Free;
			end;
		end;
	end;

procedure TForm3.DrawCellBitmap(ASource: TMemoryStream; ACX, ACY: Integer;
		out AImg: TPNGImage);
	var
	x,
	y: Integer;
	bsl: PByte;
	asl: PByteArray;
	c: TColor;
	b: Byte;

	begin
	AImg:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, ACX, ACY);

	if  ASource.Size = 0 then
		Exit;

	for x:= 0 to ACX - 1 do
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
	end;

procedure TForm3.DrawTestImage(AColor: TColor; out AImg: TPNGImage);
	var
	x,
	y: Integer;
	bsl: PByte;
	asl: PByteArray;

	begin
	AImg:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, 15, 15);
	for x:= 0 to 14 do
		for y:= 0 to 14 do
			begin
			bsl:= PByte(AImg.Scanline[y]);
			Inc(bsl,  x * 3);
			asl:= AImg.AlphaScanline[y];

			asl[x]:= $FF;

			bsl^:= (AColor and $FF0000) shr 16;
			Inc(bsl);
			bsl^:= (AColor and $00FF00) shr 8;
			Inc(bsl);
			bsl^:= (AColor and $0000FF);
			end;
	end;

const
	ARR_LIT_BOOL: array[Boolean] of string = ('false', 'true');
	ARR_LIT_TERM: array[Boolean] of string = ('/', '');

procedure TForm3.DumpCostumeInfo;
	procedure DoAdd(const AString: string; const AIndent: Integer = 0);
		begin
		RichEdit1.Lines.Add(StringOfChar(#32, 4 * AIndent) + AString);
		end;

	procedure DoDumpResInfo;
		const
		ARR_LIT_ANIMBLOCK: array[0..5] of string = (
				'<!-- walking -->', '<!-- standing -->', '<!-- head -->',
				'<!-- mouth open -->', '<!-- mouth closed -->', '<!-- mouth talk -->');
		ARR_LIT_ANIMDIRCT: array[0..5] of string = (
				'    <!-- R -->','    <!-- L -->', '    <!-- D -->',
				'    <!-- U -->', '    <!-- Near L/R -->',
				'    <!-- Far L/R -->');

		var
		i: Integer;
		s: string;

		begin
		with FCostume.resInfo do
			begin
			DoAdd('<resInfo id="$' + IntToHex(unkId,4) + '" hdr="$' +
					IntToHex(unkHdr1, 4) + '" size="$' +
					IntToHex(size, 4) +'">', 1);

			DoAdd('<unkHdr>$' + IntToHex(unkHdr2, 4) + '</unkHdr>', 2);
			DoAdd('<numAnims>' + IntToStr(numAnims) + '</numAnims>', 2);
			DoAdd('<noMirror>' + ARR_LIT_BOOL[NoMirror] + '</noMirror>', 2);
			DoAdd('<format>$' + IntToHex(format,2) + '</format>', 2);
			DoAdd('<palColour>' + IntToStr(colour) + '</palColour>', 2);

//			DoAdd('<cellsIndex>', 2);
//			for i:= 0 to High(cellIndex) do
//				DoAdd('<cellIndex id="' + IntToStr(i) + '">' +
//						IntToStr(cellIndex[i]) + '</cellIndex>', 3);
//			DoAdd('</cellsIndex>', 2);

			DoAdd('<animsIndex count="' + IntToStr(Length(animIndex)) + '">', 2);
			for i:= 0 to High(animIndex) do
				begin
				if  (i mod 4) = 0 then
					DoAdd(ARR_LIT_ANIMBLOCK[i div 4], 3);

				s:= '<animIndex index="' + IntToStr(i) + '">' +
						IntToStr(animIndex[i]) + '</animIndex>';

				if  i in [0, 1] then
					s:= s + ARR_LIT_ANIMDIRCT[i + 4]
				else
					s:= s + ARR_LIT_ANIMDIRCT[i mod 4];

				DoAdd(s, 3);
				end;
			DoAdd('</animsIndex>', 2);
			end;

		DoAdd('</resInfo>', 1);
		end;

	procedure DoDumpCellsCmds(AAnimInfo: Integer);
		var
		i,
		j: Integer;
		s: string;

		begin
		DoAdd('<cellsCmds>', 3);

		with FCostume.animInfos[AAnimInfo] do
			for i:= 0 to High(cellsCmds) do
				with cellsCmds[i] do
					begin
					s:= '<cellsCmd index="' + IntToStr(i) + '" ' +
							'isUsed="' + ARR_LIT_BOOL[isUsed] + '"';
					if  isUsed then
						s:= s + ' hasCmds="' + ARR_LIT_BOOL[hasCmds] + '" ' +
							'hasCell="' + ARR_LIT_BOOL[HasCell] + '" ' +
							'hasSteps="' + ARR_LIT_BOOL[hasSteps] + '" ' +
							'isOpen="' + ARR_LIT_BOOL[IsOpen] + '" ' +
							'startIdx="$' + IntToHex(startIdx, 2) + '" ' +
							'flags="$' + IntToHex(flags, 2) + '"';
					s:= s + ARR_LIT_TERM[hasCmds or hasSteps] + '>';
					DoAdd(s, 4);

					if  hasSteps then
						DoAdd('<steps isLoop="' + ARR_LIT_BOOL[IsLoop] + '">' +
								IntToStr(Steps) + '</steps>', 5);

					if  hasCmds then
						for j:= 0 to High(cmds) do
							DoAdd('<cmd step="' + IntToStr(j) + '" ' +
									(*isPause="' + ARR_LIT_BOOL[IsPause] + '" ' +
									'isUnPause="' + ARR_LIT_BOOL[IsUnpause] + '" ' +*)
									'isPlay="' + ARR_LIT_BOOL[IsPlay] + '">$' +
//									'isSound="' + ARR_LIT_BOOL[IsSound] + '">$' +
									IntToHex(cmds[j], 2) + '</cmd>', 5);

					if  hasCmds or hasSteps then
						DoAdd('</cellsCmd>', 4);
					end;

		DoAdd('</cellsCmds>', 3);
		end;

	procedure DoDumpAnimInfos;
		var
		i: Integer;

		begin
		DoAdd('<animInfos count="' + IntToStr(Length(FCostume.animInfos)) + '">', 1);

		for i:= 0 to High(FCostume.animInfos) do
			begin
			DoAdd('<animInfo index="' + IntToStr(i) + '">', 2);

			DoDumpCellsCmds(i);

			DoAdd('</animInfo>', 2)
			end;

		DoAdd('</animInfos>', 1);
		end;

	procedure DoDumpAnimCmds;
		var
		i: Integer;

		begin
		DoAdd('<animCmds count="' + IntToStr(Length(FCostume.animCmds)) + '">', 1);

		for i:= 0 to High(FCostume.animCmds) do
			with FCostume.animCmds[i] do
				DoAdd('<animCmd index="' + IntToStr(i) + '">' + IntToHex(cmd, 2) +
						'</animCmd>', 2);

		DoAdd('</animCmds>', 1);
		end;

	procedure DoDumpCellInfos;
		var
		i: Integer;

		begin
		DoAdd('<cellInfos count="' + IntToStr(Length(FCostume.cellInfos)) + '">', 1);

		for i:= 0 to High(FCostume.cellInfos) do
			with FCostume.cellInfos[i] do
				DoAdd('<imgListRef index="' + IntToStr(i) + '">$' +
						IntToHex(cmdImgListIdx, 2) + '</imgListRef>', 2);

		DoAdd('</cellInfos>', 1);
		end;

	procedure DoDumpCmdImageInfo(AImgCmdIdx: Integer);
		var
		i: Integer;
		s: string;

		begin
		with FCostume.imageCmdIndex[AImgCmdIdx] do
			begin
			DoAdd('<cmdImgInfos count="' + IntToStr(Length(cmdImgIdxs))(* + '" '+
					'e="$' + IntToHex(e, 4) *) + '">', 3);

			for i:= 0 to High(cmdImgIdxs) do
				begin
				if  cmdImgIdxs[i] = $FF then
					s:= IntToHex(cmdData[i], 4)
				else
					s:= IntToStr(cmdImgIdxs[i]);

				DoAdd('<cmdImgInfo index="' + IntToStr(i) + (*'" ' +
						'isImage="' + ARR_LIT_BOOL[cmdImgIdxs[i] <> $FF] + '" '+
						'isData="' + ARR_LIT_BOOL[cmdImgIdxs[i] = $FF] +*) '">'+
						(*'$' +*) s + '</cmdImgInfo>', 4);
				end;

			DoAdd('</cmdImgInfos>', 3);
			end;
		end;

	procedure DoDumpImageCmdIndex;
		var
		i: Integer;

		begin
		DoAdd('<imgCmdIdxs count="' + IntToStr(Length(FCostume.imageCmdIndex)) + '">', 1);

		for i:= 0 to High(FCostume.imageCmdIndex) do
			with FCostume.imageCmdIndex[i] do
				begin
				DoAdd('<imgCmdIdx index="' + IntToStr(i) + '"' +
						ARR_LIT_TERM[Length(cmdImgIdxs) <> 0] + '>', 2);

				if Length(cmdImgIdxs) <> 0 then
					begin
					DoDumpCmdImageInfo(i);
					DoAdd('</imgCmdIdx>', 2);
					end;
				end;

		DoAdd('</imgCmdIdxs>', 1);
		end;

	procedure DoDumpImageInfos;
		const
		LIT_INDENT3 = '            ';

		var
		i,
		j: Integer;
		s: string;

		begin
		DoAdd('<imgInfos count="' + IntToStr(Length(FCostume.imageInfos)) + '">', 1);

		for i:= 0 to High(FCostume.imageInfos) do
			with FCostume.imageInfos[i] do
				begin
				DoAdd('<imgInfo index="' + IntToStr(i) + '" ' +
					'width="' + IntToStr(width) + '" ' +
					'height="' + IntToStr(height) + '" ' +
					'relativeX="' + IntToStr(relativeX) + '" ' +
					'relativeY="' + IntToStr(relativeY) + '" ' +
					'moveX="' + IntToStr(moveX) + '" ' +
					'moveY="' + IntToStr(moveY) + '" ' +
					'size="' + IntToStr(Length(data)) + '">', 2);

				s:= LIT_INDENT3;

				for j:= 0 to Length(data) - 1 do
					begin
					if  (j > 0) and ((j mod 16) = 0) then
						s:= s + #13#10 + LIT_INDENT3;

					s:= s + '$' + IntToHex(data[j], 2) + ' ';
					end;

				DoAdd(s);
				DoAdd('</imgInfo>', 2);
				end;

		DoAdd('</imgInfos>', 1);
		end;


	begin
	RichEdit1.Lines.BeginUpdate;
	try
		RichEdit1.Lines.Clear;

		DoAdd('<costume version="2">');

		DoDumpResInfo;
		DoDumpAnimInfos;
		DoDumpAnimCmds;
		DoDumpCellInfos;
		DoDumpImageCmdIndex;
		DoDumpImageInfos;

		DoAdd('</costume>');

		finally
		RichEdit1.Lines.EndUpdate;
		end;
	end;


end.
