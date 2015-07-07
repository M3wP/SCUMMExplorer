unit SPUTMDecV2Types;

interface

uses
	Classes, Graphics, PNGImage;

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
//		This is redundant?  I still want it.
//		frameIndex: array of Byte;
		animIndex: array of Byte;

		function NoMirror: Boolean;
		function Format: Byte;
	end;

	TSCUMMCostLimbCmdsInfoV2 = record
		isUsed: Boolean;
		cmdIdx: Byte;
		flags: Byte;

		hasCmd: Boolean;
		cmd: Byte;
		hasSteps: Boolean;

		function HasCell: Boolean;			//cmd
		function IsAvail: Boolean;			//cmdIdx
		function IsPlay: Boolean;			//cmd
		function IsLoop: Boolean;			//flags
		function Steps: Byte;				//flags

//		Not used?
		function IsPause: Boolean;			//cmd
		function IsUnPause: Boolean;		//cmd
		function IsSound: Boolean;			//cmd
	end;

	TSCUMMCostAnimInfoV2 = record
		limbCmds: array[0..15] of TSCUMMCostLimbCmdsInfoV2;

		function IsFramesUsed(const AFrames: Byte): Boolean;
	end;

	TSCUMMCostLimbCmdV2 = record
		cmd: Byte;
	end;

	TSCUMMCostLimbInfoV2 = record
		cmdFrameListIdx: Byte;
	end;

	TSCUMMCostCmdFrameInfoV2 = record
//		Debugging.  If there are further problems, you'll need to uncomment this
//			here and in the three places it is used.
{$IFDEF DEBUG}
//		e: Word;
//		cmdData: array of Word;
{$ENDIF}

		cmdFrameIdxs: array of Byte;
	end;

	TSCUMMCostFrameInfoV2 = record
		width,
		height: Word;
		relativeX,
		relativeY: SmallInt;
		moveX,
		moveY: SmallInt;
		data: array of Byte;

		procedure DecodeProcV2(AObjColour: Byte; ADest: TMemoryStream);
	end;

	PSCUMMCostumeV2 = ^TSCUMMCostumeV2;
	TSCUMMCostumeV2 = record
		resInfo: TSCUMMCostResInfoV2;
		animInfos: array of TSCUMMCostAnimInfoV2;
		animCmds: array of TSCUMMCostLimbCmdV2;
		limbInfos: array of TSCUMMCostLimbInfoV2;
		cmdFrameIndex: array of TSCUMMCostCmdFrameInfoV2;
		frameInfos: array of TSCUMMCostFrameInfoV2;

		procedure Clear;
		procedure LoadFromStream(AStream: TStream);
	end;

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

procedure DecodeCostumeFrame(ASource: TMemoryStream; ACX, ACY: Integer;
		AMirror: Boolean; APalette: array of TColor; out AImg: TPNGImage);

procedure DecodeRoomImage(AData: TMemoryStream; AWidth, AHeight: Word;
		APalette: array of TColor; out ADest: TPNGImage);

procedure DecodeObjectImage(AData: TMemoryStream; AWidth, AHeight: Word;
		APalette: array of TColor; out ADest: TPNGImage);


implementation

uses
	SysUtils;

procedure DecodeCostumeFrame(ASource: TMemoryStream; ACX, ACY: Integer;
		AMirror: Boolean; APalette: array of TColor; out AImg: TPNGImage);
	var
	x,
	y,
	w: Integer;
	bsl: PByte;
	asl: PNGImage.PByteArray;
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
			c:= APalette[b];

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

procedure DecodeRoomImage(AData: TMemoryStream; AWidth, AHeight: Word;
		APalette: array of TColor; out ADest: TPNGImage);
	var
	dstTmp: array of Byte;
	dst: PByte;
//	table: TStripTable;
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

			ADest.Pixels[theX, theY]:= APalette[dst^];
			ADest.AlphaScanline[theY]^[theX]:= $FF;

			Inc(dst);
			end;
	end;

procedure DecodeObjectImage(AData: TMemoryStream; AWidth, AHeight: Word;
		APalette: array of TColor; out ADest: TPNGImage);
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

			ADest.Pixels[theX, theY]:= APalette[dst^];
			ADest.AlphaScanline[theY]^[theX]:= $FF;

			Inc(dst);
			end;
	end;


{ TSCUMMCostResInfoV2 }

function TSCUMMCostResInfoV2.Format: Byte;
	begin
	Result:= fmtByte and $7F;
	end;

function TSCUMMCostResInfoV2.NoMirror: Boolean;
	begin
	Result:= (fmtByte and $80) <> 0;
	end;

{ TSCUMMCostLimbCmdsInfoV2 }

function TSCUMMCostLimbCmdsInfoV2.HasCell: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd < $7B);
	end;

function TSCUMMCostLimbCmdsInfoV2.IsAvail: Boolean;
	begin
	Result:= isUsed and (cmdIdx = $FF);
	end;

function TSCUMMCostLimbCmdsInfoV2.IsLoop: Boolean;
	begin
	Result:= isUsed and hasCmd and hasSteps and ((flags and $80) = 0);
	end;

function TSCUMMCostLimbCmdsInfoV2.IsPause: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd = $79);
	end;

function TSCUMMCostLimbCmdsInfoV2.IsPlay: Boolean;
	begin
	Result:= isUsed and hasCmd and hasSteps;
	end;

function TSCUMMCostLimbCmdsInfoV2.IsSound: Boolean;
	begin
	Result:= isUsed and hasCmd and ((cmd and $80) <> 0);
	end;

function TSCUMMCostLimbCmdsInfoV2.IsUnPause: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd = $7A);
	end;

function TSCUMMCostLimbCmdsInfoV2.Steps: Byte;
	begin
	if  isUsed and hasCmd and hasSteps then
		Result:= (flags and $7F)
	else
		Result:= $FF;
	end;

{ TSCUMMCostAnimInfoV2 }

function TSCUMMCostAnimInfoV2.IsFramesUsed(const AFrames: Byte): Boolean;
	begin
	Result:= limbCmds[AFrames].isUsed;
	end;

{ TSCUMMCostFrameInfoV2 }

procedure TSCUMMCostFrameInfoV2.DecodeProcV2(AObjColour: Byte; ADest: TMemoryStream);
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
	SetLength(limbInfos, 0);
	SetLength(frameInfos, 0);
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

//	procedure InitAnims;
//		var
//		n: Integer;
//
//		begin
//		for n:= 15 downto 0 do
//			with animInfos[animCount].cellsCmds[n] do
//				begin
//				isUsed:= False;
//				cmdIdx:= $FF;
//				flags:= $FF;
//				hasCmd:= False;
//				cmd:= $FF;
//				hasSteps:= False;
//				end;
//		end;

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
//		immediately following the previous).
//
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
//				with animInfos[animCount].cellsCmds[n] do
//					begin
//					isUsed:= False;
//					cmdIdx:= $FF;
//					flags:= $FF;
//					hasCmd:= False;
//					cmd:= $FF;
//					hasSteps:= False;
//					end;

				if  ((m and $FFFF) <> 0)
				and ((m and $8000) <> 0) then
					begin
					animInfos[animCount].limbCmds[n].isUsed:= True;
					AStream.Read(animInfos[animCount].limbCmds[n].cmdIdx, 1);

					if  animInfos[animCount].limbCmds[n].cmdIdx < $FF then
						begin
						AStream.Read(animInfos[animCount].limbCmds[n].flags, 1);
						animInfos[animCount].limbCmds[n].hasCmd:= True;

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
		j: Integer;
		c: Byte;

		begin
		for i:= 0 to High(animInfos) do
			for j:= 0 to 15 do
				if  animInfos[i].limbCmds[j].isUsed
				and animInfos[i].limbCmds[j].hasCmd then
					begin
					c:= animCmds[animInfos[i].limbCmds[j].cmdIdx].cmd;

					animInfos[i].limbCmds[j].cmd:= c;
					animInfos[i].limbCmds[j].hasSteps:= not (c in [$7A, $79]);
					end;
		end;

	procedure LoadFramesFromStream;
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

//dengland	There is something amiss.  The table will end up with an image
//					at this point in the stream but also that there should be more
//					index data.  From what I can tell, valid table data stops
//					appearing right now.
{			for j:= 0 to 15 do
				for k:= 0 to l[j] - 1 do
					if  (t[j, k] > s)
					and (t[j, k] < Result) then
						Result:= t[j, k];}
			end;

		begin
		i:= 0;
		FillChar(l[0], 16, 0);
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
				'Error reading cell cmd to image indexes, position: ' +
						IntToStr(AStream.Position) + ', expected: ' + IntToStr(s));

		SetLength(limbInfos, Length(cellOffs));
		for c:= 0 to i - 1 do
			for x:= 0 to High(limbInfos) do
				if  p[c] = cellOffs[x] then
					limbInfos[15 - x].cmdFrameListIdx:= c;

//		As above, there is a problem with the list and any invalid references
//			should be patched out.
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
		k,
		l: Integer;

		begin
		SetLength(frameInfos, q.Count + 1);
		for i:= 0 to q.Count do
			begin
//dengland  In Zak, getting 0's in q list so I'm going to just try skipping them
			if  (i < q.Count)
			and (Integer(q[i]) = 0) then
				Continue;

			AStream.Read(frameInfos[i].width, 2);
			AStream.Read(frameInfos[i].height, 2);
			AStream.Read(frameInfos[i].relativeX, 2);
			AStream.Read(frameInfos[i].relativeY, 2);
			AStream.Read(frameInfos[i].moveX, 2);
			AStream.Read(frameInfos[i].moveY, 2);

			if  i < q.Count then
				k:= Integer(q[i])
			else
				k:= AStream.Size - 4;

			l:= k - (AStream.Position - 4);

			Assert((l > 0) and ((AStream.Size - AStream.Position) >= l),
					'Error reading image info header - k: ' + IntToStr(k) +
					' l: ' + IntToStr(l) + ' Size: ' + IntToStr(AStream.Size) +
					' Position: ' + IntToStr(AStream.Position));

			SetLength(frameInfos[i].data, l);
			for j:= 0 to l - 1 do
				AStream.Read(frameInfos[i].data[j], 1);
			end;
		end;

	procedure FixUpImageIndex;
		var
		i,
		j: Integer;

		begin
		q.Insert(0, Pointer(e));
		SetLength(cmdFrameIndex, 16);
		for i:= 0 to 15 do
			begin
{$IFDEF DEBUG}
//			cmdCellIndex[i].e:= e;
//			SetLength(cmdCellIndex[i].cmdData, l[i]);
{$ENDIF}

			SetLength(cmdFrameIndex[i].cmdFrameIdxs, l[i]);

			for j:= 0 to l[i] - 1 do
//				if t[i, j] < e then
				if  q.IndexOf(Pointer(t[i, j])) = -1 then
					begin
					Assert(False, 'There appears to be an error in the frameCmdIndex');
{$IFDEF DEBUG}
//					cmdCellIndex[i].cmdData[j]:= t[i, j];
//					cmdCellIndex[i].cmdCellIdxs[j]:= $FF;
{$ENDIF}
					end
				else
					begin
					cmdFrameIndex[i].cmdFrameIdxs[j]:= q.IndexOf(Pointer(t[i, j]));
{$IFDEF DEBUG}
//					cmdCellIndex[i].cmdData[j]:= $FF;
{$ENDIF}
					end;
			end;
		end;

	begin
	Clear;
	LoadResInfoFromStream;

//	InitAnims;
	LoadAnimsFromStream;
	FixUpAnimsIndex;

	LoadAnimCmdsFromStream;

	FixUpAnimsCmdIndex;

	q:= TList.Create;
	try
		LoadFramesFromStream;

		LoadImagesFromStream;
		FixUpImageIndex;

		finally
		q.Free;
		end;
	end;

end.
