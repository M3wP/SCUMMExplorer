unit SCUMMCostumeV2;

interface

uses
	Classes;

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
//		cellIndex: array of Byte;
		animIndex: array of Byte;

		function NoMirror: Boolean;
		function Format: Byte;
	end;

	TSCUMMCostLayerCmdsInfoV2 = record
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
		layerCmds: array[0..15] of TSCUMMCostLayerCmdsInfoV2;

		function IsCellsUsed(const ACells: Byte): Boolean;
	end;

	TSCUMMCostLayerCmdV2 = record
		cmd: Byte;
	end;

	TSCUMMCostLayerInfoV2 = record
		cmdCellListIdx: Byte;
	end;

	TSCUMMCostCmdCellInfoV2 = record
//		Debugging.  If there are further problems, you'll need to uncomment this
//			here and in the three places it is used.
{$IFDEF DEBUG}
//		e: Word;
//		cmdData: array of Word;
{$ENDIF}

		cmdCellIdxs: array of Byte;
	end;

	TSCUMMCostCellInfoV2 = record
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
		animCmds: array of TSCUMMCostLayerCmdV2;
		layerInfos: array of TSCUMMCostLayerInfoV2;
		cmdCellIndex: array of TSCUMMCostCmdCellInfoV2;
		cellInfos: array of TSCUMMCostCellInfoV2;

		procedure Clear;
		procedure LoadFromStream(AStream: TStream);
	end;

	TSCUMMCostAnimState = (sasNotVisible, sasRunning, sasEnded);
	TSCUMMCostAnimMode = (samPaused, samRunning);

	TSCUMMCostAnimation = record
		state: TSCUMMCostAnimState;
		mode: TSCUMMCostAnimMode;
		startIdx: Byte;
		currCmd: Byte;
		steps: Byte;
		currStep: Byte;
		loop: Boolean;
		included: Boolean;
	end;

	TSCUMMCostAnimations = array[0..15] of TSCUMMCostAnimation;

	TSCUMMAnimDir = (sadRight, sadLeft, sadDown, sadUp);
	TSCUMMAnimType = (satWalk, satStand);
	TSCUMMAnimMouth = (smoNone, smoOpen, smoClosed, smoTalk);



implementation

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

function TSCUMMCostLayerCmdsInfoV2.HasCell: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd < $7B);
	end;

function TSCUMMCostLayerCmdsInfoV2.IsAvail: Boolean;
	begin
	Result:= isUsed and (cmdIdx = $FF);
	end;

function TSCUMMCostLayerCmdsInfoV2.IsLoop: Boolean;
	begin
	Result:= isUsed and hasCmd and hasSteps and ((flags and $80) = 0);
	end;

function TSCUMMCostLayerCmdsInfoV2.IsPause: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd = $79);
	end;

function TSCUMMCostLayerCmdsInfoV2.IsPlay: Boolean;
	begin
	Result:= isUsed and hasCmd and hasSteps;
	end;

function TSCUMMCostLayerCmdsInfoV2.IsSound: Boolean;
	begin
	Result:= isUsed and hasCmd and ((cmd and $80) <> 0);
	end;

function TSCUMMCostLayerCmdsInfoV2.IsUnPause: Boolean;
	begin
	Result:= isUsed and hasCmd and (cmd = $7A);
	end;

function TSCUMMCostLayerCmdsInfoV2.Steps: Byte;
	begin
	if  isUsed and hasCmd and hasSteps then
		Result:= (flags and $7F)
	else
		Result:= $FF;
	end;

{ TSCUMMCostAnimInfoV2 }

function TSCUMMCostAnimInfoV2.IsCellsUsed(const ACells: Byte): Boolean;
	begin
	Result:= layerCmds[ACells].isUsed;
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
	SetLength(layerInfos, 0);
	SetLength(cellInfos, 0);
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
					animInfos[animCount].layerCmds[n].isUsed:= True;
					AStream.Read(animInfos[animCount].layerCmds[n].cmdIdx, 1);

					if  animInfos[animCount].layerCmds[n].cmdIdx < $FF then
						begin
						AStream.Read(animInfos[animCount].layerCmds[n].flags, 1);
						animInfos[animCount].layerCmds[n].hasCmd:= True;

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
				if  animInfos[i].layerCmds[j].isUsed
				and animInfos[i].layerCmds[j].hasCmd then
					begin
					c:= animCmds[animInfos[i].layerCmds[j].cmdIdx].cmd;

					animInfos[i].layerCmds[j].cmd:= c;
					animInfos[i].layerCmds[j].hasSteps:= not (c in [$7A, $79]);
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
//				index data.  From what I can tell, valid table data stops
//				appearing right now.
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
				'Error reading cell cmd to image indexes');

		SetLength(layerInfos, Length(cellOffs));
		for c:= 0 to i - 1 do
			for x:= 0 to High(layerInfos) do
				if  p[c] = cellOffs[x] then
					layerInfos[15 - x].cmdCellListIdx:= c;

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
//		k,
		l: Integer;

		begin
		SetLength(cellInfos, q.Count + 1);
//		k:= 0;
//		i:= 0;
//		repeat
		for i:= 0 to q.Count do
			begin
//dengland  In Zak, getting 0's in q list so I'm going to just try skipping them
			if  (i < q.Count)
			and (Integer(q[i]) = 0) then
//				begin
//				Inc(i);
				Continue;
//				end;

//			k:= i;

			AStream.Read(cellInfos[i].width, 2);
			AStream.Read(cellInfos[i].height, 2);
			AStream.Read(cellInfos[i].relativeX, 2);
			AStream.Read(cellInfos[i].relativeY, 2);
			AStream.Read(cellInfos[i].moveX, 2);
			AStream.Read(cellInfos[i].moveY, 2);

			if  i < q.Count then
				l:= Integer(q[i])
			else
				l:= AStream.Size - 4;

			Dec(l, (AStream.Position - 4));

			Assert((l > 0) and ((AStream.Size - AStream.Position) >= l),
					'Error reading image info header');

			SetLength(cellInfos[i].data, l);
			for j:= 0 to l - 1 do
				AStream.Read(cellInfos[i].data[j], 1);

//			Inc(i);
//			Inc(k);
			end;
//			until i > q.Count;
		end;

	procedure FixUpImageIndex;
		var
		i,
		j: Integer;

		begin
		q.Insert(0, Pointer(e));
		SetLength(cmdCellIndex, 16);
		for i:= 0 to 15 do
			begin
{$IFDEF DEBUG}
//			cmdCellIndex[i].e:= e;
//			SetLength(cmdCellIndex[i].cmdData, l[i]);
{$ENDIF}

			SetLength(cmdCellIndex[i].cmdCellIdxs, l[i]);

			for j:= 0 to l[i] - 1 do
//				if t[i, j] < e then
				if  q.IndexOf(Pointer(t[i, j])) = -1 then
					begin
					Assert(False, 'There appears to be an error in the imageCmdIndex');
{$IFDEF DEBUG}
//					cmdCellIndex[i].cmdData[j]:= t[i, j];
//					cmdCellIndex[i].cmdCellIdxs[j]:= $FF;
{$ENDIF}
					end
				else
					begin
					cmdCellIndex[i].cmdCellIdxs[j]:= q.IndexOf(Pointer(t[i, j]));
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
		LoadCellsFromStream;

		LoadImagesFromStream;
		FixUpImageIndex;

		finally
		q.Free;
		end;
	end;


{ TSCUMMCostImageInfoV2 }

procedure TSCUMMCostCellInfoV2.DecodeProcV2(AObjColour: Byte;
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


end.
