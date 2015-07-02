unit SCUMMCostume;

interface

uses
	Types, Classes, SCUMMActor;

type
	TClassicCostumeLoader = class(TBaseCostumeLoader)
	public
		_id: Integer;
		_baseptr: PByte;
		_animCmds: PByte;
		_dataOffsets: PByte;
		_palette: PByte;
		_frameOffsets: PByte;
		_numColors: Byte;
		_numAnim: Byte;
		_format: Byte;
		_mirror: Boolean;

		constructor Create(vm: Pointer); override; //ScummEngine *

		procedure loadCostume(id: Integer; stream: TMemoryStream); override;
		procedure costumeDecodeData(a: TActor; frame: Integer;
				usemask: Cardinal; stream: TMemoryStream); override;
		function  increaseAnims(a: TActor): Byte; overload; override;

	protected
		function  increaseAnim(a: TActor; slot: Integer): Byte; overload;
	end;


	TClassicCostumeRenderer = class(TBaseCostumeRenderer)
	protected
		_loaded: TClassicCostumeLoader;

//	must wrap at 256
		_scaleIndexX: byte;
		_scaleIndexY: byte;
		_palette: array[0..31] of Word;

	public
		constructor Create(scumm: Pointer); override;
		destructor  Destroy; override;

		procedure setPalette(palette: PWord); override;
		procedure setFacing(const a: TActor); override;
		procedure setCostume(costume, shadow: Integer;
				stream: TMemoryStream); override;

	protected
		function  drawLimb(const a: TActor; limb: Integer): Byte; override;

		procedure proc3(var v1: TBaseCostumeRenderer.TCodec1);
//		procedure proc3_ami(var v1: TCodec1);
//		procedure procC64(var v1; TCodec1; actor: Integer);
//		procedure procPCEngine(var v1: TCodec1);

		function  mainRoutine(xmoveCur, ymoveCur, limb: Integer): Byte;
	end;


implementation

uses
	SysUtils;

{ TClassicCostumeLoader }

function newDirToOldDir(dir: Integer): Integer;
	begin
	Result:= 3;

	if  (dir >= 71) and (dir <= 109) then
		Result:= 1
	else if (dir >= 109) and (dir <= 251) then
		Result:= 2
	else if (dir >= 251) and (dir <= 289) then
		Result:= 0;
	end;


procedure TClassicCostumeLoader.costumeDecodeData(a: TActor; frame: Integer;
		usemask: Cardinal; stream: TMemoryStream);
	var
	r: PByte;
	baseptr: PByte;
	mask,
	j: Cardinal;
	i: Integer;
	extra,
	cmd: Byte;
	anim: Integer;

	begin
	loadCostume(a._costume, stream);

	anim:= newDirToOldDir(a.getFacing) + frame * 4;

	if  anim > _numAnim then
		Exit;

//	if (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine)
//		baseptr = _dataOffsets + anim * 2 + 2;
//	else
		baseptr:= _baseptr;

//	r = baseptr + READ_LE_UINT16(_dataOffsets + anim * 2);
	r:= baseptr + PWord(PByte(_dataOffsets + anim * 2))^;
	if  r = baseptr then
		Exit;

//	if (_vm->_game.version == 1) {
//		mask = *r++ << 8;
//	} else {
//		mask:= READ_LE_UINT16(r);
		mask:= PWord(r)^;
		Inc(r, 2);
//	}

	i:= 0;
	repeat
		if  (mask and $8000) <> 0 then
			begin
//			if ((_vm->_game.version <= 3) &&
//				!(_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine))
//				begin
				j:= r^;
				Inc(r);

				if  j = $FF then
					j:= $FFFF;
//				end
//			else
//				begin
//				j = READ_LE_UINT16(r);
//				r += 2;
//				end;

			if  (usemask and $8000) <> 0 then
				begin
				if (j = $FFFF) then
					begin
					a._cost.curpos[i]:= $FFFF;
					a._cost.start[i]:= 0;
					a._cost.frame[i]:= frame;
					end
				else
					begin
					extra:= r^;
					Inc(r);
					cmd:= _animCmds[j];
					if  cmd = $7A then
						a._cost.stopped:= a._cost.stopped and not (1 shl i)
					else if cmd = $79 then
						a._cost.stopped:= a._cost.stopped or (1 shl i)
					else
						begin
						a._cost.curpos[i]:= j;
						a._cost.start[i]:= j;

						a._cost._end[i]:= j + (extra and $7F);
						if  (extra and $80) <> 0 then
							a._cost.curpos[i]:=  a._cost.curpos[i] or $8000;
						a._cost.frame[i]:= frame;
						end;
					end;
				end
			else if j <> $FFFF then
				Inc(r);
			end;

		Inc(i);
		usemask:= usemask shl 1;
		mask:= mask shl 1;

		until (mask and $FFFF) = 0;
	end;

constructor TClassicCostumeLoader.Create(vm: Pointer);
	begin
	inherited;

	_id:= -1;
	_baseptr:= nil;
	_animCmds:= nil;
	_dataOffsets:= nil;
	_palette:= nil;
	_frameOffsets:= nil;
	_numColors:= 0;
	_numAnim:= 0;
	_format:= 0;
	_mirror:= false;
	end;

function TClassicCostumeLoader.increaseAnim(a: TActor; slot: Integer): Byte;
	var
	highflag,
	i,
	_end: Integer;
	code,
	nc: Byte;
	sound: Cardinal;

	begin
	Result:= 0;

	if  a._cost.curpos[slot] = $FFFF then
		Exit;

	highflag:= a._cost.curpos[slot] and $8000;
	i:= a._cost.curpos[slot] and $7FFF;
	_end:= a._cost._end[slot];
	code:= _animCmds[i] and $7F;

//	if (_vm->_game.version <= 3) {
		if (_animCmds[i] and $80) <> 0 then
			Inc(a._cost.soundCounter);
//	}

	repeat
		if highflag = 0 then
			begin
			if  i >= _end then
				begin
				Inc(i);
				i:= a._cost.start[slot];
				end
			else
				Inc(i);
			end
		else if  i <> _end then
			Inc(i);

		nc:= _animCmds[i];

		if  nc = $7C then
			begin
			Inc(a._cost.animCounter);
			if (a._cost.start[slot] <> _end) then
				Continue;
			end
		else
			begin
//			if (_vm->_game.version >= 6)
				begin
				if  (nc >= $71) and (nc <= $78) then
					begin
//					sound:= (_vm->_game.heversion == 60) ? 0x78 - nc : nc - 0x71;
					sound:= nc - $71;
//					_vm->_sound->addSoundToQueue2(a->_sound[sound]);
					if (a._cost.start[slot] <> _end) then
						Continue;
					end
				end
//			else
//				begin
//				if (nc == 0x78)
//					begin
//					a->_cost.soundCounter++;
//					if (a->_cost.start[slot] != end)
//						continue;
//					end;
//				end;
			end;

		a._cost.curpos[slot]:= i or highflag;
		if  (_animCmds[i] and $7F) <> code then
			Result:= 1
		else
			Result:= 0;
		Exit;

		until False;
	end;

function TClassicCostumeLoader.increaseAnims(a: TActor): Byte;
	var
	i: Integer;

	begin
	Result:= 0;

	i:= 0;
	while i <> 16 do
		begin
		if  a._cost.curpos[i] <> $FFFF then
			Result:= Result + increaseAnim(a, i);

		Inc(i);
		end;
	end;

procedure TClassicCostumeLoader.loadCostume(id: Integer; stream: TMemoryStream);
	var
	ptr: PByte;

	begin
	_id:= id;
//	byte *ptr = _vm->getResourceAddress(rtCostume, id);

	ptr:= stream.Memory;

//	if (_vm->_game.version >= 6)
//		ptr += 8;
//	else if (_vm->_game.features & GF_OLD_BUNDLE)
//dengland For what we're looking at, this would take precedence.
		Inc(ptr, 2);
//	else if (_vm->_game.features & GF_SMALL_HEADER)
//		ptr += 0;
//	else
//		ptr += 2;

	_baseptr:= ptr;

	_numAnim:= ptr[6];
	_format:= ptr[7] and $7F;
	_mirror:= (ptr[7] and $80) <> 0;
	_palette:= ptr + 8;

//	if (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine) {
//		_numColors = 16;
//
//		ptr += 8 + 17;
//		_animCmds = READ_LE_UINT16(ptr) + ptr + 2;
//		_frameOffsets = ptr + 2;
//		_dataOffsets = ptr + 34;
//		return;
//	}

	case _format of
		$57:				// Only used in V1 games
			_numColors:= 0;
		$58:
			_numColors:= 16;
		$59:
			_numColors:= 32;
		$60:				// New since version 6
			_numColors:= 16;
		$61:				// New since version 6
			_numColors:= 32;
		else
			Assert(False, Format('Costume %d with format 0x%X is invalid', [id, _format]));
		end;


// In GF_OLD_BUNDLE games, there is no actual palette, just a single color byte.
// Don't forget, these games were designed around a fixed 16 color HW palette :-)
// In addition, all offsets are shifted by 2; we accomodate that via a separate
// _baseptr value (instead of adding tons of if's throughout the code).
//	if (_vm->_game.features & GF_OLD_BUNDLE) {
		if _format = $57 then
			_numColors:= 0
		else
			_numColors:= 1;
		Inc(_baseptr, 2);
//	}
	Inc(ptr, 8 + _numColors);
	_frameOffsets:= ptr + 2;
	if  _format = $57 then
		begin
		_dataOffsets:= ptr + 18;
		Inc(_baseptr, 4);
		end
	else
		_dataOffsets:= ptr + 34;

//	_animCmds:= _baseptr + READ_LE_UINT16(ptr);
	_animCmds:= _baseptr + PWord(ptr)^;
	end;

{ TClassicCostumeRenderer }

constructor TClassicCostumeRenderer.Create(scumm: Pointer);
	begin
	inherited;

	_loaded:= TClassicCostumeLoader.Create(scumm);
	end;

destructor TClassicCostumeRenderer.Destroy;
	begin
	_loaded.Free;

	inherited;
	end;

function TClassicCostumeRenderer.drawLimb(const a: TActor; limb: Integer): Byte;
	var
	i: Integer;
	code: Integer;
	baseptr,
	frameptr: PByte;
	cost: PCostumeData;
	costumeInfo: PCostumeInfo;
	xmoveCur,
	ymoveCur: Integer;

	begin
	cost:= @a._cost;
	Result:= 0;

// If the specified limb is stopped or not existing, do nothing.
	if (cost^.curpos[limb] = $FFFF)
	or ((cost^.stopped and (1 shl limb)) <> 0) then
		Exit;

// Determine the position the limb is at
	i:= cost^.curpos[limb] and $7FFF;

	baseptr:= _loaded._baseptr;

//	Get the frame pointer for that limb
//	if (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine)
//		baseptr = _loaded._frameOffsets + limb * 2 + 2;
//	frameptr:= baseptr + READ_LE_UINT16(_loaded._frameOffsets + limb * 2);
	frameptr:= baseptr + PWord(PByte(_loaded._frameOffsets + limb * 2))^;

// 	Determine the offset to the costume data for the limb at position i
	code:= _loaded._animCmds[i] and $7F;

// 	Code 0x7B indicates a limb for which there is nothing to draw
	if (code <> $7B) then
		begin
//		if (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine)
//			baseptr = frameptr + code * 2 + 2;
//		_srcptr:= baseptr + READ_LE_UINT16(frameptr + code * 2);
		_srcptr:= baseptr + PWord(PByte(frameptr + code * 2))^;

//		if (!(_vm->_game.features & GF_OLD256) || code < 0x79) {
		if  code < $79 then
			begin
//			if (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine) {
//				_numBlocks = _srcptr[0];
//				_width = _srcptr[1] * 16;
//				_height = _srcptr[2] * 16;
//				xmoveCur = _xmove + PCE_SIGNED(_srcptr[3]);
//				ymoveCur = _ymove + PCE_SIGNED(_srcptr[4]);
//				_xmove += PCE_SIGNED(_srcptr[5]);
//				_ymove -= PCE_SIGNED(_srcptr[6]);
//				_srcptr += 7;
			if  _loaded._format = $57 then
				begin
				_width:= _srcptr[0] * 8;
				_height:= _srcptr[1];
				xmoveCur:= _xmove + ShortInt(_srcptr[2]) * 8;
				ymoveCur:= _ymove - ShortInt(_srcptr[3]);
				_xmove:= _xmove + ShortInt(_srcptr[4]) * 8;
				_ymove:= _ymove - ShortInt(_srcptr[5]);
				_srcptr:= _srcptr + 6;
				end
			else
				begin
				costumeInfo:= PCostumeInfo(_srcptr);
//				_width:= READ_LE_UINT16(&costumeInfo->width);
				_width:= costumeInfo^.width;
//				_height = READ_LE_UINT16(&costumeInfo->height);
				_height:= costumeInfo^.height;
//				xmoveCur = _xmove + (int16)READ_LE_UINT16(&costumeInfo->rel_x);
				xmoveCur:= _xmove + SmallInt(costumeInfo^.rel_x);
//				ymoveCur = _ymove + (int16)READ_LE_UINT16(&costumeInfo->rel_y);
				ymoveCur:= _ymove + SmallInt(costumeInfo^.rel_y);
//				_xmove += (int16)READ_LE_UINT16(&costumeInfo->move_x);
				_xmove:= _xmove + SmallInt(costumeInfo^.move_x);
//				_ymove -= (int16)READ_LE_UINT16(&costumeInfo->move_y);
				_ymove:= _ymove - SmallInt(costumeInfo^.move_y);
				_srcptr:= _srcptr + 12;
				end;

			Result:= mainRoutine(xmoveCur, ymoveCur, limb);
			end;
		end;
	end;

function TClassicCostumeRenderer.mainRoutine(xmoveCur, ymoveCur, limb: Integer): Byte;
	const
	scaletableSize = 128;
	newAmiCost = False;
	pcEngCost = False;

	var
	i,
	skip: Integer;
	drawFlag: Byte;
	use_scaling: Boolean;
	startScaleIndexX: Byte;
	ex1,
	ex2: Integer;
	rect: TRect;
	step: Integer;
	v1: TCodec1;
	sz: Integer;

	begin
	skip:= 0;
	drawFlag:= 1;

//	const bool newAmiCost = (_vm->_game.version == 5) && (_vm->_game.platform == Common::kPlatformAmiga);
//	const bool pcEngCost = (_vm->_game.id == GID_LOOM && _vm->_game.platform == Common::kPlatformPCEngine);

//	v1.scaletable = smallCostumeScaleTable;

	if  _loaded._numColors = 32 then
		begin
		v1.mask:= 7;
		v1._shr:= 3;
		end
	else
		begin
		v1.mask:= 15;
		v1._shr:= 4;
		end;

	case _loaded._format of
		$60, $61:
//			This format is used e.g. in the Sam&Max intro
			begin
			ex1:= _srcptr[0];
			ex2:= _srcptr[1];
			Inc(_srcptr, 2);

			if  (ex1 <> $FF) or (ex2 <> $FF) then
				begin
//				ex1:= READ_LE_UINT16(_loaded._frameOffsets + ex1 * 2);
				ex1:= PWord(PByte(_loaded._frameOffsets + ex1 * 2))^;
//				_srcptr:= _loaded._baseptr + READ_LE_UINT16(_loaded._baseptr + ex1 + ex2 * 2) + 14;
				_srcptr:= _loaded._baseptr + PWord(PByte(_loaded._baseptr + ex1 + ex2 * 2))^ + 14;
				end;
			end;
		end;

//	use_scaling:= (_scaleX != 0xFF) || (_scaleY != 0xFF);
	use_scaling:= False;

//	hack in this for our requirements.
//	v1.x:= _actorX;
//	v1.y:= _actorY;
	v1.x:= 0;
	v1.y:= 0;

//	if  use_scaling then
//		begin
////		Scale direction
//		v1.scaleXstep:= -1;
//		if  xmoveCur < 0 then
//			begin
//			xmoveCur = -xmoveCur;
//			v1.scaleXstep = 1;
//			end;
//
////		It's possible that the scale indexes will overflow and wrap
////		around to zero, so it's important that we use the same
////		method of accessing it both when calculating the size of the
////		scaled costume, and when drawing it. See bug #1519667.
//
//		if  _mirror then
//			begin
////			Adjust X position
//			startScaleIndexX:= scaletableSize - xmoveCur;
//			_scaleIndexX:= startScaleIndexX;
//
//			for i:= 0 to xmoveCur - 1 do
//				begin
//				if  v1.scaletable[_scaleIndexX++] < _scaleX)
//					v1.x -= v1.scaleXstep;
//				end;
//
//			rect.left = rect.right = v1.x;
//
//			_scaleIndexX = startScaleIndexX;
//			for (i = 0; i < _width; i++) {
//				if (rect.right < 0) {
//					skip++;
//					startScaleIndexX = _scaleIndexX;
//				}
//				if (v1.scaletable[_scaleIndexX++] < _scaleX)
//					rect.right++;
//			end
//		else {
//			/* No mirror */
//			/* Adjust X position */
//			startScaleIndexX = _scaleIndexX = xmoveCur + scaletableSize;
//			for (i = 0; i < xmoveCur; i++) {
//				if (v1.scaletable[_scaleIndexX--] < _scaleX)
//					v1.x += v1.scaleXstep;
//			}
//
//			rect.left = rect.right = v1.x;
//
//			_scaleIndexX = startScaleIndexX;
//			for (i = 0; i < _width; i++) {
//				if (rect.left >= _out.w) {
//					startScaleIndexX = _scaleIndexX;
//					skip++;
//				}
//				if (v1.scaletable[_scaleIndexX--] < _scaleX)
//					rect.left--;
//			}
//		end;
//		_scaleIndexX = startScaleIndexX;
//
//		if (skip)
//			skip--;
//
//		step = -1;
//		if (ymoveCur < 0) {
//			ymoveCur = -ymoveCur;
//			step = 1;
//		}
//
//		_scaleIndexY = scaletableSize - ymoveCur;
//		for (i = 0; i < ymoveCur; i++) {
//			if (v1.scaletable[_scaleIndexY++] < _scaleY)
//				v1.y -= step;
//		}
//
//		rect.top = rect.bottom = v1.y;
//		_scaleIndexY = scaletableSize - ymoveCur;
//		for (i = 0; i < _height; i++) {
//			if (v1.scaletable[_scaleIndexY++] < _scaleY)
//				rect.bottom++;
//		}
//
//		_scaleIndexY = scaletableSize - ymoveCur;
//	} else {

//		if  not _mirror then
			xmoveCur:= -xmoveCur;

//	hack in this for our requirements.
//		Inc(v1.x, xmoveCur);
//		Inc(v1.y, ymoveCur);

//		if  _mirror then
//			begin
//			rect.left:= v1.x;
//			rect.right:= v1.x + _width;
//			end
//		else
			begin
			rect.left:= v1.x - _width;
			rect.right:= v1.x;
			end;

		rect.top:= v1.y;
		rect.bottom:= rect.top + _height;
//	}

	v1.skip_width:= _width;

//	if _mirror then
//		v1.scaleXstep:= 1
//	else
		v1.scaleXstep:= -1;

//	if (_vm->_game.version == 1)
//		 V1 games uses 8 x 8 pixels for actors
//		_vm->markRectAsDirty(kMainVirtScreen, rect.left, rect.right + 8, rect.top, rect.bottom, _actorID);
//	else
//		_vm->markRectAsDirty(kMainVirtScreen, rect.left, rect.right + 1, rect.top, rect.bottom, _actorID);


//	if  (rect.top >= _out.h) or (rect.bottom <= 0) then
//		return 0;
//
//	if (rect.left >= _out.w || rect.right <= 0)
//		return 0;

	v1.replen:= 0;

//	if  _mirror then
//		begin
//		if  not use_scaling then
//			skip:= -v1.x;
//
//		if  skip > 0 then
//			begin
//			if  (not newAmiCost)
//			and (not pcEngCost)
//			and (_loaded._format <> $57) then
//				begin
//				Dec(v1.skip_width, skip);
//				codec1_ignorePakCols(v1, skip);
//				v1.x:= 0;
//				end;
//			end
//		else
//			begin
////			skip:= rect.right - _out.w;
//			skip:= 0;
//			if (skip <= 0) then
//				drawFlag:= 2
//			else
//				Dec(v1.skip_width, skip);
//			end
//		end
//	else
//		begin
//		if  not use_scaling then
////			skip = rect.right - _out.w;
//			skip:= rect.right - 320;
//
//		if  skip > 0 then
//			begin
//			if  (not newAmiCost)
//			and (not pcEngCost)
//			and (_loaded._format <> $57) then
//				begin
//				Dec(v1.skip_width, skip);
//				codec1_ignorePakCols(v1, skip);
////				v1.x = _out.w - 1;
//				v1.x:= 320 - 1;
//				end;
//			end
//		else
//			begin
////			V1 games uses 8 x 8 pixels for actors
//			if  _loaded._format = $57 then
//				skip:= -8 - rect.left
//			else
//				skip:= -1 - rect.left;
//			if  skip <= 0 then
//				drawFlag:= 2
//			else
//				Dec(v1.skip_width, skip);
//			end;
//		end;

	if  v1.skip_width <= 0 then
		begin
		Result:= 0;
		Exit;
		end;

	if  rect.left < 0 then
		rect.left:= 0;

	if  rect.top < 0 then
		rect.top:= 0;

//	if  rect.top > _out.h then
//		rect.top:= _out.h;

//	if (rect.bottom > _out.h)
//		rect.bottom = _out.h;

	if  _draw_top > rect.top then
		_draw_top:= rect.top;

	if  _draw_bottom < rect.bottom then
		_draw_bottom:= rect.bottom;

	if  _height + rect.top >= 256 then
		begin
		Result:= 2;
		Exit;
		end;

//	v1.destptr:= (byte *)_out.getBasePtr(v1.x, v1.y);
//	hack in...
	_out.cells[limb].width:= _width;
	_out.cells[limb].height:= _height;
	sz:= _out.cells[limb].width * _out.cells[limb].height * 2;

	SetLength(_out.cells[limb].image, sz);

	v1.destptr:= @_out.cells[limb].image[0];
//	v1.mask_ptr:= @_out.cells[limb].image[0];;

	if  _loaded._format = $57 then
//		The v1 costume renderer needs the actor number, which is
//		the same thing as the costume renderer's _actorID.
//		procC64(v1, _actorID);
	else if (newAmiCost) then
//		proc3_ami(v1);
	else if (pcEngCost) then
//		procPCEngine(v1);
	else
		proc3(v1);

	Result:= drawFlag;
	end;

function revBitMask(x: Byte): Byte; inline;
	begin
	Result:= $80 shr x;
	end;


procedure TClassicCostumeRenderer.proc3(var v1: TBaseCostumeRenderer.TCodec1);
	var
	amask: array of Byte;
	mask: PByte;
	src: PByte;
	dst: PByte;
	len,
	maskbit: Byte;
	y: Integer;
	color,
	height,
	pcolor: Cardinal;
	scaleIndexY: Byte;
	masked: Boolean;
	idx: Integer;

//  This is the first time I've ever allowed such a thing in my code.
//	Horrendous!
	label
	StartPos;

	begin
	y:= v1.y;
	src:= _srcptr;
	dst:= v1.destptr;
	len:= v1.replen;
	color:= v1.repcolor;
	height:= _height;

	idx:= 0;

	scaleIndexY:= _scaleIndexY;
//	maskbit:= revBitMask(v1.x and 7);

//	SetLength(amask, _width * _height div 8);

//	mask:= v1.mask_ptr + v1.x div 8;
//	mask:= @amask[v1.x div 8];

	if  len <> 0 then
		goto StartPos;

	repeat
		len:= src^;
		Inc(src);

		color:= len shr v1._shr;
		len:= len and v1.mask;
		if  len = 0 then
			begin
			len:= src^;
			Inc(src);
			end;

		repeat
//			if (_scaleY == 255 || v1.scaletable[scaleIndexY++] < _scaleY) {
//				begin
//				if  (v1.scaletable[scaleIndexY++] < _scaleY) then
//					begin
					Inc(scaleIndexY);
//					masked:= (y < 0) or (y >= 128) or (v1.x < 0) or (v1.x >= 320) or
//							(Assigned(v1.mask_ptr) and ((PByteArray(mask)^[0] and maskbit) <> 0));
//							((mask^ and maskbit) <> 0);

					if  (color <> 0) {and (not masked)} then
						begin
//						if (_shadow_mode and $20) <> 0 then
//							pcolor:= PByteArray(_shadow_table)^[dst^]
//						else
							begin
							pcolor:= _palette[color];
//							if (pcolor = 13) and Assigned(_shadow_table) then
//								pcolor:= PByteArray(_shadow_table)^[dst^];
							end;
//						dst^:= pcolor;

						Inc(idx);

						dst^:= $FF;
						Inc(dst);
						dst^:= Byte(pcolor);
						Inc(dst);
						end
					else
						begin
						Inc(idx);

//						mask^:= mask^ or maskbit;
						dst^:= $00;
						Inc(dst);
						dst^:= $00;
						Inc(dst);
						end;

//					Inc(dst, _width);
//					Inc(mask, _width div 8);

					Inc(y);
//					end;
//				else
//					Inc(scaleIndexY2);
//				end;

			Dec(height);
			if  height = 0 then
				begin
				Dec(v1.skip_width);

				if  v1.skip_width = 0 then
					begin
					Assert(idx = _width * _height);
					Exit;
					end;

				height:= _height;
				y:= v1.y;

				scaleIndexY:= _scaleIndexY;

				if  (_scaleX = 255) then
					Assert(False, 'Whoops, need the scale table');
//				or  (v1.scaletable[_scaleIndexX] < _scaleX) then
//					begin
//					v1.x += v1.scaleXstep;
//					if (v1.x < 0 || v1.x >= _out.w)
//						return;
//					maskbit = revBitMask(v1.x & 7);
//					v1.destptr += v1.scaleXstep;
//					end;

				Inc(_scaleIndexX, v1.scaleXstep);
//				dst:= v1.destptr;
//				mask:= v1.mask_ptr + v1.x div 8;
//				mask:= @amask[v1.x div 8];
				end;
StartPos:
			Dec(len);
			until len = 0;
		until False;
	end;

//procedure TClassicCostumeRenderer.proc3_ami(var v1: TCodec1);
//	begin
//
//	end;

//procedure TClassicCostumeRenderer.procC64(var v1; TCodec1; actor: Integer);
//	begin
//
//	end;
//
//procedure TClassicCostumeRenderer.procPCEngine(var v1: TCodec1);
//	begin
//
//	end;

procedure TClassicCostumeRenderer.setCostume(costume, shadow: Integer;
		stream: TMemoryStream);
	begin
	_loaded.loadCostume(costume, stream);
	end;

procedure TClassicCostumeRenderer.setFacing(const a: TActor);
	begin
	_mirror:= (newDirToOldDir(a.getFacing) <> 0) or _loaded._mirror;
	end;

procedure TClassicCostumeRenderer.setPalette(palette: PWord);
	var
	i: Integer;
	color: Byte;

	begin
	if  _loaded._format = $57 then
		for i:= 0 to 12 do
			_palette[i]:= PWordArray(palette)^[i]
//	else if (_vm->_game.features & GF_OLD_BUNDLE) {
	else
		begin
//todo
//		if (_vm->getCurrentLights() & LIGHTMODE_actor_use_colors) {
			for i:= 0 to 15 do
				_palette[i]:= PWordArray(palette)^[i];
//		} else {
//			for (i = 0; i < 16; i++)
//				_palette[i] = 8;
//			_palette[12] = 0;
//		}
		_palette[_loaded._palette[0]]:= _palette[0];
		end;
//	} else {
//		if (_vm->getCurrentLights() & LIGHTMODE_actor_use_colors) {
//			for (i = 0; i < _loaded._numColors; i++) {
//				color = palette[i];
//				if (color == 255)
//					color = _loaded._palette[i];
//				_palette[i] = color;
//			}
//		} else {
//			memset(_palette, 8, _loaded._numColors);
//			_palette[12] = 0;
//		}
//	}
	end;

end.
