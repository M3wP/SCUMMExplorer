unit SCUMMActor;

interface

uses
	Types, Classes, Graphics;


const
	V12_X_MULTIPLIER = 8;
	V12_Y_MULTIPLIER = 2;

	V12_X_SHIFT = 3;
	V12_Y_SHIFT = 1;

type
	TMoveFlags = (MF_NEW_LEG = 1, MF_IN_LEG = 2, MF_TURN = 4,
			MF_LAST_LEG = 8, MF_FROZEN = $80);


	PCostumeData = ^TCostumeData;
	TCostumeData = record
		active: array[0..15] of Byte;
		animCounter: Word;
		soundCounter: Byte;
		soundPos: Byte;
		stopped: Word;
		curpos: array[0..15] of Word;
		start: array[0..15] of Word;
		_end: array[0..15] of Word;
		frame: array[0..15] of Word;

//		HE specific
//		uint16 heJumpOffsetTable[16];
//		uint16 heJumpCountTable[16];
//		uint32 heCondMaskTable[16];

		procedure Reset;
	end;

	PCostumeInfo = ^TCostumeInfo;
	TCostumeInfo = packed record
		width,
		height: Word;
		rel_x,
		rel_y: SmallInt;
		move_x,
		move_y: SmallInt;
	end;

//	Result type of AdjustBox functions
	TAdjustBoxResult = record
		x,
		y: SmallInt;
		box: Byte;
	end;

const
// For small header games
	kOldInvalidBox = 255;
	kNewInvalidBox = 0;


type
	TActor = class;
	TBaseCostumeRenderer = class;

	TBaseCostumeLoader = class
	protected
//		ScummEngine *_vm;

	public
		constructor Create(vm: Pointer {ScummEngine *}); virtual;// : _vm(vm) {}
		destructor  Destroy; override;

		procedure loadCostume(id: Integer; stream: TMemoryStream); virtual; abstract;
		function  increaseAnims(a: TActor): Byte; virtual; abstract;
		procedure costumeDecodeData(a: TActor; frame: Integer; usemask: Cardinal;
				stream: TMemoryStream); virtual; abstract;

		function  hasManyDirections(id: Integer): Boolean;// { return false; }
	end;

	TBitmapData = array of Byte;

	TSimpleDrawCell = record
		width,
		height: Word;
		image: TBitmapData;
	end;

	TSimpleDrawCells = array[0..15] of TSimpleDrawCell;

	PSimpleDrawBuffer = ^TSimpleDrawBuffer;
	TSimpleDrawBuffer = record
		renderer: TBaseCostumeRenderer;
		cells: TSimpleDrawCells;
	end;

//Base class for both ClassicCostumeRenderer and AkosRenderer.
	TBaseCostumeRenderer = class
	public
		_clipOverride: TRect;
		_actorID: Byte;

		_shadow_mode: byte;
		_shadow_table: PByte;

		_actorX,
		_actorY: Integer;
		_zbuf: byte;
		_scaleX,
		_scaleY: byte;

		_draw_top,
		_draw_bottom: Integer;
		_paletteNum: byte;
		_skipLimbs: Boolean;
		_actorDrawVirScr: Boolean;

	protected
//		ScummEngine *_vm;

//		Destination
		_out: PSimpleDrawBuffer;  //Graphics::Surface
		_numStrips: Integer;

//		Source pointer
		_srcptr: PByte;

//		current move offset
		_xmove,
		_ymove: Integer;

//		whether to draw the actor mirrored
		_mirror: Boolean;

		_numBlocks: Integer;

//		width and height of cel to decode
		_width,
		_height: Integer;

	public type
		TCodec1 = record
//			Parameters for the original ("V1") costume codec.
//			These ones are accessed from ARM code. Don't reorder.
			x,
			y: Integer;
//			scaletable: PByte;
			skip_width: Integer;
			destptr: PByte;
			mask_ptr: PByte;
			scaleXstep: Integer;
			mask,
			_shr: Byte;
			repcolor: Byte;
			replen: Byte;
//			These ones aren't accessed from ARM code.
			boundsRect: TRect;
			scaleXindex,
			scaleYindex: Integer;
		end;

	public
		constructor Create(scumm: Pointer); virtual; //(ScummEngine *

		destructor Destroy; override;

		procedure setPalette(palette: PWord); virtual; abstract;
		procedure setFacing(const a: TActor); virtual; abstract;
		procedure setCostume(costume, shadow: Integer;
				stream: TMemoryStream); virtual; abstract;

		function  drawCostume(const vs: PSimpleDrawBuffer {VirtScreen &};
				numStrips: Integer; const a: TActor; drawToBackBuf: Boolean): Byte;

	protected
		function  drawLimb(const a: TActor; limb: Integer): byte; virtual; abstract;

		procedure codec1_ignorePakCols(var v1: TCodec1; num: Integer);
	end;


	TActor = class
	public
		kInvalidBox: Byte;

	protected
//		_vm: PScummEngine;

//		The position of the actor inside the virtual screen.
		_pos: TPoint;

	public
		_top,
		_bottom: Integer;
		_width: Cardinal;
		_number: Byte;
		_costume: Word;
		_room: Byte;

	public
		_talkColor: Byte;
		_talkFrequency: Integer;
		_talkPan: Byte;
		_talkVolume: Byte;
		_boxscale: Word;
		_scalex, _scaley: Byte;
		_charset: Byte;
		_moving: Byte;
		_ignoreBoxes: Boolean;
		_forceClip: Byte;

		_initFrame: Byte;
		_walkFrame: Byte;
		_standFrame: Byte;
		_talkStartFrame: Byte;
		_talkStopFrame: Byte;

		_needRedraw,
		_needBgReset,
		_visible: Boolean;
		_shadowMode: Byte;
		_flip: Boolean;
		_frame: Byte;
		_walkbox: Byte;
		_talkPosX,
		_talkPosY: SmallInt;
		_talkScript,
		_walkScript: Word;
		_ignoreTurns: Boolean;
		_drawToBackBuf: Boolean;
		_layer: Integer;
		_sound: array[0..31] of Word;

		_cost: TCostumeData;

//		HE specific
//		int _heOffsX, _heOffsY;
//		bool _heSkipLimbs;
//		uint32 _heCondMask;
//		uint32 _hePaletteNum;
//		uint32 _heXmapNum;

	protected type
		TActorWalkData = record
			dest: TPoint;           // Final destination point
			destbox: Byte;                 // Final destination box
			destdir: SmallInt;                // Final destination, direction to face at

			cur: TPoint;            // Last position
			curbox: Byte;                  // Last box

			next: TPoint;           // Next position on our way to the destination, i.e. our intermediate destination

			point3: TPoint;
			deltaXFactor,
			deltaYFactor: Integer;
			xfrac,
			yfrac: Word;
		end;

	protected
		_palette: array[0..255] of Word;
		_elevation: Integer;
		_facing: Word;
		_targetFacing: Word;
		_speedx, _speedy: Cardinal;
		_animProgress,
		_animSpeed: Byte;
		_costumeNeedsInit: Boolean;
		_walkdata: TActorWalkData;
		_animVariable: array[0..26] of SmallInt;

	public
		constructor Create(scumm: Pointer {ScummEngine *}; id: Integer); virtual;
		destructor  Destroy; override;

//		procedure HideActor; virtual;
		procedure ShowActor(bcl: TBaseCostumeLoader; stream: TMemoryStream);

		procedure InitActor(mode: Integer); virtual;

//		procedure PutActor; overload;
//		procedure PutActor(room: Integer); overload;
//		procedure PutActor(x, y: Integer); overload;

//		procedure PutActor(x, y, room: Integer); overload;
		procedure SetActorWalkSpeed(newSpeedX, newSpeedY: Cardinal);

	protected
//		function  CalcMovementFactor(var next: TPoint): Integer;
//		function  ActorWalkStep: Integer;
//		function  RemapDirection(dir: Integer; is_walking: Boolean): Integer;
		procedure SetupActorScale; virtual;

//		procedure SetBox(box: Integer);
//		function  UpdateActorDirection(is_walking: Boolean): Integer;

	public
		procedure AdjustActorPos;
//		function  AdjustXYToBeInBox(dstX, dstY: Integer): TAdjustBoxResult; virtual;

//		procedure SetDirection(direction: Integer); virtual;
//		procedure FaceToObject(obj: Integer);
		procedure TurnToDirection(newdir: Integer);
//		procedure WalkActor; virtual;

		procedure DrawActorCostume(vs: PSimpleDrawBuffer;
				stream: TMemoryStream;
				const hitTestMode: Boolean = False); virtual;
		procedure PrepareDrawActorCostume(bcr: TBaseCostumeRenderer;
				stream: TMemoryStream); virtual;

//		procedure AnimateCostume; virtual;
		procedure SetActorCostume(c: Integer); virtual;

//		procedure AnimateLimb(limb, f: Integer);

//		function  ActorHitTest(x, y: Integer): Boolean;

		function  GetActorName: string;
//		procedure StartWalkActor(x, y, dir: Integer);
		procedure StopActorMoving;

	protected
//		procedure StartWalkAnim(cmd, angle: Integer);

	public
//		procedure RunActorTalkScript(f: Integer);
		procedure StartAnimActor(f: Integer; bcl: TBaseCostumeLoader;
				stream: TMemoryStream); virtual;

//		procedure RemapActorPalette(r_fact, g_fact, b_fact, threshold: Integer);
//		procedure RemapActorPaletteColor(slot, color: Integer);

//		procedure AnimateActor(anim: Integer);

//		function  IsInCurrentRoom: Boolean;

//		function  GetPos: TPoint;
//		function  GetRealPos: PPoint;
//		function  GetRoom: Integer;
		function  GetFacing: Integer;
//		procedure SetFacing(newFacing: Integer);

//		function  GetAnimVar(_var: Byte): Integer;
//		procedure SetAnimVar(_var: Byte; value: Integer);

//		procedure SetAnimSpeed(newAnimSpeed: Byte);
//		function  GetAnimSpeed: Integer;
//		function  GetAnimProgress: Integer;
//		function  GetElevation: Integer;
//		procedure SetElevation(newElevation: Integer);
//		procedure SetPalette(idx, val: Integer);
//		procedure SetScale(sx, sy: Integer);

//		procedure ClassChanged(cls: Integer; value: Boolean);

// 		Used by the save/load system:
//		virtual void saveLoadWithSerializer(Serializer *ser);

	protected
//		function  IsInClass(cls: Integer): Boolean;

//		function  IsPlayer: Boolean; virtual;

//		function  FindPathTowards(box, box2, box3: Byte; var FoundPath: TPoint): Boolean;
	end;

	TActorV3 = class(TActor)
	public
		constructor Create(scumm: Pointer; id :Integer); override;

//		procedure WalkActor; override;

	protected
//		procedure SetupActorScale; override;
//		procedure FindPathTowardsOld(box, box2, box3: Byte; var p2, p3: TPoint);
	end;

	TActorV2 = class(TActorV3)
	public
		constructor Create(scumm: Pointer; id :Integer); override;

		procedure InitActor(mode: Integer); override;
//		procedure WalkActor; override;
//		function  AdjustXYToBeInBox(dstX, dstY: Integer): TAdjustBoxResult; override;

	protected
//		function  IsPlayer: Boolean; override;

		procedure PrepareDrawActorCostume(bcr: TBaseCostumeRenderer;
				stream: TMemoryStream); override;

//		function  CheckWalkboxesHaveDirectPath(var foundPath: TPoint): Boolean; override;
	end;


implementation



procedure TCostumeData.Reset;
	var
	i: Integer;

	begin
	stopped:= 0;
	for i:= 0 to 15 do
		begin
		active[i]:= 0;
		curpos[i]:= $FFFF;
		start[i]:= $FFFF;
		_end[i]:= $FFFF;
		frame[i]:= $FFFF;
		end;
	end;

//procedure TActor.PutActor;
//	begin
//	PutActor(_pos.x, _pos.y, _room);
//	end;
//
//procedure TActor.PutActor(room: Integer);
//	begin
//	PutActor(_pos.x, _pos.y, room);
//	end;
//
//procedure TActor.PutActor(x, y: Integer);
//	begin
//	PutActor(x, y, _room);
//	end;
//
//function TActor.IsInCurrentRoom: Boolean;
//	begin
////	Result:= _room = _vm^._currentRoom;
//	end;
//
//function TActor.getPos: TPoint;
//	begin
//	Result:= _pos;
////	if (_vm^._game.version <= 2) then
////		begin
//		Result.x:= Result.x * V12_X_MULTIPLIER;
//		Result.y:= Result.y * V12_Y_MULTIPLIER;
////		end;
//	end;
//
//function TActor.getRealPos: PPoint;
//	begin
//	Result:= @_pos;
//	end;
//
//function TActor.getRoom: Integer;
//	begin
//	Result:= _room;
//	end;
//
//procedure TActor.setFacing(newFacing: Integer);
//	begin
//	_facing:= newFacing;
//	end;
//
//procedure TActor.setAnimSpeed(newAnimSpeed: Byte);
//	begin
//	_animSpeed:= newAnimSpeed;
//	_animProgress:= 0;
//	end;
//
//function TActor.getAnimSpeed: Integer;
//	begin
//	Result:= _animSpeed;
//	end;
//
//function TActor.getAnimProgress: Integer;
//	begin
//	Result:= _animProgress;
//	end;
//
//function TActor.getElevation: Integer;
//	begin
//	Result:= _elevation;
//	end;
//
//procedure TActor.setElevation(newElevation: Integer);
//	begin
//	if  _elevation <> newElevation then
//		begin
//		_elevation:= newElevation;
//		_needRedraw:= True;
//		end;
//	end;
//
//procedure TActor.setPalette(idx, val: Integer);
//	begin
//	_palette[idx] = val;
//	_needRedraw:= true;
//	end;
//
//procedure TActor.setScale(sx, sy: Integer);
//	begin
//	if  sx <> -1 then
//		_scalex:= sx;
//	if  sy <> -1 then
//		_scaley:= sy;
//	_needRedraw = true;
//	end;

procedure TActor.AdjustActorPos;
	begin
//	AdjustBoxResult abr;
//
//	abr = adjustXYToBeInBox(_pos.x, _pos.y);

//	_pos.x = abr.x;
//	_pos.y = abr.y;
	_pos.x:= 0;
	_pos.y:= 0;
//	_walkdata.destbox = abr.box;

//	setBox(abr.box);

	_walkdata.dest.x:= -1;

	stopActorMoving();
	_cost.soundCounter:= 0;
	_cost.soundPos:= 0;

//	if (_walkbox != kInvalidBox) {
//		byte flags = _vm->getBoxFlags(_walkbox);
//		if (flags & 7) {
			turnToDirection(_facing);
//		}
//	}
	end;

constructor TActor.Create(scumm: Pointer; id: Integer);
	begin
	inherited Create;

//	_vm(scumm), _number(id) {
 //	assert(_vm != 0);

	_number:= id;
	end;

destructor TActor.Destroy;
	begin

	inherited;
	end;

procedure TActor.DrawActorCostume(vs: PSimpleDrawBuffer;
		stream: TMemoryStream; const hitTestMode: Boolean);
	var
	bcr: TBaseCostumeRenderer;

	begin
	if  _costume = 0 then
		Exit;

	if  not hitTestMode then
		begin
		if  not _needRedraw then
			Exit;

		_needRedraw:= False;
		end;

	setupActorScale();

//	bcr:= _vm->_costumeRenderer;
	bcr:= vs.renderer;

	prepareDrawActorCostume(bcr, stream);

// 	If the actor is partially hidden, redraw it next frame.
//	if (bcr.drawCostume(vs, _vm->_gdi->_numStrips, this, _drawToBackBuf) & 1) {
	if (bcr.drawCostume(vs, 40, Self, _drawToBackBuf) and 1) <> 0 then
//		_needRedraw:= (_vm->_game.version <= 6);
		_needRedraw:= True;

	if  not hitTestMode then
		begin
//		Record the vertical extent of the drawn actor
		_top:= bcr._draw_top;
		_bottom:= bcr._draw_bottom;
		end;
	end;

function TActor.GetActorName: string;
	begin

	end;

function TActor.GetFacing: Integer;
	begin
	Result:= _facing;
	end;

procedure TActor.initActor(mode: Integer);
	begin
	if  mode = -1 then
		begin
		_top:= 0;
		_bottom:= 0;
		_needRedraw:= false;
		_needBgReset:= false;
		_costumeNeedsInit:= false;
		_visible:= false;
		_flip:= false;
		_speedx:= 8;
		_speedy:= 2;
		_frame:= 0;
		_walkbox:= 0;
		_animProgress:= 0;
		_drawToBackBuf:= false;
		FillChar(_animVariable[0], sizeof(_animVariable), 0);
		FillChar(_palette[0], sizeof(_palette), 0);
//		FillChar(_sound[0], sizeof(_sound), 0);
		FillChar(_cost, sizeof(TCostumeData), 0);
		FillChar(_walkdata, sizeof(TActorWalkData), 0);
		_walkdata.point3.x:= 32000;
		_walkScript:= 0;
		end;

	if (mode = 1) or (mode = -1) then
		begin
		_costume:= 0;
		_room:= 0;
		_pos.x:= 0;
		_pos.y:= 0;
		_facing:= 180;
//		if (_vm->_game.version >= 7)
//			_visible:= false;
		end
	else if mode = 2 then
		_facing:= 180;

	_elevation:= 0;
	_width:= 24;
	_talkColor:= 15;
	_talkPosX:= 0;
	_talkPosY:= -80;
	_boxscale:= $FF;
	_scaley:= $FF;
	_scalex:= $FF;
	_charset:= 0;

	FillChar(_sound[0], sizeof(_sound), 0);
	_targetFacing:= _facing;

	_shadowMode:= 0;
	_layer:= 0;

	stopActorMoving;
	setActorWalkSpeed(8, 2);

	_animSpeed:= 0;
//	if (_vm->_game.version >= 6)
//		_animProgress:= 0;

	_ignoreBoxes:= false;
//	_forceClip:= (_vm->_game.version >= 7) ? 100 : 0;
	_forceClip:= 0;
	_ignoreTurns:= false;

	_talkFrequency:= 256;
	_talkPan:= 64;
	_talkVolume:= 127;

	_initFrame:= 1;
	_walkFrame:= 2;
	_standFrame:= 3;
	_talkStartFrame:= 4;
	_talkStopFrame:= 5;

	_walkScript:= 0;
	_talkScript:= 0;

//	_vm->_classData[_number]:= (_vm->_game.version >= 7) ? _vm->_classData[0] : 0;
	end;

procedure TActor.PrepareDrawActorCostume(bcr: TBaseCostumeRenderer;
		stream: TMemoryStream);
	begin
	bcr._actorID:= _number;
//	bcr._actorX:= _pos.x - _vm->_virtscr[kMainVirtScreen].xstart;
//	bcr._actorY:= _pos.y - _elevation;
	bcr._actorX:= 0;
	bcr._actorY:= 0;

//	if (_vm->_game.version == 4 && (_boxscale & 0x8000)) {
//		bcr->_scaleX = bcr->_scaleY = _vm->getScaleFromSlot((_boxscale & 0x7fff) + 1, _pos.x, _pos.y);
//	} else {
//		bcr->_scaleX = _scalex;
//		bcr->_scaleY = _scaley;
//	}

	bcr._shadow_mode:= _shadowMode;
	Assert(_shadowMode = 0, 'Oops.  Need shadow mode.');
//	if (_vm->_game.version >= 5 && _vm->_game.heversion == 0) {
//		bcr->_shadow_table = _vm->_shadowPalette;
//	}

//	bcr->setCostume(_costume, (_vm->_game.heversion == 0) ? 0 : _heXmapNum);
	bcr.setCostume(_costume, 0, stream);
	bcr.setPalette(@_palette[0]);
	bcr.setFacing(Self);

//	if (_vm->_game.version >= 7) {
//
//		bcr->_zbuf = _forceClip;
//		if (bcr->_zbuf == 100) {
//			bcr->_zbuf = _vm->getMaskFromBox(_walkbox);
//			if (bcr->_zbuf > _vm->_gdi->_numZBuffer-1)
//				bcr->_zbuf = _vm->_gdi->_numZBuffer-1;
//		}
//
//	} else {
//		if  _forceClip <> 0 then
//			bcr._zbuf:= _forceClip
//		else if (isInClass(kObjectClassNeverClip))
			bcr._zbuf:= 0;
//		else {
//			bcr->_zbuf = _vm->getMaskFromBox(_walkbox);
//			if (_vm->_game.version == 0)
//				bcr->_zbuf &= 0x03;
//			if (bcr->_zbuf > _vm->_gdi->_numZBuffer-1)
//				bcr->_zbuf = _vm->_gdi->_numZBuffer-1;
//		}
//
//	}

	bcr._draw_top:= $7FFFFFFF;
	bcr._draw_bottom:= 0;
	end;

procedure TActor.StartAnimActor(f: Integer; bcl: TBaseCostumeLoader;
		stream: TMemoryStream);
	begin
//	if (_vm->_game.version >= 7 && !((_vm->_game.id == GID_FT) && (_vm->_game.features & GF_DEMO) && (_vm->_game.platform == Common::kPlatformDOS))) {
//		switch (f) {
//		case 1001:
//			f = _initFrame;
//			break;
//		case 1002:
//			f = _walkFrame;
//			break;
//		case 1003:
//			f = _standFrame;
//			break;
//		case 1004:
//			f = _talkStartFrame;
//			break;
//		case 1005:
//			f = _talkStopFrame;
//			break;
//		}
//
//		if (_costume != 0) {
//			_animProgress = 0;
//			_needRedraw = true;
//			if (f == _initFrame)
//				_cost.reset();
//			_vm->_costumeLoader->costumeDecodeData(this, f, (uint) - 1);
//			_frame = f;
//		}
//	} else {
		case f of
			$38:
				f:= _initFrame;
			$39:
				f:= _walkFrame;
			$3A:
				f:= _standFrame;
			$3B:
				f:= _talkStartFrame;
			$3C:
				f:= _talkStopFrame;
			end;

		Assert(f <> $3E);

//		if (isInCurrentRoom() && _costume != 0) {
			_animProgress:= 0;
			_needRedraw:= true;
			_cost.animCounter:= 0;
// 			V1 - V2 games don't seem to need a _cost.reset() at this point.
//			Causes Zak to lose his body in several scenes, see bug #771508
//			if (_vm->_game.version >= 3 && f == _initFrame) {
//				_cost.reset();
//				if (_vm->_game.heversion != 0) {
//				((ActorHE *)this)->_auxBlock.reset();
//				}
//			}
			bcl.costumeDecodeData(Self, f, Cardinal(- 1), stream);
			_frame:= f;
//		}
//	}
	end;

procedure TActor.stopActorMoving;
	begin
//	if _walkScript <> 0
//		_vm->stopScript(_walkScript);

	_moving:= 0;
//	if (_vm->_game.version == 0)
//		setDirection(_facing);
	end;

procedure TActor.TurnToDirection(newdir: Integer);
	begin
	if  (newdir = -1) or (_ignoreTurns) then
		Exit;

//	if (_vm->_game.version <= 6) {
		_moving:= Ord(MF_TURN);
		_targetFacing:= newdir;
//	} else {
//		_moving &= ~MF_TURN;
//		if (newdir != _facing) {
//			_moving |= MF_TURN;
//			_targetFacing = newdir;
//		}
	end;

procedure TActor.SetActorCostume(c: Integer);
	var
	i: Integer;

	begin
	_costumeNeedsInit:= true;

//	if (_vm->_game.features & GF_NEW_COSTUMES) {
//		memset(_animVariable, 0, sizeof(_animVariable));
//
//		_costume = c;
//		_cost.reset();
//
//		if (_visible) {
//			if (_costume) {
//				_vm->ensureResourceLoaded(rtCostume, _costume);
//			}
//			startAnimActor(_initFrame);
//		}
//	} else {
//		if (_visible) then
//			begin
//			hideActor();
//			_cost.reset;
//			_costume:= c;
//			showActor();
//		else
//			begin
			_costume:= c;
			_cost.reset();
//			end;
//	}


//	V1 zak uses palette[] as a dynamic costume color array.
//	if (_vm->_game.version <= 1)
//		return;

//	if (_vm->_game.features & GF_NEW_COSTUMES) {
//		for (i = 0; i < 256; i++)
//			_palette[i] = 0xFF;
//	} else if (_vm->_game.features & GF_OLD_BUNDLE) {
		for i:= 0 to 15 do
			_palette[i]:= i;

//		Make stuff more visible on CGA. Based on disassembly
//		if (_vm->_renderMode == Common::kRenderCGA && _vm->_game.version > 2) {
//			_palette[6] = 5;
//			_palette[7] = 15;
//		}
//	} else {
//		for (i = 0; i < 32; i++)
//			_palette[i] = 0xFF;
//	}
	end;

procedure TActor.SetActorWalkSpeed(newSpeedX, newSpeedY: Cardinal);
	begin
	if  (newSpeedX = _speedx) and (newSpeedY = _speedy) then
		Exit;

	_speedx:= newSpeedX;
	_speedy:= newSpeedY;

	if _moving <> 0 then
		begin
//		if (_vm->_game.version == 8 && (_moving & MF_IN_LEG) == 0)
//			return;
//		CalcMovementFactor(_walkdata.next);
		end;
	end;


procedure TActor.SetupActorScale;
	begin
//	if (_number == 2 && _costume == 7 && _vm->_game.id == GID_INDY3 && _vm->_currentRoom == 12) {
//		_scalex = 0x50;
//		_scaley = 0x50;
//	} else {
//TODO: The following could probably be removed
		_scalex:= $FF;
		_scaley:= $FF;
	end;

procedure TActor.ShowActor(bcl: TBaseCostumeLoader; stream: TMemoryStream);
	begin
	if _visible then
		Exit;

	adjustActorPos();

//	_vm->ensureResourceLoaded(rtCostume, _costume);

//	if (_vm->_game.version == 0) {
//		Actor_v0 *a = ((Actor_v0 *)this);
//
//		a->_costCommand = a->_costCommandNew = 0xFF;
//
//		for (int i = 0; i < 8; ++i) {
//			a->_limbFrameRepeat[i] = 0;
//			a->_limbFrameRepeatNew[i] = 0;
//		}
//
//		_cost.reset();
//
//		a->_animFrameRepeat = 1;
//		a->_speaking = 0;
//
//		startAnimActor(_standFrame);
//		_visible = true;
//		return;
//
//	} else if (_vm->_game.version <= 2) {
		_cost.reset;
		startAnimActor(_standFrame, bcl, stream);
		startAnimActor(_initFrame, bcl, stream);
		startAnimActor(_talkStopFrame, bcl, stream);
//	} else {
//		if (_costumeNeedsInit) {
//			startAnimActor(_initFrame);
//			_costumeNeedsInit = false;
//		}
//	}

	StopActorMoving;
	_visible:= true;
	_needRedraw:= true;
	end;

constructor TActorV2.Create(scumm: Pointer; id: Integer);
	begin
	inherited;

	end;

procedure TActorV2.initActor(mode: Integer);
	begin
	inherited initActor(mode);

	_speedx:= 1;
	_speedy:= 1;

	_initFrame:= 2;
	_walkFrame:= 0;
	_standFrame:= 1;
	_talkStartFrame:= 5;
	_talkStopFrame:= 4;
	end;

procedure TActorV2.PrepareDrawActorCostume(bcr: TBaseCostumeRenderer;
		stream: TMemoryStream);
	begin
	inherited;

	bcr._actorX:= _pos.x;
	bcr._actorY:= _pos.y - _elevation;

//	if (_vm->_game.version <= 2) {
		bcr._actorX:= bcr._actorX * V12_X_MULTIPLIER;
		bcr._actorY:= bcr._actorY * V12_Y_MULTIPLIER;
//	}
//	bcr._actorX -= _vm->_virtscr[kMainVirtScreen].xstart;

//	if (_vm->_game.platform == Common::kPlatformNES) {
//		// In the NES version, when the actor is facing right,
//		// we need to shift it 8 pixels to the left
//		if (_facing == 90)
//			bcr->_actorX -= 8;
//	} else if (_vm->_game.version == 0) {
//			bcr->_actorX += 12;
//	} else if (_vm->_game.version <= 2) {
		// HACK: We have to adjust the x position by one strip (8 pixels) in
		// V2 games. However, it is not quite clear to me why. And to fully
		// match the original, it seems we have to offset by 2 strips if the
		// actor is facing left (270 degree).
		// V1 games are once again slightly different, here we only have
		// to adjust the 270 degree case...
		if (_facing = 270) then
			bcr._actorX:= bcr._actorX + 16
		else
//			if (_vm->_game.version == 2)
				bcr._actorX:= bcr._actorX + 8;
//	}
	end;

{ TBaseCostumeRenderer }

procedure TBaseCostumeRenderer.codec1_ignorePakCols(var v1: TCodec1;
		num: Integer);
	begin
	num:= num * _height;

	repeat
		v1.replen:= _srcptr^;
		Inc(_srcptr);
		v1.repcolor:= v1.replen shr v1._shr;
		v1.replen:= v1.replen and v1.mask;

		if  v1.replen = 0 then
			begin
			v1.replen:= _srcptr^;
			Inc(_srcptr);
			end;

		repeat
			Dec(num);

			if  num = 0 then
				Exit;

			Dec(v1.replen);
			until v1.replen = 0;
		until False;
	end;

constructor TBaseCostumeRenderer.Create(scumm: Pointer);
	begin
	inherited Create;

	_actorID:= 0;
	_shadow_mode:= 0;
	_shadow_table:= nil;
	_actorX:= 0;
	_actorY:= 0;
	_zbuf:= 0;
	_scaleX:= 0;
	_scaleY:= 0;
	_draw_top:= 0;
	_draw_bottom:= 0;

//	_vm:= scumm;

	_numStrips:= -1;
	_srcptr:= nil;
	_xmove:= 0;
	_ymove:= 0;
	_mirror:= false;
	_width:= 0;
	_height:= 0;
	_skipLimbs:= False;
	_paletteNum:= 0;
	end;

destructor TBaseCostumeRenderer.Destroy;
	begin
	inherited;
	end;

function TBaseCostumeRenderer.drawCostume(const vs: PSimpleDrawBuffer;
		numStrips: Integer; const a: TActor; drawToBackBuf: Boolean): Byte;
	var
	i: Integer;

	begin
	result:= 0;

	_out:= vs;
//	if  drawToBackBuf then
//		_out.setPixels(vs.getBackPixels(0, 0))
//	else
//		_out.setPixels(vs.getPixels(0, 0));

//	_actorX += _vm->_virtscr[kMainVirtScreen].xstart & 7;
//	_out.w = _out.pitch / _vm->_bytesPerPixel;

// 	We do not use getBasePtr here because the offset to pixels never used
// 	_vm->_bytesPerPixel, but it seems unclear why.
//	_out.setPixels((byte *)_out.getPixels() - (_vm->_virtscr[kMainVirtScreen].xstart & 7));

	_numStrips:= numStrips;

//	if (_vm->_game.version <= 1) {
//		_xmove = 0;
//		_ymove = 0;
//	} else if (_vm->_game.features & GF_OLD_BUNDLE) {
		_xmove:= -72;
		_ymove:= -100;
//	} else {
//		_xmove:= 0;
//		_ymove:= 0;
//	}

	for i:= 0 to 15 do
		Result:= Result or drawLimb(a, i);
	end;

{ TBaseCostumeLoader }

constructor TBaseCostumeLoader.Create(vm: Pointer);
	begin
	inherited Create;

	end;

destructor TBaseCostumeLoader.Destroy;
	begin

	inherited;
	end;

function TBaseCostumeLoader.hasManyDirections(id: Integer): Boolean;
	begin
	Result:= False;
	end;

{ TActorV3 }

constructor TActorV3.Create(scumm: Pointer; id: Integer);
	begin
	inherited;

	end;

end.
