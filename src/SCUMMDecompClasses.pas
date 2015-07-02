unit SCUMMDecompClasses;

interface

uses
	Classes, SCUMMTypes, SPUTMTypes;

const
	FMT_SCUMMSCR_VARTY_GL = 'VAR_GLOBAL($%2.2x)';
	FMT_SCUMMSCR_VARTY_GB = 'VAR_BITS($4.4x)';
	FMT_SCUMMSCR_VARTY_RM = 'VAR_ROOM($%2.2x)';
	FMT_SCUMMSCR_VARTY_LC = 'VAR_LOCAL($%2.2x)';

	FMT_SCUMMSCR_VARTY_AC = 'ACTOR(%s)';
	FMT_SCUMMSCR_VARTY_OB = 'OBJECT(%s)';

	FMT_SCUMMSCR_VALUE_HX = '$%*.*x';

type
	TSCUMMScriptType = (sstGlobal, sstLocal, sstObject);
	TSCUMMScriptWrdSize = (sswDefault, sswByte, sswWord, sswDWord);
	TSCUMMScriptAddrSiz = (ssaDefault, ssaWord, ssaDWord);

	TSCUMMScriptHdr = array of Byte;

	TSCUMMScriptDecomp = class;

	TSCUMMScriptFunc = procedure(
			const ASource: TStream; const ADest: TStringStream;
			const AOpCode: Byte; const ADecomp: TSCUMMScriptDecomp);

	TSCUMMScriptOps = array[$00..$FF] of TSCUMMScriptFunc;


	TSCUMMScriptDecomp = class
	private type
		TTSCUMMScriptWrdSizeIntrn = sswByte..sswDWord;
		TTSCUMMScriptAddrSizIntrn = ssaWord..ssaDWord;

	private
		FOps: TSCUMMScriptOps;
		FGlobVars: TSPUTMGlobVarSlots;
		FDetect: TSCUMMDetectorData;
		FGameStats: TSPUTMStats;
		FWrdSize: TTSCUMMScriptWrdSizeIntrn;
		FAddrSiz: TTSCUMMScriptAddrSizIntrn;

	protected
		FHdrSize: Byte;
		FCurrType: TSCUMMScriptType;
		FCurrHdr: TSCUMMScriptHdr;
		FCurrData: TMemoryStream;

		procedure DoDecompileScript(const ADest: TStringStream);

	public
		constructor Create(const AFuncs: TSCUMMScriptOps;
				const AGlobVarMap: TSPUTMGlobVarMapping;
				const ADetectData: TSCUMMDetectorData;
				const AGameStats: TSPUTMStats;
				const AWrdSize:TSCUMMScriptWrdSize;
				const AAddrSize: TSCUMMScriptAddrSiz);
		destructor  Destroy; override;

		procedure DecompileScript(const ASource: TStream;
				const ADest: TStringStream; const AType: TSCUMMScriptType);

		function  FetchAddress: string; inline;
		function  FetchRelativeAddress: string; inline;

		function  FetchScriptByte: Byte; inline;
		function  FetchScriptWord: Cardinal; inline;

		function  FetchVar: string; overload; inline;
		function  FetchVar(const AValue: Cardinal): string; overload;
		function  FetchVarIntrn(const AValue: Cardinal): Cardinal;

		function  FetchVarOrDirectByteP1(const AOpCode: Byte): string; inline;
		function  FetchVarOrDirectByteP2(const AOpCode: Byte): string; inline;
		function  FetchVarOrDirectByteP3(const AOpCode: Byte): string; inline;

		function  FetchVarOrDirectWordP1(const AOpCode: Byte): string; inline;
		function  FetchVarOrDirectWordP2(const AOpCode: Byte): string; inline;
		function  FetchVarOrDirectWordP3(const AOpCode: Byte): string; inline;

		function  FetchResultVarPos: string;

		property  DetectData: TSCUMMDetectorData read FDetect;
		property  GameStats: TSPUTMStats read FGameStats;
		property  GlobVars: TSPUTMGlobVarSlots read FGlobVars;

		property  CurrType: TSCUMMScriptType read FCurrType;
		property  CurrHdr: TSCUMMScriptHdr read FCurrHdr;
	end;


implementation

uses
	SysUtils, SCUMMLogTypes;

{$I SCUMMEndianStreamWrapper.inc}

const
	FLG_SCUMMSCR_PMASK_P1 = $80;
	FLG_SCUMMSCR_PMASK_P2 = $40;
	FLG_SCUMMSCR_PMASK_P3 = $20;

{ TSCUMMScriptDecomp }

constructor TSCUMMScriptDecomp.Create(const AFuncs: TSCUMMScriptOps;
		const AGlobVarMap: TSPUTMGlobVarMapping;
		const ADetectData: TSCUMMDetectorData;
		const AGameStats: TSPUTMStats;
		const AWrdSize:TSCUMMScriptWrdSize;
		const AAddrSize: TSCUMMScriptAddrSiz);
	var
	i: Integer;

	begin
	Assert(ADetectData.game.ver <> scvUnk);

	inherited Create;

	Move(AFuncs[0], FOps[0], SizeOf(TSCUMMScriptOps));
	FillChar(FGlobVars[Low(TSPUTMGlobVarName)], SizeOf(TSPUTMGlobVarSlots), $FF);

	for i:= 0 to High(AGlobVarMap) do
		FGlobVars[AGlobVarMap[i].varName]:= AGlobVarMap[i].slot;

	FDetect:= ADetectData;
	FGameStats:= AGameStats;

	if  AWrdSize = sswDefault then
		begin
		FWrdSize:= sswWord;

		if  ADetectData.game.ver = scv0 then
			FWrdSize:= sswByte;
		end
	else
		FWrdSize:= AWrdSize;

	if  AAddrSize = ssaDefault then
		begin
		FAddrSiz:= ssaWord;
		end
	else
		FAddrSiz:= AAddrSize;

	FHdrSize:= 6;
	if  (FDetect.game.ver in [scv0..scv2])
	or  ((FDetect.game.ver = scv3)
	and  (not (scfOld256Colour in FDetect.game.feat))) then
		FHdrSize:= 4;
//todo dengland Versions 5 and above need some other logic (use IFF in some form
//		or another?).
	end;

procedure TSCUMMScriptDecomp.DecompileScript(const ASource: TStream;
		const ADest: TStringStream; const AType: TSCUMMScriptType);
	var
	i: Integer;
	s: Cardinal;
//	w: Word;
//	b: Byte;

	begin
	FCurrType:= AType;
//	s:= 0;

	FCurrData:= TMemoryStream.Create;
	try
		case FDetect.game.ver of
			scv0..scv3:
				begin
//todo dengland Check that the res size field is word sized for "newer" v3's.
				s:= READ_LE_UINT16(ASource, FDetect.encByte);
				SetLength(FCurrHdr, FHdrSize);
				for i:= 0 to FHdrSize - 1 do
					FCurrHdr[i]:= READ_LE_UINT8(ASource, FDetect.encByte);

				FCurrData.SetSize(s);
				ASource.Read(FCurrData.Memory^, s);
				end;
			scv4:
				;
			scv5:
				;
			scv6:
				;
			scv7:
				;
			scv8:
				;
			end;

		FCurrData.Seek(0, soFromBeginning);
		DoDecompileScript(ADest);

		finally
		FCurrData.Free;
		end;
	end;

destructor TSCUMMScriptDecomp.Destroy;
	begin

	inherited;
	end;

procedure TSCUMMScriptDecomp.DoDecompileScript(const ADest: TStringStream);
	var
	o: Byte;

	begin
	try
		while FCurrData.Position < FCurrData.Size do
			begin
			o:= READ_LE_UINT8(FCurrData, FDetect.encByte);

			try
				FOps[o](FCurrData, ADest, o, Self);

				except
				on e: Exception do
					SCUMMExpLogError(sxkDecompiler,
							'Failed to decompile script with message: %s.',
							[e.Message]);
				end;
			end;
		except
		SCUMMExpLogAbort(sxkDecompiler, 'Decompilation aborted.', []);
		end;
	end;

function TSCUMMScriptDecomp.FetchAddress: string;
	begin
	Result:= Format('$%*.*x:  ', [Ord(FAddrSiz) shl 2, Ord(FAddrSiz) shl 2,
			FCurrData.Position]);
	end;

function TSCUMMScriptDecomp.FetchRelativeAddress: string;
	var
	a: Cardinal;

	begin
	case FAddrSiz of
		ssaWord:
			a:= READ_LE_UINT16(FCurrData, FDetect.encByte);
		ssaDWord:
//fixme dengland Implement newer address sizes.
			a:= $FFFFFFFF;
		end;

	Result:= Format('  $%*.*x', [Ord(FAddrSiz) shl 2, Ord(FAddrSiz) shl 2,
			FCurrData.Position + a]);
	end;

function TSCUMMScriptDecomp.FetchResultVarPos: string;
	var
	a: Integer;
	v: Cardinal;

	begin
	v:= FetchScriptWord;

	if  (v and $2000) <> 0 then
		begin
		a:= FetchScriptWord;

		if (a and $2000) <> 0 then
			Inc(v, FetchVarIntrn(a and (not $2000)))
		else
			Inc(v, a and $0FFF);

		v:= v and (not $2000);
		end;

	Result:= Format(FMT_SCUMMSCR_VALUE_HX, [Ord(FWrdSize), Ord(FWrdSize), v]);
	end;

function TSCUMMScriptDecomp.FetchScriptByte: Byte;
	begin
	Result:= READ_LE_UINT8(FCurrData, FDetect.encByte);
	end;

function TSCUMMScriptDecomp.FetchScriptWord: Cardinal;
	begin
	case FWrdSize of
		sswByte:
			Result:= READ_LE_UINT8(FCurrData, FDetect.encByte);
		sswWord:
			Result:= READ_LE_UINT16(FCurrData, FDetect.encByte);
		else
//fixme dengland Need other size fetches.
			Result:= 0;
		end;
	end;

function  TSCUMMScriptDecomp.FetchVarIntrn(const AValue: Cardinal): Cardinal;
	var
	a: Integer;
	v: Cardinal;
	bit: Integer;

	begin
	v:= AValue;

	if  ((v and $2000) <> 0)
	and (FDetect.game.ver <= scv5) then
		begin
		a:= FetchScriptWord;

		if  (a and $2000) <> 0 then
			Inc(v, FetchVarIntrn(a and (not $2000)))
		else
			Inc(v, a and $0FFF);

		v:= v and (not $2000);
		end;

	if  (v and $F000) = 0 then
		begin
//		if (!_copyProtection) {
//			if (var == 490 && _game.id == GID_MONKEY2) {
//				var = 518;
//			}
//		}

//		if  (FGlobVars[sgvSubtitles] <> $FF)
//		and (v = FGlobVars[sgvSubtitles]) then
//			begin
//			if  not (sgoNoSubtitles in FDetect.game.guiOpts) then
//				Result:= 1
//			else
//				Result:= 0;
//
//			Exit;
//			end;
//
//		if  (FGlobVars[sgvNoSubtitles] <> $FF)
//		and (v = FGlobVars[sgvNoSubtitles]) then
//			begin
//			if  sgoNoSubtitles in FDetect.game.guiOpts then
//				Result:= 0
//			else
//				Result:= 1;
//
//			Exit;
//			end;

		Assert((v > 0) and (Integer(v) < FGameStats.numVariables - 1),
				'failed global variable (reading)');
//		return _scummVars[var];
//fixme dengland  It occurs to me that since I'm not able to calculate the
//		current values in the variables, I can't actually provide the value
//		here.  I don't know how this would have been compiled.  If this
//		functionality must be supported then I have to make the outer, calling
//		function much more complex.
		Assert(False, 'Using global var as pointer/modulo/index not supported!');
		Result:= v;
		Exit;
		end;

	if  (v and $8000) <> 0 then
		begin
		if  FDetect.game.subv >= 80 then
			begin
			v:= v and $0FFF;
			Assert((v > 0) and (Integer(v) < FGameStats.numRoomVariables - 1),
					'failed room variable (reading)');
//			return _roomVars[var];
//fixme dengland See below...
			Assert(False, 'Using room var as pointer/modulo/index not supported!');
			Result:= v;
			Exit;
			end
//dengland Isn't this the same as saying its scv0..scv2 or (scv3 + not scfOld256Colour)?
		else if (FDetect.game.ver <= scv3)
		and (not ((FDetect.game.id = scgIndy3)
		and  (FDetect.game.plat = scpFMTowns)))
		and (not ((FDetect.game.id = scgLoom)
		and  (FDetect.game.plat = scpPCEngine))) then
			begin
			bit:= v and $0F;
			v:= (v shr 4) and $FF;

//			if (not _copyProtection) then
//				if  (FDetect.game.id = scgLoom)
//				and (FDetect.game.plat = scpFMTowns)
//				and (v = 214)
//				and (bit = 15) then
//					begin
//					Result:= 0;
//					Exit;
//					end
//				else if (FDetect.game.id = scgZak)
//				and (FDetect.game.plat = scpFMTowns)
//				and (v = 151)
//				and (bit = 8) then
//					begin
//					Result:= 0;
//					Exit;
//					end;

			Assert((v > 0) and (Integer(v) < FGameStats.numVariables - 1),
					'failed global variable (reading)');
			Assert(False, 'Using global var as pointer/modulo/index not supported!');
//fixme dengland  "Dereferencing" issue for bit vars .
//			return (_scummVars[ var ] & ( 1 << bit ) ) ? 1 : 0;
			Result:= 0;
			Exit;
			end
		else
			begin
			v:= $7FFF;
//			if (!_copyProtection) {
//				if (_game.id == GID_INDY3 && (_game.platform == Common::kPlatformFMTowns) && var == 1508)
//					return 0;

			Assert((v > 0) and (Integer(v) < FGameStats.numBitVariables - 1),
					'failed bit variable (reading)');
			Assert(False, 'Using bit var as pointer/modulo/index not supported!');
//fixme dengland As above...
//			return (_bitVars[var >> 3] & (1 << (var & 7))) ? 1 : 0;
			Result:= 0;
			Exit;
			end;
		end;

	if  (v and $4000) <> 0 then
		begin
		if  scfFewLocals in FDetect.game.feat then
			v:= v and $0F
		else
			v:= v and $0FFF;

		if  FDetect.game.subv >= 80 then
			Assert((v > 0) and (v < 25), 'failed local variable (reading)')
		else
			Assert((v > 0) and (v < 20), 'failed local variable (reading)');

		Assert(False, 'Using local var as pointer/modulo/index not supported!');
//		return vm.localvar[_currentScript][var];
		Result:= 0;
		Exit;
		end;

	Assert(False, 'Illegal varbits (r)');
	Result:= $FFFFFFFF;
	end;

function TSCUMMScriptDecomp.FetchVar(const AValue: Cardinal): string;
	var
	a: Integer;
	v,
	b: Cardinal;
	bit: Integer;

	begin
	v:= AValue;

	if  ((v and $2000) <> 0)
	and (FDetect.game.ver <= scv5) then
		begin
		a:= FetchScriptWord;

		if  (a and $2000) <> 0 then
			Inc(v, FetchVarIntrn(a and (not $2000)))
		else
			Inc(v, a and $0FFF);

		v:= v and (not $2000);
		end;

	if  (v and $F000) = 0 then
		begin
//		if (!_copyProtection) {
//			if (var == 490 && _game.id == GID_MONKEY2) {
//				var = 518;
//			}
//		}

//		if  (FGlobVars[sgvSubtitles] <> $FF)
//		and (v = FGlobVars[sgvSubtitles]) then
//			begin
////fixme dengland Is this a correct translation?
//			if  not (sgoNoSubtitles in FDetect.game.guiOpts) then
//				Result:= '1'
//			else
//				Result:= '0';
//
//			Exit;
//			end;
//
//		if  (FGlobVars[sgvNoSubtitles] <> $FF)
//		and (v = FGlobVars[sgvNoSubtitles]) then
//			begin
////fixme dengland Is this a correct translation?
//			if  sgoNoSubtitles in FDetect.game.guiOpts then
//				Result:= '0'
//			else
//				Result:= '1';
//
//			Exit;
//			end;

		Assert((v > 0) and (Integer(v) < FGameStats.numVariables - 1),
				'failed global variable (reading)');
		Result:= Format(FMT_SCUMMSCR_VARTY_GL, [v]);
		Exit;
		end;

	if  (v and $8000) <> 0 then
		begin
		if  FDetect.game.subv >= 80 then
			begin
			v:= v and $0FFF;
			Assert((v > 0) and (Integer(v) < FGameStats.numRoomVariables - 1),
					'failed room variable (reading)');
			Result:= Format(FMT_SCUMMSCR_VARTY_RM, [v]);
			Exit;
			end
//dengland Isn't this the same as saying its scv0..scv2 or (scv3 + not scfOld256Colour)?
		else if (FDetect.game.ver <= scv3)
		and (not ((FDetect.game.id = scgIndy3)
		and  (FDetect.game.plat = scpFMTowns)))
		and (not ((FDetect.game.id = scgLoom)
		and  (FDetect.game.plat = scpPCEngine))) then
			begin
			b:= v;
			bit:= v and $0F;
			v:= (v shr 4) and $FF;

//			if (not _copyProtection) then
//				if  (FDetect.game.id = scgLoom)
//				and (FDetect.game.plat = scpFMTowns)
//				and (v = 214)
//				and (bit = 15) then
//					begin
//					Result:= '0';
//					Exit;
//					end
//				else if (FDetect.game.id = scgZak)
//				and (FDetect.game.plat = scpFMTowns)
//				and (v = 151)
//				and (bit = 8) then
//					begin
//					Result:= '0';
//					Exit;
//					end;

			Assert((b > 0) and (Integer(b) < FGameStats.numBitVariables - 1),
					'failed bit variable (reading)');
			Result:= Format(FMT_SCUMMSCR_VARTY_GB, [b]);;
			Exit;
			end
		else
			begin
			v:= $7FFF;
//			if (!_copyProtection) {
//				if (_game.id == GID_INDY3 && (_game.platform == Common::kPlatformFMTowns) && var == 1508)
//					return 0;

			Assert((v > 0) and (Integer(v) < FGameStats.numBitVariables - 1),
					'failed bit variable (reading)');
			Result:= Format(FMT_SCUMMSCR_VARTY_GB, [v]);;
			Exit;
			end;
		end;

	if  (v and $4000) <> 0 then
		begin
		if  scfFewLocals in FDetect.game.feat then
			v:= v and $0F
		else
			v:= v and $0FFF;

		if  FDetect.game.subv >= 80 then
			Assert((v > 0) and (v < 25), 'failed local variable (reading)')
		else
			Assert((v > 0) and (v < 20), 'failed local variable (reading)');

		Result:= Format(FMT_SCUMMSCR_VARTY_LC, [v]);
		Exit;
		end;

	Assert(False, 'Illegal varbits (r)');
	Result:= '$FFFFFFFF';
	end;

function TSCUMMScriptDecomp.FetchVar: string;
	begin
	Result:= FetchVar(FetchScriptWord);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectByteP1(const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P1}$80) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$2.2x', [FetchScriptByte]);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectByteP2(const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P2}$40) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$2.2x', [FetchScriptByte]);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectByteP3(const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P3}$20) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$2.2x', [FetchScriptByte]);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectWordP1(
		const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P1}$80) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$*.*x', [Ord(FWrdSize), Ord(FWrdSize),
				Integer(FetchScriptWord)]);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectWordP2(
		const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P2}$40) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$*.*x', [Ord(FWrdSize), Ord(FWrdSize),
				Integer(FetchScriptWord)]);
	end;

function TSCUMMScriptDecomp.FetchVarOrDirectWordP3(
		const AOpCode: Byte): string;
	begin
	if (AOpCode and {FLG_SCUMMSCR_PMASK_P3}$20) <> 0 then
		Result:= FetchVar
	else
		Result:= Format('$*.*x', [Ord(FWrdSize), Ord(FWrdSize),
				Integer(FetchScriptWord)]);
	end;

end.
