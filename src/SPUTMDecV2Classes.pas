unit SPUTMDecV2Classes;

interface

uses
	Classes, SCUMMConsts, SCUMMTypes, SCUMMClasses, SCUMMDecompClasses,
	SPUTMTypes, SPUTMClasses;

type
	TSCUMMExpDecV2 = class(TSCUMMExpDecoder)
	protected
		FRootData: PSCUMMExpObjData;
		FObjInfo: TSPUTMObjectInfoArr;
		FResData: TSPUTMResData;

		class function  GetName: string; override;
		class function  GetDescription: string; override;

		class function  GetCompatibility(const ACallIdx: Integer;
				out ASupports: TSCUMMExpDecCompat): Boolean; override;

		class function  CanDecodeGame(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData): Boolean; override;

		function  GetGameDesc: string; override;

		procedure DecodePath(const AInfoPath: TSCUMMExpDecInfoPath); override;

		procedure ReadGlobalObjects(AFile: TStream);
		procedure ReadResTypeList(AFile: TStream; AResType: TSPUTMResType);
	end;


	function  SPUTMDecV2PlgQuery(const ACoreDesc: PSCUMMPluginDesc;
			APluginDesc: PSCUMMPluginDesc): Boolean; stdcall;

	function  SPUTMDecV2PlgInit: Boolean; stdcall; export;
	procedure SPUTMDecV2PlgPrep; stdcall; export;
	procedure SPUTMDecV2PlgRels; stdcall; export;
	procedure SPUTMDecV2PlgFinl; stdcall; export;


implementation

uses
	TypInfo, SysUtils, System.Generics.Collections, System.Generics.Defaults,
	VCL.Graphics, VCL.Imaging.PNGImage,
	SCUMMLogTypes, SPUTMStrs,
	SPUTMPlatAmigaConsts, SPUTMPlatAmigaTypes,
	SPUTMDecV2Consts, SPUTMDecV2Strs, SPUTMDecV2Types, FrameSPUTMDecV2Costume;


{$I SCUMMEndianStreamWrapper.inc}


function  SPUTMDecV2PlgQuery(const ACoreDesc: PSCUMMPluginDesc;
			APluginDesc: PSCUMMPluginDesc): Boolean;
	begin
	APluginDesc^.major:= VAL_SCUMMEXP_DECDV2_VMAJ;
	APluginDesc^.minor:= VAL_SCUMMEXP_DECDV2_VMIN;
	APluginDesc^.build:= VAL_SCUMMEXP_DECDV2_VBLD;
	APluginDesc^._type:= LIT_SCUMMEXP_DECDV2_VTYP;
	APluginDesc^.stage:= LIT_SCUMMEXP_DECDV2_VSTG;
	APluginDesc^.name:= LIT_SCUMMEXP_DECDV2_VNAM;
	APluginDesc^.desc:= LIT_SCUMMEXP_DECDV2_VDSC;

	Result:= True;
	end;

function  SPUTMDecV2PlgInit: Boolean; stdcall;
	begin
	SCUMMExpDecodeReflector.AddDecoder(TSCUMMExpDecV2);
	SCUMMExpViewerReflector.AddViewer(TSPUTMDecV2CostumeFrame);

	Result:= True;
	end;

procedure SPUTMDecV2PlgPrep; stdcall;
	begin

	end;

procedure SPUTMDecV2PlgRels; stdcall;
	begin

	end;

procedure SPUTMDecV2PlgFinl; stdcall;
	begin

	end;

exports
	SPUTMDecV2PlgQuery name 'SCUMMExpPlgQuery',
	SPUTMDecV2PlgInit name 'SCUMMExpPlgInit',
	SPUTMDecV2PlgPrep name 'SCUMMExpPlgPrep',
	SPUTMDecV2PlgRels name 'SCUMMExpPlgRels',
	SPUTMDecV2PlgFinl name 'SCUMMExpPlgFinl';

//Constants
const
	VAL_SCUMMDC2_MASK_OFOWNER = $0F;
	VAL_SCUMMDC2_SHR_OFSTATE = 4;
	VAL_SCUMMDC2_RES_INVOFFSET = $FFFFFFFF;

resourcestring
	STR_SCUMMDC2_XMLHDR_DESC = '<?xml version="1.0" encoding="UTF-8"?>';

//Decoder functions
type
	TDecoderRoutine = procedure(const ASelf: TSCUMMExpDecV2;
			const AInfoPath: TSCUMMExpDecInfoPath;
			const AIndex: Integer;
			const AObjData: PSCUMMExpObjData);

procedure DoWriteXMLIntValue(AStream: TStringStream; AIndent, AName: string;
		AValue: Integer); inline;
	var
	s: string;

	begin
	s:= AIndent + '<scumm:' + AName + '>' + IntToStr(AValue) + '</scumm:' +
			AName + '>'#13#10;
	AStream.WriteString(s);
	end;

procedure DoWriteXMLHexValue(AStream: TStringStream; AIndent, AName: string;
		AValue: Integer; const ADigits: Integer = 4); inline;
	var
	s: string;

	begin
	s:= AIndent + '<scumm:' + AName + '>$' + IntToHex(AValue, ADigits) +
			'</scumm:' + AName + '>'#13#10;
	AStream.WriteString(s);
	end;

procedure DoDecodeRootCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath; const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	f: TFileStream;
	n: TSCUMMHostNode;
	i: Integer;
	m: Word;

	procedure ReadClassicIndexFile;
		begin
//		Actually, this was hard coded...
		if  ASelf.DetectData.game.id = scgManiac then
			if  ASelf.DetectData.game.ver = scv0 then
				begin
				AObjData^.gameStats._numGlobalObjects:= 256;
				AObjData^.gameStats._numRooms:= 55;
				AObjData^.gameStats._numCostumes:= 25;
				AObjData^.gameStats._numScripts:= 160;
				AObjData^.gameStats._numSounds:= 70;
				end
			else if ASelf.DetectData.game.plat = scpNES then
				begin
				AObjData^.gameStats._numGlobalObjects:= 775;
				AObjData^.gameStats._numRooms:= 55;

//				costumes 25-36 are special. see v1MMNEScostTables[] in costume.cpp
//				costumes 37-76 are room graphics resources
//				costume 77 is a character set translation table
//				costume 78 is a preposition list
//				costume 79 is unused but allocated, so the total is a nice even number :)
				AObjData^.gameStats._numCostumes:= 80;
				AObjData^.gameStats._numScripts:= 200;
				AObjData^.gameStats._numSounds:= 100;
				end
			else
				begin
				AObjData^.gameStats._numGlobalObjects:= 800;
				AObjData^.gameStats._numRooms:= 55;
				AObjData^.gameStats._numCostumes:= 35;
				AObjData^.gameStats._numScripts:= 200;
				AObjData^.gameStats._numSounds:= 100;
				end
		else if ASelf.DetectData.game.id = scgZak then
			if  ASelf.DetectData.game.plat = scpC64 then
				begin
				AObjData^.gameStats._numGlobalObjects:= 775;
				AObjData^.gameStats._numRooms:= 59;
				AObjData^.gameStats._numCostumes:= 38;
				AObjData^.gameStats._numScripts:= 155;
				AObjData^.gameStats._numSounds:= 127;
				end
			else
				begin
				AObjData^.gameStats._numGlobalObjects:= 775;
				AObjData^.gameStats._numRooms:= 61;
				AObjData^.gameStats._numCostumes:= 37;
				AObjData^.gameStats._numScripts:= 155;
				AObjData^.gameStats._numSounds:= 120;
				end;
		end;

	procedure ReadEnhancedIndexFile;
		begin
		AObjData^.gameStats._numGlobalObjects:=
				READ_LE_UINT16(f, ASelf.DetectData.encByte);

		f.Seek(AObjData^.gameStats._numGlobalObjects, soCurrent);
		AObjData^.gameStats._numRooms:= READ_LE_UINT8(f, ASelf.DetectData.encByte);

		f.Seek(AObjData^.gameStats._numRooms * 3, soCurrent);
		AObjData^.gameStats._numCostumes:= READ_LE_UINT8(f, ASelf.DetectData.encByte);

		f.Seek(AObjData^.gameStats._numCostumes * 3, soCurrent);
		AObjData^.gameStats._numScripts:= READ_LE_UINT8(f, ASelf.DetectData.encByte);

		f.Seek(AObjData^.gameStats._numScripts * 3, soCurrent);
		AObjData^.gameStats._numSounds:= READ_LE_UINT8(f, ASelf.DetectData.encByte);

//		_fileHandle->clearErr();
		f.Seek(0, soFromBeginning);

		READ_LE_UINT16(f);
		ASelf.ReadGlobalObjects(f);
		ASelf.ReadResTypeList(f, srtRoom);
		ASelf.ReadResTypeList(f, srtCostume);
		ASelf.ReadResTypeList(f, srtScript);
		ASelf.ReadResTypeList(f, srtSound);
		end;

	begin
//	This one is pretty simple for a V2 game.

//	Determine if using "classic" index file (hard coded) or "enhanced".
//fixme dengland We may need to be careful of NES, C64 and AppleII versions that
//		read virtual files via a disk image.
	if  ASelf.HostNode.FindObjectInNodes(
			Format(ASelf.DetectData.fp.pattern, [0]), n, i) then
		begin
		FillChar(AObjData^.gameStats, SizeOf(TSPUTMStats), 0);

//      Not caching this because its only used here.
		f:= TFileStream.Create(n.Objects[i], fmOpenRead);
		try
			m:= READ_LE_UINT16(f, ASelf.DetectData.encByte);

			case m of
				$0100:
					ReadEnhancedIndexFile;
				$0A31:
					ReadClassicIndexFile;
				$4643:
//					if (!(_game.platform == Common::kPlatformNES))
//						error("Use maniac target");
//					debug("NES V1 game detected");
//					assert(_game.version == 1);
					ReadClassicIndexFile;
				$0132:
//					debug("C64 V1 game detected");
//					if (_game.id == GID_MANIAC) {
//						assert(_game.version == 0);
//					} else {
//						assert(_game.version == 1);
//					}
					ReadClassicIndexFile;
				$0032:
//					debug("Apple II V1 game detected");
//					assert(_game.version == 0);
					ReadClassicIndexFile;
				else
					SCUMMExpLogError(sxkDecoder,
							'Unknown magic id (%4.4x) - this version is unsupported.',
							[m]);
				end;

			AObjData^.decoded:= True;
			ASelf.FRootData:= AObjData;

			finally
			f.Free;
			end;

//dengland As far as I can determine, in SCUMMVM a call to readMAXS is only done
//		for the enhanced versions of v2 or for versions 3 and above.  I don't
//		like that very much so I'm going to always set these values here.

//dengland In SCUMMVM the V2 engine "inherits" from the V3 one which in turn
//		"inherits" from V4.  I really don't like how they have done it by
//		splitting it over the files the way they did but never mind.  So, I got
//		these values from the V4 implementation of readMAXS.  Apparently, they
//		may not be correct, however.
//		readMAXS(0);

		AObjData^.gameStats.numVariables:= 800;				// 800
		AObjData^.gameStats.numBitVariables:= 4096;			// 2048
		AObjData^.gameStats.numLocalObjects:= 200;				// 200
		AObjData^.gameStats.numArray:= 50;
		AObjData^.gameStats.numVerbs:= 100;
		AObjData^.gameStats.numNewNames:= 50;

//fixme dengland ??
//		AObjData^.gameStats.objectRoomTable = NULL;

//??	Is this V4 related?
		AObjData^.gameStats._numCharsets:= 9;					// 9

		AObjData^.gameStats.numInventory:= 80;					// 80
		AObjData^.gameStats.numGlobalScripts:= 200;
		AObjData^.gameStats.numFlObject:= 50;

//fixme dengland ??
//		AObjData^.gameStats.shadowPaletteSize = 256;
//		SCUMMVM says:  Needs to be removed later
//		AObjData^.gameStats.shadowPalette = (byte *) calloc(_shadowPaletteSize, 1);

//dengland This is hard-coded.
		if  ASelf.FDetectData.game.id = scgManiac then
			AObjData^.gameStats._numActors:= 25
		else
			AObjData^.gameStats._numActors:= 13;
		end;
	end;

procedure DoDecodeRoomsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath; const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	g: TSCUMMExpGlobId;
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	n: TSCUMMHostNode;
	j: Integer;


	begin
//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to High(ASelf.FResData[srtRoom]) do
		if  (ASelf.FResData[srtRoom, i].roomOffs <> VAL_SCUMMDC2_RES_INVOFFSET)
//fixme dengland I need a routine in FHostNode to do this for me properly already
		and (ASelf.FHostNode.FindObjectInNodes(
				Format(ASelf.FDetectData.fp.pattern, [i]), n, j))  then
			begin
			g:= SCUMMExpCreateGlobID;
			p[High(p)]:= g;

			FillChar(r, SizeOf(TSCUMMExpObjData), 0);
			r.id:= g;
			r.name:= Format('Room %3.3d', [i]);
			r.index:= i;
			r.enumType:= sxeRoomCntnr;
			r.decoded:= False;
			r.dataType:= sxdNode;

			SCUMMExpEnumCache.SetDataForPath(p, r);
			end;

	AObjData^.decoded:= True;
	end;

procedure DoDecodeRoomCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
//	This is easy, its all statically generated
	AObjData^.decoded:= True;
	end;

procedure DoDecodeRoomScrpCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomBoxesCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomZPlanesCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

type
	TObjCompare = class(TInterfacedObject, IComparer<PSCUMMObjectData>)
	public
		function Compare(const Left, Right: PSCUMMObjectData): Integer;
	end;

function TObjCompare.Compare(const Left, Right: PSCUMMObjectData): Integer;
	begin
	Result:= Left^.obj_nr - Right^.obj_nr;
	end;


procedure DoDecodeObjectsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i,
	j,
	k: Integer;
	n: TSCUMMHostNode;
	m: TMemoryStream;
	l: TList<PSCUMMObjectData>;
	s: string;
	ptr,
	optr: PByte;
	ipos: Word;
	cnt: Byte;
	obj: PSCUMMObjectData;
	srt: TObjCompare;
	p: TSCUMMExpGlobIdArr;
	g: TSCUMMExpGlobId;
	r: TSCUMMExpObjData;

	begin
	l:= TList<PSCUMMObjectData>.Create;

	k:= ASelf.FRootData^.gameStats._numRooms;
	SCUMMExpProgressBegin(k + 4);
	for i:= 1 to k do
		begin
		s:= 'Processing Room #' + IntToStr(i);
		SCUMMExpProgressUpdate(s, i - 1);

		if  ASelf.FHostNode.FindObjectInNodes(Format(ASelf.FDetectData.fp.pattern,
				[i]), n, j) then
			begin
			m:= SCUMMFileCache.LoadOrRecallFile(n.Objects[j],
					ASelf.DetectData.EncByte);

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

				l.Add(obj);
				Dec(cnt);
				Inc(ptr, 2);
				Inc(ipos, 2);
				end;
			end;

		SCUMMExpProgressUpdate(s, i);
		end;

	SCUMMExpProgressUpdate('Sorting Objects...', k + 1);
	srt:= TObjCompare.Create;
	l.Sort(srt);
	SCUMMExpProgressUpdate('Sorting Objects...', k + 2);

	SCUMMExpProgressUpdate('Creating Object List...', k + 3);
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to l.Count - 1 do
		begin
		g:= SCUMMExpCreateGlobID;
		p[High(p)]:= g;

		FillChar(r, SizeOf(TSCUMMExpObjData), 0);
		r.id:= g;
		r.name:= Format('Object %3.3d', [l[i]^.obj_nr]);
		r.index:= i;
		r.enumType:= sxeObjectCntnr;
		r.decoded:= False;
		r.dataType:= sxdRaw;
		r.rawData:= l[i];

		SCUMMExpEnumCache.SetDataForPath(p, r);
		end;
	SCUMMExpProgressUpdate('Creating Object List...', k + 4);

	SCUMMExpProgressEnd;
	l.Free;

	AObjData^.decoded:= True;
	end;

procedure DoDecodeObjectCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
//  Don't need to do anything here.
	AObjData^.decoded:= True;
	end;

procedure DoDecodeScriptsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	g: TSCUMMExpGlobId;
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	n: TSCUMMHostNode;
	j: Integer;

	begin
//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

//dengland Script 0 is actually not permitted for execution.  Seems to have a
//		strange offset, as well.
	for i:= 1 to High(ASelf.FResData[srtScript]) do
		if  (ASelf.FResData[srtScript, i].roomOffs <> VAL_SCUMMDC2_RES_INVOFFSET)
//fixme dengland I need a routine in FHostNode to do this for me properly already
		and (ASelf.FHostNode.FindObjectInNodes(
				Format(ASelf.FDetectData.fp.pattern, [
				ASelf.FResData[srtScript, i].roomNo]), n, j))  then
			begin
			g:= SCUMMExpCreateGlobID;
			p[High(p)]:= g;

			FillChar(r, SizeOf(TSCUMMExpObjData), 0);
			r.id:= g;
			r.name:= Format('Script %3.3d', [i]);
			r.index:= i;
			r.enumType:= sxeScript;
			r.decoded:= False;
			r.dataType:= sxdScript;

			SCUMMExpEnumCache.SetDataForPath(p, r);
			end;

	AObjData^.decoded:= True;
	end;

procedure DoDecodeSoundsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeSoundCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeCostumesCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	g: TSCUMMExpGlobId;
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	n: TSCUMMHostNode;
	j: Integer;

	begin
//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to High(ASelf.FResData[srtCostume]) do
		if  (ASelf.FResData[srtCostume, i].roomOffs <> VAL_SCUMMDC2_RES_INVOFFSET)
//fixme dengland I need a routine in FHostNode to do this for me properly already
		and (ASelf.FHostNode.FindObjectInNodes(
				Format(ASelf.FDetectData.fp.pattern,
				[ASelf.FResData[srtCostume, i].roomNo]), n, j))  then
			begin
			g:= SCUMMExpCreateGlobID;
			p[High(p)]:= g;

			FillChar(r, SizeOf(TSCUMMExpObjData), 0);
			r.id:= g;
			r.name:= Format('Costume %2.2d', [i]);
			r.index:= i;
			r.enumType:= sxeCostumeCntnr;
			r.decoded:= False;
			r.dataType:= sxdNode;

			SCUMMExpEnumCache.SetDataForPath(p, r);
			end;

	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	c: PSCUMMCostumeV2;
//	f: TFileStream;
	f: TMemoryStream;
	m: TMemoryStream;
	sz: Word;
	d: Cardinal;
	n: TSCUMMHostNode;
	b: Byte;
	i,
	j: Integer;

	begin
	if  ASelf.FHostNode.FindObjectInNodes(Format(ASelf.FDetectData.fp.pattern,
			[ASelf.FResData[srtCostume, AObjData.index].roomNo]), n, j) then
		begin
		f:= SCUMMFileCache.LoadOrRecallFile(n.Objects[j], ASelf.DetectData.encByte);
		m:= TMemoryStream.Create;
//		f:= TFileStream.Create(n.Objects[j], fmOpenRead);
//		try
			d:= 0;
//			need to inject 4 bytes because they aren't there in v2
			m.Write(d, 4);

			f.Position:= ASelf.FResData[srtCostume, AObjData.index].roomOffs;
			sz:= READ_LE_UINT16(f);

//			This is a slow way to do it?
			m.SetSize(sz + 4);
			f.Position:= ASelf.FResData[srtCostume, AObjData.index].roomOffs;
			for i:= 0 to sz - 1 do
				begin
				b:= READ_LE_UINT8(f);
				m.Write(b, 1);
				end;

//			finally
//			f.Free;
//			end;

		c:= New(PSCUMMCostumeV2);
		m.Position:= 0;
		c^.LoadFromStream(m);
		m.Free;

		AObjData^.decoded:= False;
		AObjData^.dataType:= sxdNode;
		AObjData^.nodeData:= c;

		AObjData^.decoded:= True;
		end;
	end;

procedure DoDecodeCostumeAnimsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	g: TSCUMMExpGlobId;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

//	Animations

//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to High(c^.animInfos) do
		begin
		g:= SCUMMExpCreateGlobID;
		p[High(p)]:= g;

		FillChar(r, SizeOf(TSCUMMExpObjData), 0);
		r.id:= g;
		r.name:= Format('Animation %2.2d', [i]);
		r.index:= i;
		r.enumType:= sxeCostumeAnimCntnr;
		r.decoded:= False;
		r.dataType:= sxdNode;

		SCUMMExpEnumCache.SetDataForPath(p, r);
		end;

//	Animations actions table
	g:= SCUMMExpCreateGlobID;
	p[High(p)]:= g;

	FillChar(r, SizeOf(TSCUMMExpObjData), 0);
	r.id:= g;
	r.name:= 'Actions Table';
	r.index:= 0;
	r.enumType:= sxeAnimActsTbl;
	r.decoded:= False;
	r.dataType:= sxdXML;

	SCUMMExpEnumCache.SetDataForPath(p, r);

//	Animation Command Sequences
	g:= SCUMMExpCreateGlobID;
	p[High(p)]:= g;

	FillChar(r, SizeOf(TSCUMMExpObjData), 0);
	r.id:= g;
	r.name:= 'Command Sequence Table';
	r.index:= 0;
	r.enumType:= sxeAnimCmdSeqTbl;
	r.decoded:= False;
	r.dataType:= sxdXML;

	SCUMMExpEnumCache.SetDataForPath(p, r);

	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeAnimCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;

	begin
//	Animation sequence
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;
	p[High(p)]:= SCUMMExpCreateGlobID;;

//todo dengland A "properties" object is always added.  I'm not sure I want it in v2
//		costumes.  I want a "sequence" object, instead.  With the current
//		functionality, I'm not sure that I can delete it here.  Perhaps I should just
//		use the properties object instead of this sequence one.

	FillChar(r, SizeOf(TSCUMMExpObjData), 0);
	r.id:= p[High(p)];
	r.name:= 'Sequence';
	r.index:= 0;
	r.enumType:= sxeAnimSequence;
	r.decoded:= False;
	r.dataType:= sxdXML;

	SCUMMExpEnumCache.SetDataForPath(p, r);

	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeFramesCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	g: TSCUMMExpGlobId;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

//	Frames

//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to High(c^.frameInfos) do
		begin
		g:= SCUMMExpCreateGlobID;
		p[High(p)]:= g;

		FillChar(r, SizeOf(TSCUMMExpObjData), 0);
		r.id:= g;
		r.name:= Format('Frame %2.2d', [i]);
		r.index:= i;
		r.enumType:= sxeCostumeFrameCntnr;
		r.decoded:= False;
		r.dataType:= sxdNode;

		SCUMMExpEnumCache.SetDataForPath(p, r);
		end;

	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeFrameCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
//	All done "statically".
	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeLimbsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	r: TSCUMMExpObjData;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	g: TSCUMMExpGlobId;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

//	Limbs

//	Assume this isn't in the game root...
	SetLength(p, Length(AInfoPath) + 1);
	for i:= 0 to High(AInfoPath) do
		p[i]:= AInfoPath[i].id;

	for i:= 0 to High(c^.limbInfos) do
		begin
		g:= SCUMMExpCreateGlobID;
		p[High(p)]:= g;

		FillChar(r, SizeOf(TSCUMMExpObjData), 0);
		r.id:= g;
		r.name:= Format('Limb %2.2d Properties', [i]);
		r.index:= i;
		r.enumType:= sxeLimbProps;
		r.decoded:= False;
		r.dataType:= sxdXML;

		SCUMMExpEnumCache.SetDataForPath(p, r);
		end;

//	Limb frame command table

	g:= SCUMMExpCreateGlobID;
	p[High(p)]:= g;

	FillChar(r, SizeOf(TSCUMMExpObjData), 0);
	r.id:= g;
	r.name:= 'Frame Command Table';
	r.index:= 0;
	r.enumType:= sxeLimbFrameCmdTbl;
	r.decoded:= False;
	r.dataType:= sxdXML;

	SCUMMExpEnumCache.SetDataForPath(p, r);

	AObjData^.decoded:= True;
	end;

procedure DoDecodePalettesCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeCharsetsCntnr(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomScript(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomBoxMatrix(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomBoxData(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomZPlaneMask(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomOnEnter(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomOnExit(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomScaleData(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeRoomBackground(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	n: TSCUMMHostNode;
	j: Integer;
	m: TMemoryStream;
	w,
	h,
	ipos: Word;

	begin
//	Get the room data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeRoomCntnr);

	if  d^.index > 0 then
		if  ASelf.FHostNode.FindObjectInNodes(Format(ASelf.FDetectData.fp.pattern,
				[d^.index]), n, j) then
			begin
			m:= SCUMMFileCache.LoadOrRecallFile(n.Objects[j],
					ASelf.DetectData.encByte);
			try
				w:= PWord(PByte(m.Memory) + 4)^;
				h:= PWord(PByte(m.Memory) + 6)^;

				iPos:= PWord(PByte(m.Memory) + 10)^;
				m.Position:= ipos;

				DecodeRoomImage(m, w, h, AObjData^.imageData);
				AObjData^.decoded:= True;

				finally
				m.Free;
				end;
			end;
	end;

procedure DoDecodeRoomProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeObjectVerbScript(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeObjectBackground(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;

	begin
//	Get the object data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeObjectCntnr);

	AObjData^.imageData:= PSCUMMObjectData(d^.rawData)^.img;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeObjectImage(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
//  There is only a background image for V2 objects.
	end;

procedure DoDecodeObjectProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeScript(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeSoundData(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeSoundProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeAnimSequence(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeAnimActsTbl(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeAnimCmdSeqTbl(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeAnimProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeFrameImage(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	g: TPngImage;
	m: TMemoryStream;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 3);
	for i:= 0 to High(AInfoPath) - 3 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

//	Get the frame parent
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeFrameCntnr);

	m:= TMemoryStream.Create;
	try
		c^.frameInfos[d^.index].DecodeProcV2(c^.resInfo.colour, m);

		m.Position:= 0;
		DrawCostumeFrame(m, c^.frameInfos[d^.index].width,
				c^.frameInfos[d^.index].height, False, g);
		finally
		m.Free;
		end;

	AObjData^.dataType:= sxdImage;
	AObjData^.imageData:= g;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeFrameProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	s: TStringStream;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 3);
	for i:= 0 to High(AInfoPath) - 3 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

//	Get the frame parent
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeFrameCntnr);

	s:= TStringStream.Create('', TEncoding.UTF8);
	s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

	s.WriteString('<scumm:frameProps>'#13#10);

	DoWriteXMLIntValue(s, #9, 'width', c^.frameInfos[d^.index].width);
	DoWriteXMLIntValue(s, #9, 'height', c^.frameInfos[d^.index].height);
	DoWriteXMLIntValue(s, #9, 'relativeX', c^.frameInfos[d^.index].relativeX);
	DoWriteXMLIntValue(s, #9, 'relativeY', c^.frameInfos[d^.index].relativeY);
	DoWriteXMLIntValue(s, #9, 'moveX', c^.frameInfos[d^.index].moveX);
	DoWriteXMLIntValue(s, #9, 'moveY', c^.frameInfos[d^.index].moveY);

	s.WriteString('</scumm:frameProps>'#13#10);

	AObjData^.dataType:= sxdXML;
	AObjData^.xmlData:= s;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeLimbFrameCmdTbl(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i,
	j: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	s: TStringStream;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 2);
	for i:= 0 to High(AInfoPath) - 2 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

	s:= TStringStream.Create('', TEncoding.UTF8);
	s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

	s.WriteString('<scumm:frameCmdTbl>'#13#10);

	for i:= 0 to High(c^.cmdFrameIndex) do
		begin
		s.WriteString(#9'<scumm:cmdsFrames index="' + IntToStr(i) + '">'#13#10);

		for j:= 0 to High(c^.cmdFrameIndex[i].cmdFrameIdxs) do
			s.WriteString(#9#9'<scumm:cmdFrame index="' + IntToStr(j) + '" ' +
					'frame="' + IntToStr(c^.cmdFrameIndex[i].cmdFrameIdxs[j]) +
					'"/>'#13#10);

		s.WriteString(#9'</scumm:cmdsFrames>'#13#10);
		end;

	s.WriteString('</scumm:frameCmdTbl>'#13#10);

	AObjData^.dataType:= sxdXML;
	AObjData^.xmlData:= s;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeLimbProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	s: TStringStream;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 2);
	for i:= 0 to High(AInfoPath) - 2 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

	s:= TStringStream.Create('', TEncoding.UTF8);
	s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

	s.WriteString('<scumm:limb index="' + IntToStr(AObjData^.index) + '" ' +
			'frameCmdTblIdx = "' +
			IntToStr(c^.limbInfos[AObjData^.index].cmdFrameListIdx) + '">'#13#10);

	s.WriteString('</scumm:limb>'#13#10);

	AObjData^.dataType:= sxdXML;
	AObjData^.xmlData:= s;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumePaletteMap(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
//	p: TSCUMMExpGlobIdArr;
//	d: PSCUMMExpObjData;
//	c: PSCUMMCostumeV2;
	s: TStringStream;

	begin
//	Get the costume data
//	SetLength(p, Length(AInfoPath) - 1);
//	for i:= 0 to High(AInfoPath) - 1 do
//		p[i]:= AInfoPath[i].id;
//	d:= SCUMMExpEnumCache.GetDataForPath(p);
//
//	Assert(d^.enumType = sxeCostumeCntnr);
//
//	c:= PSCUMMCostumeV2(d^.nodeData);

	s:= TStringStream.Create('', TEncoding.UTF8);
	s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

	s.WriteString('<scumm:costpalmap index="0">'#13#10);
	s.WriteString(#9'<scumm:palmap index="0" colour="-1">'#13#10);
	s.WriteString(#9'<scumm:palmap index="1" colour="0">'#13#10);

	for i:= 2 to 15 do
		s.WriteString(#9'<scumm:palmap index="' + IntToStr(i) + '" colour="' +
				IntToStr(i) +'">'#13#10);

	s.WriteString('</scumm:costpalmap index="0">'#13#10);

	AObjData^.dataType:= sxdXML;
	AObjData^.xmlData:= s;
	AObjData^.decoded:= True;
	end;

procedure DoDecodeCostumeProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	c: PSCUMMCostumeV2;
	s: TStringStream;

	begin
//	Get the costume data
	SetLength(p, Length(AInfoPath) - 1);
	for i:= 0 to High(AInfoPath) - 1 do
		p[i]:= AInfoPath[i].id;
	d:= SCUMMExpEnumCache.GetDataForPath(p);

	Assert(d^.enumType = sxeCostumeCntnr);

	c:= PSCUMMCostumeV2(d^.nodeData);

	s:= TStringStream.Create('', TEncoding.UTF8);
	s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

	s.WriteString('<scumm:costume index="' + IntToStr(d^.index) + '">'#13#10);

	with c^.resInfo do
		begin
		s.WriteString(#9'<scumm:resInfo id="$' + IntToHex(unkId,4) + '" hdr="$' +
				IntToHex(unkHdr1, 4) + '" size="$' +
				IntToHex(size, 4) +'">'#13#10);

		DoWriteXMLHexValue(s, #9#9, 'unkHdr', unkHdr2);

		DoWriteXMLIntValue(s, #9#9, 'numAnims', numAnims);
		s.WriteString(#9#9'<scumm:noMirror>' + ARR_LIT_BOOL[NoMirror] +
				'</scumm:noMirror>'#13#10);
		DoWriteXMLHexValue(s, #9#9, 'format', format, 2);
		DoWriteXMLIntValue(s, #9#9, 'palColour', colour);
		s.WriteString(#9'</scumm:resInfo>'#13#10);
		end;

	s.WriteString('</scumm:costume>'#13#10);

	AObjData^.dataType:= sxdXML;
	AObjData^.xmlData:= s;
	AObjData^.decoded:= True;
	end;

procedure DoDecodePaletteProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeCharsetMap(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	begin
	end;

procedure DoDecodeGameProps(
		const ASelf: TSCUMMExpDecV2;
		const AInfoPath: TSCUMMExpDecInfoPath;
		const AIndex: Integer;
		const AObjData: PSCUMMExpObjData);
	var
	s: TStringStream;
	l1,
	l2,
	l3: string;
	t: PTypeInfo;

	procedure WriteMAXSToXML;
		begin
		s.WriteString(#9'<scumm:maxs>'#13#10);
		DoWriteXMLIntValue(s, #9#9, 'numGlobalObjects',
				ASelf.FRootData^.gameStats._numGlobalObjects);
		DoWriteXMLIntValue(s, #9#9, 'numRooms',
				ASelf.FRootData^.gameStats._numRooms);
		DoWriteXMLIntValue(s, #9#9, 'numCostumes',
				ASelf.FRootData^.gameStats._numCostumes);
		DoWriteXMLIntValue(s, #9#9, 'numScripts',
				ASelf.FRootData^.gameStats._numScripts);
		DoWriteXMLIntValue(s, #9#9, 'numSounds',
				ASelf.FRootData^.gameStats._numSounds);
		s.WriteString(#13#10);

		DoWriteXMLIntValue(s, #9#9, 'numVariables',
				ASelf.FRootData^.gameStats.numVariables);
		DoWriteXMLIntValue(s, #9#9, 'numBitVariables',
				ASelf.FRootData^.gameStats.numBitVariables);
		DoWriteXMLIntValue(s, #9#9, 'numLocalObjects',
				ASelf.FRootData^.gameStats.numLocalObjects);
		DoWriteXMLIntValue(s, #9#9, 'numArray',
				ASelf.FRootData^.gameStats.numArray);
		DoWriteXMLIntValue(s, #9#9, 'numVerbs',
				ASelf.FRootData^.gameStats.numVerbs);
		DoWriteXMLIntValue(s, #9#9, 'numNewNames',
				ASelf.FRootData^.gameStats.numNewNames);

		DoWriteXMLIntValue(s, #9#9, 'numCharsets',
				ASelf.FRootData^.gameStats._numCharsets);
		DoWriteXMLIntValue(s, #9#9, 'numActors',
				ASelf.FRootData^.gameStats._numActors);

		DoWriteXMLIntValue(s, #9#9, 'numInventory',
				ASelf.FRootData^.gameStats.numInventory);
		DoWriteXMLIntValue(s, #9#9, 'numGlobalScripts',
				ASelf.FRootData^.gameStats.numGlobalScripts);
		DoWriteXMLIntValue(s, #9#9, 'numFlObject',
				ASelf.FRootData^.gameStats.numFlObject);

		s.WriteString(#9'</scumm:maxs>'#13#10#13#10);
		end;

	procedure WriteGlobObjInfoToXML;
		var
		i: Integer;

		begin
		s.WriteString(#9'<scumm:globObjInfo>'#13#10);
		for i:= 0 to High(ASelf.FObjInfo) do
			s.WriteString(
					Format(#9#9'<%s index="%d" owner="%d">$%2.2x</%0:s>'#13#10,
					['scumm:objState', i, ASelf.FObjInfo[i].owner,
					ASelf.FObjInfo[i].state]));
		s.WriteString(#9'</scumm:globObjInfo>'#13#10);
		end;

	procedure WriteGlobResInfoToXML;
		var
		i: Integer;
		r: TSPUTMResType;

		begin
		s.WriteString(#9'<scumm:globResInfo>'#13#10);
		for r:= Low(TSPUTMResType) to High(TSPUTMResType) do
			if  Length(ASelf.FResData[r]) > 0 then
				begin
				t:= TypeInfo(TSPUTMResType);
				l1:= GetEnumName(t, Ord(r));
				s.WriteString(#9#9'<scumm:resType type="'+ l1 +'">'#13#10);

				for i:= 0 to High(ASelf.FResData[r]) do
					s.WriteString(Format(
							#9#9#9'<%s index="%d" roomNo="%d">$%4.4x</%0:s>'#13#10,
							['scumm:resOffset', i, ASelf.FResData[r,i].roomNo,
							ASelf.FResData[r,i].roomOffs]));

				s.WriteString(#9#9'</scumm:resType>'#13#10);
				end;
		s.WriteString(#9'</scumm:globResInfo>'#13#10);
		end;

	begin
	if  not AObjData^.decoded then
		begin
		s:= TStringStream.Create('', TEncoding.UTF8);

		s.WriteString(STR_SCUMMDC2_XMLHDR_DESC + #13#10#13#10);

		t:= TypeInfo(TSCUMMCoreGame);
		l1:= GetEnumName(t, Ord(ASelf.FDetectData.game.id));
		t:= TypeInfo(TSCUMMCoreVersion);
		l2:= GetEnumName(t, Ord(ASelf.FDetectData.game.ver));
		t:= TypeInfo(TSCUMMCorePlatform);
		l3:= GetEnumName(t, Ord(ASelf.FDetectData.game.plat));

//todo dengland Actually write out the data.
		s.WriteString('<scumm:game type="' + l1 +
				'" name="' + ASelf.FDetectData.game.name +
				'" variant="' + ASelf.FDetectData.game.variant +
				'" version="' + l2 +
				'" subVer="' + IntToStr(ASelf.FDetectData.game.subv) +
				'" platform="' + l3 + '">'#13#10);

		WriteMAXSToXML;
		WriteGlobObjInfoToXML;
		WriteGlobResInfoToXML;

		s.WriteString('</scumm:game>'#13#10);

		AObjData^.dataType:= sxdXML;
		AObjData^.xmlData:= s;
		AObjData^.decoded:= True;
		end;
	end;

const
	ARR_FNC_SCUMMDC2_DECFNC_ENMTYP:
			array[Low(TSCUMMExpEnumType)..High(TSCUMMExpEnumType)] of TDecoderRoutine = (
		nil,
		DoDecodeRootCntnr,
		DoDecodeRoomsCntnr,
		DoDecodeRoomCntnr,
		DoDecodeRoomScrpCntnr,
		DoDecodeRoomBoxesCntnr,
		DoDecodeRoomZPlanesCntnr,
		DoDecodeObjectsCntnr,
		DoDecodeObjectCntnr,
		DoDecodeScriptsCntnr,
		DoDecodeSoundsCntnr,
		DoDecodeSoundCntnr,
		DoDecodeCostumesCntnr,
		DoDecodeCostumeCntnr,
		DoDecodeCostumeAnimsCntnr,
		DoDecodeCostumeAnimCntnr,
		DoDecodeCostumeFramesCntnr,
		DoDecodeCostumeFrameCntnr,
		DoDecodeCostumeLimbsCntnr,
		DoDecodePalettesCntnr,
		DoDecodeCharsetsCntnr,
		DoDecodeRoomScript,
		DoDecodeRoomBoxMatrix,
		DoDecodeRoomBoxData,
		DoDecodeRoomZPlaneMask,
		DoDecodeRoomOnEnter,
		DoDecodeRoomOnExit,
		DoDecodeRoomScaleData,
		DoDecodeRoomBackground,
		DoDecodeRoomProps,
		DoDecodeObjectVerbScript,
		DoDecodeObjectBackground,
		DoDecodeObjectImage,
		DoDecodeObjectProps,
		DoDecodeScript,
		DoDecodeSoundData,
		DoDecodeSoundProps,
		DoDecodeAnimSequence,
		DoDecodeAnimActsTbl,
		DoDecodeAnimCmdSeqTbl,
		DoDecodeAnimProps,
		DoDecodeFrameImage,
		DoDecodeFrameProps,
		DoDecodeLimbFrameCmdTbl,
		DoDecodeLimbProps,
		DoDecodeCostumePaletteMap,
		DoDecodeCostumeProps,
		DoDecodePaletteProps,
		DoDecodeCharsetMap,
		DoDecodeGameProps);


{ TSCUMMExpDecV2 }

class function TSCUMMExpDecV2.CanDecodeGame(AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData): Boolean;
	begin
	Result:= True;
	end;

procedure TSCUMMExpDecV2.DecodePath(const AInfoPath: TSCUMMExpDecInfoPath);
	var
//	i: Integer;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;

	begin
//	We actually have to check every node in the InfoPath...
	SetLength(p, 2);
	p[0]:= AInfoPath[0].id;

//dengland I'm not checking each node now since it is theoretically impossible
//		that we'd need to create nodes up to the last one at this point.  Also,
//		there seems to be a strange bug involved in doing so.
//	for i:= 0 to High(AInfoPath) do
//		begin
////		First of all, get the node data from the cache
//		if  i = 0 then
//			p[1]:= VAL_SCUMMEXP_GLOBID_NULL
//		else
//			p[1]:= AInfoPath[High(AInfoPath)].id;
//
//		d:= SCUMMExpEnumCache.GetDataForPath(p);
//
////		Check it hasn't been decoded already
//		if  not d^.decoded then
////			Do decoding required for its enum type
//			try
//				ARR_FNC_SCUMMDC2_DECFNC_ENMTYP[AInfoPath[i].enumType](Self,
//						AInfoPath, i, d);
//
//				except
//				SCUMMExpLogAbort(sxkDetector, 'Unable to decode game data.', []);
//				end;
//		end;

	if  Length(AInfoPath) = 1 then
		p[1]:= VAL_SCUMMEXP_GLOBID_NULL
	else
		p[1]:= AInfoPath[High(AInfoPath)].id;

	d:= SCUMMExpEnumCache.GetDataForPath(p);
	if  not d^.decoded then
//		Do decoding required for its enum type
		try
			ARR_FNC_SCUMMDC2_DECFNC_ENMTYP[
					AInfoPath[High(AInfoPath)].enumType](Self, AInfoPath,
					High(AInfoPath), d);

			except
			SCUMMExpLogAbort(sxkDetector, 'Unable to decode game data.', []);
			end;
	end;

class function TSCUMMExpDecV2.GetCompatibility(const ACallIdx: Integer;
		out ASupports: TSCUMMExpDecCompat): Boolean;
	begin
	if  ACallIdx > High(ARR_REC_SCUMMEXP_DECDV2_PROPS) then
		Result:= False
	else
		begin
		ASupports:= ARR_REC_SCUMMEXP_DECDV2_PROPS[ACallIdx];
		Result:= True;
		end;
	end;

class function TSCUMMExpDecV2.GetDescription: string;
	begin
	Result:= STR_SCUMMEXP_DECDVS_DECDS;
	end;

function TSCUMMExpDecV2.GetGameDesc: string;
	begin
	if FDetectData.game.id = scgManiac then
		Result:= STR_SCUMMEXP_DECDDS_MMNSN
	else if FDetectData.game.id = scgZak then
		Result:= STR_SCUMMEXP_DECDDS_ZKMCK
	else
		Result:= STR_SCUMMEXP_DECDDS_OTHER;
	end;

class function TSCUMMExpDecV2.GetName: string;
	begin
	Result:= STR_SCUMMEXP_DECDVS_DECNM;
	end;

procedure TSCUMMExpDecV2.ReadGlobalObjects(AFile: TStream);
	var
	i,
	n: Integer;
	b: Byte;
	begin
//	int i;
	n:= READ_LE_UINT16(AFile, FDetectData.EncByte);
//	assert(num == _numGlobalObjects);

	SetLength(FObjInfo, n);

	for i:= 0 to n - 1 do
		begin
		b:= READ_LE_UINT8(AFile, FDetectData.EncByte);
		FObjInfo[i].owner:= b and VAL_SCUMMDC2_MASK_OFOWNER;
		FObjInfo[i].state:= b shr VAL_SCUMMDC2_SHR_OFSTATE;
		end;
	end;

procedure TSCUMMExpDecV2.ReadResTypeList(AFile: TStream;
		AResType: TSPUTMResType);
	var
	num: SmallInt;
	idx: SmallInt;

	begin
//	debug(9, "readResTypeList(%s)", nameOfResType(type));

	num:= READ_LE_UINT8(AFile, FDetectData.EncByte);

//	if  num >= $FF then
//		error("Too many %ss (%d) in directory", nameOfResType(type), num);

	SetLength(FResData[AResType], num);

	if  AResType = srtRoom then
		begin
		for idx:= 0 to num - 1 do
			FResData[AResType, idx].roomno:= idx;
		AFile.Seek(num, soFromCurrent);
		end
	else
		for idx:= 0 to num - 1 do
			FResData[AResType, idx].roomno:= READ_LE_UINT8(AFile,
					FDetectData.EncByte);

	for idx:= 0 to num - 1 do
		begin
		FResData[AResType, idx].roomoffs:= READ_LE_UINT16(AFile,
				FDetectData.EncByte);

		if  FResData[AResType, idx].roomoffs = $FFFF then
			FResData[AResType, idx].roomoffs:= VAL_SCUMMDC2_RES_INVOFFSET;
		end;
	end;

end.
