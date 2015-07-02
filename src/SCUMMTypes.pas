unit SCUMMTypes;

interface

uses
	TypInfo, Classes;

//The types/consts declared in this unit have been converted from those in
//		SCUMMVM.  Some simplification has been made for the context in which
//		this unit will be used.  The types converted originate in a number of
//		different source files.

//Some nice terms to know about
//	- SPUTM
//		The "real" name for the engine, SCUMM Presentation Utility (TM).
//	- SCUMM
//		The actual scripting language.
//	- IMUSE
//		The MIDI control system, allowing dynamic music.
//	- SMUSH
//		A movie compression format and player.
//	- INSANE
//		The event management system used in V7+ games.
//	- MMUCAS
//		The memory allocation system used in The Curse of Monkey Island. V8 only.


type
//dengland  I need this for handling sets with more than 32 possible elements.
//		It is a slight hack...  It needs to be checked on FPC to be sure that it
//		will work (different endians, too).  System.TypInfo hasn't been updated
//		to handle types larger than 32bit...
	TInt64Set = set of 0..SizeOf(Int64) * 8 - 1;

	TSCUMMCoreVersion = (scvUnk = 0, scv0, scv1, scv2, scv3, scv4, scv5, scv6,
			scv7, scv8);

	TSCUMMCorePlatform = (scpUnk = 0, scpC64, scpAmiga, scpAtariST, scpNES,
			scpAppleIIGS, scpAppleMac, scpPCDOS, scpPCLinux, scpPCWindows,
			scpFMTowns, scpCoCo3, scpAcorn, scpSegaCD, scp3DO, scpPCEngine,
			scpPC98, scpWii, scpPSX, scpCDi, scpIOS, scpOS2, scpBeOS);

//todo dengland Check this is alright.  Why does SCUMMVM use -1 for sclUnk?  It
//		would seriously ruin my day if it was actually necessary.
	TSCUMMCoreLanguage = (sclUnk = 0{-1}, sclZH_CNA, sclZH_TWN, sclHR_HRV,
			sclCZ_CZE, sclNL_NLD, sclEN_ANY, sclEN_GRB, sclEN_USA, sclFR_FRA,
			sclDE_DEU, sclGR_GRE, sclHE_ISR, sclHU_HUN, sclIT_ITA,	sclJA_JPN,
			sclKO_KOR,  sclLV_LAT, sclNB_NOR, sclPL_POL, sclPT_BRA, sclRU_RUS,
			sclES_ESP, sclSE_SWE);

	TSCUMMCoreFeature = (scfDemo, scfNewCostumes, scfEncrypted, scfSmallHeaders,
			scfOldBundles, scf16Colour, scfOld256Colour, scfAudioTracks,
			scfFewLocals, scfHELocalised, scfHE985, scfHighColour,
			scfMacContainers);
	TSCUMMCoreFeatures = set of TSCUMMCoreFeature;

	TSCUMMDecryptKey = Byte;

	TSCUMMCoreGame = (scgUnk = 0, scgCustom, scgManiac, scgZak, scgIndy3,
			scgLoom, scgPass, scgMonkey, scgMonkeyEGA, scgMonkeyVGA, scgMonkey2,
			scgIndy4, scgTentacle, scgSamNMax, scgFT, scgDig, scgCMI,
			scgHEGame, scgHEPuttDemo, scgHEFBear, scgHEPuttMoon, scgHEFunPack,
			scgHEPuttZoo, scgHEFreddi3, scgHEBirthdayRed, scgHEBirthdayYellow,
			scgHETreasureHunt, scgHEPuttRace, scgHEFunShop, scgHEFootball,
			scgHEFootball2002, scgHESoccer, scgHESoccerMLS, scgHESoccer2004,
			scgHEBaseball2001, scgHEBaseball2003, scgHEBasketball,
			scgHEMoonBase, scgHECup);

const
	VAL_SET_SCUMMLUCAS: set of TSCUMMCoreGame = [scgManiac..scgCMI];
	VAL_SET_SCUMMHUMEN: set of TSCUMMCoreGame = [scgHEGame..scgHECup];

//dengland These aren't comprehensive (flexible) enough for our purposes.
//type
//	TSCUMMCoreResource = (scrInvalid = 0, scrRoom, scrScript, scrCostume,
//			scrSound, scrInventory, scrCharset, scrString, scrVerb,
//			scrActorName, scrBuffer, scrScaleTable, scrTemp, scrFlObject,
//			scrMatrix, scrBox, scrObjectName, scrRoomScripts, scrRoomImage,
//			scrImage, scrTalkie, scrSpoolBuffer);
//
//const
//	scrFirst: TSCUMMCoreResource = scrRoom;
//	scrLast: TSCUMMCoreResource = scrSpoolBuffer;

type
	TSCUMMMD5 = array[0..15] of Byte;
	TSCUMMResID = Word;

	TSCUMMMusicType = (
			smtInvalid = -1,	// Invalid output
			smtAuto = 0,		// Auto
			smtNull,			// Null
			smtPCSPK,			// PC Speaker
			smtPCJR,			// PCjr
			smtCMS,				// CMS
			smtADLIB,			// AdLib
			smtC64,				// C64
			smtAmiga,			// Amiga
			smtAppleIIGS,		// Apple IIGS
			smtTOWNS,			// FM-TOWNS
			smtPC98,			// PC98
			smtGM,				// General MIDI
			smtMT32,			// MT-32
			smtGS				// Roland GS
		);

	TSCUMMMusicFlag = (
			smfPCSPK,		// PC Speaker: Maps to smtPCSPK and smtPCJR
			smfCMS,			// Creative Music System/Gameblaster: Maps to smtCMS
			smfPCJR,		// Tandy/PC Junior driver
			smfADLIB,		// AdLib: Maps to smtADLIB
			smfC64,
			smfAmiga,
			smfAppleIIGS,
			smfTOWNS,		// FM-TOWNS: Maps to smtTOWNS
			smfPC98,		// FM-TOWNS: Maps to smtPC98
			smfMIDI,		// Real MIDI
			smfPreferMT32,	// MT-32 output is preferred
			smfPreferGM		// GM output is preferred
		);
	TSCUMMMusicFlags = set of TSCUMMMusicFlag;

//dengland I'm not entirely sure I want these.  They may be helpful when trying
//		to quickly determine whether or not specific data is available or should
//		be used?
//dengland I've had to add the missing elements in because there is no TypeInfo
//		produced for the enumeration if I don't (and that's a real pain).
	TSCUMMGUIOpt = (
// 			This was "SGO_NONE" but with sets it is unnecessary.  Keeping it
//					in order for the enumerated values to align with the SCUMMVM
//					defines.
			sgoNotAnOption00 = $00,

			sgoNoSubtitles = $01,
			sgoNoMusic,
			sgoNoSpeech,
			sgoNoSFX,
			sgoNoMIDI,
			sgoNoLaunchLoad,

			sgoMIDIPCSPK,

//			?? 8-9
			sgoNotAnOption08 = $08,
			sgoNotAnOption09 = $09,

			sgoMIDICMS = $0A,
			sgoMIDIPCJR,
			sgoMIDIADLIB,
			sgoMIDIC64,
			sgoMIDIAmiga,
			sgoMIDIAppleIIGS,
			sgoMIDITOWNS,
			sgoMIDIPC98,

//			?? 18-19
			sgoNotAnOption18 = $12,
			sgoNotAnOption19 = $13,

			sgoMIDIMT32 = $14,
			sgoMIDIGM,

			sgoNoAspect,

//			?? 23-29
			sgoNotAnOption23 = $17,
			sgoNotAnOption24 = $18,
			sgoNotAnOption25 = $19,
			sgoNotAnOption26 = $1A,
			sgoNotAnOption27 = $1B,
			sgoNotAnOption28 = $1C,
			sgoNotAnOption29 = $1D,

			sgoRenderHERCGreen = $1E,
			sgoRenderHERCAmber,
			sgoRenderCGA,
			sgoRenderEGA,
			sgoRenderVGA,
			sgoRenderAmiga,
			sgoRenderFMTOWNS,
			sgoRenderPC9821,

//			?? 38-39
			sgoNotAnOption38 = $26,
			sgoNotAnOption39 = $27,

			sgoRenderPC9801 = $28,

//			Special GUIO flags for the AdvancedDetector's caching of game
//					specific options.
			sgoGameOptions1,
			sgoGameOptions2,
			sgoGameOptions3,
			sgoGameOptions4,
			sgoGameOptions5,
			sgoGameOptions6,
			sgoGameOptions7);
	TSCUMMGUIOpts = set of TSCUMMGUIOpt;

	PSCUMMGameProps = ^TSCUMMGameProps;
	TSCUMMGameProps = record
		name: string;
		variant: string;
		prefTag: string;
		id: TSCUMMCoreGame;
		ver: TSCUMMCoreVersion;
		subv: Byte;
		musicFlgs: TSCUMMMusicFlags;
		feat: TSCUMMCoreFeatures;
		plat: TSCUMMCorePlatform;
		guiOpts: TSCUMMGUIOpts;

		procedure Assign(const ASource: TSCUMMGameProps);
	end;

	TSCUMMFilenameGen = (sfgDiskNum, sfgDiskNumSteam, sfgRoomNum,
			sfgRoomNumSteam, sfgHEMac, sfgHEMacNoParens, sfgHEPC, sfgHEIOS,
			sfgUnchanged);

	TSCUMMFilenamePattern = record
		pattern: string;
		genMethod: TSCUMMFilenameGen;
	end;

	PSCUMMGameFilenamePat = ^TSCUMMGameFilenamePat;
	TSCUMMGameFilenamePat = record
		gameid: string;
		pattern: string;
		genMethod: TSCUMMFilenameGen;
		language: TSCUMMCoreLanguage;
		platfrm: TSCUMMCorePlatform;
		variant: string;

		procedure Assign(const ASource: TSCUMMGameFilenamePat);
	end;

	PSCUMMGameMD5Map = ^TSCUMMGameMD5Map;
	TSCUMMGameMD5Map = record
		md5: TSCUMMMD5;
		gameid: string;
		variant: string;
		extra: string;
		filesize: Integer;
		language: TSCUMMCoreLanguage;
		platfrm: TSCUMMCorePlatform;

		procedure Assign(const ASource: TSCUMMGameMD5Map);
	end;

	TSCUMMDetectorData = record
		fp: TSCUMMFilenamePattern;
		game: TSCUMMGameProps;
		language: TSCUMMCoreLanguage;
		md5: TSCUMMMD5;
		extra: string;
		encByte: Byte;

		procedure Assign(const ASource: TSCUMMDetectorData);
	end;

	TSCUMMDetectorResult = (sdrError = -1, sdrUnk, sdrConclusive,
			sdrInconclusive);

	PSCUMMPluginDesc = ^TSCUMMPluginDesc;
	TSCUMMPluginDesc = record
		major,
		minor: Byte;
		build: Word;
		_type: PWideChar;
		stage: WideChar;
		name: PWideChar;
		desc: PWideChar;
	end;

	TSCUMMPluginQuery = function(const ACoreDesc: PSCUMMPluginDesc;
			APluginDesc: PSCUMMPluginDesc): Boolean; stdcall;
	TSCUMMPluginInit = function: Boolean; stdcall;
	TSCUMMPluginPrep = procedure; stdcall;
	TSCUMMPluginRels = procedure; stdcall;
	TSCUMMPluginFinl = procedure; stdcall;


function  SCUMMComputeStreamMD5(AStream: TStream; var AMD5: TSCUMMMD5;
		const AMaxLen: Cardinal): Boolean;

function  SCUMMMD5ToString(const AMD5: TSCUMMMD5): AnsiString;
function  SCUMMStringToMD5(const AString: AnsiString): TSCUMMMD5;

//dengland  I need these Int64 versions for handling sets with more than 32
//		possible elements.  It is a slight hack...  It needs to be checked on
//		FPC to be sure that it will work (different endians, too).
//		System.TypInfo hasn't been updated for types larger than 32bit!
function SetToString64(EnumInfo: PTypeInfo; Value: TInt64Set;
		Brackets: Boolean = False): AnsiString; overload;
function StringToSet64(EnumInfo: PTypeInfo; const Value: AnsiString): Int64; overload;


implementation

uses
	SysUtils, AnsiStrings;


{$I SCUMMEndianPointerWrapper.inc}


type
	TSCUMMMD5Context = record
	private
		FTotal: array[0..1] of Cardinal;
		FState: array[0..3] of Cardinal;
		FBuffer: array[0..63] of Byte;

		procedure Process(ABuf: PByte);

	public
		procedure Start;
		procedure Update(ABuf: PByte; ACount: Cardinal);
		procedure Finish(var AMD5: TSCUMMMD5);
	end;

{ TSCUMMMD5Context }

const
	ARR_VAL_SCUMMMD5_PADDING: array[0..63] of Byte = (
			$80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

procedure TSCUMMMD5Context.Finish(var AMD5: TSCUMMMD5);
	var
	last,
	padn: Cardinal;
	high,
	low: Cardinal;
	msglen: array[0..7] of Byte;

	begin
	high:= (FTotal[0] shr 29) or (FTotal[1] shl 3);
	low:= FTotal[0] shl  3;

	PUT_UINT32(low,  @msglen[0], 0);
	PUT_UINT32(high, @msglen[0], 4);

	last:= FTotal[0] and $3F;
	if  last < 56 then
		padn:= 56 - last
	else
		padn:= 120 - last;

	Update(@ARR_VAL_SCUMMMD5_PADDING[0], padn);
	Update(@msglen[0], 8);

	PUT_UINT32(FState[0], @AMD5[0],  0);
	PUT_UINT32(FState[1], @AMD5[0],  4);
	PUT_UINT32(FState[2], @AMD5[0],  8);
	PUT_UINT32(FState[3], @AMD5[0], 12);
	end;

//This is pretty nasty.  I hope there are no artefacts from the conversion and
//		that I haven't otherwise broken the logic.  For some reason the F
//		routines don't match my pascal reference.  I guess someone has
//		determined that these versions are likely to be better??
procedure TSCUMMMD5Context.Process(ABuf: PByte);
//define S(x, n) ((x << n) | ((x & 0xFFFFFFFF) >> (32 - n)))
	function S(AX: Cardinal; AN: Byte): Cardinal; inline;
		begin
		Result:= ((AX shl AN) or ((AX and $FFFFFFFF) shr (32 - AN)));
		end;

//define P(a, b, c, d, k, s, t)
//	{
//		a += F(b,c,d) + X[k] + t; a = S(a,s) + b;
//	}

//define F(x, y, z) (z ^ (x & (y ^ z)))
	procedure P1(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AD xor (AB and (AC xor AD))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (y ^ (z & (x ^ y)))
	procedure P2(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AC xor (AD and (AB xor AC))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (x ^ y ^ z)
	procedure P3(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AB xor AC xor AD) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

//define F(x, y, z) (y ^ (x | ~z))
	procedure P4(var AA: Cardinal; AB, AC, AD: Cardinal; AX: Cardinal; AN: Byte;
			AT: Cardinal); inline;
		begin
		Inc(AA, (AC xor (AB or (not AD))) + AX + AT);
		AA:= S(AA, AN) + AB;
		end;

	var
	X: array[0..15] of Cardinal;
	A,
	B,
	C,
	D: Cardinal;

	begin
	GET_UINT32(X[0],  ABuf,  0);
	GET_UINT32(X[1],  ABuf,  4);
	GET_UINT32(X[2],  ABuf,  8);
	GET_UINT32(X[3],  ABuf, 12);
	GET_UINT32(X[4],  ABuf, 16);
	GET_UINT32(X[5],  ABuf, 20);
	GET_UINT32(X[6],  ABuf, 24);
	GET_UINT32(X[7],  ABuf, 28);
	GET_UINT32(X[8],  ABuf, 32);
	GET_UINT32(X[9],  ABuf, 36);
	GET_UINT32(X[10], ABuf, 40);
	GET_UINT32(X[11], ABuf, 44);
	GET_UINT32(X[12], ABuf, 48);
	GET_UINT32(X[13], ABuf, 52);
	GET_UINT32(X[14], ABuf, 56);
	GET_UINT32(X[15], ABuf, 60);

	A:= FState[0];
	B:= FState[1];
	C:= FState[2];
	D:= FState[3];

	P1(A, B, C, D, X[  0],  7, $D76AA478);
	P1(D, A, B, C, X[  1], 12, $E8C7B756);
	P1(C, D, A, B, X[  2], 17, $242070DB);
	P1(B, C, D, A, X[  3], 22, $C1BDCEEE);
	P1(A, B, C, D, X[  4],  7, $F57C0FAF);
	P1(D, A, B, C, X[  5], 12, $4787C62A);
	P1(C, D, A, B, X[  6], 17, $A8304613);
	P1(B, C, D, A, X[  7], 22, $FD469501);
	P1(A, B, C, D, X[  8],  7, $698098D8);
	P1(D, A, B, C, X[  9], 12, $8B44F7AF);
	P1(C, D, A, B, X[ 10], 17, $FFFF5BB1);
	P1(B, C, D, A, X[ 11], 22, $895CD7BE);
	P1(A, B, C, D, X[ 12],  7, $6B901122);
	P1(D, A, B, C, X[ 13], 12, $FD987193);
	P1(C, D, A, B, X[ 14], 17, $A679438E);
	P1(B, C, D, A, X[ 15], 22, $49B40821);

	P2(A, B, C, D, X[  1],  5, $F61E2562);
	P2(D, A, B, C, X[  6],  9, $C040B340);
	P2(C, D, A, B, X[ 11], 14, $265E5A51);
	P2(B, C, D, A, X[  0], 20, $E9B6C7AA);
	P2(A, B, C, D, X[  5],  5, $D62F105D);
	P2(D, A, B, C, X[ 10],  9, $02441453);
	P2(C, D, A, B, X[ 15], 14, $D8A1E681);
	P2(B, C, D, A, X[  4], 20, $E7D3FBC8);
	P2(A, B, C, D, X[  9],  5, $21E1CDE6);
	P2(D, A, B, C, X[ 14],  9, $C33707D6);
	P2(C, D, A, B, X[  3], 14, $F4D50D87);
	P2(B, C, D, A, X[  8], 20, $455A14ED);
	P2(A, B, C, D, X[ 13],  5, $A9E3E905);
	P2(D, A, B, C, X[  2],  9, $FCEFA3F8);
	P2(C, D, A, B, X[  7], 14, $676F02D9);
	P2(B, C, D, A, X[ 12], 20, $8D2A4C8A);

	P3(A, B, C, D, X[  5],  4, $FFFA3942);
	P3(D, A, B, C, X[  8], 11, $8771F681);
	P3(C, D, A, B, X[ 11], 16, $6D9D6122);
	P3(B, C, D, A, X[ 14], 23, $FDE5380C);
	P3(A, B, C, D, X[  1],  4, $A4BEEA44);
	P3(D, A, B, C, X[  4], 11, $4BDECFA9);
	P3(C, D, A, B, X[  7], 16, $F6BB4B60);
	P3(B, C, D, A, X[ 10], 23, $BEBFBC70);
	P3(A, B, C, D, X[ 13],  4, $289B7EC6);
	P3(D, A, B, C, X[  0], 11, $EAA127FA);
	P3(C, D, A, B, X[  3], 16, $D4EF3085);
	P3(B, C, D, A, X[  6], 23, $04881D05);
	P3(A, B, C, D, X[  9],  4, $D9D4D039);
	P3(D, A, B, C, X[ 12], 11, $E6DB99E5);
	P3(C, D, A, B, X[ 15], 16, $1FA27CF8);
	P3(B, C, D, A, X[  2], 23, $C4AC5665);

	P4(A, B, C, D, X[  0],  6, $F4292244);
	P4(D, A, B, C, X[  7], 10, $432AFF97);
	P4(C, D, A, B, X[ 14], 15, $AB9423A7);
	P4(B, C, D, A, X[  5], 21, $FC93A039);
	P4(A, B, C, D, X[ 12],  6, $655B59C3);
	P4(D, A, B, C, X[  3], 10, $8F0CCC92);
	P4(C, D, A, B, X[ 10], 15, $FFEFF47D);
	P4(B, C, D, A, X[  1], 21, $85845DD1);
	P4(A, B, C, D, X[  8],  6, $6FA87E4F);
	P4(D, A, B, C, X[ 15], 10, $FE2CE6E0);
	P4(C, D, A, B, X[  6], 15, $A3014314);
	P4(B, C, D, A, X[ 13], 21, $4E0811A1);
	P4(A, B, C, D, X[  4],  6, $F7537E82);
	P4(D, A, B, C, X[ 11], 10, $BD3AF235);
	P4(C, D, A, B, X[  2], 15, $2AD7D2BB);
	P4(B, C, D, A, X[  9], 21, $EB86D391);

	Inc(FState[0], A);
	Inc(FState[1], B);
	Inc(FState[2], C);
	Inc(FState[3], D);
	end;

procedure TSCUMMMD5Context.Start;
	begin
	FTotal[0]:= 0;
	FTotal[1]:= 0;

	FState[0]:= $67452301;
	FState[1]:= $EFCDAB89;
	FState[2]:= $98BADCFE;
	FState[3]:= $10325476;
	end;

procedure TSCUMMMD5Context.Update(ABuf: PByte; ACount: Cardinal);
	var
	left,
	fill: Cardinal;
	len: Cardinal;
	input: PByte;

	begin
	len:= ACount;
	input:= ABuf;

	if  len = 0 then
		Exit;

	left:= FTotal[0] and $3F;
	fill:= 64 - left;

	Inc(FTotal[0], len);
	FTotal[0]:= FTotal[0] and $FFFFFFFF;

	if  FTotal[0] < len then
		Inc(FTotal[1]);

	if  (left <> 0) and (len >= fill) then
		begin
//		memcpy((void *)(ctx->buffer + left), (const void *)input, fill);
		Move(input^, FBuffer[left], fill);

		Process(@FBuffer[0]);
		Dec(len, fill);
		Inc(input, fill);
		left:= 0;
		end;

	while len >= 64 do
		begin
		Process(input);
		Dec(len, 64);
		Inc(input, 64);
		end;

	if  len > 0 then
//		memcpy((void *)(ctx->buffer + left), (const void *)input, length);
		Move(input^, FBuffer[left], len);
	end;

function  SCUMMComputeStreamMD5(AStream: TStream; var AMD5: TSCUMMMD5;
		const AMaxLen: Cardinal): Boolean;
	var
	ctx: TSCUMMMD5Context;
	i: Integer;
	buf: array[0..999] of Byte;
	restricted: Boolean;
	readlen: Cardinal;
	len: Cardinal;

	begin
	len:= AMaxLen;
	FillChar(AMD5, SizeOf(AMD5), 0);

	restricted:= (len <> 0);

	if  (not restricted) or (SizeOf(buf) <= len) then
		readlen:= SizeOf(buf)
	else
		readlen:= len;

	ctx.Start;

	i:= AStream.Read(buf, readlen);
	while i > 0 do
		begin
		ctx.Update(@buf[0], i);

		if  restricted then
			begin
			Dec(len, i);
			if  len = 0 then
				Break;

			if  SizeOf(buf) > len then
				readlen:= len;
			end;

		i:= AStream.Read(buf, readlen);
		end;

	ctx.Finish(AMD5);
	Result:= True;
	end;

function  SCUMMMD5ToString(const AMD5: TSCUMMMD5): AnsiString;
	var
	i: Integer;

	begin
	Result:= '';
	for i:= 0 to 15 do
		Result:= Result + AnsiStrings.Format('%2.2x', [AMD5[i]]);
	end;

function  SCUMMStringToMD5(const AString: AnsiString): TSCUMMMD5;
	const
	SET_VAL_ANSI_DIGIT = [#$30..#$39];
	SET_VAL_ANSI_UPPER = [#$41..#$46];
	SET_VAL_ANSI_LOWER = [#$61..#$66];

	var
	i: Integer;
	c: AnsiChar;
	h, l: Byte;

	begin
	Assert(Length(AString) >= 32, 'MD5 string length must be 32');

	h:= $00;
	for i:= 1 to 32 do
		begin
		c:= AString[i];
		if  c in SET_VAL_ANSI_LOWER then
			c:= AnsiChar(Byte(c) - $20);

		Assert((c in SET_VAL_ANSI_DIGIT) or (c in SET_VAL_ANSI_UPPER),
				'MD5 string must contain only characters 0..9 and A..F');

		if  c in SET_VAL_ANSI_DIGIT then
			l:= Byte(c) - $30
		else
			l:= Byte(c) - $37;

		if  (i mod 2) = 0 then
			Result[i shr 1 - 1]:= h + l
		else
			h:= l shl 4;
		end;
	end;


function SetToString64(EnumInfo: PTypeInfo; Value: TInt64Set;
		Brackets: Boolean): AnsiString; overload;
	var
//	S: TInt64Set;
	I: Integer;

	begin
	Result := '';
//	Int64(S):= Value;
//	TypeInfo:= GetTypeData(TypeInfo)^.CompType^;
	for I:= 0 to SizeOf(Int64) * 8 - 1 do
		if  I in Value then
			begin
			if  Result <> '' then
				Result := Result + ',';

			Result:= Result + AnsiString(GetEnumName(EnumInfo, I));
			end;

	if  Brackets then
		Result:= '[' + Result + ']';
	end;

function StringToSet64(EnumInfo: PTypeInfo;
		const Value: AnsiString): Int64; overload;
	var
	P: PAnsiChar;
	EnumName: AnsiString;
	EnumValue: NativeInt;
//	EnumInfo: PTypeInfo;

//	Grab the next enum name
	function NextWord(var P: PAnsiChar): AnsiString;
		var
		i: Integer;

		begin
		i:= 0;

//		scan til whitespace
		while not (P[i] in [',', ' ', #0,']']) do
			Inc(i);

		SetString(Result, P, i);

//		skip whitespace
		while (P[i] in [',', ' ',']']) do
			Inc(i);

		Inc(P, i);
		end;

	begin
	Result:= 0;
	if  Value = '' then
		Exit;
	P:= PAnsiChar(Value);

//	skip leading bracket and whitespace
	while (P^ in ['[',' ']) do
		Inc(P);

//	EnumInfo:= GetTypeData(TypeInfo)^.CompType^;
	EnumName:= NextWord(P);
	while EnumName <> '' do
		begin
		EnumValue:= GetEnumValue(EnumInfo, string(EnumName));
		if  EnumValue < 0 then
			raise EPropertyConvertError.CreateFmt(
					'Invalid property element: %s', [EnumName]);
		Include(TInt64Set(Result), EnumValue);
		EnumName:= NextWord(P);
		end;
	end;


{ TSCUMMGameFilenamePat }

procedure TSCUMMGameFilenamePat.Assign(const ASource: TSCUMMGameFilenamePat);
	begin
	gameid:= ASource.gameid;
	UniqueString(gameid);

	pattern:= ASource.pattern;
	UniqueString(pattern);

	genMethod:= ASource.genMethod;
	language:= ASource.language;
	platfrm:= ASource.platfrm;

	variant:= ASource.variant;
	UniqueString(variant);
	end;

{ TSCUMMGameMD5Map }

procedure TSCUMMGameMD5Map.Assign(const ASource: TSCUMMGameMD5Map);
	begin
	Move(ASource.md5, md5, SizeOf(TSCUMMMD5));

	gameid:= ASource.gameid;
	UniqueString(gameid);

	variant:= ASource.variant;
	UniqueString(variant);

	extra:= ASource.extra;
	UniqueString(extra);

	filesize:= ASource.filesize;
	language:= ASource.language;
	platfrm:= ASource.platfrm;
	end;

{ TSCUMMGameProps }

procedure TSCUMMGameProps.Assign(const ASource: TSCUMMGameProps);
	begin
	name:= ASource.name;
	UniqueString(name);

	variant:= ASource.variant;
	UniqueString(variant);

	prefTag:= ASource.prefTag;
	UniqueString(prefTag);

	id:= ASource.id;
	ver:= ASource.ver;
	subv:= ASource.subv;
	musicFlgs:= ASource.musicFlgs;
	feat:= ASource.feat;
	plat:= ASource.plat;

	guiOpts:= ASource.guiOpts;
	end;

{ TSCUMMDetectorData }

procedure TSCUMMDetectorData.Assign(const ASource: TSCUMMDetectorData);
	begin
	fp:= ASource.fp;
	game.Assign(ASource.game);
	language:= ASource.language;

	Move(ASource.md5[0], md5[0], SizeOf(TSCUMMMD5));

	extra:= ASource.extra;
	UniqueString(extra);

	encByte:= ASource.encByte;
	end;

end.
