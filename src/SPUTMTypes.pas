unit SPUTMTypes;

interface

uses
	Classes, VCL.Imaging.PNGImage, SCUMMTypes;

type
	TSPUTMObjectInfo = record
		owner,
		state: Byte;
	end;

	TSPUTMObjectInfoArr = array of TSPUTMObjectInfo;

	TSPUTMResType = (srtInvalid = 0, srtRoom = 1,
			srtScript = 2, srtCostume = 3, srtSound = 4, srtInventory = 5,
			srtCharset = 6, srtString = 7, srtVerb = 8, srtActorName = 9,
			srtBuffer = 10, srtScaleTable = 11, srtTemp = 12, srtFlObject = 13,
			srtMatrix = 14, srtBox = 15, srtObjectName = 16,
			srtRoomScripts = 17, srtRoomImage = 18, srtImage = 19,
			srtTalkie = 20, srtSpoolBuffer = 21);

const
	srtFirst: TSPUTMResType = srtRoom;
	srtLast: TSPUTMResType = srtSpoolBuffer;

type
	TSPUTMResInfo = record
		roomNo: Byte;
		roomOffs: Cardinal;
	end;

	TSPUTMResInfoArr = array of TSPUTMResInfo;

	TSPUTMResData = array[TSPUTMResType] of TSPUTMResInfoArr;

	TSPUTMGlobVarName = (
//			V0 to V5
			sgvKEYPRESS,
			sgvSYNC,
			sgvEGO,
			sgvCameraPosX,
			sgvHaveMsg,
			sgvROOM,
			sgvOVERRIDE,
			sgvMachineSpeed,
			sgvME,
			sgvNumActor,
			sgvCurrentLights,
			sgvCURRENTDRIVE,
			sgvCURRENTDISK,
			sgvTmr1,
			sgvTmr2,
			sgvTmr3,
			sgvMusicTimer,
			sgvActorRangeMin,
			sgvActorRangeMax,
			sgvCameraMinX,
			sgvCameraMaxX,
			sgvTimerNext,
			sgvVirtMouseX,
			sgvVirtMouseY,
			sgvRoomResource,
			sgvLastSound,
			sgvCutSceneExitKey,
			sgvOptionsKey,
			sgvTalkActor,
			sgvCameraFastX,
			sgvScrollScript,
			sgvEntryScript,
			sgvEntryScript2,
			sgvExitScript,
			sgvExitScript2,
			sgvVerbScript,
			sgvSentenceScript,
			sgvInventoryScript,
			sgvCutSceneStartScript,
			sgvCutSceneEndScript,
			sgvCharInc,
			sgvWalkToObj,
			sgvDEBUGMODE,
			sgvHEAPSPACE,
			sgvRestartKey,
			sgvPauseKey,
			sgvMouseX,
			sgvMouseY,
			sgvTIMER,
			sgvTimerTotal,
			sgvSoundCard,
			sgvVideoMode,
			sgvMainMenuKey,
			sgvFIXEDDISK,
			sgvCURSORSTATE,
			sgvUSERPUT,
			sgvSOUNDRESULT,
			sgvTalkStopKey,
			sgvFadeDelay,
			sgvNOSUBTITLES,

//			V5+
			sgvSOUNDPARAM,
			sgvSOUNDPARAM2,
			sgvSOUNDPARAM3,
			sgvINPUTMODE,
			sgvMEMORY_PERFORMANCE,
			sgvVIDEO_PERFORMANCE,
			sgvROOM_FLAG,
			sgvGAME_LOADED,
			sgvNEW_ROOM,

//			V4/V5
			sgvV5_TALK_STRING_Y,

//			V6+
			sgvROOM_WIDTH,
			sgvROOM_HEIGHT,
			sgvSUBTITLES,
			sgvV6_EMSSPACE,

//			V7/V8 specific variables
			sgvCAMERA_POS_Y,
			sgvCAMERA_MIN_Y,
			sgvCAMERA_MAX_Y,
			sgvCAMERA_THRESHOLD_X,
			sgvCAMERA_THRESHOLD_Y,
			sgvCAMERA_SPEED_X,
			sgvCAMERA_SPEED_Y,
			sgvCAMERA_ACCEL_X,
			sgvCAMERA_ACCEL_Y,
			sgvCAMERA_DEST_X,
			sgvCAMERA_DEST_Y,
			sgvCAMERA_FOLLOWED_ACTOR,

//			V7/V8 specific variables
			sgvVERSION_KEY,
			sgvDEFAULT_TALK_DELAY,
			sgvCUSTOMSCALETABLE,
			sgvBLAST_ABOVE_TEXT,
			sgvVOICE_MODE,
			sgvMUSIC_BUNDLE_LOADED,
			sgvVOICE_BUNDLE_LOADED,

//			V7/V8
			sgvLEFTBTN_DOWN,
			sgvRIGHTBTN_DOWN,

//			V6/V72HE/V7/V8
			sgvLEFTBTN_HOLD,
			sgvRIGHTBTN_HOLD,

//			V6/V7 (not HE)
			sgvSAVELOAD_SCRIPT,
			sgvSAVELOAD_SCRIPT2,

//			V6/V7 specific variables (FT & Sam & Max specific)
			sgvCHARSET_MASK,

//			V6 specific variables
			sgvV6_SOUNDMODE,

//			V1/V2 specific variables
			sgvCHARCOUNT,
			sgvVERB_ALLOWED,
			sgvACTIVE_VERB,
			sgvACTIVE_OBJECT1,
			sgvACTIVE_OBJECT2,

//			HE specific variables
// 			Used in setActorRedrawFlags()
			sgvREDRAW_ALL_ACTORS,
//			Used in setActorCostume()
			sgvSKIP_RESET_TALK_ACTOR,
//			Used in o_startSound()
			sgvSOUND_CHANNEL,
//			Used in startHETalkSound()
			sgvTALK_CHANNEL,
//			Used in processSoundCode()
			sgvSOUNDCODE_TMR,
//			Used in findFreeSoundChannel()
			sgvRESERVED_SOUND_CHANNELS,

//			Used in scummLoop()
			sgvMAIN_SCRIPT,

//			Used in runScript()/runObjectScript()
			sgvSCRIPT_CYCLE,
//			Used in runAllScripts()
			sgvNUM_SCRIPT_CYCLES,

//			Exists both in V7 and in V72HE:
			sgvNUM_GLOBAL_OBJS);

	TSPUTMGlobVarSlots = array[TSPUTMGlobVarName] of Byte;

	TSPUTMGlobVarMap = record
		varName: TSPUTMGlobVarName;
		slot: Byte;
	end;

	TSPUTMGlobVarMapping = array of TSPUTMGlobVarMap;

//In here we need to define a class for doing the enumeration of the game
//		resources providing the information as a virtual file system.  It should
//		be called something like TSCUMMExpEnumerator.  It needs to provide the
//		following file system outline:
//
//			*Root n*							- Game root node
//			+-!Rooms directory 					- A container node for the rooms
//			|  +-*Room n directory* 			- A node for each room
//			|  |  +-!Scripts directory			- Local scripts directory
//			|  |  |  +-*Room scripts*
//			|  |  +-!Boxes directory
//			|  |  |  +-*Box n data*
//			|  |  |  +--Box matrix data
//			|  |  +-!ZPlanes directory
//			|  |  |  +-*ZPlane n mask*
//			|  |  +--Room n OnEnter script
//			|  |  +--Room n OnExit script
//			|  |  +--Room n scale data
//			|  |  +--Room n background
//			|  |  +--Room n properties
//			+-!Objects directory				- Objects container node
//			|  +-*Object n directory*
//			|  |  +-*Object verb scripts*
//			|  |  +--Object background
//			|  |  +--Object image
//			|  |  +--Object properties
//			+-!Scripts directory				- A node for the global scripts
//			|  +-*Script n*
//			+-!Sounds directory					- Container
//			|  +-*Sound n directories*
//			|  |  +--Sound n data
//			|  |  +--Sound n properties
//			+-!Costumes directory
//			|  +-*Costume n directories*
//			|  |  +-!Animations directory
//			|  |  |  +-*Animation n directory*
//			|  |  |  |  +-*Animation n sequences*
//			|  |  |  |  +--Animation n properties
//			|  |  |  +--Animation activities table
//			|  |  |  +--Command sequence table
//			|  |  +-!Frames directory
//			|  |  |  +-*Frame n directory*
//			|  |  |  |  +--Frame n image
//			|  |  |  |  +--Frame n properties
//			|  |  +-!Limbs directory
//			|  |  |  +-*Limbs n properties*
//			|  |  |  +--Limb frame command table
//			|  |  +--Costume palette map
//			|  |  +--Costume n properties
//			+-!Palettes directory
//			|  +-*Palette n properties*
//			+-!Charsets directory
//			|  +-*Charset n map*
//			+--Game properties

//dengland Many of the above "directories" are actually represented in the data
//		files as "child blocks" of the room files.  I'm flattening the whole
//		structure a bit because there are variations across the SCUMM versions
//		(bundles, disks, separate sound and font files...) and because it is a
//		more logical and accessible presentation of the game data this way.  The
//		various property "files" will contain a reference to the room container
//		in order to properly link them together.  The names of the variant files
//		may also reflect this connection?

//dengland I'm going to use a URI notation for representing the node and data
//		structure.  It is really for descriptive represenation only.  I may at
//		some point enable some kind of file system relection and enumeration
//		via this notation?  In any case, it is really very handy for use as a
//		descriptive tool.  At the moment, the "game" element should be the
//		string name for the game type and "data" should be "inst=<n>" to
//		identify a specific instance of the game in the configured host nodes or
//		"path='...'" for something more specific?  I'm not sure I should allow
//		that, though.

//NB:	scumm:<game,data>: will give us the //<root*> ID for internal path.

//		Static container nodes
//			scumm:<game;data>://Rooms/
//			scumm:<game;data>://Rooms/<room*>/Scripts/
//			scumm:<game;data>://Rooms/<room*>/Boxes/
//			scumm:<game;data>://Rooms/<room*>/ZPlanes/
//			scumm:<game;data>://Objects/
//			scumm:<game;data>://Scripts/
//			scumm:<game;data>://Sounds/
//			scumm:<game;data>://Costumes/
//			scumm:<game;data>://Costumes/<costume*>/Animations/
//			scumm:<game;data>://Costumes/<costume*>/Frames/
//			scumm:<game;data>://Costumes/<costume*>/Limbs/
//			scumm:<game;data>://Palettes/
//			scumm:<game;data>://Charsets/
//
//		Variable container nodes
//			scumm:<game;data>://
//			scumm:<game;data>://Rooms/<room*>/
//			scumm:<game;data>://Objects/<object*>/
//			scumm:<game;data>://Sounds/<sound*>/
//			scumm:<game;data>://Costumes/<costume*>/
//			scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/
//			scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/
//
//		Static data nodes
//			scumm:<game;data>://Rooms/<room*>/Boxes/<box matrix>
//			scumm:<game;data>://Rooms/<room*>/<room OnEnter script>
//			scumm:<game;data>://Rooms/<room*>/<room OnExit script>
//			scumm:<game;data>://Rooms/<room*>/<room scale data>
//			scumm:<game;data>://Rooms/<room*>/<room background>
//			scumm:<game;data>://Rooms/<room*>/<room props>
//			scumm:<game;data>://Objects/<objects*>/<object background>
//			scumm:<game;data>://Objects/<objects*>/<object image>
//			scumm:<game;data>://Objects/<objects*>/<object props>
//			scumm:<game;data>://Sounds/<sound*>/<sound data>
//			scumm:<game;data>://Sounds/<sound*>/<sound props>
//			scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/<anim props>
//			scumm:<game;data>://Costumes/<costume*>/Animations/<animactivity table>
//			scumm:<game;data>://Costumes/<costume*>/Animations/<cmdsequence table>
//			scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/<frame image>
//			scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/<frame props>
//			scumm:<game;data>://Costumes/<costume*>/Limbs/<limbframecmd table>
//			scumm:<game;data>://Costumes/<costume*>/<costume palette map>
//			scumm:<game;data>://Costumes/<costume*>/<costume props>
//			scumm:<game;data>://<game props>
//
//		Variable data nodes
//			scumm:<game;data>://Rooms/<room*>/Scripts/<room script*>
//			scumm:<game;data>://Rooms/<room*>/Boxes/<box data*>
//			scumm:<game;data>://Rooms/<room*>/ZPlanes/<zplane mask*>
//			scumm:<game;data>://Objects/<objects*>/<object verb script*>
//			scumm:<game;data>://Scripts/<script*>
//			scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/<anim sequence*>
//			scumm:<game;data>://Costumes/<costume*>/Limbs/<limbs props*>
//			scumm:<game;data>://Palettes/<palette props*>
//			scumm:<game;data>://Charsets/<charset map*>
//
//		Required data file types:
//			xml			.xml
//			image		.png
//			script		.scp?


//	We'll also need a class for caching the data tree.


	TSCUMMExpEnumType = (
			sxeUnknown,				//Not known

			sxeRootCntnr,			//scumm:<game;data>://
			sxeRoomsCntnr,			//scumm:<game;data>://Rooms/
			sxeRoomCntnr,			//scumm:<game;data>://Rooms/<room*>/
			sxeRoomScrpCntnr,		//scumm:<game;data>://Rooms/<room*>/Scripts/
			sxeRoomBoxesCntnr,		//scumm:<game;data>://Rooms/<room*>/Boxes/
			sxeRoomZPlanesCntnr,	//scumm:<game;data>://Rooms/<room*>/ZPlanes/
			sxeObjectsCntnr,		//scumm:<game;data>://Objects/
			sxeObjectCntnr,			//scumm:<game;data>://Objects/<object*>/
			sxeScriptsCntnr,		//scumm:<game;data>://Scripts/
			sxeSoundsCntnr,			//scumm:<game;data>://Sounds/
			sxeSoundCntnr,			//scumm:<game;data>://Sounds/<sound*>/
			sxeCostumesCntnr,		//scumm:<game;data>://Costumes/
			sxeCostumeCntnr,		//scumm:<game;data>://Costumes/<costume*>/
			sxeCostumeAnimsCntnr,	//scumm:<game;data>://Costumes/<costume*>/Animations/
			sxeCostumeAnimCntnr,	//scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/
			sxeCostumeFramesCntnr,	//scumm:<game;data>://Costumes/<costume*>/Frames/
			sxeCostumeFrameCntnr,	//scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/
			sxeCostumeLimbsCntnr,	//scumm:<game;data>://Costumes/<costume*>/Limbs/
			sxePalettesCntnr,		//scumm:<game;data>://Palettes/
			sxeCharsetsCntnr,		//scumm:<game;data>://Charsets/

			sxeRoomScript,          //scumm:<game;data>://Rooms/<room*>/Scripts/<room script*>
			sxeRoomBoxMatrix,		//scumm:<game;data>://Rooms/<room*>/Boxes/<box matrix>
			sxeRoomBoxData,			//scumm:<game;data>://Rooms/<room*>/Boxes/<box data*>
			sxeRoomZPlaneMask,		//scumm:<game;data>://Rooms/<room*>/ZPlanes/<zplane mask*>
			sxeRoomOnEnter,			//scumm:<game;data>://Rooms/<room*>/<room OnEnter script>
			sxeRoomOnExit,			//scumm:<game;data>://Rooms/<room*>/<room OnExit script>
			sxeRoomScaleData,		//scumm:<game;data>://Rooms/<room*>/<room scale data>
			sxeRoomBackground,		//scumm:<game;data>://Rooms/<room*>/<room background>
			sxeRoomProps,			//scumm:<game;data>://Rooms/<room*>/<room props>
			sxeObjectVerbScript,	//scumm:<game;data>://Objects/<objects*>/<object verb script*>
			sxeObjectBackground,	//scumm:<game;data>://Objects/<objects*>/<object background>
			sxeObjectImage,			//scumm:<game;data>://Objects/<objects*>/<object image>
			sxeObjectProps,			//scumm:<game;data>://Objects/<objects*>/<object props>
			sxeScript,				//scumm:<game;data>://Scripts/<script*>
			sxeSoundData,			//scumm:<game;data>://Sounds/<sound*>/<sound data>
			sxeSoundProps,			//scumm:<game;data>://Sounds/<sound*>/<sound props>
			sxeAnimSequence,		//scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/<anim sequence*>
			sxeAnimActsTbl,			//scumm:<game;data>://Costumes/<costume*>/Animations/<animactivity table>
			sxeAnimCmdSeqTbl,		//scumm:<game;data>://Costumes/<costume*>/Animations/<cmdsequence table>
			sxeAnimProps,			//scumm:<game;data>://Costumes/<costume*>/Animations/<animation*>/<anim props>
			sxeFrameImage,			//scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/<frame image>
			sxeFrameProps,			//scumm:<game;data>://Costumes/<costume*>/Frames/<frame*>/<frame props>
			sxeLimbFrameCmdTbl,		//scumm:<game;data>://Costumes/<costume*>/Limbs/<limbframecmd table>
			sxeLimbProps,			//scumm:<game;data>://Costumes/<costume*>/Limbs/<limbs props*>
			sxeCostumePaletteMap,	//scumm:<game;data>://Costumes/<costume*>/<costume palette map>
			sxeCostumeProps,		//scumm:<game;data>://Costumes/<costume*>/<costume props>
			sxePaletteProps,		//scumm:<game;data>://Palettes/<palette props*>
			sxeCharsetMap,			//scumm:<game;data>://Charsets/<charset map*>
//dengland  I may need more here for handling .nut type charsets that can have
//		animations and palette mapping.
			sxeGameProps			//scumm:<game;data>://<game props>
			);

	TSCUMMExpDataType = (sxdInvalid, sxdRoot, sxdNode, sxdRaw, sxdXML, sxdImage,
			sxdScript);

	TSCUMMExpGlobId = TGUID;
	TSCUMMExpGlobIdArr = array of TSCUMMExpGlobId;

	TSCUMMExpGlobIds = record
		gameId,
		nodeId: TSCUMMExpGlobId;
	end;

	TSCUMMExpNodeInfo = record
		id: TSCUMMExpGlobId;
		name: string;
		enumType: TSCUMMExpEnumType;
		dataType: TSCUMMExpDataType;
	end;

	TSCUMMExpNodeInfoArr = array of TSCUMMExpNodeInfo;


	TSPUTMStats = record
//		Maximums values
		numVariables,
		numBitVariables,
		numLocalObjects,
		numGlobalObjects,
		numArray,
		numVerbs,
		numFlObject,
		numInventory,
		numNewNames,
		numGlobalScripts,
		numRoomVariables,
		numPalettes,
		numSprites,
		numTalkies,
		numUnk,
		HEHeapSize: Integer;

//fixme dengland In TSPUTMStats, these come from the file handling class in
//		SCUMMVM.  I wonder if they can be collapsed with the above?  They should
//		be populated from the index file or constant data for old versions?
		_numGlobalObjects,
		_numRooms,
		_numCostumes,
		_numScripts,
		_numSounds,
		_numCharsets,
		_numActors: Integer;
	end;

    PSCUMMExpObjData = ^TSCUMMExpObjData;
	TSCUMMExpObjData = record
		id: TSCUMMExpGlobId;
		name: string;
		index: Integer;
		enumType: TSCUMMExpEnumType;
		decoded: Boolean;
//todo dengland Confirm that we want to do this via a variant record instead of
//		generic pointer and casting.
		case dataType: TSCUMMExpDataType of
			sxdInvalid: (
				);
			sxdRoot: (
				gameStats: TSPUTMStats);
			sxdNode:(
				nodeData: Pointer);
			sxdRaw: (
				rawData: Pointer);
			sxdXML: (
				xmlData: TStringStream);
			sxdImage: (
				imageData: TPNGImage);
			sxdScript: (
				scriptData: TStringStream);
		end;

	TSPUTMPalette = array[0..255] of Byte;

	TSCUMMExpObjPath = record
{$IFOPT D+}
	private
		FNS: string;
{$ENDIF}

	public
		Root: TSCUMMExpGlobId;
		Nodes: TSCUMMExpGlobIdArr;
		Obj: TSCUMMExpGlobId;

		procedure Initialise;

		function  FromString(const AValue: string): Boolean;
		function  ToString: string;

		function  FromArray(const AArray: TSCUMMExpGlobIdArr): Boolean;
		function  ToArray: TSCUMMExpGlobIdArr;
	end;


	TSCUMMExpDecInfo = record
		id: TSCUMMExpGlobId;
		enumType: TSCUMMExpEnumType;
		index: Integer;
	end;

	TSCUMMExpDecInfoPath = array of TSCUMMExpDecInfo;

	TSCUMMExpDecCompat = record
		game: TSCUMMCoreGame;
		vers: TSCUMMCoreVersion;
		subv: Byte;
		feat: TSCUMMCoreFeatures;
		plat: TSCUMMCorePlatform;
	end;

	TSCUMMExpViewerCompat = record
		game: TSCUMMCoreGame;
		vers: TSCUMMCoreVersion;
		subv: Byte;
		feat: TSCUMMCoreFeatures;
		plat: TSCUMMCorePlatform;
		enum: TSCUMMExpEnumType;
	end;

	function  SCUMMExpCreateGlobID: TSCUMMExpGlobId;
	procedure SCUMMExpAssignObjData(const ASource: TSCUMMExpObjData;
			var ADest: TSCUMMExpObjData);

implementation

uses
	SysUtils, StrUtils, SCUMMConsts, SPUTMStrs;


function SCUMMExpCreateGlobID: TSCUMMExpGlobId;
	begin
	if  CreateGUID(Result) <> 0 then
//fixme dengland Should SCUMMExpCreateGlobID raise an exception when fail?
		Result:= VAL_SCUMMEXP_GLOBID_NULL;
	end;


procedure SCUMMExpAssignObjData(const ASource: TSCUMMExpObjData;
		var ADest: TSCUMMExpObjData);
	begin
	if  ASource.id <> VAL_SCUMMEXP_GLOBID_NULL then
		ADest.id:= ASource.id;

	ADest.name:= ASource.name;
	UniqueString(ADest.name);

	ADest.index:= ASource.index;
	ADest.enumType:= ASource.enumType;
	ADest.decoded:= ASource.decoded;

	ADest.dataType:= ASource.dataType;
	case ADest.dataType of
		sxdInvalid:
			;
		sxdRoot:
			ADest.gameStats:= ASource.gameStats;
		sxdNode:
			;
		sxdRaw:
			ADest.rawData:= ASource.rawData;
		sxdXML:
			ADest.xmlData:= ASource.xmlData;
		sxdImage:
			ADest.imageData:= ASource.imageData;
		sxdScript:
			ADest.scriptData:= ASource.scriptData;
		end;
	end;


{ TSCUMMExpObjPath }

function TSCUMMExpObjPath.FromArray(const AArray: TSCUMMExpGlobIdArr): Boolean;
	var
	i,
	j: Integer;

	begin
	Initialise;

	i:= Low(AArray);
	if  Length(AArray) > 0 then
		begin
		Root:= AArray[i];
		Inc(i);
		end;

	j:= High(AArray) - i;
	if j < 0 then
		j:= 0;
	SetLength(Nodes, j);

	j:= i;
	for i:= j to High(AArray) - 1 do
		Nodes[Pred(i)]:= AArray[i];

	if  Length(AArray) > 1 then
		Obj:= AArray[High(AArray)];

	Result:= True;
	end;

function TSCUMMExpObjPath.FromString(const AValue: string): Boolean;
	var
	c,
	p,
	d,
	r,
	i: Integer;
	o: Boolean;
{$IFOPT D+}
	s: string;
{$ENDIF}

	begin
	Result:= False;
	Initialise;
//
//	'//{blah-blah}/{blah-blah}/{blah-blah}'
//
//	          111111111122222222
//	 123456789012345678901234567

	d:= Pos('}', AValue);
	if  (CompareStr(Copy(AValue, 1, 2), LIT_SCUMMEXP_PATHDL_ROOT) = 0)
	and (d > 3) then
		try
			c:= 0;
			p:= d;
			r:= p + 1;
{$IFOPT D+}
			s:= Copy(AValue, 3, p - 2);
			Root:= StringToGUID(s);
			Assert(CompareStr(s, GUIDToString(Root)) = 0,
					'Root GUID conversion failure!');
{$ELSE}
			Root:= StringToGUID(Copy(AValue, 3, p - 2));
{$ENDIF}
			while (p > 0) and (p < Length(AValue)) do
				begin
				p:= PosEx(LIT_SCUMMEXP_PATHDL_NODE, AValue, p + 1);
				if  p > 0 then
					Inc(c);
				end;

			if  p < Length(AValue) then
				o:= True
			else
				o:= False;

			Dec(c);
			if  c > 0 then
				SetLength(Nodes, c);

			i:= 0;
			p:= r;
			d:= p;
			while c > 0 do
				begin
				d:= PosEx('}', AValue, p);

{$IFOPT D+}
				s:= Copy(AValue, p + 1, d - p);
				FNS:= s;

				Nodes[i]:= StringToGUID(s);
				Assert(CompareStr(s, GUIDToString(Nodes[i])) = 0,
					'Node GUID conversion failure!');
{$ELSE}
				Nodes[i]:= StringToGUID(Copy(AValue, p + 1, d - p));
{$ENDIF}

				p:= d + 2;
				p:= PosEx(LIT_SCUMMEXP_PATHDL_NODE, AValue, p);
				if p > 0 then
					d:= p
				else
					Inc(d);

				Inc(i);
				Dec(c);
				end;

			if  o then
{$IFOPT D+}
				begin
				s:= Copy(AValue, d + 1, Length(AValue) - d);
				Obj:= StringToGUID(s);
				Assert(CompareStr(s, GUIDToString(Obj)) = 0,
					'Object GUID conversion failure!');
				end;
{$ELSE}
				Obj:= StringToGUID(Copy(AValue, d + 1, Length(AValue) - d));
{$ENDIF}

			Result:= True;
			except

			end;
	end;

procedure TSCUMMExpObjPath.Initialise;
	begin
	Root:= VAL_SCUMMEXP_GLOBID_NULL;
	SetLength(Nodes, 0);
	Obj:= VAL_SCUMMEXP_GLOBID_NULL;
	end;

function TSCUMMExpObjPath.ToArray: TSCUMMExpGlobIdArr;
	var
	b: Boolean;
	c: Integer;

	begin
	c:= Length(Nodes) + 1;

	if  CompareStr(GUIDToString(VAL_SCUMMEXP_GLOBID_NULL),
			GUIDToString(Obj)) = 0 then
		b:= False
	else
		b:= True;

	if  b then
		Inc(c);

	SetLength(Result, c);
	Result[0]:= Root;
	for c:= 0 to High(Nodes) do
		Result[c + 1]:= Nodes[c];

	if  b then
		Result[High(Result)]:= Obj;
	end;

function TSCUMMExpObjPath.ToString: string;
	var
	i: Integer;

	begin
	Result:= LIT_SCUMMEXP_PATHDL_ROOT + GUIDToString(Root) +
			LIT_SCUMMEXP_PATHDL_NODE;

	for i:= Low(Nodes) to High(Nodes) do
{$IFOPT D+}
		begin
		Result:= Result + GUIDToString(Nodes[i]) + LIT_SCUMMEXP_PATHDL_NODE;
		if  i = High(Nodes) then
			Assert(CompareStr(FNS, GUIDToString(Nodes[i])) = 0,
					'Convert Node GUID failed!');
		end;
{$ELSE}
		Result:= Result + GUIDToString(Nodes[i]) + LIT_SCUMMEXP_PATHDL_NODE;
{$ENDIF}

	if  Obj <> VAL_SCUMMEXP_GLOBID_NULL then
		Result:= Result + GUIDToString(Obj);
	end;

end.
