unit SCUMMClasses;

interface

uses
	TypInfo, Classes, System.Generics.Collections, SCUMMKBClasses, SCUMMTypes;

type
//	Delphi and FPC runtimes already do a lot of work for us in handling
//		transformations between host path and file objects to/from strings.  It
//		is uncessary here to do more than store a string to identify a path
//		or file object.  The actual work in enumeration does require some small
//		degree of care in order to be platform agnostic and is negligible for
//		Delphi 2010 onwards (IOUtils) but a little tricky for FPC.

//	In SCUMMVM, there is a lot of work to create something of a virtual file
//		system ("archives") and a generic file system wrapper ("fs").  There is
//		then additional work to store MD5 data.  All of this is uncessessary
//		for us (as above and for the functionality required) so I am summarising
//		it into a single class, TSCUMMHostNode.  There will be much more work
//		required to create a real virtual namespace for the explorer and SPUTM
//		enumerators though so keeping this simple is a relief.

	TSCUMMHostNode = class
	private
		FParent: TSCUMMHostNode;
		FPath: string;
		FNodes: TList;
		FObjects: TStringList;
		FMD5s: TStringList;

		procedure BuildNode(const APath: string; const ADepth: Integer);
		procedure AddObject(const AObject: string);

	protected
		function  GetPath: string;

		function  GetNodeCount: Integer;
		function  GetNodes(const AIndex: Integer): TSCUMMHostNode;

		function  GetObjectCount: Integer;
		function  GetObjects(const AIndex: Integer): string;

		function  GetObjectMD5(const AIndex: Integer): TSCUMMMD5;

	public
		constructor Create(const APath: string; const ADepth: Integer;
				AParent: TSCUMMHostNode = nil);
		destructor  Destroy; override;

		procedure CalculateObjectMD5(const AIndex: Integer);
		function  FindObjectInNodes(const AObject: string;
				out ANode: TSCUMMHostNode; out AIndex: Integer;
				const ARecurseNodes: Boolean = True): Boolean;

		property  Path: string read GetPath;

		property  NodeCount: Integer read GetNodeCount;
		property  Nodes[const AIndex: Integer]: TSCUMMHostNode read GetNodes;

		property  ObjectCount: Integer read GetObjectCount;
		property  Objects[const AIndex: Integer]: string read GetObjects; default;

		property  ObjectMD5[const AIndex: Integer]: TSCUMMMD5 read GetObjectMD5;
	end;


//	SCUMMVM implements these as static const arrays in detection_tables.h and
//		scumm-md5.h but we won't be doing that here (to allow extension and a
//		simpler growth pattern from data conversion).  So instead, we have a
//		knowledgebase sections and entry lists.
	TSCUMMKBFilenamePatterns = class(TSCUMMKBSection)
	private
		FEntries: TList;
		FTypeFilenameGen,
		FTypeLanguage,
		FTypePlatform: PTypeInfo;

	protected
		class function  GetName: string; override;
		class function  GetDescription: string; override;
		class function  GetFilename: string; override;

		function  GetEntryCount: Integer; override;
		function  GetEntries(const AIndex: Integer): PSCUMMGameFilenamePat;

		procedure Clear; override;
		procedure AddEntryFromString(const AString: string); override;

		procedure ReadEntryFromStream(const AStream: TStream);
		procedure ReadFromStream(const AStream: TStream); override;

		procedure WriteEntryToStream(const AIndex: Integer;
				const AStream: TStream); override;
		procedure WriteEntryToString(const AIndex: Integer;
				var AString: string); override;

	public
		constructor Create; override;
		destructor  Destroy; override;

		property  Entries[const AIndex: Integer]: PSCUMMGameFilenamePat read GetEntries; default;
	end;

	TSCUMMKBMD5Mapping = class(TSCUMMKBSection)
	private
		FEntries: TList;
		FTypeLanguage,
		FTypePlatform: PTypeInfo;

	protected
		class function  GetName: string; override;
		class function  GetDescription: string; override;
		class function  GetFilename: string; override;

		function  GetEntryCount: Integer; override;
		function  GetEntries(const AIndex: Integer): PSCUMMGameMD5Map;

		procedure Clear; override;
		procedure AddEntryFromString(const AString: string); override;

		procedure ReadEntryFromStream(const AStream: TStream);
		procedure ReadFromStream(const AStream: TStream); override;

		procedure WriteEntryToStream(const AIndex: Integer;
				const AStream: TStream); override;
		procedure WriteEntryToString(const AIndex: Integer;
				var AString: string); override;

	public
		constructor Create; override;
		destructor  Destroy; override;

		function  FindEntryMD5(const AMD5: TSCUMMMD5): PSCUMMGameMD5Map;

		property  Entries[const AIndex: Integer]: PSCUMMGameMD5Map read GetEntries; default;
	end;

	TSCUMMKBGameVariants = class(TSCUMMKBSection)
	private
		FEntries: TList;
		FTypeGame,
		FTypeVersion,
		FTypeMusicFlags,
		FTypeFeatures,
		FTypePlatform,
		FTypeGUIOpt: PTypeInfo;

	protected
		class function  GetName: string; override;
		class function  GetDescription: string; override;
		class function  GetFilename: string; override;

		function  GetEntryCount: Integer; override;
		function  GetEntries(const AIndex: Integer): PSCUMMGameProps;

		procedure Clear; override;
		procedure AddEntryFromString(const AString: string); override;

		procedure ReadEntryFromStream(const AStream: TStream);
		procedure ReadFromStream(const AStream: TStream); override;

		procedure WriteEntryToStream(const AIndex: Integer;
				const AStream: TStream); override;
		procedure WriteEntryToString(const AIndex: Integer;
				var AString: string); override;

	public
		constructor Create; override;
		destructor  Destroy; override;

		property  Entries[const AIndex: Integer]: PSCUMMGameProps read GetEntries; default;
	end;


//	The work invovled in detecting a game from an installation path is done
//		with this class.  This is a simplification of the same logic as is in
//		SCUMMVM detection.h/detection.cpp.

//dengland  I notice that the MD5 entries (PSCUMMGameMD5Map) can override the
//		language and platform but not the music and feature flags.  This seems
//		to be somewhat incorrect.  Because of this, the platform should be
//		checked first and the music flags and feature flags updated accordingly
//		or only used as a hint where they are determined valid for the platform.

	TSCUMMDetector = class
	public
		class function DetectSCUMM(const ANode: TSCUMMHostNode;
				out ADetection: TSCUMMDetectorData): TSCUMMDetectorResult;
	end;


	TSCUMMPluginMngr = class
	private type
		TSCUMMPluginMngrState = (spmUnknown, spmReady, spmDoneQuery,
				spmDoneInit, spmDonePrep, spmDoneRelease, spmDoneFinl);

		PSCUMMPluginData = ^TSCUMMPluginData;
		TSCUMMPluginData = record
			_file: string;
			hInst: HINST;
			desc: TSCUMMPluginDesc;
			query: TSCUMMPluginQuery;
			init: TSCUMMPluginInit;
			prep: TSCUMMPluginPrep;
			rels: TSCUMMPluginRels;
			finl: TSCUMMPluginFinl;
		end;

	protected
		FState: TSCUMMPluginMngrState;

		FPath: string;
		FPlugins: TList<PSCUMMPluginData>;

		procedure Clear;
		procedure Remove(AIndex: Integer);

		function  GetPluginCount: Integer;
		function  GetPluginDesc(AIndex: Integer): PSCUMMPluginDesc;
		function  GetPluginFileName(AIndex: Integer): string;

	public
		constructor Create(const APath: string);
		destructor  Destroy; override;

		procedure QueryPlugins;
		procedure InitPlugins;
		procedure PrepPlugins;
		procedure RelsPlugins;
		procedure FinlPlugins;

		property  PluginCount: Integer read GetPluginCount;
		property  PluginDesc[AIndex: Integer]: PSCUMMPluginDesc
				read GetPluginDesc; default;
		property  PluginFileName[AIndex: Integer]: string
				read GetPluginFileName;
	end;


var
	SCUMMPluginMngr: TSCUMMPluginMngr;


implementation

uses
	Windows, Types, SysUtils, IOUtils, SCUMMLogTypes, SCUMMConsts;

{ TSCUMMHostNode }

procedure TSCUMMHostNode.AddObject(const AObject: string);
	begin
	FObjects.Add(AObject);
	FMD5s.Add('');
	end;

(*static BaseScummFile *openDiskImage(const Common::FSNode &node, const GameFilenamePattern *gfp) {
	Common::String disk1 = node.getName();
	BaseScummFile *diskImg;

	SearchMan.addDirectory("tmpDiskImgDir", node.getParent());

	if (disk1.hasSuffix(".prg")) { // NES
		diskImg = new ScummNESFile();
	} else { // C64 or Apple //gs
		// setup necessary game settings for disk image reader
		GameSettings gs;
		memset(&gs, 0, sizeof(GameSettings));
		gs.gameid = gfp->gameid;
		gs.id = (Common::String(gfp->gameid) == "maniac" ? GID_MANIAC : GID_ZAK);
		gs.platform = gfp->platform;

		// determine second disk file name
		Common::String disk2(disk1);
		for (Common::String::iterator it = disk2.begin(); it != disk2.end(); ++it) {
			// replace "xyz1.(d64|dsk)" by "xyz2.(d64|dsk)"
			if (*it == '1') {
				*it = '2';
				break;
			}
		}

		// open image
		diskImg = new ScummDiskImage(disk1.c_str(), disk2.c_str(), gs);
	}

	if (diskImg->open(disk1.c_str()) && diskImg->openSubFile("00.LFL")) {
		debug(0, "Success");
		return diskImg;
	}
	delete diskImg;
	return 0;
}*)


procedure TSCUMMHostNode.BuildNode(const APath: string; const ADepth: Integer);
	var
	node: TSCUMMHostNode;
	sda: TStringDynArray;
	i: Integer;

	begin
//fixme dengland This needs to be revised for FPC compatibilty

//	We are assuming that existence checking for the initial node is done
//		externally.  Further checking is unnecessary.

//	Get the file objects
	sda:= TDirectory.GetFiles(APath);
	for i:= Low(sda) to High(sda) do
		AddObject(sda[i]);

//fixme dengland We need to check if the file is a .d64, .dsk or .prg file and
//		if so, create a virtual node and virtual file entries for it


//	If we still need to walk the tree, get the directory nodes and descend.
	if ADepth > 0 then
		begin
		sda:= TDirectory.GetDirectories(APath);
		for i:= Low(sda) to High(sda) do
			begin
			node:= TSCUMMHostNode.Create(sda[i], ADepth - 1, Self);
			FNodes.Add(node);
			end;
		end;
	end;

const
	VAL_MAX_SCUMMMD5_LENGTH = 1024 * 1024;

procedure TSCUMMHostNode.CalculateObjectMD5(const AIndex: Integer);
	var
	fs: TFileStream;
	md5: TSCUMMMD5;

	begin
	if  FMD5s.Count >= AIndex then
		if  FMD5s[AIndex] = '' then
			begin
			fs:= TFileStream.Create(Objects[AIndex], fmOpenRead);
			try
				FillChar(md5, SizeOf(TSCUMMMD5), 0);
				SCUMMComputeStreamMD5(fs, md5, VAL_MAX_SCUMMMD5_LENGTH);
				FMD5s[AIndex]:= string(SCUMMMD5ToString(md5));

				finally
				fs.Free;
				end;
			end;
	end;

constructor TSCUMMHostNode.Create(const APath: string; const ADepth: Integer;
		AParent: TSCUMMHostNode);
	begin
	inherited Create;

	FParent:= AParent;
	FPath:= APath;
	UniqueString(FPath);

	FNodes:= TList.Create;
	FObjects:= TStringList.Create;
	FMD5s:= TStringList.Create;

	BuildNode(FPath, ADepth);
	end;

destructor TSCUMMHostNode.Destroy;
	var
	i: Integer;

	begin
	FMD5s.Free;
	FObjects.Free;

	for i:= FNodes.Count - 1 downto 0 do
		TSCUMMHostNode(FNodes[i]).Free;
	FNodes.Free;

	inherited;
	end;

function TSCUMMHostNode.FindObjectInNodes(const AObject: string;
		out ANode: TSCUMMHostNode; out AIndex: Integer;
		const ARecurseNodes: Boolean): Boolean;
	var
	n: string;
	i: Integer;

	begin
//fixme dengland This needs to be revised for FPC compatibilty
//fixme dengland  This should try to determine where to look based on any path
//		information in AObject

	ANode:= nil;
	AIndex:= -1;
	Result:= False;

	n:= TPath.GetFileName(AObject);
	for i:= 0 to FObjects.Count - 1 do
		if  CompareText(n, TPath.GetFileName(FObjects[i])) = 0 then
			begin
			ANode:= Self;
			AIndex:= i;
			Result:= True;
			Exit;
			end;

	if ARecurseNodes then
		for i:= 0 to FNodes.Count - 1 do
			if  TSCUMMHostNode(FNodes[i]).FindObjectInNodes(AObject, ANode,
					AIndex, ARecurseNodes) then
				begin
				Result:= True;
				Exit;
				end;
	end;

function TSCUMMHostNode.GetNodeCount: Integer;
	begin
	Result:= FNodes.Count;
	end;

function TSCUMMHostNode.GetNodes(const AIndex: Integer): TSCUMMHostNode;
	begin
	Result:= TSCUMMHostNode(FNodes[AIndex]);
	end;

function TSCUMMHostNode.GetObjectCount: Integer;
	begin
	Result:= FObjects.Count;
	end;

function TSCUMMHostNode.GetObjectMD5(const AIndex: Integer): TSCUMMMD5;
	begin
	if  FMD5s[AIndex] = '' then
		CalculateObjectMD5(AIndex);

	Result:= SCUMMStringToMD5(AnsiString(FMD5s[AIndex]));
	end;

function TSCUMMHostNode.GetObjects(const AIndex: Integer): string;
	begin
	Result:= FObjects[AIndex];
	end;

function TSCUMMHostNode.GetPath: string;
	begin
	Result:= FPath;
	end;

{ TSCUMMDetector }

function GenerateFilenameForDetection(const APattern: string;
		const AMethod: TSCUMMFilenameGen;
		const APlatform: TSCUMMCorePlatform): string;
	begin
	case AMethod of
		sfgDiskNum, sfgRoomNum:
			Result:= Format(APattern, [0]);
		sfgDiskNumSteam, sfgRoomNumSteam:
			begin
//fixme dengland Implement the steam pattern to filename gen
//			const SteamIndexFile *indexFile = lookUpSteamIndexFile(pattern, platform);
//			if (!indexFile) {
//				error("Unable to find Steam executable from detection pattern");
//			} else {
//				result = indexFile->executableName;
//			}
			end;
		sfgHEPC, sfgHEIOS:
			Result:= Format('%s.he0', [APattern]);
		sfgHEMac:
			Result:= Format('%s (0)', [APattern]);
		sfgHEMacNoParens:
			Result:= Format('%s 0', [APattern]);
		sfgUnchanged:
			Result:= APattern;
		else
			SCUMMExpLogError(sxkDetector,
					'GenerateFilenameForDetection: Unsupported genMethod', []);
		end;
	end;

// The following function tries to detect the language for COMI and DIG
function DetectLanguage(const ANode: TSCUMMHostNode;
		AID: TSCUMMCoreGame): TSCUMMCoreLanguage;
	var
	objn: TSCUMMHostNode;
	obji: Integer;
	f: string;
	fs: TFileStream;
	sz: Int64;

	begin
//	First try to detect Chinese translation
	if  ANode.FindObjectInNodes('chinese_gb16x12.fnt', objn, obji) then
		begin
		SCUMMExpLogDebug(sxkDetector, 'Chinese detected', []);
		Result:= sclZH_CNA;
		Exit;
		end;

	Result:= sclUnk;

//	Now try to detect COMI and Dig by language files
	if  not (AID in [scgCMI, scgDig]) then
		Exit;

//	Check for LANGUAGE.BND (Dig) resp. LANGUAGE.TAB (CMI).
//			These are usually inside the "RESOURCE" subdirectory.
//			If found, we match based on the file size (should we
//			ever determine that this is insufficient, we can still
//			switch to MD5 based detection).
	if  AID = scgCMI then
		f:= 'LANGUAGE.TAB'
	else
		f:= 'LANGUAGE.BND';

//todo dengland SCUMMVM checks for the file in the 'RESOURCE' node specifically.
//		I'm not doing that here.  Is it required?

	if  ANode.FindObjectInNodes(f, objn, obji) then
		begin
		fs:= TFile.OpenRead(objn[obji]);
		try
			sz:= fs.Size;
			finally
			fs.Free;
			end;

		if  AID = scgCMI then
			case sz of
				439080:	// 2daf3db71d23d99d19fc9a544fcf6431
					Result:= sclEN_ANY;
				322602:	// caba99f4f5a0b69963e5a4d69e6f90af
					Result:= sclZH_TWN;
				493252:	// 5d59594b24f3f1332e7d7e17455ed533
					Result:= sclDE_DEU;
				461746:	// 35bbe0e4d573b318b7b2092c331fd1fa
					Result:= sclFR_FRA;
				443439:	// 4689d013f67aabd7c35f4fd7c4b4ad69
					Result:= sclIT_ITA;
				398613:	// d1f5750d142d34c4c8f1f330a1278709
					Result:= sclKO_KOR;
				440586:	// 5a1d0f4fa00917bdbfe035a72a6bba9d
					Result:= sclPT_BRA;
				454457,	// 0e5f450ec474a30254c0e36291fb4ebd
				394083:	// ad684ca14c2b4bf4c21a81c1dbed49bc
					Result:= sclRU_RUS;
				449787:	// 64f3fe479d45b52902cf88145c41d172
					Result:= sclES_ESP;
				end
		else	// The DIG
			case sz of
				248627:	// 1fd585ac849d57305878c77b2f6c74ff
					Result:= sclDE_DEU;
				257460:	// 04cf6a6ba6f57e517bc40eb81862cfb0
					Result:= sclFR_FRA;
				231402:	// 93d13fcede954c78e65435592182a4db
					Result:= sclIT_ITA;
				228772:	// 5d9ad90d3a88ea012d25d61791895ebe
					Result:= sclPT_BRA;
				229884:	// d890074bc15c6135868403e73c5f4f36
					Result:= sclES_ESP;
				223107:	// 64f3fe479d45b52902cf88145c41d172
					Result:= sclJA_JPN;
				180730:	// 424fdd60822722cdc75356d921dad9bf
					Result:= sclZH_TWN;
				end;
		end;
	end;

function DetectSpeech(ANode: TSCUMMHostNode; AGE: PSCUMMGameProps): Boolean;
	const
	LIT_SCUMMEXP_SPEECHMON = 'monster';
	ARR_LIT_SCUMMEXP_SPEECHEXT: array[0..3] of string = (
			'sou', 'sof', 'sog', 'so3');

	var
	i,
	j: Integer;

	a: array[0..1] of string;
	f: string;

	objn: TSCUMMHostNode;
	obji: Integer;

	begin
	Result:= False;

//	FMTOWNS monkey and monkey2 games don't have speech but may have .sou files
	if  AGE^.id in [scgMonkey, scgMonkey2] then
		if  AGE^.plat = scpFMTowns then
			Exit;

	a[0]:= AGE^.name;
	a[1]:= LIT_SCUMMEXP_SPEECHMON;

	for i:= 0 to 1 do
		for j:= 0 to High(ARR_LIT_SCUMMEXP_SPEECHEXT) do
			begin
			f:= a[i] + '.' + ARR_LIT_SCUMMEXP_SPEECHEXT[j];

			if  ANode.FindObjectInNodes(f, objn, obji) then
				begin
				Result:= True;
				Exit;
				end;
			end;
	end;

procedure ComputeGameSettingsFromMD5(const ANode: TSCUMMHostNode;
		const AGFP: PSCUMMGameFilenamePat; const AMME: PSCUMMGameMD5Map;
		var ADetection: TSCUMMDetectorData);
	var
	gv: TSCUMMKBGameVariants;
	ge: PSCUMMGameProps;
	i: Integer;

	begin
	ADetection.language:= AMME^.language;
	ADetection.extra:= AMME^.extra;

	gv:= SCUMMKnowledgeBase.GetSectionOfClass(TSCUMMKBGameVariants) as
			TSCUMMKBGameVariants;
	Assert(Assigned(gv));

//	Compute the precise game settings using gameVariantsTable.
	for i:= 0 to gv.EntryCount - 1 do
		begin
		ge:= gv.Entries[i];

		if  (Length(ge^.name) = 0)
		or  (CompareText(AMME^.gameid, ge^.name) = 0) then
			begin
//			The gameid either matches, or is empty. The latter indicates
//					a generic entry, currently used for some generic HE settings.
			if  (Length(ge^.variant) = 0)
			or  (CompareText(AMME^.variant, ge^.variant) = 0) then
				begin
//				Perfect match found, use it and stop the loop
				ADetection.game.Assign(ge^);
				ADetection.game.name:= AMME^.gameid;
				UniqueString(ADetection.game.name);

//				Set the platform value. The value from the MD5 record has
//						highest priority; if missing (i.e. set to unknown) we
//						try to use that from the filename pattern record instead.
				if  AMME^.platfrm <> scpUnk then
					ADetection.game.plat:= AMME^.platfrm
				else if AGFP^.platfrm <> scpUnk then
					ADetection.game.plat:= AGFP^.platfrm;

//				HACK: Special case to distinguish the V1 demo from the full
//						version (since they have identical MD5):
				if  (ADetection.game.id = scgManiac)
				and (CompareText(AGFP^.pattern, '%02d.MAN') = 0) then
					ADetection.extra:= 'V1 Demo';

//				HACK: If 'Demo' occurs in the extra string, set the scfDemo
//						flag, required by some game demos (e.g. Dig, FT and COMI).
				if	(Length(ADetection.extra) > 0)
				and (Pos('Demo', ADetection.extra) > 0) then
					Include(ADetection.game.feat, scfDemo);

//				HACK: Try to detect languages for translated games
				if  ADetection.language = sclUnk then
					ADetection.language:= DetectLanguage(ANode,
							ADetection.game.id);

//				HACK: Detect between 68k and PPC versions
				if  (ADetection.game.plat = scpAppleMac)
				and (ADetection.game.ver >= scv5)
				and (ADetection.game.subv = 0)
				and (CompareText(AGFP^.pattern, 'Data') = 0) then
					Include(ADetection.game.feat,  scfMacContainers);

				Break;
				end;
			end;
		end;
	end;

//fixme dengland This routine should return TSCUMMDetectorResult so we know if
//		the tests were conclusive or not?
function TestGame(const AGE: PSCUMMGameProps; const AMME: PSCUMMGameMD5Map;
		const ANode: TSCUMMHostNode; const AFile: string): Boolean;
	var
	f: string;
	fs: TFileStream;
	buf: array[0..5] of Byte;

	objn: TSCUMMHostNode;
	obji: Integer;

	has58LFL,
	has84LFL,
	has86LFL,
	has98LFL,
	has903LFL,
	hasDisk02: Boolean;

	begin
	Result:= False;

//	At this point, we know that the gameid matches, but no variant was
//		specified, yet there are multiple ones. So we try our best to
//		distinguish between the variants.  To do this, we take a close look at
//		the detection file and try to filter out some cases.

	try
		fs:= TFile.OpenRead(AFile);
		try
			f:= TPath.GetFileName(AFile);

			if  (CompareText(f, 'maniac1.d64') = 0)
			or  (CompareText(f, 'maniac1.dsk') = 0)
			or  (CompareText(f, 'zak1.d64') = 0) then
//				TODO
			else if CompareText(f, '00.LFL') = 0 then
				begin
//				Used in V1, V2, V3 games.
				if  AGE^.ver > scv3 then
					Exit;

//				Read a few bytes to narrow down the game.
				fs.Read(buf, 6);

				if  (buf[0] = $BC)
				and (buf[1] = $B9) then
//					The NES version of MM
					if  (AGE^.id = scgManiac)
					and (AGE^.plat = scpNES) then
						begin
//						perfect match
						Result:= True;
						Exit;
						end
				else if ((buf[0] = $CE) and (buf[1] = $F5))  		// PC
				or ((buf[0] = $CD) and (buf[1] = $FE)) then			// C64
					begin
//					Could be V0 or V1.
//							Candidates: maniac classic, zak classic

					if  AGE^.ver >= scv2 then
						Exit;

//					Zak has 58.LFL, Maniac doesn't have it.
					has58LFL:= ANode.FindObjectInNodes('58.LFL', objn, obji);
					if  (AGE^.id = scgManiac)
					and (not has58LFL) then
//						??
					else if (AGE^.id = scgZak) and has58LFL then
//						??
					else
						Exit;
					end
				else if (buf[0] = $FF) and (buf[1] = $FE) then
					begin
//					GF_OLD_BUNDLE: could be V2 or old V3.
//							Note that GF_OLD_BUNDLE is true if and only if
//							GF_OLD256 is false.  Candidates: maniac enhanced,
//							zak enhanced, indy3ega, loom

					if  ((AGE^.ver <> scv2)
					and  (AGE^.ver <> scv3))
					or  (scfOld256Colour in AGE^.feat) then
						Exit;

//					We distinguish the games by the presence/absence of
//							certain files. In the following, '+' means the file
//							present, '-' means the file is absent.
//
//						maniac:    -58.LFL, -84.LFL,-86.LFL, -98.LFL
//
//						zak:       +58.LFL, -84.LFL,-86.LFL, -98.LFL
//						zakdemo:   +58.LFL, -84.LFL,-86.LFL, -98.LFL
//
//						loom:      +58.LFL, -84.LFL,+86.LFL, -98.LFL
//						loomdemo:  -58.LFL, +84.LFL,-86.LFL, -98.LFL
//
//						indy3:     +58.LFL, +84.LFL,+86.LFL, +98.LFL
//						indy3demo: -58.LFL, +84.LFL,-86.LFL, +98.LFL

					has58LFL:= ANode.FindObjectInNodes('58.LFL', objn, obji);
					has84LFL:= ANode.FindObjectInNodes('84.LFL', objn, obji);
					has86LFL:= ANode.FindObjectInNodes('86.LFL', objn, obji);
					has98LFL:= ANode.FindObjectInNodes('98.LFL', objn, obji);

					if  (AGE^.id = scgINDY3)
					and has98LFL
					and has84LFL then
//						??
					else if (AGE^.id = scgZAK)
					and (not has98LFL)
					and (not has86LFL)
					and (not has84LFL)
					and has58LFL then
//						??
					else if (AGE^.id = scgMANIAC)
					and (not has98LFL)
					and (not has86LFL)
					and (not has84LFL)
					and (not has58LFL) then
//						??
					else if (AGE^.id = scgLOOM)
					and (not has98LFL)
					and (has86LFL <> has84LFL) then
//						??
					else
						Exit;
					end
				else if (buf[4] = Byte(AnsiChar('0')))
				and (buf[5] = Byte(AnsiChar('R'))) then
					begin
//					newer V3 game
//							Candidates: indy3, indy3Towns, zakTowns, loomTowns
					if (AGE^.ver <> scv3)
					or (not (scfOld256Colour in AGE^.feat)) then
						Exit;

//					Considering that we know about *all* TOWNS versions, and
//							know their MD5s, we could simply rely on this and if
//							we find something which has an unknown MD5, assume
//							that it is an (so far unknown) version of Indy3.
//							However, there are also fan translations of the
//							TOWNS versions, so we can't do that.
//
//					But we could at least look at the resource headers to
//							distinguish TOWNS versions from regular games:
//
//						Indy3:
//						_numGlobalObjects 1000
//						_numRooms 99
//						_numCostumes 129
//						_numScripts 139
//						_numSounds 84
//
//						Indy3Towns, ZakTowns, ZakLoom demo:
//						_numGlobalObjects 1000
//						_numRooms 99
//						_numCostumes 199
//						_numScripts 199
//						_numSounds 199
//
//					Assuming that all the town variants look like the latter, we
//							can do the check like this:
//						if (numScripts == 139)
//							assume Indy3
//						else if (numScripts == 199)
//							assume towns game
//						else
//							unknown, do not accept it

//					We now try to exclude various possibilities by the presence
//							of certain LFL files. Note that we only exclude
//							something based on the *presence* of a LFL file
//							here; compared to checking for the absence of files,
//							this has the advantage that we are less likely to
//							accidentally exclude demos (which, after all, are
//							usually missing many LFL files present in the full
//							version of the game).

//					No version of Indy3 has 05.LFL but MM, Loom and Zak all have it
					if  (AGE^.id = scgINDY3)
					and (ANode.FindObjectInNodes('05.LFL', objn, obji)) then
						Exit;

//					All versions of Indy3 have 93.LFL, but no other game
					if  (AGE^.id <> scgINDY3)
					and (ANode.FindObjectInNodes('93.LFL', objn, obji)) then
						Exit;

//					No version of Loom has 48.LFL
					if  (AGE^.id = scgLOOM)
					and (ANode.FindObjectInNodes('48.LFL', objn, obji)) then
						Exit;

//					No version of Zak has 60.LFL, but most (non-demo) versions of Indy3 have it
					if  (AGE^.id = scgZAK)
					and (ANode.FindObjectInNodes('60.LFL', objn, obji)) then
						Exit;

//					All versions of Indy3 and ZakTOWNS have 98.LFL, but no other game
					if  (AGE^.id = scgLOOM)
					and (AGE^.plat <> scpPCEngine)
					and (ANode.FindObjectInNodes('98.LFL', objn, obji)) then
						Exit;
					end
				else
//					TODO: Unknown file header, deal with it. Maybe an
//							unencrypted variant...
//					Anyway, we don't know to deal with the file, so we just skip
//							it.
					;
				end
			else if CompareText(f, '000.LFL') = 0 then
				begin
//				Used in V4
//						Candidates: monkeyEGA, pass, monkeyVGA, loomcd

				if  AGE^.ver <>  scv4 then
					Exit;

//				For all of them, we have:
//					_numGlobalObjects 1000
//					_numRooms 99
//					_numCostumes 199
//					_numScripts 199
//					_numSounds 199
//
//				Any good ideas to distinguish those? Maybe by the
//						presence/absence of some files?
//						At least PASS and the monkeyEGA demo differ by 903.LFL
//						missing...  And the count of DISK??.LEC files differs
//						depending on what version you have (4 or 8 floppy
//						versions).  loomcd of course shipped on only one "disc".
//
//				pass: 000.LFL, 901.LFL, 902.LFL, 904.LFL, disk01.lec
//				monkeyEGA:  000.LFL, 901-904.LFL, DISK01-09.LEC
//				monkeyEGA DEMO: 000.LFL, 901.LFL, 902.LFL, 904.LFL, disk01.lec
//				monkeyVGA: 000.LFL, 901-904.LFL, DISK01-04.LEC
//				loomcd: 000.LFL, 901-904.LFL, DISK01.LEC

				has903LFL:= ANode.FindObjectInNodes('903.LFL', objn, obji);
				hasDisk02:= ANode.FindObjectInNodes('DISK02.LEC', objn, obji);

//				There is not much we can do based on the presence / absence
//						of files. Only that if 903.LFL is present, it can't be
//						PASS; and if DISK02.LEC is present, it can't be LoomCD
				if  (AGE^.id = scgPASS)
				and (not has903LFL)
				and (not hasDisk02) then
//					??
				else if (AGE^.id = scgLOOM)
				and has903LFL
				and (not hasDisk02) then
//					??
				else if AGE^.id = scgMonkeyVGA then
//					??
				else if AGE^.id = scgMonkeyEGA then
//					??
				else
					Exit;
				end
			else
				begin
//				Must be a V5+ game
				if  AGE^.ver < scv5 then
					Exit;

//				So at this point the gameid is determined, but not necessarily
//						the variant!

//				TODO: Add code that handles this, at least for the non-HE games.
//						Note sure how realistic it is to correctly detect HE-game
//						variants, would require me to look at a sufficiently large
//						sample collection of HE games (assuming I had the time :).

//				TODO: For Mac versions in container file, we can sometimes
//						distinguish the demo from the regular version by looking
//						at the content of the container file and then looking for
//						the *.000 file in there.
				end;

			finally
			fs.Free;
			end;

		Result:= True;

		except
		SCUMMExpLogWarn(sxkDetector, 'SCUMM testGame: failed to open "%s" ' +
				'for read access', [AFile]);
		end;
	end;


class function TSCUMMDetector.DetectSCUMM(const ANode: TSCUMMHostNode;
		out ADetection: TSCUMMDetectorData): TSCUMMDetectorResult;
	var
	gfp: TSCUMMKBFilenamePatterns;
	fpe: PSCUMMGameFilenamePat;

	gmm: TSCUMMKBMD5Mapping;
	mme: PSCUMMGameMD5Map;

	gv: TSCUMMKBGameVariants;
	ge: PSCUMMGameProps;

	i,
	j: Integer;
	f: string;

	objn: TSCUMMHostNode;
	obji: Integer;

	fs: TFileStream;
	sz: Int64;

	begin
	FillChar(ADetection, SizeOf(TSCUMMDetectorData), 0);
	Result:= sdrError;

	gfp:= SCUMMKnowledgeBase.GetSectionOfClass(TSCUMMKBFilenamePatterns) as
			TSCUMMKBFilenamePatterns;
	Assert(Assigned(gfp));

	gmm:= SCUMMKnowledgeBase.GetSectionOfClass(TSCUMMKBMD5Mapping) as
			TSCUMMKBMD5Mapping;
	Assert(Assigned(gmm));

	gv:= SCUMMKnowledgeBase.GetSectionOfClass(TSCUMMKBGameVariants) as
			TSCUMMKBGameVariants;
	Assert(Assigned(gv));

	try
		for i:= 0 to gfp.EntryCount - 1 do
			begin
			fpe:= gfp.Entries[i];

//			Get the filename needed for this pattern
			f:= GenerateFilenameForDetection(fpe^.pattern, fpe^.genMethod,
					fpe^.platfrm);

//			If the filename is not in the nodes then try the next pattern
			if  not ANode.FindObjectInNodes(f, objn, obji) then
				Continue;

//			Store what we know up to this point
			ADetection.fp.pattern:= fpe^.pattern;
			ADetection.fp.genMethod:= fpe^.genMethod;
			ADetection.game.id:= scgUnk;
			ADetection.language:= fpe^.language;
			FillChar(ADetection.md5, SizeOf(TSCUMMMD5), 0);
			ADetection.extra:= '';

//			PART 1:  Trying to find an exact match using MD5.

//fixme dengland If the filename is a .d64, .dsk or .prg file then we need to
//		get the virtual LFL.00 file contained in it, instead

			mme:= gmm.FindEntryMD5(objn.ObjectMD5[obji]);
			if  Assigned(mme) then
				begin
				ComputeGameSettingsFromMD5(ANode, fpe, mme, ADetection);

//				Print some debug info
				fs:= TFile.OpenRead(objn[obji]);
				try
					sz:= fs.Size;
					finally
					fs.Free;
					end;
				SCUMMExpLogDebug(sxkDetector,
						'Found matching file "%s" with MD5 %s, size %d',
						[objn[obji], SCUMMMD5ToString(mme^.md5), sz]);

//				Sanity check: We *should* have found a matching gameid / variant
//						at this point.  If not, we may have #ifdef'ed the entry
//						out in our detection_tables.h because we don't have the
//						required stuff compiled in, or there's a bug in our data
//						tables...
				if  ADetection.game.id <> scgUnk then
					begin
//					Add it to the list of detected games
//dengland			We're only going to try to find one

					ADetection.md5:= MME^.md5;
					Result:= sdrConclusive;
					Exit;
					end;
				end;

//			PART 2:  Fuzzy matching for files with unknown MD5.

//			We loop over the game variants matching the gameid associated to
//					the gfp record. We then try to decide for each whether it
//					could be appropriate or not.
			ADetection.md5:= objn.ObjectMD5[obji];

			for j:= 0 to gv.EntryCount - 1 do
				begin
				ge:= gv.Entries[j];

//				Skip over entries with a different gameid.
				if  (Length(ge^.name) = 0)
				or  (CompareText(fpe^.gameid, ge^.name) <> 0) then
					Continue;

				ADetection.game.Assign(ge^);
//fixme scummvm We (ab)use 'variant' for the 'extra' description for now.
				ADetection.extra:= ge^.variant;

				if  fpe^.platfrm <> scpUnk then
					ADetection.game.plat:= fpe^.platfrm;

//				If a variant has been specified, use that!
				if  Length(fpe^.variant) > 0 then
					begin
					if  CompareText(fpe^.variant, ge^.variant) = 0 then
						begin
//						perfect match found
						Result:= sdrConclusive;
						Exit;
						end;

					Continue;
					end;

//				HACK: Perhaps it is some modified translation?
				ADetection.language:= DetectLanguage(ANode, ge^.id);

//				Detect if there are speech files in this unknown game
				if  DetectSpeech(ANode, ge) then
					if  sgoNoSpeech in ADetection.game.guiopts then
						if  ge^.id in [scgMonkey, scgMonkey2] then
//todo scummvm This may need to be updated if something important gets added in
//		the top detection table for these game ids
							ADetection.game.guiopts:= []
						else
							SCUMMExpLogWarn(sxkDetector,
									'FIXME: fix NOSPEECH fallback', []);

//				Add the game/variant to the candidates list if it is consistent
//						with the file(s) we are seeing.
				if  TestGame(ge, mme, ANode, objn[obji]) then
					begin
					Result:= sdrInconclusive;
					Exit;
					end;
				end;
			end;

		finally
		if  (Result = sdrConclusive)
		or  (Result = sdrInconclusive) then
			begin
			if  (ADetection.game.ver in [scv0..scv5])
			and (not (scfOld256Colour in ADetection.game.feat)) then
				Include(ADetection.game.feat, scfEncrypted);

			if  (ADetection.game.ver in [scv0..scv4]) then
				Include(ADetection.game.feat, scfSmallHeaders);

			if  (ADetection.game.ver in [scv0..scv3])
			and (not ((scfOld256Colour in ADetection.game.feat))) then
				Include(ADetection.game.feat, scf16Colour);

//dengland  scv3 + not scfOld256Colour gives us old v3.
			if  (ADetection.game.ver in [scv0..scv3])
			or  ((ADetection.game.ver = scv3)
			and  (not (scfOld256Colour in ADetection.game.feat))) then
				Include(ADetection.game.feat, scfOldBundles);

			if (scfEncrypted in ADetection.game.feat) then
				ADetection.encByte:= $FF
			else
				ADetection.encByte:= $00;
			end;
		end;
	end;

{ TSCUMMKBFilenamePatterns }

procedure TSCUMMKBFilenamePatterns.AddEntryFromString(const AString: string);
	var
	e: TSCUMMGameFilenamePat;
	p: PSCUMMGameFilenamePat;
	i: Integer;
	t1,
	t2,
	t3,
	t4,
	t5,
	t6: string;

	begin
	i:= 1;
	if  GetNextToken(AString, i, t1)
	and GetNextToken(AString, i, t2)
	and GetNextToken(AString, i, t3)
	and GetNextToken(AString, i, t4)
	and GetNextToken(AString, i, t5)
	and GetNextToken(AString, i, t6) then
		begin
		e.gameid:= t1;
		e.pattern:= t2;
		e.genMethod:= TSCUMMFilenameGen(GetEnumValue(FTypeFilenameGen, t3));
		e.language:= TSCUMMCoreLanguage(GetEnumValue(FTypeLanguage, t4));
		e.platfrm:= TSCUMMCorePlatform(GetEnumValue(FTypePlatform, t5));
		e.variant:= t6;

		New(p);
		p^.Assign(e);
		FEntries.Add(p);
		end;
	end;

procedure TSCUMMKBFilenamePatterns.Clear;
	var
	e: PSCUMMGameFilenamePat;
	i: Integer;

	begin
	for i:= FEntries.Count - 1 downto 0 do
		begin
		e:= PSCUMMGameFilenamePat(FEntries[i]);
		Dispose(e);
		end;

	FEntries.Clear;
	end;

constructor TSCUMMKBFilenamePatterns.Create;
	begin
	inherited Create;

	FEntries:= TList.Create;

	FTypeFilenameGen:= TypeInfo(TSCUMMFilenameGen);
	FTypeLanguage:= TypeInfo(TSCUMMCoreLanguage);
	FTypePlatform:= TypeInfo(TSCUMMCorePlatform);
	end;

destructor TSCUMMKBFilenamePatterns.Destroy;
	begin
	Clear;
	FEntries.Free;

	inherited;
	end;

class function TSCUMMKBFilenamePatterns.GetDescription: string;
	begin
	Result:= 'Game Filename Patterns';
	end;

function TSCUMMKBFilenamePatterns.GetEntries(
		const AIndex: Integer): PSCUMMGameFilenamePat;
	begin
	Result:= PSCUMMGameFilenamePat(FEntries[AIndex]);
	end;

function TSCUMMKBFilenamePatterns.GetEntryCount: Integer;
	begin
	Result:= FEntries.Count;
	end;

class function TSCUMMKBFilenamePatterns.GetFilename: string;
	begin
	Result:= 'GFPEntries';
	end;

class function TSCUMMKBFilenamePatterns.GetName: string;
	begin
	Result:= 'GFPEntries';
	end;

procedure TSCUMMKBFilenamePatterns.ReadEntryFromStream(const AStream: TStream);
	var
	e: TSCUMMGameFilenamePat;
	p: PSCUMMGameFilenamePat;
	i: Integer;

	begin
	e.gameid:= ReadStringFromStream(AStream);
	e.pattern:= ReadStringFromStream(AStream);

	AStream.Read(i, SizeOf(Integer));
	e.genMethod:= TSCUMMFilenameGen(i);

	AStream.Read(i, SizeOf(Integer));
	e.language:= TSCUMMCoreLanguage(i);

	AStream.Read(i, SizeOf(Integer));
	e.platfrm:= TSCUMMCorePlatform(i);

	e.variant:= ReadStringFromStream(AStream);

	New(p);
	p^.Assign(e);

	FEntries.Add(p);
	end;

procedure TSCUMMKBFilenamePatterns.ReadFromStream(const AStream: TStream);
	begin
	while AStream.Position < AStream.Size do
		ReadEntryFromStream(AStream);
	end;

procedure TSCUMMKBFilenamePatterns.WriteEntryToStream(const AIndex: Integer;
		const AStream: TStream);
	var
	p: PSCUMMGameFilenamePat;
	i: Integer;

	begin
	p:= PSCUMMGameFilenamePat(FEntries[AIndex]);

	WriteStringToStream(p^.gameid, AStream);
	WriteStringToStream(p^.pattern, AStream);

	i:= Integer(Ord(p^.genMethod));
	AStream.Write(i, SizeOf(Integer));

	i:= Integer(Ord(p^.language));
	AStream.Write(i, SizeOf(Integer));

	i:= Integer(Ord(p^.platfrm));
	AStream.Write(i, SizeOf(Integer));

	WriteStringToStream(p^.variant, AStream);
	end;

procedure TSCUMMKBFilenamePatterns.WriteEntryToString(const AIndex: Integer;
		var AString: string);
	var
	p: PSCUMMGameFilenamePat;

	begin
	p:= PSCUMMGameFilenamePat(FEntries[AIndex]);

	AString:= AString +
			MakeStringField(p^.gameid) + #$09 +
			MakeStringField(p^.pattern) + #$09 +
			GetEnumName(FTypeFilenameGen, Ord(p^.genMethod)) + #$09 +
			GetEnumName(FTypeLanguage, Ord(p^.language)) + #$09 +
			GetEnumName(FTypePlatform, Ord(p^.platfrm)) + #$09 +
			MakeStringField(p^.variant) + #$0D#$0A;
	end;


{ TSCUMMKBMD5Mapping }

procedure TSCUMMKBMD5Mapping.AddEntryFromString(const AString: string);
	var
	e: TSCUMMGameMD5Map;
	p: PSCUMMGameMD5Map;
	i: Integer;
	t1,
	t2,
	t3,
	t4,
	t5,
	t6,
	t7: string;

	begin
	i:= 1;
	if  GetNextToken(AString, i, t1)
	and GetNextToken(AString, i, t2)
	and GetNextToken(AString, i, t3)
	and GetNextToken(AString, i, t4)
	and GetNextToken(AString, i, t5)
	and GetNextToken(AString, i, t6)
	and GetNextToken(AString, i, t7) then
		begin
		e.md5:= SCUMMStringToMD5(AnsiString(t1));
		e.gameid:= t2;
		e.variant:= t3;
		e.extra:= t4;
		if not TryStrToInt(t5, e.filesize) then
			e.filesize:= -1;
		e.language:= TSCUMMCoreLanguage(GetEnumValue(FTypeLanguage, t6));
		e.platfrm:= TSCUMMCorePlatform(GetEnumValue(FTypePlatform, t7));

		New(p);
		p^.Assign(e);
		FEntries.Add(p);
		end;
	end;

procedure TSCUMMKBMD5Mapping.Clear;
	var
	e: PSCUMMGameMD5Map;
	i: Integer;

	begin
	for i:= FEntries.Count - 1 downto 0 do
		begin
		e:= PSCUMMGameMD5Map(FEntries[i]);
		Dispose(e);
		end;

	FEntries.Clear;
	end;

constructor TSCUMMKBMD5Mapping.Create;
	begin
	inherited Create;

	FEntries:= TList.Create;

	FTypeLanguage:= TypeInfo(TSCUMMCoreLanguage);
	FTypePlatform:= TypeInfo(TSCUMMCorePlatform);
	end;

destructor TSCUMMKBMD5Mapping.Destroy;
	begin
	Clear;
	FEntries.Free;

	inherited;
	end;

function TSCUMMKBMD5Mapping.FindEntryMD5(
		const AMD5: TSCUMMMD5): PSCUMMGameMD5Map;
	var
	i: Integer;

	begin
	Result:= nil;

	for i:= 0 to FEntries.Count - 1 do
		if  CompareMem(@PSCUMMGameMD5Map(FEntries[i])^.md5, @AMD5,
				SizeOf(TSCUMMMD5)) then
			begin
			Result:= PSCUMMGameMD5Map(FEntries[i]);
			Exit;
			end;
	end;

class function TSCUMMKBMD5Mapping.GetDescription: string;
	begin
	Result:= 'Game MD5 Mapping';
	end;

function TSCUMMKBMD5Mapping.GetEntries(const AIndex: Integer): PSCUMMGameMD5Map;
	begin
	Result:= PSCUMMGameMD5Map(FEntries[AIndex]);
	end;

function TSCUMMKBMD5Mapping.GetEntryCount: Integer;
	begin
	Result:= FEntries.Count;
	end;

class function TSCUMMKBMD5Mapping.GetFilename: string;
	begin
	Result:= 'GMMEntries';
	end;

class function TSCUMMKBMD5Mapping.GetName: string;
	begin
	Result:= 'GMMEntries';
	end;

procedure TSCUMMKBMD5Mapping.ReadEntryFromStream(const AStream: TStream);
	var
	e: TSCUMMGameMD5Map;
	p: PSCUMMGameMD5Map;
	i: Integer;

	begin
	AStream.Read(e.md5, SizeOf(TSCUMMMD5));

	e.gameid:= ReadStringFromStream(AStream);
	e.variant:= ReadStringFromStream(AStream);
	e.extra:= 	ReadStringFromStream(AStream);

	AStream.Read(e.filesize, SizeOf(Integer));

	AStream.Read(i, SizeOf(Integer));
	e.language:= TSCUMMCoreLanguage(i);

	AStream.Read(i, SizeOf(Integer));
	e.platfrm:= TSCUMMCorePlatform(i);

	New(p);
	p^.Assign(e);

	FEntries.Add(p);
	end;

procedure TSCUMMKBMD5Mapping.ReadFromStream(const AStream: TStream);
	begin
	while AStream.Position < AStream.Size do
		ReadEntryFromStream(AStream);
	end;

procedure TSCUMMKBMD5Mapping.WriteEntryToStream(const AIndex: Integer;
		const AStream: TStream);
	var
	p: PSCUMMGameMD5Map;
	i: Integer;

	begin
	p:= PSCUMMGameMD5Map(FEntries[AIndex]);

	AStream.Write(p^.md5, SizeOf(TSCUMMMD5));

	WriteStringToStream(p^.gameid, AStream);
	WriteStringToStream(p^.variant, AStream);
	WriteStringToStream(p^.extra, AStream);

	AStream.Write(p^.filesize, SizeOf(Integer));

	i:= Integer(Ord(p^.language));
	AStream.Write(i, SizeOf(Integer));

	i:= Integer(Ord(p^.platfrm));
	AStream.Write(i, SizeOf(Integer));
	end;

procedure TSCUMMKBMD5Mapping.WriteEntryToString(const AIndex: Integer;
		var AString: string);
	var
	p: PSCUMMGameMD5Map;

	begin
	p:= PSCUMMGameMD5Map(FEntries[AIndex]);

	AString:= AString +
			MakeStringField(string(SCUMMMD5ToString(p^.md5))) + #$09 +
			MakeStringField(p^.gameid) + #$09 +
			MakeStringField(p^.variant) + #$09 +
			MakeStringField(p^.extra) + #$09 +
			IntToStr(p^.filesize) + #$09 +
			GetEnumName(FTypeLanguage, Ord(p^.language)) + #$09 +
			GetEnumName(FTypePlatform, Ord(p^.platfrm)) + #$0D#$0A;
	end;

{ TSCUMMKBGameVariants }

type
//todo dengland I need to verify that this will work out alright
	TSCUMMHackGUIOpts = record
		case Boolean of
			True: (int64: TInt64Set);
			False: (guiOpts: TSCUMMGUIOpts)
	end;


procedure TSCUMMKBGameVariants.AddEntryFromString(const AString: string);
	var
	e: TSCUMMGameProps;
	p: PSCUMMGameProps;
	i,
	j: Integer;
	t1,
	t2,
	t3,
	t4,
	t5,
	t6,
	t7,
	t8,
	t9,
	t10: string;
	h: TSCUMMHackGUIOpts;

	begin
	i:= 1;
	if  GetNextToken(AString, i, t1)
	and GetNextToken(AString, i, t2)
	and GetNextToken(AString, i, t3)
	and GetNextToken(AString, i, t4)
	and GetNextToken(AString, i, t5)
	and GetNextToken(AString, i, t6)
	and GetNextToken(AString, i, t7)
	and GetNextToken(AString, i, t8)
	and GetNextToken(AString, i, t9)
	and GetNextToken(AString, i, t10) then
		begin
		e.name:= t1;
		e.variant:= t2;
		e.prefTag:= t3;
		e.id:= TSCUMMCoreGame(GetEnumValue(FTypeGame, t4));
		e.ver:= TSCUMMCoreVersion(GetEnumValue(FTypeVersion, t5));
		if not TryStrToInt(t6, j) then
			j:= 0;
		e.subv:= j;
		e.musicFlgs:= TSCUMMMusicFlags(Word(StringToSet(FTypeMusicFlags, t7)));
		e.feat:= TSCUMMCoreFeatures(Word(StringToSet(FTypeFeatures, t8)));
		e.plat:= TSCUMMCorePlatform(GetEnumValue(FTypePlatform, t9));

		h.int64:= TInt64Set(StringToSet64(FTypeGUIOpt, AnsiString(t10)));
		e.guiOpts:= h.guiOpts;

		New(p);
		p^.Assign(e);
		FEntries.Add(p);
		end;
	end;

procedure TSCUMMKBGameVariants.Clear;
	var
	e: PSCUMMGameProps;
	i: Integer;

	begin
	for i:= FEntries.Count - 1 downto 0 do
		begin
		e:= PSCUMMGameProps(FEntries[i]);
		Dispose(e);
		end;

	FEntries.Clear;
	end;

constructor TSCUMMKBGameVariants.Create;
	begin
	inherited Create;

	FEntries:= TList.Create;

	FTypeGame:= TypeInfo(TSCUMMCoreGame);
	FTypeVersion:= TypeInfo(TSCUMMCoreVersion);
	FTypeMusicFlags:= TypeInfo(TSCUMMMusicFlags);
	FTypeFeatures:= TypeInfo(TSCUMMCoreFeatures);
	FTypePlatform:= TypeInfo(TSCUMMCorePlatform);
//	Needs to be the enum type not the set type
	FTypeGUIOpt:= TypeInfo(TSCUMMGUIOpt);
	end;

destructor TSCUMMKBGameVariants.Destroy;
	begin
	Clear;
	FEntries.Free;

	inherited;
	end;

class function TSCUMMKBGameVariants.GetDescription: string;
	begin
	Result:= 'Game Variants';
	end;

function TSCUMMKBGameVariants.GetEntries(
		const AIndex: Integer): PSCUMMGameProps;
	begin
	Result:= PSCUMMGameProps(FEntries[AIndex]);
	end;

function TSCUMMKBGameVariants.GetEntryCount: Integer;
	begin
	Result:= FEntries.Count;
	end;

class function TSCUMMKBGameVariants.GetFilename: string;
	begin
	Result:= 'GVTEntries';
	end;

class function TSCUMMKBGameVariants.GetName: string;
	begin
	Result:= 'GVTEntries';
	end;

procedure TSCUMMKBGameVariants.ReadEntryFromStream(const AStream: TStream);
	var
	e: TSCUMMGameProps;
	p: PSCUMMGameProps;
	i: Integer;
	w: Word;
	ll: Int64;
	h: TSCUMMHackGUIOpts;

	begin
	e.name:= ReadStringFromStream(AStream);
	e.variant:= ReadStringFromStream(AStream);
	e.prefTag:= ReadStringFromStream(AStream);

	AStream.Read(i, SizeOf(Integer));
	e.id:= TSCUMMCoreGame(i);

	AStream.Read(i, SizeOf(Integer));
	e.ver:= TSCUMMCoreVersion(i);

	AStream.Read(e.subv, SizeOf(Byte));

	AStream.Read(w, SizeOf(Word));
	e.musicFlgs:= TSCUMMMusicFlags(w);

	AStream.Read(w, SizeOf(Word));
	e.feat:= TSCUMMCoreFeatures(w);

	AStream.Read(i, SizeOf(Integer));
	e.plat:= TSCUMMCorePlatform(i);

	AStream.Read(ll, SizeOf(Int64));
	h.int64:= TInt64Set(ll);
	e.guiOpts:= h.guiOpts;

	New(p);
	p^.Assign(e);

	FEntries.Add(p);
	end;

procedure TSCUMMKBGameVariants.ReadFromStream(const AStream: TStream);
	begin
	while AStream.Position < AStream.Size do
		ReadEntryFromStream(AStream);
	end;

procedure TSCUMMKBGameVariants.WriteEntryToStream(const AIndex: Integer;
		const AStream: TStream);
	var
	p: PSCUMMGameProps;
	i: Integer;
	w: Word;
	ll: Int64;
	h: TSCUMMHackGUIOpts;

	begin
	p:= PSCUMMGameProps(FEntries[AIndex]);

	WriteStringToStream(p^.name, AStream);
	WriteStringToStream(p^.variant, AStream);
	WriteStringToStream(p^.prefTag, AStream);

	i:= Integer(Ord(p^.id));
	AStream.Write(i, SizeOf(Integer));

	i:= Integer(Ord(p^.ver));
	AStream.Write(i, SizeOf(Integer));

	AStream.Write(p^.subv, SizeOf(Byte));

	w:= Word(p^.musicFlgs);
	AStream.Write(w, SizeOf(Word));

	w:= Word(p^.feat);
	AStream.Write(w, SizeOf(Word));

	i:= Integer(Ord(p^.plat));
	AStream.Write(i, SizeOf(Integer));

	h.guiOpts:= p^.guiOpts;
	ll:= Int64(h.int64);
	AStream.Write(ll, SizeOf(Int64));
	end;

procedure TSCUMMKBGameVariants.WriteEntryToString(const AIndex: Integer;
		var AString: string);
	var
	p: PSCUMMGameProps;
	h: TSCUMMHackGUIOpts;

	begin
	p:= PSCUMMGameProps(FEntries[AIndex]);

	h.guiOpts:= p^.guiOpts;

	AString:= AString +
			MakeStringField(p^.name) + #$09 +
			MakeStringField(p^.variant) + #$09 +
			MakeStringField(p^.prefTag) + #$09 +
			GetEnumName(FTypeGame, Ord(p^.id)) + #$09 +
			GetEnumName(FTypeVersion, Ord(p^.ver)) + #$09 +
			IntToStr(p^.subv) + #$09 +
			MakeStringField(SetToString(FTypeMusicFlags, Word(p^.musicFlgs), True)) + #$09 +
			MakeStringField(SetToString(FTypeFeatures, Word(p^.feat), True)) + #$09 +
			GetEnumName(FTypePlatform, Ord(p^.plat)) + #$09 +
			MakeStringField(string(SetToString64(FTypeGUIOpt, h.int64, True))) + #$0D#$0A;
	end;
{ TSCUMMPluginMngr }

procedure TSCUMMPluginMngr.Clear;
	var
	i: Integer;

	begin
	for i:= FPlugins.Count - 1 downto 0 do
		Remove(i);
	end;

constructor TSCUMMPluginMngr.Create(const APath: string);
	var
	sda: TStringDynArray;
	s,
	p: string;
	i: Integer;
	c,
	d: PSCUMMPluginData;

	begin
	Assert(not Assigned(SCUMMPluginMngr));

	inherited Create;

	FPath:= TPath.GetDirectoryName(APath);
	FPlugins:= TList<PSCUMMPluginData>.Create;

//	Construct the core package info
	New(c);
	c^._file:= 'SCUMMExpCore.BPL';
	c^.hInst:= LoadPackage(c^._file);

	c^.query:= nil;
	c^.init:= nil;
	c^.prep:= nil;
	c^.rels:= nil;
	c^.finl:= nil;

	c^.desc.major:= VAL_SCUMMEXP_EXCORE_VMAJ;
	c^.desc.minor:= VAL_SCUMMEXP_EXCORE_VMIN;
	c^.desc.build:= VAL_SCUMMEXP_EXCORE_VBLD;
	c^.desc._type:= LIT_SCUMMEXP_EXCORE_VTYP;
	c^.desc.stage:= LIT_SCUMMEXP_EXCORE_VSTG;
	c^.desc.name:= LIT_SCUMMEXP_EXCORE_VNAM;
	c^.desc.desc:= LIT_SCUMMEXP_EXCORE_VDSC;

	FPlugins.Add(c);

//	Get the list of plugin files now
	try
		s:= TPath.DirectorySeparatorChar + LIT_SCUMMEXP_PLGPTH_LOCN;
		p:= FPath + s;

		sda:= TDirectory.GetFiles(p);
		for i:= Low(sda) to High(sda) do
			try
				if  CompareText(TPath.GetExtension(sda[i]), '.BPL') = 0 then
					begin
					New(d);

					d^._file:= s + TPath.DirectorySeparatorChar +
							TPath.GetFileName(sda[i]);
					UniqueString(d^._file);

					FillChar(d^.desc, SizeOf(TSCUMMPluginDesc), 0);

					FPlugins.Add(d);
					end
				else
					SCUMMExpLogDebug(sxkPluginMngr,
					'IGNORING file in plugins folder:  %s',
					[TPath.GetFileName(sda[i])]);

				except
				end;

		except
		end;

	SCUMMPluginMngr:= Self;

	FState:= spmReady;
	end;

destructor TSCUMMPluginMngr.Destroy;
	begin
	Assert(FState = spmDoneFinl);

	Clear;

	inherited;
	end;

procedure TSCUMMPluginMngr.FinlPlugins;
	var
	i: Integer;

	begin
//	Only do this when ready
	Assert(FState = spmDoneRelease);

//	Don't finalise the core
	for i:= FPlugins.Count - 1 downto 1 do
		begin
		FPlugins[i].finl;
		UnloadPackage(FPlugins[i].hInst);
		Remove(i);
		end;

	FState:= spmDoneFinl;
	end;

function TSCUMMPluginMngr.GetPluginCount: Integer;
	begin
	Result:= FPlugins.Count;
	end;

function TSCUMMPluginMngr.GetPluginDesc(AIndex: Integer): PSCUMMPluginDesc;
	begin
	Result:= @FPlugins[AIndex]^.desc;
	end;

function TSCUMMPluginMngr.GetPluginFileName(AIndex: Integer): string;
	begin
	Result:= FPlugins[AIndex]^._file;
	end;

procedure TSCUMMPluginMngr.InitPlugins;
	var
	i: Integer;

	begin
//	Only do this when ready
	Assert(FState = spmDoneQuery);

//	Don't initialise the core, need to always re-evaluate
	i:= 1;
	while i < FPlugins.Count do
		if not FPlugins[i]^.init then
			begin
			SCUMMExpLogWarn(sxkPluginMngr,
					'BAD RESULT initialising plugin:  %s.',
					[FPlugins[i]^._file]);
			Remove(i);
			end
		else
			begin
			SCUMMExpLogDebug(sxkPluginMngr,
					'Plugin initialised:  %s.', [FPlugins[i]^.desc.name]);
			Inc(i);
			end;

	FState:= spmDoneInit;
	end;

procedure TSCUMMPluginMngr.PrepPlugins;
	var
	i: Integer;

	begin
//	Only do this when ready
	Assert(FState = spmDoneInit);

//	Don't prepare the core
	for i:= 1 to FPlugins.Count - 1 do
		FPlugins[i].prep;

	FState:= spmDonePrep;
	end;

procedure TSCUMMPluginMngr.QueryPlugins;
	var
	f: Boolean;
	i: Integer;
	c,
	d: PSCUMMPluginDesc;

	begin
//	Only do this when ready
	Assert(FState = spmReady);

	c:= @FPlugins[0]^.desc;

//	Don't query the core, need to always re-evaluate
	i:= 1;
	while i < FPlugins.Count do
		begin
		f:= True;
		try
			FPlugins[i]^.hInst:= LoadPackage(FPath + FPlugins[i]^._file);

			if  FPlugins[i]^.hInst > 0 then
				begin
				FPlugins[i]^.query:= GetProcAddress(FPlugins[i]^.hInst,
						LIT_SCUMMEXP_PLGFNC_QUERY);
				FPlugins[i]^.init:= GetProcAddress(FPlugins[i]^.hInst,
						LIT_SCUMMEXP_PLGFNC_INIT);
				FPlugins[i]^.prep:= GetProcAddress(FPlugins[i]^.hInst,
						LIT_SCUMMEXP_PLGFNC_PREP);
				FPlugins[i]^.rels:= GetProcAddress(FPlugins[i]^.hInst,
						LIT_SCUMMEXP_PLGFNC_RELS);
				FPlugins[i]^.finl:= GetProcAddress(FPlugins[i]^.hInst,
						LIT_SCUMMEXP_PLGFNC_FINL);

				d:= @FPlugins[i]^.desc;

				if  Assigned(FPlugins[i]^.query)
				and Assigned(FPlugins[i]^.init)
				and Assigned(FPlugins[i]^.prep)
				and Assigned(FPlugins[i]^.rels)
				and Assigned(FPlugins[i]^.finl)
				and FPlugins[i].query(c, d) then
					begin
					f:= False;
					SCUMMExpLogInform(sxkPluginMngr,
							'Loaded plugin:  %s [%2.2d.%2.2d.%4.4d].',
							[d^.name, d^.major, d^.minor, d^.build]);
					end;
				end;

			except
			SCUMMExpLogWarn(sxkPluginMngr,
					'Error loading package:  "%s".', [FPlugins[i]^._file]);
			end;

		if  f then
			begin
			SCUMMExpLogDebug(sxkPluginMngr, 'Not a plugin library:  %s.',
					[FPlugins[i]^._file]);

			if  FPlugins[i]^.hInst > 0 then
				UnloadPackage(FPlugins[i]^.hInst);

			Remove(i);
			end
		else
			Inc(i);
		end;

	FState:= spmDoneQuery;
	end;

procedure TSCUMMPluginMngr.RelsPlugins;
	var
	i: Integer;

	begin
//	Only do this when ready
	Assert(FState = spmDonePrep);

//	Don't prepare the core
	for i:= 1 to FPlugins.Count - 1 do
		FPlugins[i].rels;

	FState:= spmDoneRelease;
	end;

procedure TSCUMMPluginMngr.Remove(AIndex: Integer);
	var
	d: PSCUMMPluginData;

	begin
	d:= FPlugins[AIndex];
	Dispose(d);
	FPlugins.Delete(AIndex);
	end;

end.
