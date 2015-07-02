unit SPUTMClasses;

interface

uses
	TypInfo, System.Generics.Collections, Contnrs, Classes,
	SCUMMKBClasses, SCUMMTypes, SCUMMClasses, SPUTMTypes, FrameSCUMMExpCustomViewer;


type
	TSCUMMKBStaticNodes = class(TSCUMMKBSection)
	private
		FIndex: TList<TSCUMMExpEnumType>;
		FEntries: TDictionary<TSCUMMExpEnumType, TSCUMMExpNodeInfoArr>;
		FTypeEnum,
		FTypeData: PTypeInfo;

	protected
		class function  GetName: string; override;
		class function  GetDescription: string; override;
		class function  GetFilename: string; override;

		function  GetEntryCount: Integer; override;
		function  GetEntries(
				const AEnum: TSCUMMExpEnumType): TSCUMMExpNodeInfoArr;

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

		property  Entries[const AEnum: TSCUMMExpEnumType]: TSCUMMExpNodeInfoArr
				read GetEntries; default;
	end;


//	A base type for decoding
	TSCUMMExpDecoder = class
//		Class must give a name
//		Class must list the versions it is capable of handling
//		Class must respond whether or not it can decode data for a particular
//			TSCUMMDetectorData

//		When created, must be given system file list via TSCUMMHostNode and
//			detection data via TSCUMMDetectorData

//		Given a TSCUMMExpEnumData and up to 3 index values, must give
//			TSCUMMExpDataType type, data in a TMemoryStream and a result whether
//			the decoding was successfull

	protected
		FHostNode: TSCUMMHostNode;
		FDetectData: TSCUMMDetectorData;

		class function  GetName: string; virtual; abstract;
		class function  GetDescription: string; virtual; abstract;

		class function  GetCompatibility(const ACallIdx: Integer;
				out ASupports: TSCUMMExpDecCompat): Boolean; virtual; abstract;

		class function  CanDecodeGame(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData): Boolean; virtual; abstract;

		function  GetGameDesc: string; virtual; abstract;

		procedure DecodePath(const AInfoPath: TSCUMMExpDecInfoPath); virtual; abstract;

	public
		constructor Create(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData);

		property  Name: string read GetName;
		property  Description: string read GetDescription;

		property  HostNode: TSCUMMHostNode read FHostNode;
		property  DetectData: TSCUMMDetectorData read FDetectData;
	end;

	TSCUMMExpDecoderClass = class of TSCUMMExpDecoder;

//	A type for doing the decoder reflection
	TSCUMMExpDecodeReflector = class
//		Accept TSCUMMExpDecoder descendants registration

//		Maintain registration list

//		Provide TSCUMMExpDecoder class best suited to a SCUMM version via
//			TSCUMMDetectorData
	private type
		TSCUMMExpDecClassArr = array of TSCUMMExpDecoderClass;

	private
//		FList: TList;
		FDecDict: TDictionary<TSCUMMExpDecCompat, TSCUMMExpDecClassArr>;

		function  DecInArray(const AArray: TSCUMMExpDecClassArr;
				ADecoder: TSCUMMExpDecoderClass): Boolean;

	public
		constructor Create;

		procedure AddDecoder(ADecoder: TSCUMMExpDecoderClass);

		function  GetDecoderForDetection(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData): TSCUMMExpDecoder;
	end;


//	A type for doing the custom viewer reflection
	TSCUMMExpViewerReflector = class
	private type
		TSCUMMExpViewerClassArr = array of TSCUMMExpViewerClass;

	private
		FViewerDict: TDictionary<TSCUMMExpViewerCompat, TSCUMMExpViewerClassArr>;

		function  ViewerInArray(const AArray: TSCUMMExpViewerClassArr;
				AViewer: TSCUMMExpViewerClass): Boolean;

	public
		constructor Create;

		procedure AddViewer(AViewer: TSCUMMExpViewerClass);

		function  GetViewerForPath(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData;
				APath: TSCUMMExpGlobIdArr): TSCUMMExpCustomViewerFrame;
	end;


//	A type for maintaining list of configured games.
	TSCUMMExpGames = class;

//	A type for doing the enumeration
	TSCUMMExpEnumerator = class
//		When created, must be given system file list via TSCUMMHostNode,
//			detection data via TSCUMMDetectorData and TSCUMMExpDecoder class?

//		When given a GUID path, must provide list of objects in that path
//			including information about GUID, TSCUMMExpEnumType,
//			TSCUMMExpDataType and name

//		When requested, must provide information about a specific object in a
//			GUID path with a GUID identifier, including TSCUMMExpEnumType,
//			TSCUMMExpDataType, name and TMemoryStream

	protected type
		TSCUMMEnumStaticIds = record
			staticId,
			nodeId: TSCUMMExpGlobId;
		end;

	protected
		FDecoder: TSCUMMExpDecoder;
		FGameId: TSCUMMExpGlobId;
		FKBSect: TSCUMMKBStaticNodes;
		FStaticIDs: TDictionary<TSCUMMEnumStaticIds, TSCUMMExpGlobId>;

		function  GetDecoder: TSCUMMExpDecoder;
		procedure GenerateStaticStructure(const APath: TSCUMMExpGlobIdArr;
				const AEnum: TSCUMMExpEnumType);

		property  Decoder: TSCUMMExpDecoder read GetDecoder;

	public
		constructor Create(AGames: TSCUMMExpGames; ADecoder: TSCUMMExpDecoder;
				AId: TSCUMMExpGlobId);
		destructor  Destroy; override;

		procedure DecodePath(const APath: TSCUMMExpGlobIdArr);
	end;

//	A type for maintaining list of configured games.
	TSCUMMExpGames = class
	private type
		PSCUMMExpGameRoot = ^TSCUMMExpGameRoot;
		TSCUMMExpGameRoot = record
//			id: TSCUMMExpGlobId;
			desc: string;
			hostNode: TSCUMMHostNode;
			enumerator: TSCUMMExpEnumerator;
			detectData: TSCUMMDetectorData;
		end;

	private
		FList: TSCUMMExpGlobIdArr;
		FGames: TDictionary<TSCUMMExpGlobId, TSCUMMExpGameRoot>;

	protected
		function  GetGameDesc(AID: TSCUMMExpGlobId): string;
		function  GetGameEnumerator(AID: TSCUMMExpGlobId): TSCUMMExpEnumerator;
		function  GetGameHostNode(AID: TSCUMMExpGlobId): TSCUMMHostNode;
		function  GetGameDetectData(AID: TSCUMMExpGlobId): TSCUMMDetectorData;

		procedure Clear;
		function  GetGames(AIndex: Integer): TSCUMMExpGlobId;
		function  GetCount: Integer;

	public
		constructor Create;
		destructor  Destroy; override;

		function  AddGame(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData): Boolean;

		property  GameCount: Integer read GetCount;
		property  Games[AIndex: Integer]: TSCUMMExpGlobId read GetGames; default;

		property  GameDesc[AID: TSCUMMExpGlobId]: string read GetGameDesc;
		property  GameEnumerator[AID: TSCUMMExpGlobId]: TSCUMMExpEnumerator
				read GetGameEnumerator;
		property  GameHostNode[AID: TSCUMMExpGlobId]: TSCUMMHostNode
				read GetGameHostNode;
		property  GameDetectData[AID: TSCUMMExpGlobId]: TSCUMMDetectorData
				read GetGameDetectData;
	end;

//	A type for caching the game enumeration.
	TSCUMMExpEnumCache = class
	protected type
		TSCUMMExpGlobIdTNode = class
		protected
			FData: TSCUMMExpObjData;
			FChilds: TSCUMMExpGlobIdArr;

		public
			constructor Create;
			destructor  Destroy; override;
		end;

	protected
		FTree: TObjectDictionary<TSCUMMExpGlobIds, TSCUMMExpGlobIdTNode>;

//		procedure WalkNodesForPath(const APath: TSCUMMExpGlobIdArr;
//				out ALastIndex: Integer; out ALastNode: TSCUMMExpGlobIdTNode);

		procedure AddNodeForPath(const APath: TSCUMMExpGlobIdArr;
				ANode: TSCUMMExpGlobIdTNode);
		function  GetNodeForPath(
				const APath: TSCUMMExpGlobIdArr): TSCUMMExpGlobIdTNode;

	public
		constructor Create;
		destructor  Destroy; override;

		function  IsInCache(const APath: TSCUMMExpGlobIdArr): Boolean;
		function  GetDataForPath(
				const APath: TSCUMMExpGlobIdArr): PSCUMMExpObjData;
		procedure SetDataForPath(const APath: TSCUMMExpGlobIdArr;
				const AData: TSCUMMExpObjData);
	end;

//	An abstract cache iterator
	TSCUMMExpCacheIterator = class
	protected
		FIndex: Integer;
		FCount: Integer;
		FPath: TSCUMMExpGlobIds;
		FParent: TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode;

		procedure Prepare; virtual; abstract;
		function  IsThisMatch: Boolean; virtual; abstract;

		function  GetThisData: PSCUMMExpObjData;
		procedure TestNext;

	public
		constructor Create(const APath: TSCUMMExpGlobIdArr);
		destructor  Destroy; override;

		procedure First;
		procedure Next;
		function  IsEndOfList: Boolean;

		property  ThisData: PSCUMMExpObjData read GetThisData;
		property  Index: Integer read FIndex;
	end;

//	A type for iterating the data for each node in a path
	TSCUMMExpCacheNodeIterator = class(TSCUMMExpCacheIterator)
	protected
		procedure Prepare; override;
		function  IsThisMatch: Boolean; override;
	end;

//	A type for iterating the data items in a path
	TSCUMMExpCacheDataIterator = class(TSCUMMExpCacheIterator)
	protected
		procedure Prepare; override;
		function  IsThisMatch: Boolean; override;
	end;

var
	SCUMMExpGames: TSCUMMExpGames;
	SCUMMExpDecodeReflector: TSCUMMExpDecodeReflector;
	SCUMMExpViewerReflector: TSCUMMExpViewerReflector;
	SCUMMExpEnumCache: TSCUMMExpEnumCache;


implementation

uses
	SysUtils, Vcl.Forms, SCUMMLogTypes, SPUTMStrs;

{ TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode }

constructor TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode.Create;
	begin
	inherited;

//	FNodes:= TObjectList.Create;
//	FNodes.OwnsObjects:= True;

//	FData.id:= VAL_SCUMMEXP_GLOBID_NULL;
	FData.name:= '';
	FData.enumType:= sxeUnknown;
	FData.dataType:= sxdInvalid;

	FData.decoded:= False;
	FData.index:= -1;
	end;

destructor TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode.Destroy;
	begin
//	FNodes.Clear;
//	FNodes.Free;

	inherited;
	end;

{ TSCUMMExpEnumCache }

procedure TSCUMMExpEnumCache.AddNodeForPath(const APath: TSCUMMExpGlobIdArr;
		ANode: TSCUMMExpGlobIdTNode);
	var
	k: TSCUMMExpGlobIds;
	p: TSCUMMExpGlobIdTNode;
	i: TSCUMMExpGlobId;

	begin
	k.gameId:= APath[Low(APath)];

	if Length(APath) = 1 then
		k.nodeId:= VAL_SCUMMEXP_GLOBID_NULL
	else
		k.nodeId:= APath[High(APath)];

	if  not FTree.ContainsKey(k) then
		begin
//		if Length(APath) > 1 then
//			ANode.FData.id:= k.nodeId;
		FTree.Add(k, ANode);

		i:= k.nodeId;

//		Add child references
		if Length(APath) > 1 then
			begin
			if  Length(APath) = 2 then
				k.nodeId:= VAL_SCUMMEXP_GLOBID_NULL
			else
				k.nodeId:= APath[High(APath) - 1];

			if  FTree.ContainsKey(k) then
				begin
				p:= FTree[k];

				SetLength(p.FChilds, Length(p.FChilds) + 1);
				p.FChilds[High(p.FChilds)]:= i;
				end
			else
				SCUMMExpLogDebug(sxkEnumCache,
						'Missing parent key for node "%s".', [ANode.FData.name]);
			end;

		end;
	end;

constructor TSCUMMExpEnumCache.Create;
	begin
	inherited;

	FTree:= TObjectDictionary<TSCUMMExpGlobIds, TSCUMMExpGlobIdTNode>.Create;
	end;

destructor TSCUMMExpEnumCache.Destroy;
	begin
	FTree.Clear;
	FTree.Free;

	inherited;
	end;

function TSCUMMExpEnumCache.GetDataForPath(
		const APath: TSCUMMExpGlobIdArr): PSCUMMExpObjData;
	var
//	i: Integer;
	n: TSCUMMExpGlobIdTNode;
	k: TSCUMMExpGlobIds;

	begin
	k.gameId:= APath[Low(APath)];
	if  Length(APath) > 1 then
		k.nodeId:= APath[High(APath)]
	else
		k.nodeId:= VAL_SCUMMEXP_GLOBID_NULL;

	if  FTree.TryGetValue(k, n) then
		Result:= @n.FData
	else
		Result:= nil;
	end;

function TSCUMMExpEnumCache.GetNodeForPath(
		const APath: TSCUMMExpGlobIdArr): TSCUMMExpGlobIdTNode;
	var
//	i,
//	j: Integer;
	n: TSCUMMExpGlobIdTNode;
	k: TSCUMMExpGlobIds;

	begin
	n:= nil;

	k.gameId:= APath[Low(APath)];
	if  Length(APath) > 1 then
		k.nodeId:= APath[High(APath)]
	else
		k.nodeId:= VAL_SCUMMEXP_GLOBID_NULL;

	if  FTree.TryGetValue(k, n) then
		Result:= n
	else
		begin
//		if  i = -1 then
//			t:= FTree
//		else
//			t:= n.FNodes;

//fixme dengland Need to use enumerator to do this

//		for j:= i + 1 to High(APath) do
//			begin
//			p:= TSCUMMExpGlobIdTNode.Create;
//
//			p.FData.id:= APath[j];
//			p.FIndex:= t.Count;
//
//			t.Add(p);
//			n:= p;
//			t:= n.FNodes;
//			end;

		Result:= n;
		end;

	end;

function TSCUMMExpEnumCache.IsInCache(const APath: TSCUMMExpGlobIdArr): Boolean;
	var
	k: TSCUMMExpGlobIds;

	begin
	k.gameId:= APath[Low(APath)];
	if  Length(APath) > 1 then
		k.nodeId:= APath[High(APath)]
	else
		k.nodeId:= VAL_SCUMMEXP_GLOBID_NULL;

	Result:= FTree.ContainsKey(k);
	end;

procedure TSCUMMExpEnumCache.SetDataForPath(const APath: TSCUMMExpGlobIdArr;
		const AData: TSCUMMExpObjData);
	var
	n: TSCUMMExpGlobIdTNode;

	begin
	n:= GetNodeForPath(APath);
	if  not Assigned(n) then
		begin
		n:= TSCUMMExpGlobIdTNode.Create;
		n.FData.id:= APath[High(APath)];
		n.FData.index:= -1;

		AddNodeForPath(APath, n);
		end;

	SCUMMExpAssignObjData(AData, n.FData);
	end;

//procedure TSCUMMExpEnumCache.WalkNodesForPath(const APath: TSCUMMExpGlobIdArr;
//		out ALastIndex: Integer; out ALastNode: TSCUMMExpGlobIdTNode);
//
//	function FindTreeNode(APath: TSCUMMExpGlobIdArr; AIndex: Integer;
//			out AList: TObjectList; out ALastIndex: Integer;
//			out ALastNode: TSCUMMExpGlobIdTNode): Boolean;
//		var
//		i: Integer;
//		n: TSCUMMExpGlobIdTNode;
//
//		begin
//		Result:= False;
//
//		for i:= 0 to AList.Count - 1 do
//			begin
//			n:= TSCUMMExpGlobIdTNode(AList.Items[i]);
//
//			if  CompareStr(GUIDToString(n.FData.id),
//					GUIDToString(APath[AIndex])) = 0 then
//				begin
//				ALastIndex:= AIndex;
//				ALastNode:= n;
//				AList:= n.FNodes;
//				Result:= True;
//				Break;
//				end;
//			end;
//		end;
//
//	var
//	i: Integer;
//	t: TObjectList;
//
//	begin
//	t:= FTree;
//
//	ALastIndex:= -1;
//	ALastNode:= nil;
//
//	for i:= 0 to High(APath) do
//		if  not FindTreeNode(APath, i, t, ALastIndex, ALastNode) then
//			Exit;
//	end;

{ TSCUMMExpCacheIterator }

constructor TSCUMMExpCacheIterator.Create(const APath: TSCUMMExpGlobIdArr);
	begin
	inherited Create;

	FParent:= SCUMMExpEnumCache.GetNodeForPath(APath);
	Assert(Assigned(FParent), 'Cache iterator is invalid.');

	FCount:= Length(FParent.FChilds);

	FPath.gameId:= APath[Low(APath)];
	if  Length(APath) > 1 then
		FPath.nodeId:= APath[High(APath)]
	else
		FPath.nodeId:= VAL_SCUMMEXP_GLOBID_NULL;

	Prepare;
	First;
	end;

destructor TSCUMMExpCacheIterator.Destroy;
	begin

	inherited;
	end;

procedure TSCUMMExpCacheIterator.First;
	begin
	if  FCount = 0 then
		FIndex:= -1
	else
		begin
		FIndex:= 0;
		FPath.nodeId:= FParent.FChilds[FIndex];

		TestNext;
		end;
	end;

function TSCUMMExpCacheIterator.GetThisData: PSCUMMExpObjData;
	begin
	if  not IsEndOfList then
		Result:= @SCUMMExpEnumCache.FTree[FPath].FData
	else
		Result:= nil;
	end;

function TSCUMMExpCacheIterator.IsEndOfList: Boolean;
	begin
	Result:= (FCount = 0) or (FIndex >= FCount);
	end;

procedure TSCUMMExpCacheIterator.Next;
	begin
	Inc(FIndex);
	if  not IsEndOfList then
		begin
		FPath.nodeId:= FParent.FChilds[FIndex];
		TestNext;
		end;
	end;

procedure TSCUMMExpCacheIterator.TestNext;
	begin
	while (not IsEndOfList) and (not IsThisMatch) do
		begin
		Inc(FIndex);
		FPath.nodeId:= FParent.FChilds[FIndex];
		end;
	end;

{ TSCUMMExpCacheNodeIterator }

function TSCUMMExpCacheNodeIterator.IsThisMatch: Boolean;
	var
	n: TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode;

	begin
	Result:= False;

	if  SCUMMExpEnumCache.FTree.TryGetValue(FPath, n) then
		Result:= n.FData.enumType in SET_SCUMMEXP_ENMTYP_NODETYPE;
	end;

procedure TSCUMMExpCacheNodeIterator.Prepare;
	begin
//	if  Length(FPath) = 0 then
//		FList:= SCUMMExpEnumCache.FTree
//	else
//		FList:= SCUMMExpEnumCache.GetNodeForPath(FPath).FNodes;
	end;

{ TSCUMMExpCacheDataIterator }

function TSCUMMExpCacheDataIterator.IsThisMatch: Boolean;
	var
	n: TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode;

	begin
	Result:= False;

	if  SCUMMExpEnumCache.FTree.TryGetValue(FPath, n) then
		Result:= n.FData.enumType in SET_SCUMMEXP_ENMTYP_DATATYPE;
	end;

procedure TSCUMMExpCacheDataIterator.Prepare;
	begin
//	if  Length(FPath) = 0 then
//		FList:= SCUMMExpEnumCache.FTree
//	else
//		FList:= SCUMMExpEnumCache.GetNodeForPath(FPath).FNodes;
	end;

{ TSCUMMExpDecoder }

constructor TSCUMMExpDecoder.Create(AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData);
	begin
	inherited Create;

	FHostNode:= AHostNode;
	FDetectData:= ADetectData;
	end;

{ TSCUMMExpDecodeReflector }

procedure TSCUMMExpDecodeReflector.AddDecoder(ADecoder: TSCUMMExpDecoderClass);
	var
	i: Integer;
	dc: TSCUMMExpDecodeReflector.TSCUMMExpDecClassArr;
	kc: TSCUMMExpDecCompat;

	begin
	i:= 0;
	SetLength(dc, 0);

	FillChar(kc, SizeOf(TSCUMMExpDecCompat), 0);

	while ADecoder.GetCompatibility(i, kc) do
		begin
		if  FDecDict.TryGetValue(kc, dc) then
			if  not DecInArray(dc, ADecoder) then
				begin
				SetLength(dc, Length(dc) + 1);
				dc[High(dc)]:= ADecoder;
				FDecDict[kc]:= dc;

				SCUMMExpLogInform(sxkReflector,
						'Appending decoder "%s" to supports list.',
						[ADecoder.GetName]);
				end
			else
				SCUMMExpLogWarn(sxkReflector,
						'Skipping decoder due to having been already installed!',
						[])
		else
			begin
			SetLength(dc, 1);
			dc[High(dc)]:= ADecoder;
			FDecDict.Add(kc, dc);

			SCUMMExpLogInform(sxkReflector,
					'Adding decoder "%s" to supports list.',
					[ADecoder.GetName]);
			end;

		FillChar(kc, SizeOf(TSCUMMExpDecCompat), 0);
		Inc(i);
		end;
	end;

constructor TSCUMMExpDecodeReflector.Create;
	begin
	Assert(not Assigned(SCUMMExpDecodeReflector));

	inherited;

	FDecDict:= TDictionary<TSCUMMExpDecCompat, TSCUMMExpDecClassArr>.Create;

	SCUMMExpDecodeReflector:= Self;
	end;

function TSCUMMExpDecodeReflector.DecInArray(
		const AArray: TSCUMMExpDecClassArr;
		ADecoder: TSCUMMExpDecoderClass): Boolean;
	var
	i: Integer;

	begin
	Result:= False;

	for i:= Low(AArray) to High(AArray) do
		if  AArray[i] = ADecoder then
			begin
			Result:= True;
			Break;
			end;
	end;

function TSCUMMExpDecodeReflector.GetDecoderForDetection(
		AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData): TSCUMMExpDecoder;
	var
	i: Integer;
	dc,
	kc: TSCUMMExpDecCompat;
	da: TSCUMMExpDecClassArr;

	begin
	Result:= nil;

	dc.game:= ADetectData.game.id;
	dc.vers:= ADetectData.game.ver;
	dc.subv:= ADetectData.game.subv;
	dc.feat:= ADetectData.game.feat;
	dc.plat:= ADetectData.game.plat;

	for kc in FDecDict.Keys do
		if  ((dc.game = scgUnk) or (kc.game = scgUnk) or (dc.game = kc.game))
		and ((dc.vers = scvUnk) or (kc.vers = scvUnk) or (dc.vers = kc.vers))
		and ((dc.subv = 0) or (kc.subv = 0) or (dc.subv = kc.subv))
		and ((dc.feat = []) or (kc.feat = []) or ((dc.feat * kc.feat) <> []))
		and ((dc.plat = scpUnk) or (kc.plat = scpUnk) or (dc.plat = kc.plat)) then
			begin
			da:= FDecDict[kc];
			for i:= Low(da) to High(da) do
				if  da[i].CanDecodeGame(AHostNode, ADetectData) then
					begin
					Result:= da[i].Create(AHostNode, ADetectData);
					Exit;
					end;
			end;
	end;

{ TSCUMMExpEnumerator }

constructor TSCUMMExpEnumerator.Create(AGames: TSCUMMExpGames;
		ADecoder: TSCUMMExpDecoder; AId: TSCUMMExpGlobId);
	var
	p: TSCUMMExpGlobIdArr;

	begin
	Assert(AGames = SCUMMExpGames);
	Assert(Assigned(ADecoder));

//fixme dengland Need assert for Id not in cache

	inherited Create;

	FKBSect:= SCUMMKnowledgeBase.GetSectionOfClass(TSCUMMKBStaticNodes) as
			TSCUMMKBStaticNodes;

	FDecoder:= ADecoder;
	FGameId:= AId;
	FStaticIDs:= TDictionary<TSCUMMEnumStaticIds, TSCUMMExpGlobId>.Create;

	SetLength(p, 1);
	p[0]:= FGameId;
	DecodePath(p);
	end;

procedure TSCUMMExpEnumerator.DecodePath(const APath: TSCUMMExpGlobIdArr);
	var
	j: Integer;
	i: TSCUMMExpDecInfoPath;
	p: TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode;
	u: TSCUMMExpGlobIdArr;

	begin
//	Check that path is for our game
	if  (Length(APath) = 0)
	or  not CompareMem(@APath[0], @FGameId, SizeOf(TSCUMMExpGlobId)) then
		Exit;

//	Check for static nodes that need to be created
	p:= SCUMMExpEnumCache.GetNodeForPath(APath);

//	Need to check if creating game node as well as its top level items
	if (not Assigned(p)) then
		if  Length(APath) = 1 then
			begin
			p:= TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode.Create;

			p.FData.id:= FGameId;
			p.FData.name:= FDecoder.GetGameDesc;
			p.FData.enumType:= sxeRootCntnr;
			p.FData.index:= -1;

			SCUMMExpEnumCache.AddNodeForPath(APath, p);
			end;

//	Create static nodes
	if  not p.FData.decoded then
		begin
		GenerateStaticStructure(APath, p.FData.enumType);

//		Create path for decoder
		SetLength(u, 2);
		u[0]:= APath[0];

		SetLength(i, Length(APath));
		for j:= 0 to High(APath) do
			begin
			if  j = 0 then
				u[1]:= VAL_SCUMMEXP_GLOBID_NULL
			else
				u[1]:= APath[j];

			p:= SCUMMExpEnumCache.GetNodeForPath(u);

			i[j].id:= p.FData.id;
			i[j].enumType:= p.FData.enumType;
			i[j].index:= p.FData.index;
			end;

//		Pass to decoder
		FDecoder.DecodePath(i);
		end;
	end;

destructor TSCUMMExpEnumerator.Destroy;
	begin
//fixme dengland Need to clear cached objects.

//fixme Does this leak memory?
	FStaticIDs.Free;

//fixme dengland Should this be n TSCUMMExpGames?
	FDecoder.Free;

	inherited;
	end;

procedure TSCUMMExpEnumerator.GenerateStaticStructure(
		const APath: TSCUMMExpGlobIdArr; const AEnum: TSCUMMExpEnumType);
	var
	i: Integer;
	v: TSCUMMExpNodeInfoArr;
	p: TSCUMMExpGlobIdArr;
	n: TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode;
	s: TSCUMMEnumStaticIds;
	g: TSCUMMExpGlobId;

	begin
	SetLength(p, Length(APath) + 1);
	for i:= 0 to High(APath) do
		p[i]:= APath[i];

	v:= FKBSect.GetEntries(AEnum);

	for i:= 0 to High(v) do
		begin
//dengland This feels something like a hack but it also seems acceptable.  The
//		EnumCache expects that each node/object has a unique id for each game.
//		Statically defined ones aren't adhering to this notion so we need a
//		dictionary to intercept the static ids, generating a unique id for each
//		instance (static nodes/objects can appear mulitple times for different
//		parent nodes).
		s.staticId:= v[i].id;
		s.nodeId:= p[Pred(High(p))];
		if  not FStaticIDs.TryGetValue(s, g) then
			begin
			g:= SCUMMExpCreateGlobID;
			FStaticIDs.Add(s, g);
			end;

//		p[High(p)]:= v[i].id;
		p[High(p)]:= g;
		n:= SCUMMExpEnumCache.GetNodeForPath(p);

		if  not Assigned(n) then
			begin
			n:= TSCUMMExpEnumCache.TSCUMMExpGlobIdTNode.Create;

//			n.FData.id:= v[i].id;
			n.FData.id:= g;
			n.FData.name:= v[i].name;
			n.FData.enumType:= v[i].enumType;
			n.FData.dataType:= v[i].dataType;

//			n.FData.decoded:= v[i].dataType = sxdNode;
			n.FData.decoded:= False;
			n.FData.index:= -1;

			SCUMMExpEnumCache.AddNodeForPath(p, n);
			end;
		end;
	end;

function TSCUMMExpEnumerator.GetDecoder: TSCUMMExpDecoder;
	begin
	Result:= FDecoder;
	end;


{ TSCUMMExpGames }

function TSCUMMExpGames.AddGame(AHostNode: TSCUMMHostNode;
		 ADetectData: TSCUMMDetectorData): Boolean;
	var
	d: TSCUMMExpDecoder;
	e: TSCUMMExpEnumerator;
	i: TSCUMMExpGlobId;
	p: PSCUMMExpGameRoot;

	begin
	e:= nil;
	Result:= False;

	try
		d:= SCUMMExpDecodeReflector.GetDecoderForDetection(AHostNode, ADetectData);
		if  Assigned(d) then
			begin
			i:= SCUMMExpCreateGlobID;
			e:= TSCUMMExpEnumerator.Create(Self, d, i);
			end;

		if  Assigned(e) then
			begin
			New(p);
			p^.enumerator:= e;
			p^.desc:= d.GetGameDesc;

//fixme dengland Do I need these here?
			p^.hostNode:= AHostNode;
			p^.detectData:= ADetectData;

//fixme dengland Should I be using a pointer type for the dictionary and
//		manually freeing them?
			FGames.Add(i, p^);

			SetLength(FList, Length(FList) + 1);
			FList[High(FList)]:= i;

			Result:= True;
			end;

		except
		raise;
		end;
	end;

procedure TSCUMMExpGames.Clear;
	begin
	FGames.Clear;
	SetLength(FList, 0);
	end;

constructor TSCUMMExpGames.Create;
	begin
	Assert(not Assigned(SCUMMExpGames));

	inherited;

	SetLength(FList, 0);
	FGames:= TDictionary<TSCUMMExpGlobId, TSCUMMExpGameRoot>.Create;

	SCUMMExpGames:= Self;
	end;

destructor TSCUMMExpGames.Destroy;
	begin
	Clear;
	FGames.Free;

	SCUMMExpGames:= nil;

	inherited;
	end;

function TSCUMMExpGames.GetCount: Integer;
	begin
	Result:= Length(FList);
	end;

function TSCUMMExpGames.GetGameDesc(AID: TSCUMMExpGlobId): string;
	begin
	Result:= FGames[AID].desc;
	end;

function TSCUMMExpGames.GetGameDetectData(AID: TSCUMMExpGlobId): TSCUMMDetectorData;
	begin
	Result:= FGames[AID].detectData;
	end;

function TSCUMMExpGames.GetGameEnumerator(
		AID: TSCUMMExpGlobId): TSCUMMExpEnumerator;
	begin
	Result:= FGames[AID].enumerator;
	end;

function TSCUMMExpGames.GetGameHostNode(AID: TSCUMMExpGlobId): TSCUMMHostNode;
	begin
	Result:= FGames[AID].hostNode;
	end;

function TSCUMMExpGames.GetGames(AIndex: Integer): TSCUMMExpGlobId;
	begin
	Result:= FList[AIndex];
	end;

{ TSCUMMKBStaticNodes }

procedure TSCUMMKBStaticNodes.AddEntryFromString(const AString: string);
	var
	i,
	j,
	n: Integer;
	t1,
	t2,
	t3,
	t4: string;
	k: TSCUMMExpEnumType;
	v: TSCUMMExpNodeInfoArr;

	begin
	i:= 1;

//	We get the enum type key and the count of infos that follow
	if  GetNextToken(AString, i, t1)
	and GetNextToken(AString, i, t2) then
		if  TryStrToInt(t2, n) then
			begin
			k:= TSCUMMExpEnumType(GetEnumValue(FTypeEnum, t1));
			SetLength(v, n);

			for j:= 0 to n - 1 do
				if  GetNextToken(AString, i, t1)
				and GetNextToken(AString, i, t2)
				and GetNextToken(AString, i, t3)
				and GetNextToken(AString, i, t4) then
					begin
					v[j].id:= StringToGUID(t1);
					v[j].name:= t2;
					v[j].enumType:= TSCUMMExpEnumType(GetEnumValue(FTypeEnum, t3));
					v[j].dataType:= TSCUMMExpDataType(GetEnumValue(FTypeData, t4));
					end;

			FEntries.Add(k, v);
			FIndex.Add(k);
			end;
	end;

procedure TSCUMMKBStaticNodes.Clear;
	begin
	FIndex.Clear;
	FEntries.Clear;
	end;

constructor TSCUMMKBStaticNodes.Create;
	begin
	inherited;

//	Use an index to overcome the limitation of TEnumerable in that we will need
//		to iterate by index instead of key.
	FIndex:= TList<TSCUMMExpEnumType>.Create;
	FEntries:= TDictionary<TSCUMMExpEnumType, TSCUMMExpNodeInfoArr>.Create;
	FTypeEnum:= TypeInfo(TSCUMMExpEnumType);
	FTypeData:= TypeInfo(TSCUMMExpDataType);
	end;

destructor TSCUMMKBStaticNodes.Destroy;
	begin
	Clear;
	FEntries.Free;
	FIndex.Free;

	inherited;
	end;

class function TSCUMMKBStaticNodes.GetDescription: string;
	begin
	Result:= 'Enum Type Static Node Mappings.';
	end;

function TSCUMMKBStaticNodes.GetEntries(
		const AEnum: TSCUMMExpEnumType): TSCUMMExpNodeInfoArr;
	begin
	if not FEntries.TryGetValue(AEnum, Result) then
		SetLength(Result, 0);
	end;

function TSCUMMKBStaticNodes.GetEntryCount: Integer;
	begin
	Result:= FEntries.Count;
	end;

class function TSCUMMKBStaticNodes.GetFilename: string;
	begin
	Result:= 'ESNMapping';
	end;

class function TSCUMMKBStaticNodes.GetName: string;
	begin
	Result:= 'ESNMapping';
	end;

procedure TSCUMMKBStaticNodes.ReadEntryFromStream(const AStream: TStream);
	var
	i,
	j,
	n: Integer;
	k: TSCUMMExpEnumType;
	v: TSCUMMExpNodeInfoArr;

	begin
	AStream.Read(i, SizeOf(Integer));
	k:= TSCUMMExpEnumType(i);

	AStream.Read(n, SizeOf(Integer));
	SetLength(v, n);

	for j:= 0 to n - 1 do
		begin
		AStream.Read(v[j].id, SizeOf(TSCUMMExpGlobId));
		v[j].name:= ReadStringFromStream(AStream);
		AStream.Read(i, SizeOf(Integer));
		v[j].enumType:= TSCUMMExpEnumType(i);
		AStream.Read(i, SizeOf(Integer));
		v[j].dataType:= TSCUMMExpDataType(i);
		end;

	FEntries.Add(k, v);
	FIndex.Add(k);
	end;

procedure TSCUMMKBStaticNodes.ReadFromStream(const AStream: TStream);
	begin
	while AStream.Position < AStream.Size do
		ReadEntryFromStream(AStream);
	end;

procedure TSCUMMKBStaticNodes.WriteEntryToStream(const AIndex: Integer;
		const AStream: TStream);
	var
	i,
	j: Integer;
	k: TSCUMMExpEnumType;
	v: TSCUMMExpNodeInfoArr;

	begin
	k:= FIndex[AIndex];
	v:= FEntries[k];

	j:= Ord(k);
	AStream.Write(j, SizeOf(Integer));

	j:= Length(v);
	AStream.Write(j, SizeOf(Integer));

	for i:= 0 to High(v) do
		begin
		AStream.Write(v[i].id, SizeOf(TSCUMMExpGlobId));
		WriteStringToStream(v[i].name, AStream);
		j:= Ord(v[i].enumType);
		AStream.Write(j, SizeOf(Integer));
		j:= Ord(v[i].dataType);
		AStream.Write(j, SizeOf(Integer));
		end;
	end;

procedure TSCUMMKBStaticNodes.WriteEntryToString(const AIndex: Integer;
		var AString: string);
	var
	i,
	j: Integer;
	k: TSCUMMExpEnumType;
	v: TSCUMMExpNodeInfoArr;

	begin
	k:= FIndex[AIndex];
	v:= FEntries[k];

	j:= Length(v);

	AString:= AString +
			GetEnumName(FTypeEnum, Ord(k)) + #$09 +
			IntToStr(j) + #09;

	for i:= 0 to High(v) do
		begin
		AString:= AString +
				MakeStringField(GUIDToString(v[i].id)) + #$09 +
				MakeStringField(v[i].name) + #$09 +
				GetEnumName(FTypeEnum, Ord(v[i].enumType)) + #$09 +
				GetEnumName(FTypeData, Ord(v[i].dataType));

		if  i <> High(v) then
			AString:= AString + #$09;
		end;
	end;

{ TSCUMMExpViewerReflector }

procedure TSCUMMExpViewerReflector.AddViewer(AViewer: TSCUMMExpViewerClass);
	var
	i: Integer;
	vc: TSCUMMExpViewerReflector.TSCUMMExpViewerClassArr;
	kc: TSCUMMExpViewerCompat;

	begin
	i:= 0;
	SetLength(vc, 0);

	FillChar(kc, SizeOf(TSCUMMExpViewerCompat), 0);

	while AViewer.GetCompatibility(i, kc) do
		begin
		if  FViewerDict.TryGetValue(kc, vc) then
			if  not ViewerInArray(vc, AViewer) then
				begin
				SetLength(vc, Length(vc) + 1);
				vc[High(vc)]:= AViewer;
				FViewerDict[kc]:= vc;

				SCUMMExpLogInform(sxkReflector,
						'Appending viewer "%s" to supports list.',
						[AViewer.GetName]);
				end
			else
				SCUMMExpLogWarn(sxkReflector,
						'Skipping viewer due to having been already installed!',
						[])
		else
			begin
			SetLength(vc, 1);
			vc[High(vc)]:= AViewer;
			FViewerDict.Add(kc, vc);

			SCUMMExpLogInform(sxkReflector,
					'Adding viewer "%s" to supports list.',
					[AViewer.GetName]);
			end;

		FillChar(kc, SizeOf(TSCUMMExpViewerCompat), 0);
		Inc(i);
		end;
	end;

constructor TSCUMMExpViewerReflector.Create;
	begin
	Assert(not Assigned(SCUMMExpViewerReflector));

	inherited;

	FViewerDict:= TDictionary<TSCUMMExpViewerCompat, TSCUMMExpViewerClassArr>.Create;

	SCUMMExpViewerReflector:= Self;
	end;

function TSCUMMExpViewerReflector.GetViewerForPath(AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData; APath: TSCUMMExpGlobIdArr): TSCUMMExpCustomViewerFrame;
	var
	i: Integer;
	dc,
	kc: TSCUMMExpViewerCompat;
	da: TSCUMMExpViewerClassArr;
	d: PSCUMMExpObjData;

	begin
	Result:= nil;

	d:= SCUMMExpEnumCache.GetDataForPath(APath);

	dc.game:= ADetectData.game.id;
	dc.vers:= ADetectData.game.ver;
	dc.subv:= ADetectData.game.subv;
	dc.feat:= ADetectData.game.feat;
	dc.plat:= ADetectData.game.plat;
	dc.enum:= d^.enumType;

	for kc in FViewerDict.Keys do
		if  ((dc.game = scgUnk) or (kc.game = scgUnk) or (dc.game = kc.game))
		and ((dc.vers = scvUnk) or (kc.vers = scvUnk) or (dc.vers = kc.vers))
		and ((dc.subv = 0) or (kc.subv = 0) or (dc.subv = kc.subv))
		and ((dc.feat = []) or (kc.feat = []) or ((dc.feat * kc.feat) <> []))
		and ((dc.plat = scpUnk) or (kc.plat = scpUnk) or (dc.plat = kc.plat))
		and ((dc.enum = sxeUnknown) or (kc.enum = sxeUnknown) or (dc.enum = kc.enum)) then
			begin
			da:= FViewerDict[kc];
			for i:= Low(da) to High(da) do
				if  da[i].CanViewPath(AHostNode, ADetectData, APath) then
					begin
					Result:= da[i].Create(Application);
					Exit;
					end;
			end;
	end;

function TSCUMMExpViewerReflector.ViewerInArray(
		const AArray: TSCUMMExpViewerClassArr; AViewer: TSCUMMExpViewerClass): Boolean;
	var
	i: Integer;

	begin
	Result:= False;

	for i:= Low(AArray) to High(AArray) do
		if  AArray[i] = AViewer then
			begin
			Result:= True;
			Break;
			end;
	end;

end.
