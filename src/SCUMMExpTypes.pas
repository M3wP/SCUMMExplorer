unit SCUMMExpTypes;

interface

uses
	Classes, SCUMMLogTypes, SCUMMTypes, SCUMMClasses, SPUTMTypes, SPUTMClasses;

type
	PSCUMMExpLogMsg = ^TSCUMMExpLogMsg;
	TSCUMMExpLogMsg = record
		_Type: TSCUMMExpLogType;
		Time: TDateTime;
		Kind: TSCUMMExpLogKind;
		Msg: string;
	end;


procedure SCUMMExpInitialise;
procedure SCUMMExpFinalise;

procedure SCUMMExpQueryPlugins;
procedure SCUMMExpInitPlugins;
procedure SCUMMExpPrepPlugins;


procedure SCUMMExpLogHandler(const AType: TSCUMMExpLogType;
		const ATime: TDateTime; const AKind: TSCUMMExpLogKind;
		const AMsg: string);
procedure EmptySCUMMExpLogMsgList;


var
	SCUMMExpLogMsgList: TThreadList;


implementation

uses
	SysUtils, Forms, SCUMMExpStrs, SCUMMKBClasses;


procedure SCUMMExpInitialise;
	begin
	SCUMMPluginMngr:= TSCUMMPluginMngr.Create(Application.ExeName);

	SCUMMKnowledgeBase:= TSCUMMKnowledgeBase.Create;
	SCUMMKnowledgeBase.AddSection(TSCUMMKBFilenamePatterns);
	SCUMMKnowledgeBase.AddSection(TSCUMMKBMD5Mapping);
	SCUMMKnowledgeBase.AddSection(TSCUMMKBGameVariants);
	SCUMMKnowledgeBase.AddSection(TSCUMMKBStaticNodes);

	SCUMMExpEnumCache:= TSCUMMExpEnumCache.Create;
	SCUMMExpViewerReflector:= TSCUMMExpViewerReflector.Create;
	SCUMMExpDecodeReflector:= TSCUMMExpDecodeReflector.Create;
	SCUMMExpGames:= TSCUMMExpGames.Create;
	end;


procedure SCUMMExpFinalise;
	begin
	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_FINMSG_RELSCORE, []);
	FreeAndNil(SCUMMExpDecodeReflector);
	FreeAndNil(SCUMMExpEnumCache);
	FreeAndNil(SCUMMExpGames);

	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_FINMSG_RELSKNBS, []);
	FreeAndNil(SCUMMKnowledgeBase);

	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_FINMSG_RELSLOGI, []);
	SCUMMExpLogUninstallHandler;
	EmptySCUMMExpLogMsgList;
	FreeAndNil(SCUMMExpLogMsgList);
	end;

procedure SCUMMExpQueryPlugins;
	begin

	end;

procedure SCUMMExpInitPlugins;
	begin

	end;

procedure SCUMMExpPrepPlugins;
	begin

	end;


procedure SCUMMExpLogHandler(const AType: TSCUMMExpLogType;
		const ATime: TDateTime; const AKind: TSCUMMExpLogKind;
		const AMsg: string);
	var
	m: PSCUMMExpLogMsg;

	begin
	New(m);

	m^._Type:= AType;
	m^.Time:= ATime;
	m^.Kind:= AKind;
	m^.Msg:= AMsg;

	UniqueString(m^.Msg);

	with SCUMMExpLogMsgList.LockList do
		try
			Add(m);

			finally
			SCUMMExpLogMsgList.UnlockList;
			end;
	end;

procedure EmptySCUMMExpLogMsgList;
	var
	i: Integer;
	m: PSCUMMExpLogMsg;

	begin
	with SCUMMExpLogMsgList.LockList do
		try
			for i:= Count - 1 downto 0 do
				begin
				m:= PSCUMMExpLogMsg(Items[i]);
				System.Dispose(m);

				Delete(i);
				end;

			finally
			SCUMMExpLogMsgList.UnlockList;
			end;
	end;


end.
