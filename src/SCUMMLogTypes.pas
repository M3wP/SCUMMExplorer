unit SCUMMLogTypes;

interface

type
	TSCUMMExpLogType = (sxtDebug, sxtInform, sxtWarn, sxtError, sxtHalt);
	TSCUMMExpLogKind = (sxkUnknown, sxkPluginMngr, sxkExplorer,
			sxkKnowledgeBase, sxkDetector, sxkReflector, sxkEnumerator,
			sxkEnumCache, sxkDecoder, sxkDecompiler, sxkViewer);
	TSCUMMExpLogMask = set of TSCUMMExpLogKind;
	TSCUMMExpLogHandler = procedure(const AType: TSCUMMExpLogType;
			const ATime: TDateTime; const AKind: TSCUMMExpLogKind;
			const AMsg: string);

procedure SCUMMExpLogDebug(const cm: TSCUMMExpLogKind; msg: string; args: array of const);
procedure SCUMMExpLogInform(const cm: TSCUMMExpLogKind; msg: string; args: array of const);
procedure SCUMMExpLogWarn(const cm: TSCUMMExpLogKind; msg: string; args: array of const);
procedure SCUMMExpLogError(const cm: TSCUMMExpLogKind; msg: string; args: array of const);
procedure SCUMMExpLogAbort(const cm: TSCUMMExpLogKind; msg: string; args: array of const);

//fixme dengland This should probably get TTextRec's to be more flexible.  This
//		also requires that the internal fields be changed.  FPC compatibility
//		needs to be considered.
procedure SCUMMExpLogRedirect(const AOutput, AError: Text);
procedure SCUMMExpLogInstallHandler(const AHandler: TSCUMMExpLogHandler);
procedure SCUMMExpLogUninstallHandler;

var
	SCUMMExpLogMask: TSCUMMExpLogMask = [];


implementation

uses
	SysUtils;

type
	PText = ^Text;

var
	FLogHandler: TSCUMMExpLogHandler;
	FLogOut,
	FLogErr: PText;

procedure SCUMMExpLogRedirect(const AOutput, AError: Text);
	begin
	FLogOut:= @AOutput;
	FLogErr:= @AError;
	end;

procedure SCUMMExpLogInstallHandler(const AHandler: TSCUMMExpLogHandler);
	begin
	FLogHandler:= AHandler;
	end;

procedure SCUMMExpLogUninstallHandler;
	begin
	FLogHandler:= nil;
	end;

function  DoFormatMessage(const AMsg: string): string; inline;
	begin
	Result:= FormatDateTime('hh:nn:ss.zzz', Now) + ':  ' + AMsg;
	end;

procedure SCUMMExpLogDebug(const cm: TSCUMMExpLogKind; msg: string;
		args: array of const);
	var
	s: string;

	begin
	if  not (cm in SCUMMExpLogMask) then
		begin
		s:= Format(msg, args);

		if Assigned(FLogHandler) then
			FLogHandler(sxtDebug, Now, cm, s)
		else if Assigned(FLogOut) then
			Writeln(FLogOut^, DoFormatMessage(s));
		end;
	end;

procedure SCUMMExpLogInform(const cm: TSCUMMExpLogKind; msg: string;
		args: array of const);
	var
	s: string;

	begin
	if  not (cm in SCUMMExpLogMask) then
		begin
		s:= Format(msg, args);

		if Assigned(FLogHandler) then
			FLogHandler(sxtInform, Now, cm, s)
		else if Assigned(FLogOut) then
			Writeln(FLogOut^, DoFormatMessage(s));
		end;
	end;

procedure SCUMMExpLogWarn(const cm: TSCUMMExpLogKind; msg: string;
		args: array of const);
	var
	s: string;

	begin
	s:= 'WARNING: ' + Format(msg, args);

	if Assigned(FLogHandler) then
		FLogHandler(sxtWarn, Now, cm, s)
	else if Assigned(FLogErr) then
		Writeln(FLogErr^, DoFormatMessage(s));
	end;

procedure SCUMMExpLogError(const cm: TSCUMMExpLogKind; msg: string;
		args: array of const);
	var
	s: string;

	begin
	s:= 'ERROR: ' + Format(msg, args);

	if Assigned(FLogHandler) then
		FLogHandler(sxtError, Now, cm, s)
	else if Assigned(FLogErr) then
		Writeln(FLogErr^, DoFormatMessage(s));

	raise Exception.Create(s);
	end;

procedure SCUMMExpLogAbort(const cm: TSCUMMExpLogKind; msg: string;
		args: array of const);
	var
	s: string;

	begin
	s:= 'HALTED: ' + Format(msg, args);

	if Assigned(FLogHandler) then
		FLogHandler(sxtHalt, Now, cm, s)
	else if Assigned(FLogOut) then
		Writeln(FLogOut^, DoFormatMessage(s));
	end;


initialization
	FLogHandler:= nil;
	if  TTextRec(Output).Mode <> fmClosed then
		FLogOut:= @Output
	else
		FLogOut:= nil;

	if  TTextRec(ErrOutput).Mode <> fmClosed then
		FLogErr:= @ErrOutput
	else
		FLogErr:= nil;

end.
