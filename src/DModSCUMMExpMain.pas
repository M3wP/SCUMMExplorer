unit DModSCUMMExpMain;

{$IFDEF FPC}
	{$FATAL UNSUPPORTED PLATFORM}
{$ELSE}
	{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}


interface

uses
	System.SysUtils, System.Classes, Vcl.AppEvnts, Vcl.Dialogs, Vcl.Menus,
	System.Actions, Vcl.ActnList, Vcl.ImgList, Vcl.Controls, SCUMMExpClasses,
	FrameSCUMMExpMain, System.ImageList;

type
	TSCUMMExpMainDMod = class(TDataModule)
		imgLstBrowserSml: TImageList;
		ActionList1: TActionList;
		actToolsConfig: TAction;
		actToolsOptions: TAction;
		MainMenu1: TMainMenu;
		File1: TMenuItem;
		Edit1: TMenuItem;
		View1: TMenuItem;
		ools1: TMenuItem;
		Configure1: TMenuItem;
		N1: TMenuItem;
		Options1: TMenuItem;
		Help1: TMenuItem;
		FileOpenDialog1: TFileOpenDialog;
		ApplicationEvents1: TApplicationEvents;
		actHelpAbout: TAction;
		About1: TMenuItem;
		imgLstLogMsgs: TImageList;
		imgLstBrowseMed: TImageList;

		procedure DataModuleCreate(Sender: TObject);
		procedure DataModuleDestroy(Sender: TObject);
		procedure actToolsConfigExecute(Sender: TObject);
		procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
		procedure actHelpAboutExecute(Sender: TObject);
	private
		FMainFrame: TSCUMMExpMainFrame;

	protected
		function  GetMainFrame: TSCUMMExpMainFrame;
		procedure SetMainFrame(AMainFrame: TSCUMMExpMainFrame);

	public
		procedure Prepare;

		property  MainFrame: TSCUMMExpMainFrame read GetMainFrame write SetMainFrame;
	end;

var
	SCUMMExpMainDMod: TSCUMMExpMainDMod;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
	IOUtils, Forms, SCUMMLogTypes, SCUMMKBClasses,
	SCUMMConsts, SCUMMTypes, SCUMMClasses,
	SPUTMTypes, SPUTMStrs, SPUTMClasses,
	SCUMMExpStrs, SCUMMExpTypes,
	FormSCUMMExpSplash, FormSCUMMExpLog, FormSCUMMExpAbout, FormSCUMMExpConfig;

{$R *.dfm}

procedure TSCUMMExpMainDMod.actHelpAboutExecute(Sender: TObject);
	begin
    SCUMMExpAboutForm.ShowModal;
	end;

procedure TSCUMMExpMainDMod.actToolsConfigExecute(Sender: TObject);
	var
	node: TSCUMMHostNode;
	dd: TSCUMMDetectorData;
	dr: TSCUMMDetectorResult;

	begin
//todo dengland Implement config of game in config form
	if  FileOpenDialog1.Execute then
		if  TDirectory.Exists(FileOpenDialog1.FileName) then
			begin
			node:= TSCUMMHostNode.Create(FileOpenDialog1.FileName, 3);
			dr:= TSCUMMDetector.DetectSCUMM(node, dd);

			if  (dr <> sdrError)
			and (dr <> sdrUnk) then
				begin
				SCUMMExpGames.AddGame(node, dd);
				FMainFrame.UpdateDisplay;
				end;
			end;
	end;

procedure TSCUMMExpMainDMod.ApplicationEvents1Idle(Sender: TObject;
		var Done: Boolean);
	begin
	SCUMMExpLogForm.CopyCurrentMessageQueue;

	Done:= True;
	end;


procedure TSCUMMExpMainDMod.DataModuleCreate(Sender: TObject);
	begin
//todo Read from ini
	SCUMMExpLogMask:= [];

	SCUMMExpLogInstallHandler(SCUMMExpLogHandler);
	end;

procedure TSCUMMExpMainDMod.DataModuleDestroy(Sender: TObject);
	begin
	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_FINMSG_RELSENVR, []);

	SCUMMExpFinalise;
	end;

function TSCUMMExpMainDMod.GetMainFrame: TSCUMMExpMainFrame;
	begin
	Result:= FMainFrame;
	end;

procedure TSCUMMExpMainDMod.Prepare;
	procedure PerformTest;
		var
//		s: string;
//		p: TSCUMMExpObjPath;
//		d: TSCUMMExpObjData;
//		e: PSCUMMExpObjData;
//		g: TSCUMMExpGlobId;
//		a: TSCUMMExpGlobIdArr;
//		i: Integer;

		k: TSCUMMExpLogKind;

		begin
//		Dump out dummy messages so I know what they look like.
		for k:= Low(TSCUMMExpLogKind) to High(TSCUMMExpLogKind) do
			SCUMMExpLogDebug(k, 'This is a dummy message from outer ' +
					'space.  Do not adjust your TV set!', []);

//		g:= SCUMMExpCreateGlobID;
//
//		d.id:= SCUMMExpCreateGlobID;
//		d.name:= 'Test Room';
//		d.enumType:= sxeRoomCntnr;
//		d.dataType:= sxdNode;
//
//		s:= LIT_SCUMMEXP_PATHDL_ROOT + GUIDToString(g) +
//				LIT_SCUMMEXP_PATHDL_NODE + GUIDToString(VAL_SCUMMEXP_GLOBID_ROOMS) +
//				LIT_SCUMMEXP_PATHDL_NODE + GUIDToString(d.id);
//
//		p.FromString(s);
//		a:= p.ToArray;
//
//		SCUMMExpEnumCache.SetDataForPath(a, @d);
//
//		if not SCUMMExpEnumCache.IsInCache(a) then
//			ShowMessage('Oops!')
//		else
//			begin
//			e:= SCUMMExpEnumCache.GetDataForPath(a);
//
//			if  CompareStr(GUIDToString(d.id), GUIDToString(e^.id)) = 0 then
//				ShowMessage('Okay!')
//			else
//				ShowMessage('Failed!');
//			end;

		SCUMMExpLogAbort(sxkExplorer, 'Internal testing complete.', []);
		end;

	var
	sf: TSCUMMExpSplashForm;

	begin
	SCUMMExpLogMsgList:= TThreadList.Create;
	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_INIFMT_INITLOGI,
			[FormatDateTime(STR_SCUMMEXP_INIFMT_INITLOGT, Now)]);

	sf:= TSCUMMExpSplashForm.Create(Self);
	try
//		------------------------------------------------------------------------
//dengland Need to say pretty please by making multiple, different valued
//		updates to progress bar for it to actually update.  Is this because of
//		"Smooth" mode?
		SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_INIMSG_INITCORE, []);
		sf.lblProgress.Caption:= STR_SCUMMEXP_INIMSG_INITCORE;

		sf.prgbrProgress.Position:= 10;
		sf.Update;

		SCUMMExpInitialise;

		SetCurrentDir(TPath.GetDirectoryName(Application.ExeName) +
				TPath.DirectorySeparatorChar + LIT_SCUMMEXP_PLGPTH_LOCN);

		SCUMMPluginMngr.QueryPlugins;
		SCUMMPluginMngr.InitPlugins;

		SetCurrentDir(TPath.GetDirectoryName(Application.ExeName));

//		Sleep(500);
//		------------------------------------------------------------------------
		sf.prgbrProgress.Position:= 25;
		sf.Update;

		SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_INIMSG_PREPKNBS, []);
		sf.lblProgress.Caption:= STR_SCUMMEXP_INIMSG_PREPKNBS;

		sf.prgbrProgress.Position:= 35;
		sf.Update;

		SCUMMKnowledgeBase.Prepare(TPath.Combine(
				TPath.GetDirectoryName(Application.ExeName),
				LIT_SCUMMEXP_KBPATH_LOCN));

		SCUMMPluginMngr.PrepPlugins;

//		Sleep(500);
//		------------------------------------------------------------------------
		sf.prgbrProgress.Position:= 75;
		sf.Update;

		SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_INIMSG_PREPENVR, []);
		sf.lblProgress.Caption:= STR_SCUMMEXP_INIMSG_PREPENVR;
		sf.prgbrProgress.Position:= 85;
		sf.Update;

		Application.CreateForm(TSCUMMExpConfigForm, SCUMMExpConfigForm);
		Application.CreateForm(TSCUMMExpLogForm, SCUMMExpLogForm);
		Application.CreateForm(TSCUMMExpAboutForm, SCUMMExpAboutForm);

//		Sleep(500);
//		------------------------------------------------------------------------
		sf.prgbrProgress.Position:= 100;
		sf.Update;

		finally
		sf.Release;
		end;

	PerformTest;

	SCUMMExpLogInform(sxkExplorer, STR_SCUMMEXP_INIMSG_EXPREADY, []);
	Application.MainForm.BringToFront;
	end;

procedure TSCUMMExpMainDMod.SetMainFrame(AMainFrame: TSCUMMExpMainFrame);
	begin
	FMainFrame:= AMainFrame;
	end;

end.
