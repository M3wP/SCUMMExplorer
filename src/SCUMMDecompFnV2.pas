unit SCUMMDecompFnV2;

interface

uses
	Classes, SCUMMDecompClasses;


procedure SPUTMDecV2PutActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IsGreaterEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2DrawObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetActorElevation(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2AssignVarWordIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetObjPreposition(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ResourceRoutines(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2PutActorAtObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfNotState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2PanCameraTo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ActorOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ActorFromPos(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ClearState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2DoSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetBitVar(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfClassOfIs(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2WalkActorTo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetActorY(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2LoadRoomWithEgo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetOwnerOf(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2AddIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2AssignVarByte(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2PutActorInRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Delay(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfNotState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetBitVar(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetCameraAt(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2RoomOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2FindObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2WalkActorToObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IsLessEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Subtract(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2WaitForActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetActorElevation(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfNotState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Cutscene(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2StartScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetActorX(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IsLess(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ClearState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ChainScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2WaitForSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2PickupObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SetState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2BeginOverride(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Add(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Dummy(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfNotState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2CursorCommand(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2StopScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ClearState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SubIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetObjPreposition(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Lights(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2ClearState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IsGreater(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2VerbOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2GetActorWalkBox(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2IfState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2Restart(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2SwitchCostumeSet(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2DrawSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2WaitForMessage(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV2EndCutscene(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);


implementation

uses
	SysUtils, SCUMMDecompFnV5;


function  SPUTMDecV2GetResultPos(const ADecomp: TSCUMMScriptDecomp): Integer;
	begin
	Result:= Integer(ADecomp.FetchScriptByte);
	end;

procedure SPUTMDecV2PutActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	a:= Format(FMT_SCUMMSCR_VARTY_AC, [ADecomp.FetchVarOrDirectByteP1(AOpCode)]);
	x:= ADecomp.FetchVarOrDirectByteP2(AOpCode);
	y:= ADecomp.FetchVarOrDirectByteP3(AOpCode);

	ADest.WriteString(l + 'put-actor ' + a + ' at ' + x + ', ' + y + #13#10);
	end;

procedure SPUTMDecV2IsGreaterEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	a:= ADecomp.FetchVar;
	x:= ADecomp.FetchVarOrDirectWordP1(AOpCode);
	y:= ADecomp.FetchRelativeAddress;

	ADest.WriteString(l + 'is-greater-equal ' + a + ' ' + x + ' else ' + y + #13#10);
	end;

procedure SPUTMDecV2DrawObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	a:= Format(FMT_SCUMMSCR_VARTY_OB, [ADecomp.FetchVarOrDirectWordP1(AOpCode)]);
	x:= ADecomp.FetchVarOrDirectByteP2(AOpCode);
	y:= ADecomp.FetchVarOrDirectByteP3(AOpCode);

	ADest.WriteString(l + 'draw-object ' + a + ' at ' + x + ', ' + y + #13#10);
	end;

procedure SPUTMDecV2GetActorElevation(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;
	v: Integer;

	begin
	l:= ADecomp.FetchAddress;

	v:= SPUTMDecV2GetResultPos(ADecomp);

	x:= Format(FMT_SCUMMSCR_VARTY_AC, [ADecomp.FetchVarOrDirectByteP1(AOpCode)]);
	a:= SPUTMDecV5XlatResultPos(ADecomp.DetectData, ADecomp.GameStats, v);

	ADest.WriteString(l + 'get-actor-elevation ' + a + ' ' + x + #13#10);
	end;

procedure SPUTMDecV2SetState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

//	int obj = getActiveObject();
//	putState(obj, getState(obj) | kObjectState_08);
//	markObjectRectAsDirty(obj);
//	clearDrawObjectQueue();

	ADest.WriteString(l + 'set-active-obj-state08'#13#10);
	end;

procedure SPUTMDecV2AssignVarWordIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;
	v: Integer;

	begin
	l:= ADecomp.FetchAddress;

//	getResultPosIndirect();
	v:= ADecomp.FetchScriptByte;
	a:= Format(FMT_SCUMMSCR_VARTY_GL, [v]);
	x:= ADecomp.FetchVarOrDirectWordP1(AOpCode);
//	a:= SPUTMDecV5XlatResultPos(ADecomp.DetectData, ADecomp.GameStats, v);

	ADest.WriteString(l + 'assign-var-word-indirect (' + a + ') ' + x + #13#10);
	end;

procedure SPUTMDecV2SetObjPreposition(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ResourceRoutines(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2PutActorAtObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfNotState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2PanCameraTo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ActorOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ActorFromPos(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ClearState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2DoSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetBitVar(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfClassOfIs(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2WalkActorTo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2GetActorY(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2LoadRoomWithEgo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetOwnerOf(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2AddIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2AssignVarByte(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2PutActorInRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Delay(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfNotState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2GetBitVar(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetCameraAt(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2RoomOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2FindObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2WalkActorToObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IsLessEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Subtract(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2WaitForActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetActorElevation(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfNotState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Cutscene(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2StartScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2GetActorX(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IsLess(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ClearState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ChainScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2WaitForSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfState08(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2PickupObject(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SetState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2BeginOverride(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Add(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Dummy(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfNotState02(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2CursorCommand(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2StopScript(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ClearState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SubIndirect(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2GetObjPreposition(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfState04(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Lights(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2ClearState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IsGreater(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2VerbOps(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2GetActorWalkBox(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2IfState01(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2Restart(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2SwitchCostumeSet(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2DrawSentence(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2WaitForMessage(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;

procedure SPUTMDecV2EndCutscene(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	var
	l,
	a,
	x,
	y: string;

	begin
	l:= ADecomp.FetchAddress;

	end;
end.
