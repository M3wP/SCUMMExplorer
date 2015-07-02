unit SCUMMDecompFnV5;

interface

uses
	Classes, SCUMMTypes, SPUTMTypes, SCUMMDecompClasses;


procedure SPUTMDecV5StopObjectCode(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5StartMusic(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetActorRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5IsNotEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5FaceActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5WalkActorToActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetObjectOwner(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5AnimateActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5Print(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetRandomNr(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5JumpRelative(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5Move(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5StartSound(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5StopMusic(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5SetVarRange(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5EqualZero(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5DelayVariable(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetDist(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5StopSound(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5Increment(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5IsEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5ActorFollowCamera(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5SetObjectName(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetActorMoving(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetActorFacing(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetClosestObjActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5IsScriptRunning(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5GetActorCostume(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5LoadRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5IsSoundRunning(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5BreakHere(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5NotEqualZero(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5Decrement(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5PseudoRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);

procedure SPUTMDecV5PrintEgo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);


function  SPUTMDecV5XlatResultPos(const ADetectData: TSCUMMDetectorData;
		const AGameStats: TSPUTMStats; const AV: Integer): string;


implementation

uses
	SysUtils;


function  SPUTMDecV5XlatResultPos(const ADetectData: TSCUMMDetectorData;
		const AGameStats: TSPUTMStats; const AV: Integer): string;
	var
	v: Integer;
	bit: Integer;

	begin
	v:= AV;

	if (v and $F000) = 0 then
		begin
		Result:= Format(FMT_SCUMMSCR_VARTY_GL, [v]);
		Exit;
		end;

	if (v and $8000) <> 0 then
		begin
		if  ADetectData.game.subv >= 80 then
			begin
			v:= $0FFF;
			Assert((v > 0) and (v < AGameStats.numRoomVariables - 1),
					'room variable (writing)');
			Result:= Format(FMT_SCUMMSCR_VARTY_RM, [v]);
			end
		else if (ADetectData.game.ver <= scv3)
		and (not ((ADetectData.game.id = scgIndy3)
		and  (ADetectData.game.plat = scpFMTowns)))
		and (not ((ADetectData.game.id = scgLoom)
		and  (ADetectData.game.plat = scpPCEngine))) then
			begin
//			In the old games, the bit variables were using the same memory as
//				the normal variables!

//			bit:= v and $0F;
//			v:= (v shr 4) and $FF;
//			Assert((v > 0) and (v < AGameStats.numVariables - 1),
//					'variable (writing)');
//			if (value)
//				_scummVars[var] |= ( 1 << bit );
//			else
//				_scummVars[var] &= ~( 1 << bit );
			Assert((v > 0) and (v < AGameStats.numBitVariables - 1),
					'bit variable (writing)');
			Result:= Format(FMT_SCUMMSCR_VARTY_GB, [v]);
			end
		else
			begin
			v:= v and $7FFF;
			Assert((v > 0) and (v < AGameStats.numBitVariables - 1),
					'bit variable (writing)');

			Result:= Format(FMT_SCUMMSCR_VARTY_GB, [v]);
			end;
		Exit;
		end;

	if  (v and $4000) <> 0 then
		begin
		if  scfFewLocals in ADetectData.game.feat then
			v:= v and $0F
		else
			v:= v and $0FFF;

		if  ADetectData.game.subv >= 80 then
			Assert((v > 0) and (v < 25), 'local variable (writing)')
		else
			Assert((v > 0) and (v < 20), 'local variable (writing)');

		Result:= Format(FMT_SCUMMSCR_VARTY_LC, [v]);
		Exit;
		end;

	Assert(False, 'Illegal varbits (w)');
	end;

function  SPUTMDecV5GetResultPos(const ADecomp: TSCUMMScriptDecomp): Integer;
	var
	a: Integer;

	begin
//dengland I don't like that a was declared as an integer.  I'll have to use them
//		throughout...
	Result:= Integer(ADecomp.FetchScriptWord);

	if  (Result and $2000) <> 0 then
		begin
		a:= Integer(ADecomp.FetchScriptWord);

		if (a and $2000) <> 0 then
			Inc(Result, ADecomp.FetchVarIntrn(a and (not $2000)))
		else
			Inc(Result, a and $0FFF);

//		Result:= Format(FMT_SCUMMSCR_VALUE_HX, [4, 4, v]);
		Result:= Result and (not $2000);
		end;
	end;


procedure SPUTMDecV5StopObjectCode(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5StartMusic(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetActorRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5IsNotEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5FaceActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5WalkActorToActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetObjectOwner(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5AnimateActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5Print(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetRandomNr(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5JumpRelative(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5Move(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5StartSound(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5StopMusic(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5SetVarRange(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5EqualZero(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5DelayVariable(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetDist(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5StopSound(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5Increment(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5IsEqual(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5ActorFollowCamera(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5SetObjectName(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetActorMoving(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetActorFacing(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetClosestObjActor(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5IsScriptRunning(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5GetActorCostume(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5LoadRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5IsSoundRunning(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5BreakHere(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5NotEqualZero(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5Decrement(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5PseudoRoom(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

procedure SPUTMDecV5PrintEgo(const ASource: TStream;
		const ADest: TStringStream; const AOpCode: Byte;
		const ADecomp: TSCUMMScriptDecomp);
	begin

	end;

end.

