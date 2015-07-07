unit FrameSPUTMDecV2Costume;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
	Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SCUMMTypes, SCUMMClasses,
	SPUTMTypes, SPUTMDecV2Types, FrameSCUMMExpCustomViewer, Vcl.StdCtrls, Vcl.ExtCtrls,
	Vcl.ComCtrls, VCL.Imaging.PNGImage;

type
	TSCUMMCostAnimState = (sasNotVisible, sasRunning, sasEnded);
	TSCUMMCostAnimMode = (samPaused, samRunning);

	TSCUMMCostAnimation = record
		state: TSCUMMCostAnimState;
		mode: TSCUMMCostAnimMode;
		startIdx: Byte;
		currCmd: Byte;
		steps: Byte;
		currStep: Byte;
		loop: Boolean;
		included: Boolean;
	end;

	TSCUMMCostAnimations = array[0..15] of TSCUMMCostAnimation;

	TSCUMMAnimDir = (sadRight, sadLeft, sadDown, sadUp);
	TSCUMMAnimType = (satWalk, satStand);
	TSCUMMAnimMouth = (smoNone, smoOpen, smoClosed, smoTalk);

	TSPUTMDecV2CostumeFrame = class(TSCUMMExpCustomViewerFrame)
		Panel2: TPanel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label1: TLabel;
		PaintBox1: TPaintBox;
		Label12: TLabel;
		TrackBar2: TTrackBar;
		TrackBar1: TTrackBar;
		CheckBox9: TCheckBox;
		CheckBox10: TCheckBox;
		CheckBox11: TCheckBox;
		CheckBox12: TCheckBox;
		CheckBox13: TCheckBox;
		CheckBox14: TCheckBox;
		TrackBar3: TTrackBar;
		TrackBar4: TTrackBar;
		TrackBar5: TTrackBar;
		TrackBar6: TTrackBar;
		Panel1: TPanel;
		Label10: TLabel;
		Label11: TLabel;
		Label8: TLabel;
		Label14: TLabel;
		Button2: TButton;
		Button3: TButton;
		TrackBar8: TTrackBar;
		CheckBox2: TCheckBox;
		CheckBox3: TCheckBox;
		CheckBox4: TCheckBox;
		CheckBox5: TCheckBox;
		CheckBox6: TCheckBox;
		CheckBox7: TCheckBox;
		CheckBox8: TCheckBox;
		RadioGroup2: TRadioGroup;
		GroupBox1: TGroupBox;
		Label13: TLabel;
		RadioButton1: TRadioButton;
		RadioButton2: TRadioButton;
		CheckBox1: TCheckBox;
		ComboBox1: TComboBox;
		CheckBox17: TCheckBox;
		CheckBox18: TCheckBox;
		CheckBox19: TCheckBox;
		CheckBox15: TCheckBox;
		CheckBox16: TCheckBox;
		ListBox1: TListBox;
		Timer1: TTimer;
		procedure Timer1Timer(Sender: TObject);
		procedure TrackBar8Change(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure PaintBox1Paint(Sender: TObject);
	private
//		FName: string;
		FCostume: PSCUMMCostumeV2;
		FAnimations: TSCUMMCostAnimations;
		FMemStream: TMemoryStream;
		FUtilised: array[9..14] of TCheckBox;
		FSequences: array[9..14] of TTrackBar;
		FState: array[9..14] of TCheckBox;
		FDrawBuf: TBitmap;
		FMirror: Boolean;
		FDetectData: TSCUMMDetectorData;

		procedure IncludeAnimation(var AAnimations: TSCUMMCostAnimations;
				AAnim: Integer; ACells: Byte; const AMerge: Boolean = True);

		procedure FetchFromControls(var ADirect: TSCUMMAnimDir;
				var AType: TSCUMMAnimType;
				var AUseNearFar, ANear, ANoBody, AUseHead: Boolean;
				var AMouth: TSCUMMAnimMouth);

		procedure PrepareAnimations(var AAnimations: TSCUMMCostAnimations;
				var AMirror: Boolean; ADirect: TSCUMMAnimDir;
				AType: TSCUMMAnimType; AUseNearFar: Boolean; ANear: Boolean;
				ANoBody: Boolean; AUseHead: Boolean; AMouth: TSCUMMAnimMouth);

		procedure PrepareInformation;
		procedure InitialiseAnimations(var AAnimations: TSCUMMCostAnimations);
		procedure PrepareControls(AAnimations: TSCUMMCostAnimations);
		procedure ProgressAnimations(var AAnimations: TSCUMMCostAnimations);

		procedure PrepareFrame(AAnims: TSCUMMCostAnimations;
				AMirror: Boolean; APoint: TPoint; AFrame: TPNGImage); overload;
		procedure PrepareFrame(AAnims: TSCUMMCostAnimations;
				AMirror: Boolean; APoint: TPoint; AFrame: TBitmap); overload;

	public
		class function  GetName: string; override;
		class function  GetDescription: string; override;

		class function  GetCompatibility(const ACallIdx: Integer;
				out ASupports: TSCUMMExpViewerCompat): Boolean; override;

		class function  CanViewPath(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData;
				APath: TSCUMMExpGlobIdArr): Boolean; override;

		procedure PreviewPath(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData;
				APath: TSCUMMExpGlobIdArr); override;
	end;

implementation

{$R *.dfm}

uses
	Math, SPUTMDecV2Strs, SPUTMPlatAmigaConsts, SPUTMPlatPCDOSConsts, SPUTMClasses;


procedure DrawPngWithAlpha(Src, Dest: TPNGImage; const R: TRect);
	var
	X,
	Y: Integer;
	Alpha: PByte;

	begin
	Src.Draw(Dest.Canvas, R);

//	I have no idea why standard implementation of TPNGImage.Draw doesn't
//			apply transparency even to its own kind.
	for Y:= R.Top to R.Bottom - 1 do
		for X:= R.Left to R.Right - 1 do
			begin
			Alpha:= @Dest.AlphaScanline[Y]^[X];
			Alpha^:= Min(255, Alpha^ +
					Src.AlphaScanline[Y - R.Top]^[X - R.Left]);
			end;
	end;

type
	TCellDrawFunc = procedure(ACell: TPNGImage; ADest: Pointer; ARect: TRect);

procedure PNGDrawFunc(ACell: TPNGImage; ADest: Pointer; ARect: TRect);
	begin
	DrawPngWithAlpha(ACell, TPNGImage(ADest), ARect);
	end;

procedure BitmapDrawFunc(ACell: TPNGImage; ADest: Pointer; ARect: TRect);
	begin
	ACell.DrawUsingPixelInformation(TBitmap(ADest).Canvas,
			Point(ARect.Left, ARect.Top));
	end;

procedure DrawCellToFrame(ACostume: PSCUMMCostumeV2;
		AAnims: TSCUMMCostAnimations; AMirror: Boolean; AStream: TMemoryStream;
		AAnim: Byte; APoint: TPoint; AFunc: TCellDrawFunc; AData: Pointer;
		APlatform: TSCUMMCorePlatform);
	var
	p: TPoint;
	v,
	q: Byte;
	cell: TPNGImage;

	begin
	p.X:= APoint.X;
	p.Y:= APoint.Y;

	if  (AAnims[AAnim].state = sasRunning) then
//	or  ((FAnimations[i].state = sasEnded)
//	and  (FAnimations[i].currCell < $FF)) then
		begin
		v:= ACostume^.limbInfos[AAnim].cmdFrameListIdx;
		q:= ACostume^.cmdFrameIndex[v].cmdFrameIdxs[AAnims[AAnim].currCmd and $7F];

//FIXME dengland HACK!!!  This hack is for Zak since in it we are getting an image
//      with no data that seems to have to be there for everything except where it is
//		referred to.  If I remove it, all of the other animations are incorrect.  So,
//		if we are going to try to use an empty image, look to the next one instead.
		if Length(ACostume.frameInfos[q].data) = 0 then
			Inc(q);

		if  AMirror then
			Dec(p.X, ACostume^.frameInfos[q].relativeX +
					ACostume^.frameInfos[q].width - 72)
		else
			p.X:= p.X - 72 + ACostume^.frameInfos[q].relativeX;

		Inc(p.Y, ACostume^.frameInfos[q].relativeY +
				ACostume^.frameInfos[q].moveY);

		AStream.Clear;
		ACostume^.frameInfos[q].DecodeProcV2(ACostume^.resInfo.colour,
				AStream);

		AStream.Seek(0, soFromBeginning);

		if  APlatform = scpPCDOS then
			DecodeCostumeFrame(AStream, ACostume^.frameInfos[q].width,
					ACostume^.frameInfos[q].height, AMirror, ARR_PLTFRM_PCDOS_CSTPAL,
					cell)
		else
			DecodeCostumeFrame(AStream, ACostume^.frameInfos[q].width,
					ACostume^.frameInfos[q].height, AMirror, ARR_PLTFRM_AMIGA_CSTPAL,
					cell);

		try
// 			cell.DrawUsingPixelInformation(frame.Canvas, p);
//			DrawPngWithAlpha(cell, AFrame, Rect(p.X, p.Y,
//					p.X + cell.Width, p.Y + cell.Height));
			AFunc(cell, AData, Rect(p.X, p.Y, p.X + cell.Width,
					p.Y + cell.Height));

			finally
			cell.Free;
			end;
		end;
	end;

{ TSPUTMDecV2CostumeFrame }

procedure TSPUTMDecV2CostumeFrame.Button2Click(Sender: TObject);
	begin
	Timer1.Enabled:= not Timer1.Enabled;
	if  Timer1.Enabled then
		Button2.Caption:= 'Stop'
	else
		Button2.Caption:= 'Run';
	end;

procedure TSPUTMDecV2CostumeFrame.Button3Click(Sender: TObject);
	var
	dir: TSCUMMAnimDir;
	atype: TSCUMMAnimType;
	useNearFar,
	useNear,
	noBody,
	useHead: Boolean;
	mouth: TSCUMMAnimMouth;

	begin
	InitialiseAnimations(FAnimations);

	FetchFromControls(dir, atype, useNearFar, useNear, noBody, useHead, mouth);
	PrepareAnimations(FAnimations, FMirror, dir, atype, useNearFar, useNear,
			noBody, useHead, mouth);

	PrepareControls(FAnimations);
    PaintBox1.Invalidate;
	end;

class function TSPUTMDecV2CostumeFrame.CanViewPath(AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData; APath: TSCUMMExpGlobIdArr): Boolean;
	begin
	Result:= True;
	end;

procedure TSPUTMDecV2CostumeFrame.FetchFromControls(var ADirect: TSCUMMAnimDir;
		var AType: TSCUMMAnimType; var AUseNearFar, ANear, ANoBody, AUseHead: Boolean;
		var AMouth: TSCUMMAnimMouth);
	begin
	ADirect:= TSCUMMAnimDir(RadioGroup2.ItemIndex);
	if  RadioButton1.Checked then
		AType:= satWalk
	else
		AType:= satStand;
	AUseNearFar:= CheckBox17.Checked;
	ANear:= CheckBox18.Checked;
	ANoBody:= CheckBox19.Checked;
	AUseHead:= CheckBox1.Checked;
	AMouth:= TSCUMMAnimMouth(ComboBox1.ItemIndex);
	end;

class function TSPUTMDecV2CostumeFrame.GetCompatibility(const ACallIdx: Integer;
		out ASupports: TSCUMMExpViewerCompat): Boolean;
	begin
	if  ACallIdx > High(ARR_REC_SCUMMEXP_COVWV2_PROPS) then
		Result:= False
	else
		begin
		ASupports:= ARR_REC_SCUMMEXP_COVWV2_PROPS[ACallIdx];
		Result:= True;
		end;
	end;

class function TSPUTMDecV2CostumeFrame.GetDescription: string;
	begin
	Result:= STR_SCUMMEXP_COVWVS_VWRDS;
	end;

class function TSPUTMDecV2CostumeFrame.GetName: string;
	begin
	Result:= STR_SCUMMEXP_COVWVS_VWRNM;
	end;

procedure TSPUTMDecV2CostumeFrame.IncludeAnimation(
		var AAnimations: TSCUMMCostAnimations; AAnim: Integer; ACells: Byte;
		const AMerge: Boolean);
	var
	i: Integer;
	a: Integer;

	begin
	a:= FCostume^.resInfo.animIndex[AAnim];

	for i:= Low(AAnimations) to High(AAnimations) do
		begin
		if  (ACells = $FF)
		or  (ACells = i) then
			if  (High(FCostume^.animInfos) >= a)
			and FCostume^.animInfos[a].IsFramesUsed(i) then
				begin
				if  FCostume^.animInfos[a].limbCmds[i].IsAvail then
					AAnimations[i].state:= sasEnded
				else
					AAnimations[i].state:= sasRunning;

				if  FCostume^.animInfos[a].limbCmds[i].IsPause then
					AAnimations[i].mode:= samPaused
				else
					AAnimations[i].mode:= samRunning;

				AAnimations[i].startIdx:=
						FCostume^.animInfos[a].limbCmds[i].cmdIdx;
				AAnimations[i].currCmd:=
						FCostume^.animInfos[a].limbCmds[i].cmd;
				AAnimations[i].steps:=
						FCostume^.animInfos[a].limbCmds[i].Steps;
				AAnimations[i].currStep:= 0;
				AAnimations[i].loop:=
						FCostume^.animInfos[a].limbCmds[i].IsLoop;
				end
			else if not AMerge then
				AAnimations[i].state:= sasNotVisible;

		if  (AAnimations[i].state = sasRunning)
		and  (AAnimations[i].mode = samRunning) then
			AAnimations[i].included:= True;
		end;
	end;

procedure TSPUTMDecV2CostumeFrame.InitialiseAnimations(
		var AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	begin
	for i:= High(AAnimations) downto Low(AAnimations) do
		begin
		AAnimations[i].state:= sasNotVisible;
		AAnimations[i].mode:= samPaused;
		AAnimations[i].startIdx:= $FF;
		AAnimations[i].currCmd:= $FF;
		AAnimations[i].steps:= $FF;
		AAnimations[i].currStep:= $FF;
		AAnimations[i].loop:= False;
		AAnimations[i].included:= False;
		end;
	end;

procedure TSPUTMDecV2CostumeFrame.PaintBox1Paint(Sender: TObject);
	var
	p: TPoint;

	begin
	FDrawBuf.Canvas.Brush.Color:= clBtnFace;
	FDrawBuf.Canvas.Brush.Style:= bsSolid;
	FDrawBuf.Canvas.FillRect(Rect(0, 0, 319, 199));

	p.X:= 80;
	p.Y:= -10;
	PrepareFrame(FAnimations, FMirror, p, FDrawBuf);

//	PaintBox1.Canvas.Draw(0, 0, FDrawBuf);
	PaintBox1.Canvas.StretchDraw(Rect(0, 0, 320, 240), FDrawBuf);
	end;

procedure TSPUTMDecV2CostumeFrame.PrepareAnimations(
		var AAnimations: TSCUMMCostAnimations; var AMirror: Boolean;
		ADirect: TSCUMMAnimDir; AType: TSCUMMAnimType; AUseNearFar, ANear, ANoBody,
		AUseHead: Boolean; AMouth: TSCUMMAnimMouth);
	var
	a: Integer;

	begin
	AMirror:= (ADirect = sadRight) and not FCostume^.resInfo.NoMirror;

//	This logic needs a clean up
	if  not ANoBody then
		if  not AUseNearFar then
			begin
			if  AType = satWalk then
				a:= 0
			else
				a:= 1;

			IncludeAnimation(AAnimations, a * 4 + Ord(ADirect), $FF);
			end
		else
			begin
			if  AType = satWalk then
				begin
				a:= 1;
				if  ANear then
					begin
					if (ADirect > sadLeft) then
						a:= Ord(ADirect);
					end
				else
					begin
					if (ADirect > sadLeft) then
						a:= Ord(ADirect)
					else
						a:= 0;
					end;
				end
			else
				a:= 4 + Ord(ADirect);

			IncludeAnimation(AAnimations, a, $FF);
			end;

	if  AUseHead then
		begin
		a:= 8 + Ord(ADirect);

//		if  not ANoBody then
//			IncludeAnimation(AAnimations, a, 12)
//		else
			IncludeAnimation(AAnimations, a, $FF)
		end;

	if  AMouth > smoNone then
		begin
		a:= 8 + (4 * Ord(AMouth)) + Ord(ADirect);
//		IncludeAnimation(AAnimations, a, 11);
		IncludeAnimation(AAnimations, a, $FF);
		end;
	end;

procedure TSPUTMDecV2CostumeFrame.PrepareControls(AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	begin
	for i:= 14 downto 9 do
		begin
		FUtilised[i].Checked:= AAnimations[i].state <> sasNotVisible;

		FState[i].State:= cbChecked;
		if  AAnimations[i].mode = samPaused then
			FState[i].State:= cbGrayed
		else if AAnimations[i].state = sasEnded then
			FState[i].State:= cbUnchecked;

		FSequences[i].Min:= 1;
		if  AAnimations[i].steps < $FF then
			FSequences[i].Max:= AAnimations[i].steps + 1
		else
			FSequences[i].Max:= 2;

		FSequences[i].Position:= 1;
		if  AAnimations[i].steps < $FF then
			FSequences[i].Position:= AAnimations[i].currStep + 1;
		end;

	ListBox1.Clear;
	for i:= 0 to 8 do
		if  AAnimations[i].included then
			ListBox1.Items.Add('Limb ' + IntToStr(i));

	if  AAnimations[15].included then
			ListBox1.Items.Add('Limb 15');
	end;

procedure TSPUTMDecV2CostumeFrame.PrepareFrame(AAnims: TSCUMMCostAnimations;
		AMirror: Boolean; APoint: TPoint; AFrame: TBitmap);
	var
	i: Byte;

	begin
	for i:= High(AAnims) downto Low(AAnims) do
		DrawCellToFrame(FCostume, AAnims, AMirror, FMemStream, i, APoint,
				BitmapDrawFunc, AFrame, FDetectData.game.plat);
	end;

procedure TSPUTMDecV2CostumeFrame.PrepareFrame(AAnims: TSCUMMCostAnimations;
		AMirror: Boolean; APoint: TPoint; AFrame: TPNGImage);
	var
	i: Byte;

	begin
	for i:= High(AAnims) downto Low(AAnims) do
		DrawCellToFrame(FCostume, AAnims, AMirror, FMemStream, i, APoint,
				PNGDrawFunc, AFrame, FDetectData.game.plat);
	end;

procedure TSPUTMDecV2CostumeFrame.PrepareInformation;
	var
	i,
	j: Byte;
	a: set of 0..15;

	begin
//	Label9.Caption:= FName;
	CheckBox8.Checked:= FCostume^.resInfo.NoMirror;

	CheckBox15.Checked:= Length(FCostume^.animInfos) = 0;

	a:= [];
	if Length(FCostume^.animInfos) > 0 then
		for i:= Low(FCostume^.animInfos) to High(FCostume^.animInfos) do
			for j:= Low(FCostume^.animInfos[i].limbCmds) to
					High(FCostume^.animInfos[i].limbCmds) do
				if  FCostume^.animInfos[i].limbCmds[j].isUsed then
					Include(a, j);
	a:= a - [9..14];
	CheckBox16.Checked:= a <> [];
	end;

procedure TSPUTMDecV2CostumeFrame.PreviewPath(AHostNode: TSCUMMHostNode;
		ADetectData: TSCUMMDetectorData; APath: TSCUMMExpGlobIdArr);
	var
	d: PSCUMMExpObjData;

	begin
	FDetectData:= ADetectData;

//TODO This is for first time create.  Eventually, these viewer frames should be
//      pooled so this code would need to be run only the first time - in init.
	TrackBar8Change(Self);
	FMemStream:= TMemoryStream.Create;
	FDrawBuf:= TBitmap.Create;
	FDrawBuf.Width:= 160;
	FDrawBuf.Height:= 100;

	FMirror:= False;

	InitialiseAnimations(FAnimations);

	FUtilised[9]:= CheckBox2;
	FUtilised[10]:= CheckBox3;
	FUtilised[11]:= CheckBox4;
	FUtilised[12]:= CheckBox5;
	FUtilised[13]:= CheckBox6;
	FUtilised[14]:= CheckBox7;

	FState[9]:= CheckBox9;
	FState[10]:= CheckBox10;
	FState[11]:= CheckBox11;
	FState[12]:= CheckBox12;
	FState[13]:= CheckBox13;
	FState[14]:= CheckBox14;

	FSequences[9]:= TrackBar2;
	FSequences[10]:= TrackBar1;
	FSequences[11]:= TrackBar3;
	FSequences[12]:= TrackBar4;
	FSequences[13]:= TrackBar5;
	FSequences[14]:= TrackBar6;

//  This code is from "loading"
	InitialiseAnimations(FAnimations);

//	FName:= TPath.GetFileNameWithoutExtension(AFileName);

	d:= SCUMMExpEnumCache.GetDataForPath(APath);

	Assert(d^.enumType = sxeCostumeCntnr);

	FCostume:= PSCUMMCostumeV2(d^.nodeData);

	PrepareInformation;

	Button3.Enabled:= True;
	Button3Click(Self);
	end;

procedure TSPUTMDecV2CostumeFrame.ProgressAnimations(
		var AAnimations: TSCUMMCostAnimations);
	var
	i: Integer;

	begin
	for i:= Low(AAnimations) to High(AAnimations) do
		if  (AAnimations[i].state = sasRunning)
		and (AAnimations[i].mode = samRunning) then
			begin
			if  AAnimations[i].currStep <= AAnimations[i].steps then
				Inc(AAnimations[i].currStep);

			if  (AAnimations[i].currStep > AAnimations[i].steps) then
				if (AAnimations[i].loop) then
					AAnimations[i].currStep:= 0
				else
					AAnimations[i].currStep:= AAnimations[i].steps;

			AAnimations[i].currCmd:= FCostume^.animCmds[
					AAnimations[i].startIdx + AAnimations[i].currStep].cmd;
			end;
	end;

procedure TSPUTMDecV2CostumeFrame.Timer1Timer(Sender: TObject);
	begin
	PrepareControls(FAnimations);
	PaintBox1.Invalidate;
	ProgressAnimations(FAnimations);
	end;

procedure TSPUTMDecV2CostumeFrame.TrackBar8Change(Sender: TObject);
	var
	v: Integer;

	begin
	v:= TrackBar8.Max - TrackBar8.Position + TrackBar8.Min;
	Timer1.Interval:= v;
	TrackBar8.Hint:= IntToStr(v) + 'ms';
	end;

end.
