unit FormSCUMMExpLog;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	VirtualTrees, Vcl.ImgList, DModSCUMMExpMain;

type
	TSCUMMExpLogForm = class(TForm)
		vsttrMessages: TVirtualStringTree;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure vsttrMessagesGetImageIndex(Sender: TBaseVirtualTree;
				Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
				var Ghosted: Boolean; var ImageIndex: Integer);
		procedure vsttrMessagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
				Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
		procedure vsttrMessagesBeforeCellPaint(Sender: TBaseVirtualTree;
				TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
				CellPaintMode: TVTCellPaintMode; CellRect: TRect;
				var ContentRect: TRect);
	private
		FMsgList: TList;

	protected
		procedure CreateParams(var Params: TCreateParams); override;

	public
		procedure CopyCurrentMessageQueue;
		procedure EmptyLocalMessageQueue;
	end;

var
	SCUMMExpLogForm: TSCUMMExpLogForm;

implementation

{$R *.dfm}

uses
	SCUMMExpTypes, SCUMMLogTypes, SCUMMLogStrs;

const
	ARR_CLR_SCUMMEXP_LOGCLRS: array[TSCUMMExpLogKind] of TColor = (
		TColor($8888E3), TColor($EE8F99), TColor($8AD3E6),
		TColor($81D68B), TColor($D7B381), TColor($8ABAE6),
		TColor($A7BD71), TColor($91F1EC), TColor($F187CC),
		TColor($F0B686), TColor($DCB8E6));

{ TRAWMessagesForm }

procedure TSCUMMExpLogForm.CopyCurrentMessageQueue;
	var
	u: Boolean;
	i: Integer;

	begin
	u:= False;

	with SCUMMExpLogMsgList.LockList do
		try
			if  Count > 0 then
				begin
				u:= True;

				for i:= 0 to Count - 1 do
					FMsgList.Add(Items[i]);

				Clear;
				end;
			finally
			SCUMMExpLogMsgList.UnlockList;
			end;

	if  u then
		begin
		vsttrMessages.RootNodeCount:= FMsgList.Count;
		vsttrMessages.ScrollIntoView(vsttrMessages.GetLast, False, False);
		end;
	end;

procedure TSCUMMExpLogForm.CreateParams(var Params: TCreateParams);
	begin
	inherited;

	Params.WndParent:= 0;
	end;

procedure TSCUMMExpLogForm.EmptyLocalMessageQueue;
	var
	i: Integer;

	begin
	vsttrMessages.RootNodeCount:= 0;

	for i:= FMsgList.Count - 1 downto 0 do
		Dispose(PSCUMMExpLogMsg(FMsgList.Items[i]));

	FMsgList.Clear;
	end;

procedure TSCUMMExpLogForm.FormCreate(Sender: TObject);
	begin
	vsttrMessages.NodeDataSize:= 0;
	FMsgList:= TList.Create;
	end;

procedure TSCUMMExpLogForm.FormDestroy(Sender: TObject);
	begin
	EmptyLocalMessageQueue;
	FMsgList.Free;
	end;

procedure TSCUMMExpLogForm.vsttrMessagesBeforeCellPaint(
		Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
		Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
		var ContentRect: TRect);
	var
	m: PSCUMMExpLogMsg;

	begin
	if  (CellPaintMode = cpmPaint)
	and not (vsSelected in Node.States) then
		begin
		m:= PSCUMMExpLogMsg(FMsgList[Node^.Index]);
		TargetCanvas.Brush.Color:=
				ARR_CLR_SCUMMEXP_LOGCLRS[m^.Kind];
		TargetCanvas.Brush.Style:= bsSolid;
		TargetCanvas.FillRect(CellRect);
		end;
	end;

procedure TSCUMMExpLogForm.vsttrMessagesGetImageIndex(Sender: TBaseVirtualTree;
		Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
		var Ghosted: Boolean; var ImageIndex: Integer);
	begin
	if  (Kind in [ikNormal, ikSelected])
	and (Column = 0) then
		ImageIndex:= Ord(PSCUMMExpLogMsg(FMsgList[Node^.Index])^._Type)
	else
		ImageIndex:= -1;
	end;

procedure TSCUMMExpLogForm.vsttrMessagesGetText(Sender: TBaseVirtualTree;
		Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
		var CellText: string);
	var
	m: PSCUMMExpLogMsg;

	begin
	if  Column > 0 then
		begin
		m:= PSCUMMExpLogMsg(FMsgList[Node^.Index]);

		if  Column = 1 then
			CellText:= FormatDateTime('hh:nn:ss.zzz', m^.Time)
		else if Column = 2 then
			CellText:= ARR_LIT_SCUMMEXP_LOGKIND[m^.Kind]
		else
			 CellText:= m^.Msg;
		end
	else
		CellText:= '';
	end;

end.
