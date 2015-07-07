unit FrameSCUMMExpMain;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
	Vcl.ComCtrls, Vcl.ExtCtrls, VCL.Imaging.PNGImage, SPUTMTypes, Vcl.StdCtrls,
	FrameSCUMMExpCustomViewer;

type
	TSCUMMExpMainFrame = class(TFrame)
		Panel1: TPanel;
		Splitter2: TSplitter;
		ListView1: TListView;
		Splitter1: TSplitter;
		TreeView1: TTreeView;
		Panel2: TPanel;
		RichEdit1: TRichEdit;
		ScrollBox1: TScrollBox;
		Image1: TImage;
		procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
		procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
				var AllowExpansion: Boolean);
		procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
		procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
				Selected: Boolean);
		procedure ListView1DblClick(Sender: TObject);
	private
		FLastViewer: TSCUMMExpCustomViewerFrame;

	protected
		procedure ClearAll;
		procedure ClearDisplay;
		procedure ClearPreview;
		procedure ConstructPathForNode(const ANode: TTreeNode;
				var APath: TSCUMMExpGlobIdArr);
		procedure RequestDecodePath(APath: TSCUMMExpGlobIdArr);
	public
		procedure UpdateDisplay;
	end;

implementation

uses
	SCUMMTypes, SCUMMClasses, SPUTMStrs, SPUTMClasses, SCUMMExpTypes,
	DModSCUMMExpMain;

{$R *.dfm}

{ TSCUMMExpMainFrame }

procedure TSCUMMExpMainFrame.ClearAll;
	begin
	TreeView1.Items.Clear;
	ClearDisplay;
	end;

procedure TSCUMMExpMainFrame.ClearDisplay;
	begin
	ListView1.Items.Clear;
	ClearPreview;
	end;

procedure TSCUMMExpMainFrame.ClearPreview;
	begin
	if  Assigned(FLastViewer) then
		begin
		FLastViewer.Visible:= False;
		FLastViewer.Free;
        FLastViewer:= nil;
		end;

	ScrollBox1.Visible:= False;
	RichEdit1.Visible:= False;

	Image1.Picture.Bitmap.Width:= 0;
	Image1.Picture.Bitmap.Height:= 0;

	RichEdit1.Lines.Clear;
	end;

procedure TSCUMMExpMainFrame.ConstructPathForNode(const ANode: TTreeNode;
		var APath: TSCUMMExpGlobIdArr);
	var
	i: Integer;
	c: TTreeNode;

	begin
	c:= ANode;

//	Construct path for selected
	if  ANode.Level = 0 then
		begin
		SetLength(APath, 2);
		APath[1]:= VAL_SCUMMEXP_GLOBID_NULL;
		end
	else
		SetLength(APath, ANode.Level + 1);

	for i:= ANode.Level downto 0 do
		begin
		if  Assigned(c.Data) then
			APath[i]:= PSCUMMExpObjData(c.Data)^.id
		else
			APath[i]:= VAL_SCUMMEXP_GLOBID_NULL;

		c:= c.Parent;
		end;
	end;

procedure TSCUMMExpMainFrame.ListView1DblClick(Sender: TObject);
	var
	n: TTreeNode;
	d: PSCUMMExpObjData;

	begin
//	From the selected node type list item, get the tree node
	d:= PSCUMMExpObjData(ListView1.Selected.Data);

	if  Assigned(d)
	and (d^.enumType in SET_SCUMMEXP_ENMTYP_NODETYPE) then
		begin
//		pass off the update to the tree view event
		n:= TreeView1.Selected.Item[ListView1.Selected.Index];

//		TreeView1Change(Sender, n);
		TreeView1.Select(n, []);
		end;
	end;

procedure TSCUMMExpMainFrame.ListView1SelectItem(Sender: TObject;
		Item: TListItem; Selected: Boolean);
	var
	s: TSCUMMExpGlobIdArr;
//	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;
	hn: TSCUMMHostNode;
	dd: TSCUMMDetectorData;
	f: TSCUMMExpCustomViewerFrame;

	begin
	ClearPreview;

	if  Selected
	and Assigned(Item.Data) then
		begin
		d:= PSCUMMExpObjData(Item.Data);
		ConstructPathForNode(TreeView1.Selected, s);
		if  not (s[High(s)] = VAL_SCUMMEXP_GLOBID_NULL) then
			SetLength(s, Length(s) + 1);

		s[High(s)]:= d^.id;

//		if  d^.enumType in SET_SCUMMEXP_ENMTYP_DATATYPE then
//			begin
			if  not d^.decoded then
				RequestDecodePath(s);

			if  d^.decoded then
				begin
//TODO dengland Implement custom preview frames.
				hn:= SCUMMExpGames.GameHostNode[s[0]];
				dd:= SCUMMExpGames.GameDetectData[s[0]];
				f:= SCUMMExpViewerReflector.GetViewerForPath(hn, dd, s);

				if  Assigned(f) then
					begin
					f.PreviewPath(hn, dd, s);
					f.Parent:= Panel2;
					f.Align:= alClient;
					FLastViewer:= f;
					f.Visible:= True;
					end
				else if  d^.enumType in SET_SCUMMEXP_ENMTYP_DATATYPE then
					begin
					if d^.dataType in [sxdXML, sxdScript] then
						begin
						RichEdit1.Visible:= True;

						if d^.dataType = sxdXML then
							begin
							d^.xmlData.Seek(0, soFromBeginning);
							RichEdit1.Lines.LoadFromStream(d^.xmlData);
							end
						else
							begin
							d^.scriptData.Seek(0, soFromBeginning);
							RichEdit1.Lines.LoadFromStream(d^.scriptData);
							end;
						end
					else if d^.dataType = sxdImage then
						begin
						ScrollBox1.Visible:= True;
	//					d^.imageData.Seek(0, soFromBeginning);
	//					Image1.Picture.Bitmap.LoadFromStream(d^.imageData);
						Image1.Picture.Graphic.Assign(d^.imageData);
                        end;
					end;
				end;
//			end;
		end;
	end;

procedure TSCUMMExpMainFrame.RequestDecodePath(APath: TSCUMMExpGlobIdArr);
	var
	e: TSCUMMExpEnumerator;

	begin
	e:= SCUMMExpGames.GameEnumerator[APath[0]];
	if  Assigned(e) then
		e.DecodePath(APath);
	end;

procedure TSCUMMExpMainFrame.TreeView1Change(Sender: TObject; Node: TTreeNode);
	var
	t: TTreeNode;
	l: TListItem;
	s,
	p: TSCUMMExpGlobIdArr;
	n: TSCUMMExpCacheNodeIterator;
	o: TSCUMMExpCacheDataIterator;
	d: PSCUMMExpObjData;
	f: Boolean;

	begin
	ListView1.Items.BeginUpdate;
	TreeView1.Items.BeginUpdate;
	try
		ClearDisplay;

//		Construct path for selected
		ConstructPathForNode(Node, s);

//		Check need decode
		d:= SCUMMExpEnumCache.GetDataForPath(s);
		if  not d^.decoded then
			RequestDecodePath(s);

//		Populate tree and list with container items
		f:= Node.Count = 0;
		n:= TSCUMMExpCacheNodeIterator.Create(s);
		try
			SetLength(p, 2);
			p[0]:= s[0];
			while not n.IsEndOfList do
				begin
//				Construct path for data item
				p[1]:= n.ThisData^.id;

				d:= SCUMMExpEnumCache.GetDataForPath(p);
				if  f then
					begin
					t:= TreeView1.Items.AddChildObject(Node, d^.name, d);
					t.ImageIndex:= 0;
					end;

				l:= ListView1.Items.Add;
				l.Caption:= d^.name;
				l.Data:= d;
				l.ImageIndex:= 0;

				n.Next;
				end;

			finally
			n.Free;
			end;

//		Populate list with data items
		o:= TSCUMMExpCacheDataIterator.Create(s);
		try
			SetLength(p, 2);
			p[0]:= s[0];
			while not o.IsEndOfList do
				begin
//				Construct path for data item
				p[1]:= o.ThisData^.id;

				d:= SCUMMExpEnumCache.GetDataForPath(p);
				l:= ListView1.Items.Add;
				l.Caption:= d^.name;
				l.Data:= d;
				l.ImageIndex:= 2;

				o.Next;
				end;

			finally
			n.Free;
			end;

		finally
		TreeView1.Items.EndUpdate;
		ListView1.Items.EndUpdate;
		end;
	end;

procedure TSCUMMExpMainFrame.TreeView1Collapsed(Sender: TObject;
		Node: TTreeNode);
	begin
	Node.ImageIndex:= 0;
	Node.SelectedIndex:= Node.ImageIndex;
	end;

procedure TSCUMMExpMainFrame.TreeView1Expanding(Sender: TObject;
		Node: TTreeNode; var AllowExpansion: Boolean);
	begin
	Node.ImageIndex:= 1;
	Node.SelectedIndex:= Node.ImageIndex;
	end;

procedure TSCUMMExpMainFrame.UpdateDisplay;
	var
	i: Integer;
	t: TTreeNode;
	g: TSCUMMExpGlobId;
	p: TSCUMMExpGlobIdArr;
	d: PSCUMMExpObjData;

	begin
//fixme dengland Should I implement the main frame using TVirtualStringTrees?
	TreeView1.Items.BeginUpdate;
	try
		ClearAll;

		SetLength(p, 1);
		for i:= 0 to SCUMMExpGames.GameCount - 1 do
			begin
			g:= SCUMMExpGames[i];
			p[0]:= g;
			d:= SCUMMExpEnumCache.GetDataForPath(p);

			t:= TreeView1.Items.AddChildObject(nil, SCUMMExpGames.GameDesc[g], d);
			t.ImageIndex:= 0;
			end;

		finally
		TreeView1.Items.EndUpdate;
		end;

	TreeView1.Select(TreeView1.Items[0]);
	end;

end.
