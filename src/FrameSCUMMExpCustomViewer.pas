unit FrameSCUMMExpCustomViewer;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
	Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SCUMMTypes, SCUMMClasses,
	SPUTMTypes;

type
	TSCUMMExpCustomViewerFrame = class(TFrame)
	protected
//		FHostNode: TSCUMMHostNode;
//		FDetectData: TSCUMMDetectorData;

	public
//		constructor Create(AHostNode: TSCUMMHostNode;
//				ADetectData: TSCUMMDetectorData);

		class function  GetName: string; virtual; abstract;
		class function  GetDescription: string; virtual; abstract;

		class function  GetCompatibility(const ACallIdx: Integer;
				out ASupports: TSCUMMExpViewerCompat): Boolean; virtual; abstract;

		class function  CanViewPath(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData;
				APath: TSCUMMExpGlobIdArr): Boolean; virtual; abstract;

		procedure PreviewPath(AHostNode: TSCUMMHostNode;
				ADetectData: TSCUMMDetectorData;
				APath: TSCUMMExpGlobIdArr); virtual; abstract;

		property  Name: string read GetName;
		property  Description: string read GetDescription;

//		property  HostNode: TSCUMMHostNode read FHostNode;
//		property  DetectData: TSCUMMDetectorData read FDetectData;
	end;

	TSCUMMExpViewerClass = class of TSCUMMExpCustomViewerFrame;


implementation

{$R *.dfm}

end.
