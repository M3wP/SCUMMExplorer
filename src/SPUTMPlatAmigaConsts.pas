unit SPUTMPlatAmigaConsts;


interface

uses
	VCL.Graphics;

const
	AMIGA_PALETTE: array[0..15] of TColor = (
		TColor($000000),	//black
		TColor($BD0000),	//blue
		TColor($00BA00),	//green
		TColor($BDBA00),	//dark cyan
		TColor($0000BD),	//red
		TColor($BD00BD),	//purple
		TColor($0075BD),	//brown
		TColor($BDBABD),	//light gray
		TColor($737573),	//dark gray
		TColor($FF7573),	//light blue
		TColor($00FF00),	//light green
		TColor($FFFF00),	//light cyan
		TColor($8C8AFF),	//pink
		TColor($FF00FF),	//light purple
		TColor($00FFFF),	//*yellow*
		TColor($FFFFFF));	//white


implementation

end.
