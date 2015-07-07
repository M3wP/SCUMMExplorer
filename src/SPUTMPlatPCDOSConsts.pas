unit SPUTMPlatPCDOSConsts;

interface

uses
	VCL.Graphics;

const
	LIT_PLTFRM_PCDOS_DESCRP = 'PC DOS';

//	static const byte tableEGAPalette[] = {
//		0x00, 0x00, 0x00, 	0x00, 0x00, 0xAA, 	0x00, 0xAA, 0x00, 	0x00, 0xAA, 0xAA,
//		0xAA, 0x00, 0x00, 	0xAA, 0x00, 0xAA, 	0xAA, 0x55, 0x00, 	0xAA, 0xAA, 0xAA,
//		0x55, 0x55, 0x55, 	0x55, 0x55, 0xFF, 	0x55, 0xFF, 0x55, 	0x55, 0xFF, 0xFF,
//		0xFF, 0x55, 0x55, 	0xFF, 0x55, 0xFF, 	0xFF, 0xFF, 0x55, 	0xFF, 0xFF, 0xFF
//	};

	ARR_PLTFRM_PCDOS_IMGPAL: array[0..15] of TColor = (
		TColor($000000),	//black
		TColor($AA0000),	//blue
		TColor($00AA00),	//green
		TColor($AAAA00),	//dark cyan
		TColor($0000AA),	//red
		TColor($AA00AA),	//purple
		TColor($0055AA),	//brown
		TColor($AAAAAA),	//light gray
		TColor($555555),	//dark gray
		TColor($FF5555),	//light blue
		TColor($55FF55),	//light green
		TColor($FFFF55),	//light cyan
		TColor($5555FF),	//pink
		TColor($FF55FF),	//light purple
		TColor($55FFFF),	//*yellow*
		TColor($FFFFFF));	//white

	ARR_PLTFRM_PCDOS_CSTPAL: array[0..15] of TColor = (
		TColor($FF000000),	//transparent
		TColor($000000),	//black
		TColor($00AA00),	//green
		TColor($AAAA00),	//dark cyan
		TColor($0000AA),	//red
		TColor($AA00AA),	//purple
		TColor($0055AA),	//brown
		TColor($AAAAAA),	//light gray
		TColor($555555),	//dark gray
		TColor($FF5555),	//light blue
		TColor($55FF55),	//light green
		TColor($FFFF55),	//light cyan
		TColor($5555FF),	//pink
		TColor($FF55FF),	//light purple
		TColor($55FFFF),	//*yellow*
		TColor($FFFFFF));	//white

implementation

end.
