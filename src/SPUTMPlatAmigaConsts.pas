unit SPUTMPlatAmigaConsts;


interface

uses
	VCL.Graphics;

const
	LIT_PLTFRM_AMIGA_DESCRP = 'Amiga';

//	static const byte tableAmigaPalette[] = {
//		0x00, 0x00, 0x00, 	0x00, 0x00, 0xBB, 	0x00, 0xBB, 0x00, 	0x00, 0xBB, 0xBB,
//		0xBB, 0x00, 0x00, 	0xBB, 0x00, 0xBB, 	0xBB, 0x77, 0x00, 	0xBB, 0xBB, 0xBB,
//		0x77, 0x77, 0x77, 	0x77, 0x77, 0xFF, 	0x00, 0xFF, 0x00, 	0x00, 0xFF, 0xFF,
//		0xFF, 0x88, 0x88, 	0xFF, 0x00, 0xFF, 	0xFF, 0xFF, 0x00, 	0xFF, 0xFF, 0xFF
//	};
//
//	static const byte tableAmigaMIPalette[] = {
//		0x00, 0x00, 0x00, 	0x00, 0x00, 0xAA, 	0x00, 0x88, 0x22, 	0x00, 0x66, 0x77,
//		0xBB, 0x66, 0x66, 	0xAA, 0x22, 0xAA, 	0x88, 0x55, 0x22, 	0x77, 0x77, 0x77,
//		0x33, 0x33, 0x33, 	0x22, 0x55, 0xDD, 	0x22, 0xDD, 0x44, 	0x00, 0xCC, 0xFF,
//		0xFF, 0x99, 0x99, 	0xFF, 0x55, 0xFF, 	0xFF, 0xFF, 0x77, 	0xFF, 0xFF, 0xFF
//	};

	ARR_PLTFRM_AMIGA_IMGPAL: array[0..15] of TColor = (
		TColor($000000),	//black
		TColor($BB0000),	//blue
		TColor($00BB00),	//green
		TColor($BBBB00),	//dark cyan
		TColor($0000BB),	//red
		TColor($BB00BB),	//purple
		TColor($0077BB),	//brown
		TColor($BBBBBB),	//light gray
		TColor($777777),	//dark gray
		TColor($FF7777),	//light blue
		TColor($00FF00),	//light green
		TColor($FFFF00),	//light cyan
		TColor($8888FF),	//pink
		TColor($FF00FF),	//light purple
		TColor($00FFFF),	//*yellow*
		TColor($FFFFFF));	//white

	ARR_PLTFRM_AMIGA_CSTPAL: array[0..15] of TColor = (
		TColor($FF000000),	//transparent
		TColor($BB0000),	//blue
		TColor($00BB00),	//green
		TColor($BBBB00),	//dark cyan
		TColor($0000BB),	//red
		TColor($BB00BB),	//purple
		TColor($0077BB),	//brown
		TColor($BBBBBB),	//light gray
		TColor($777777),	//dark gray
		TColor($FF7777),	//light blue
		TColor($00FF00),	//light green
		TColor($FFFF00),	//light cyan
		TColor($8888FF),	//pink
		TColor($FF00FF),	//light purple
		TColor($00FFFF),	//*yellow*
		TColor($FFFFFF));	//white

//	ARR_PLTFRM_AMIGA_IMGPAL: array[0..15] of TColor = (
//		TColor($000000),	//black
//		TColor($BD0000),	//blue
//		TColor($00BA00),	//green
//		TColor($BDBA00),	//dark cyan
//		TColor($0000BD),	//red
//		TColor($BD00BD),	//purple
//		TColor($0075BD),	//brown
//		TColor($BDBABD),	//light gray
//		TColor($737573),	//dark gray
//		TColor($FF7573),	//light blue
//		TColor($00FF00),	//light green
//		TColor($FFFF00),	//light cyan
//		TColor($8C8AFF),	//pink
//		TColor($FF00FF),	//light purple
//		TColor($00FFFF),	//*yellow*
//		TColor($FFFFFF));	//white
//
//	ARR_PLTFRM_AMIGA_CSTPAL: array[0..15] of TColor = (
//		TColor($FF000000),	//transparent
//		TColor($000000),	//black
//		TColor($00BA00),	//green
//		TColor($BDBA00),	//dark cyan
//		TColor($0000BD),	//red
//		TColor($BD00BD),	//purple
//		TColor($0075BD),	//brown
//		TColor($BDBABD),	//light gray
//		TColor($737573),	//dark gray
//		TColor($FF7573),	//light blue
//		TColor($00FF00),	//light green
//		TColor($FFFF00),	//light cyan
//		TColor($8C8AFF),	//pink
//		TColor($FF00FF),	//light purple
//		TColor($00FFFF),	//*yellow*
//		TColor($FFFFFF));	//white


implementation

end.
