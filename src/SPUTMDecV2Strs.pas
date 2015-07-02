unit SPUTMDecV2Strs;

interface

uses
	SCUMMTypes, SPUTMTypes;

const
	ARR_REC_SCUMMEXP_DECDV2_PROPS: array[0..0] of TSCUMMExpDecCompat = (
		(game: scgUnk;
		 vers: scv2;
		 subv: 0;
		 feat: [];
		 plat: scpUnk));

	ARR_REC_SCUMMEXP_COVWV2_PROPS: array[0..0] of TSCUMMExpViewerCompat = (
		(game: scgUnk;
		 vers: scv2;
		 subv: 0;
		 feat: [];
		 plat: scpUnk;
		 enum: sxeCostumeCntnr));

resourcestring
	STR_SCUMMEXP_DECDDS_OTHER = 'Unknown';
	STR_SCUMMEXP_DECDDS_MMNSN = 'Maniac Mansion';
	STR_SCUMMEXP_DECDDS_ZKMCK = 'Zak McKracken and the Alien Mindbenders';

	STR_SCUMMEXP_DECDVS_DECNM = 'SCUMM V2 Decoder';
	STR_SCUMMEXP_DECDVS_DECDS = 'Decoder for all SCUMM V2 games.';

	STR_SCUMMEXP_COVWVS_VWRNM = 'SCUMM V2 Costume Viewer';
	STR_SCUMMEXP_COVWVS_VWRDS = 'Viewer for SCUMM V2 costumes.';


implementation

end.
