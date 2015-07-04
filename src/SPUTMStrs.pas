unit SPUTMStrs;

interface

uses
	SPUTMTypes;

const
//When browsing the game data heirachy, we will need to identify the elements
//		contained within the current node.  So, given a path, we need to be
//		able to retrieve the contents and the descriptions/types of those items.

//In particular, we have a case where we have a combination of statically
//		determined path and data elements as well as variable numbers of certain
//		elements.  We will need "unique" identifiers for each data/node element
//		in a path which will refer to the appropriate TSCUMMExpEnumType and data
//		for it.  This will allow specification of ./<room*>/ nodes as well as
//		./Room/ ones (etc).

//These ids will always be relative to a path so we can have multiple
//		VAL_SCUMMEXP_SOBJID_RMSCRPTS instances in our tree but only one for
//		each node in the path:
//			//<root*>/VAL_SCUMMEXP_SOBJID_ROOMS/<room n id>/.

//This mechanism is a little strange compared to other implementations but we
//		will also be able to use it in order to provide viewers/editors for the
//		nodes.  So, given a path we will be able to look up the
//		TSCUMMExpEnumType for it and its data.

	VAL_SCUMMEXP_GLOBID_NULL: TSCUMMExpGlobId = '{00000000-0000-0000-0000-000000000000}';

//	This tells us which cacheable types are node ("folder") ones
	SET_SCUMMEXP_ENMTYP_NODETYPE: set of TSCUMMExpEnumType = [Succ(Low(TSCUMMExpEnumType))..Pred(sxeRoomScript)];
//	This tells us which cacheable types are data ("file") ones
	SET_SCUMMEXP_ENMTYP_DATATYPE: set of TSCUMMExpEnumType = [sxeUnknown, sxeRoomScript..High(TSCUMMExpEnumType)];

	LIT_SCUMMEXP_PROGRS_DEFDETAL = 'Decoding';


implementation

end.
