unit SCUMMLogStrs;

interface

uses
	SCUMMLogTypes;

const
	ARR_LIT_SCUMMEXP_LOGKIND: array[TSCUMMExpLogKind] of string = (
			'Unknown', 'PluginManager', 'Explorer', 'KnowledgeBase',
			'Detector', 'Reflector', 'Enumerator', 'EnumCache', 'Decoder',
			'Decompiler', 'Viewer');


implementation

end.
