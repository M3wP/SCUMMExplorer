unit SCUMMKBClasses;

interface

uses
	SysUtils, Classes;

//Here we implement a simple, extensible knowledge base class.  The knowledge
//		base contains sections which contain entries.  Sections are defined by
//		sub classing TSCUMMKBSection and providing functionality for the
//		abstract methods.  Sections are then added (as class references) at
//		run-time to the TSCUMMKnowledgeBase container class.  The knowledge base
//		is then initialised ("read in") from the UI as part of its loading.
//
//		The TSCUMMKnowledgeBase handles all of the management details including
//		driving the sections to read from a binary stream when available or
//		"building" from tab delimited text files as necessary - if the binary
//		files are out of date, for example.  Some limited manipulation of
//		a section's entries is possible during run-time.
//
//		A section descendant should declare methods or properties to access
//		section specific entry data directly.  A reference to a section
//		instance can be obtained via the SCUMMKnowledgeBase global.  These
//		references should be cast as required in order to access the entries
//		they contain.
//
//		The knowledge base functionality was deemed necessary since SCUMMVM
//		contains many, very large constant arrays of complex, structured data.
//		Conversion of that data is going to be a arduous chore and this
//		functionality allows a simpler conversion process.  Additionally, the
//		knowledge base is extensible, allows for modular expansion and
//		simplified application enhancement, centralised data management,
//		run-time manipulation and user configuration.


//todo Header information should be stamped into the data files in order to
//		test for version changes and rebuild on this condition, as well.

//todo Resolve endian issues and best implementation.

//todo Single data file capability formed from chunks?

//todo Implement as wrapper to in process SQL server and tables?  I just don't
//		think the volume is going to warrant it  --  ever.

type
	ESCUMMKBSectionExists = class(Exception);
	ESCUMMKBConvertError = class(Exception);

	TSCUMMKBSection = class
	private
		FStale: Boolean;

	protected
		function  GetNextToken(const AString: string; var AStart: Integer;
				var AToken: string): Boolean;
		function  ReadStringFromStream(const AStream: TStream): string;
		procedure WriteStringToStream(const AString: string;
				const AStream: TStream);
		function  MakeStringField(const AString: string): string;

		class function  GetName: string; virtual; abstract;
		class function  GetDescription: string; virtual; abstract;
		class function  GetFilename: string; virtual; abstract;

		function  GetEntryCount: Integer; virtual; abstract;

		procedure Clear; virtual; abstract;
		procedure AddEntryFromString(const AString: string); virtual; abstract;

		procedure ReadFromStream(const AStream: TStream); virtual; abstract;

		procedure WriteEntryToStream(const AIndex: Integer;
				const AStream: TStream); virtual; abstract;
		procedure WriteEntryToString(const AIndex: Integer;
				var AString: string); virtual; abstract;

		property  Stale: Boolean read FStale write FStale;

	public
    	constructor Create; virtual;

		procedure WriteToStream(const AStream: TStream);
		procedure WriteToStrings(const AStrings: TStrings);

		property  Name: string read GetName;
		property  Description: string read GetDescription;

		property  Filename: string read GetFilename;
		property  EntryCount: Integer read GetEntryCount;
	end;

	TSCUMMKBSectionClass = class of TSCUMMKBSection;

	TSCUMMKnowledgeBase = class
	private
		FSectionNames: TStringList;
		FSectionInsts: TList;

	protected
		function  GetSectionCount: Integer;
		function  GetSections(const AIndex: Integer): TSCUMMKBSection;
		function  GetSectionByName(const AName: string): TSCUMMKBSection;

		procedure DoBuildFromStrings(const ASection: TSCUMMKBSection;
				const AStrings: TStrings);

	public
		constructor Create;
		destructor  Destroy; override;

		procedure AddSection(const AClass: TSCUMMKBSectionClass);
		procedure Prepare(const APath: string);
		procedure BuildFromStrings(const AName: string; const AStrings: TStrings);

		function  GetSectionOfClass(const AClass: TSCUMMKBSectionClass): TSCUMMKBSection;

		property  SectionCount: Integer read GetSectionCount;
		property  Sections[const AIndex: Integer]: TSCUMMKBSection read GetSections; default;
		property  SectionByName[const AName: string]: TSCUMMKBSection read GetSectionByName;
	end;

var
	SCUMMKnowledgeBase: TSCUMMKnowledgeBase;

implementation

uses
	Types, StrUtils, IOUtils, DateUtils, SCUMMLogTypes;

{ TSCUMMKBSection }

constructor TSCUMMKBSection.Create;
	begin
	inherited Create;

	FStale:= False;

//fixme This should make sure that we're only being created by the knowledge
//		base container.
	end;

function TSCUMMKBSection.GetNextToken(const AString: string;
		var AStart: Integer; var AToken: string): Boolean;
	var
	n: Integer;

	begin
	n:= PosEx(#$09, AString, AStart);

	if  n < 0 then
		Result:= False
	else if n > 0 then
		Result:= True
	else if  AStart <= Length(AString) then
			begin
			n:= Length(AString) + 1;
			Result:= True;
			end
		else
			Result:= False;

	if  Result then
		begin
		AToken:= Trim(Copy(AString, AStart, n - AStart));
		AStart:= n + 1;
		end;
	end;

function TSCUMMKBSection.MakeStringField(const AString: string): string;
	begin
	if  Length(AString) = 0 then
		Result:= ' '
	else
		Result:= AString;
	end;

function TSCUMMKBSection.ReadStringFromStream(const AStream: TStream): string;
	var
	l: Word;
	u: UTF8String;

	begin
	if  AStream.Read(l, 2) = 2 then
		if l > 0 then
			begin
			SetLength(u, l);
			if  AStream.Read(u[1], l) <> l then
				raise ESCUMMKBConvertError.Create(
						'Out of data reading string from stream');

			Result:= string(u);
			end
		else
			Result:= '';
	end;

procedure TSCUMMKBSection.WriteStringToStream(const AString: string;
		const AStream: TStream);
	var
	l: Word;
	u: UTF8String;

	begin
	u:= UTF8String(AString);
	l:= Length(u);

	AStream.Write(l, 2);
	AStream.Write(u[1], l);
	end;

procedure TSCUMMKBSection.WriteToStream(const AStream: TStream);
	var
	i: Integer;

	begin
	for i:= 0 to EntryCount - 1 do
		WriteEntryToStream(i, AStream);
	end;

procedure TSCUMMKBSection.WriteToStrings(const AStrings: TStrings);
	var
	s: string;
	i: Integer;

	begin
	for i:= 0 to EntryCount - 1 do
		begin
		WriteEntryToString(i, s);
		AStrings.Add(s);
		end;
	end;


{ TSCUMMKnowledgeBase }

procedure TSCUMMKnowledgeBase.AddSection(const AClass: TSCUMMKBSectionClass);
	var
	n: string;
	s: TSCUMMKBSection;

	begin
	n:= AClass.GetName;
	if  FSectionNames.IndexOf(n) <> -1 then
		raise ESCUMMKBSectionExists.Create('Section "' + n +
				'" already exists in knowledge base.');

	s:= AClass.Create;
	FSectionNames.AddObject(n, TObject(Pointer(AClass)));
	FSectionInsts.Add(s);

	SCUMMExpLogInform(sxkKnowledgeBase, 'Added section "%s".', [n]);
	end;

procedure TSCUMMKnowledgeBase.BuildFromStrings(const AName: string;
		const AStrings: TStrings);
	begin
	DoBuildFromStrings(SectionByName[AName], AStrings);
	end;

constructor TSCUMMKnowledgeBase.Create;
	begin
//fixme This should check that there is only ever a single instance as the
//		SCUMMKnowledgeBase global.

	inherited Create;

	FSectionNames:= TStringList.Create;
	FSectionInsts:= TList.Create;
	end;

destructor TSCUMMKnowledgeBase.Destroy;
	var
	i: Integer;

	begin
	for i:= FSectionInsts.Count - 1 downto 0 do
		TSCUMMKBSection(FSectionInsts[i]).Free;

	FSectionInsts.Free;
	FSectionNames.Free;

	inherited;
	end;

procedure TSCUMMKnowledgeBase.DoBuildFromStrings(
		const ASection: TSCUMMKBSection; const AStrings: TStrings);
	var
	s: string;
	i: Integer;

	begin
	for i:= 0 to AStrings.Count - 1 do
		begin
		s:= AStrings[i];
		if  Length(s) > 0 then
			if  s[1] <> '#' then
				ASection.AddEntryFromString(s);
		end;
	end;

function TSCUMMKnowledgeBase.GetSectionByName(
		const AName: string): TSCUMMKBSection;
	var
	i: Integer;

	begin
	i:= FSectionNames.IndexOf(AName);
	if  i >= 0 then
		Result:= TSCUMMKBSection(FSectionInsts[i])
	else
		Result:= nil;
	end;

function TSCUMMKnowledgeBase.GetSectionCount: Integer;
	begin
	Result:= FSectionNames.Count;
	end;

function TSCUMMKnowledgeBase.GetSectionOfClass(
		const AClass: TSCUMMKBSectionClass): TSCUMMKBSection;
	var
	i: Integer;

	begin
	Result:= nil;
	for i:= 0 to FSectionNames.Count - 1 do
		if  TSCUMMKBSectionClass(Pointer(FSectionNames.Objects[i])) = AClass then
			begin
			Result:= TSCUMMKBSection(FSectionInsts[i]);
			Break;
			end;
	end;

function TSCUMMKnowledgeBase.GetSections(
		const AIndex: Integer): TSCUMMKBSection;
	begin
	Result:= TSCUMMKBSection(FSectionInsts[AIndex]);
	end;

procedure TSCUMMKnowledgeBase.Prepare(const APath: string);
	var
	i: Integer;
	s: TSCUMMKBSection;
	n,
	sp,
	bp: string;
	fs: TFileStream;
	sl: TStringList;
	be,
	br: Boolean;

	begin
//fixme This needs revision for FPC compatibility
	sl:= TStringList.Create;
	try
		for i:= 0 to FSectionInsts.Count - 1 do
			begin
			n:= TSCUMMKBSectionClass(FSectionNames.Objects[i]).GetFilename;
			s:= TSCUMMKBSection(FSectionInsts[i]);
			s.Clear;

			sp:= TPath.Combine(APath, TPath.ChangeExtension(n, 'tab'));
			bp:= TPath.Combine(APath, TPath.ChangeExtension(n, 'dat'));

			be:= TFile.Exists(bp);
			br:= False;
			if  TFile.Exists(sp)
			and ((not be)
			or   (CompareDateTime(TFile.GetLastWriteTime(bp),
					TFile.GetLastWriteTime(sp)) = LessThanValue)) then
				br:= True;

			if  br then
				begin
				fs:= TFileStream.Create(sp, fmOpenRead);
				try
					sl.LoadFromStream(fs);
					DoBuildFromStrings(s, sl);

					SCUMMExpLogInform(sxkKnowledgeBase,
							'Loaded section "%s" from string table.',
							[TSCUMMKBSectionClass(FSectionNames.Objects[i]).GetName]);

					finally
					fs.Free;
					end;

				fs:= TFileStream.Create(bp, fmCreate);
				try
					s.WriteToStream(fs);

					SCUMMExpLogInform(sxkKnowledgeBase,
							'Saved section "%s" to binary stream.',
							[TSCUMMKBSectionClass(FSectionNames.Objects[i]).GetName]);

					finally
					fs.Free;
					end;
				end
			else if be then
				begin
				fs:= TFileStream.Create(bp, fmOpenRead);
				try
					s.ReadFromStream(fs);

					SCUMMExpLogInform(sxkKnowledgeBase,
							'Loaded section "%s" from binary stream.',
							[TSCUMMKBSectionClass(FSectionNames.Objects[i]).GetName]);

					finally
					fs.Free;
					end;
				end;
			end;

		finally
		sl.Free;
		end;
	end;


end.
