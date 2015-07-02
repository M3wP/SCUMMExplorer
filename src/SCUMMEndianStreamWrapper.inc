function  READ_LE_UINT8(AStream: TStream; const AEncByte: Byte = $00): Byte; inline;
	begin
	AStream.Read(Result, 1);

	if  AEncByte > 0 then
		Result:= Result xor AEncByte;
	end;

function  READ_LE_UINT16(AStream: TStream; const AEncByte: Byte = $00): Word; inline;
	var
	a,
	b: Byte;

	begin
	a:= READ_LE_UINT8(AStream, AEncByte);
	b:= READ_LE_UINT8(AStream, AEncByte);

	Result:= (b shl 8) or a;
	end;
