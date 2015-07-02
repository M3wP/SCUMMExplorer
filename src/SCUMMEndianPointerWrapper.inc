function  READ_LE_UINT32(ptr: Pointer): Cardinal; inline;
	begin
	Result:= PCardinal(ptr)^;
	end;

procedure WRITE_LE_UINT32(ptr: Pointer; value: Cardinal); inline;
	begin
	PCardinal(ptr)^:= value;
	end;

procedure GET_UINT32(var val: Cardinal; base: PByte; offs: Integer); inline;
	begin
	val:= READ_LE_UINT32(base + offs);
	end;

procedure PUT_UINT32(val: Cardinal; base: PByte; offs: Integer); inline;
	begin
	WRITE_LE_UINT32(base + offs, val);
	end;
