unit SPUTMPlatAmigaTypes;

interface

uses
	Classes, VCL.Imaging.PNGImage;


procedure DrawCostumeFrame(ASource: TMemoryStream; ACX, ACY: Integer;
		AMirror: Boolean; out AImg: TPNGImage);

procedure DecodeRoomImage(AData: TMemoryStream; AWidth, AHeight: Word;
		out ADest: TPNGImage);

implementation

uses
	VCL.Graphics, SPUTMPlatAmigaConsts, SysUtils;


procedure DrawCostumeFrame(ASource: TMemoryStream; ACX, ACY: Integer;
		AMirror: Boolean; out AImg: TPNGImage);
	var
	x,
	y,
	w: Integer;
	bsl: PByte;
	asl: Vcl.Imaging.pngimage.PByteArray;
	c: TColor;
	b: Byte;

	begin
	AImg:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, ACX, ACY);

//dengland Have to be able to handle this for Zak.
	if  ASource.Size = 0 then
		Exit;

	if  AMirror then
		begin
		x:= ACX - 1;
		w:= 0;
		end
	else
		begin
		x:= 0;
		w:= ACX - 1;
		end;

//	for x:= 0 to ACX - 1 do
	repeat
		for y:= 0 to ACY - 1 do
			begin
			bsl:= PByte(AImg.Scanline[y]);
			Inc(bsl,  x * 3);
			asl:= AImg.AlphaScanline[y];

			ASource.Read(b, 1);
			asl[x]:= b;

			ASource.Read(b, 1);
			c:= AMIGA_PALETTE[b];

			bsl^:= (c and $FF0000) shr 16;
			Inc(bsl);
			bsl^:= (c and $00FF00) shr 8;
			Inc(bsl);
			bsl^:= (c and $0000FF);
			end;

		if AMirror then
			begin
			Dec(x);
			if  x < 0 then
				Break;
			end
		else
			begin
			Inc(x);
			if  x > w then
				Break;
			end;
		until False;
	end;

procedure DecodeRoomImage(AData: TMemoryStream; AWidth, AHeight: Word;
		out ADest: TPNGImage);
	var
	dstTmp: array of Byte;
	dst: PByte;
//	table: TStripTable;
//	byte *mask_ptr;
	src: PByte;
	color,
	data: Byte;
	run: Integer;
	dither: Boolean;
	dither_table: array[0..127] of Byte;
	ptr_dither_table: PByte;
	theX,
	theY,
	maxX: Integer;
//	dstX,
//	dstY: Integer;

	begin
	Assert(AHeight <= 128, 'Height is ' + IntToStr(AHeight));

//	GenerateStripTable(AData, AWidth, AHeight, table);

	SetLength(dstTmp, AWidth * AHeight);
	dst:= @dstTmp[0];

//	data:= 0;
	dither:= False;
	FillChar(dither_table[0], SizeOf(dither_table), 0);

//	run:= table.run[0];
//	color:= table.color[0];
//	src:= PByte(AData.Memory) + AData.Position + table.offsets[0];
//	theX:= 0;
//	maxX:= AWidth;
	run:= 1;
	color:= 0;
	src:= PByte(AData.Memory) + AData.Position;
	theX:= 0;
	maxX:= AWidth;

	while theX < maxX do
		begin
		ptr_dither_table:= @dither_table[0];

		for theY:= 0 to AHeight - 1 do
			begin
			Dec(run);
			if  run = 0 then
				begin
				data:= src^;
				Inc(src);

				if  (data and $80) <> 0 then
					begin
					run:= data and $7F;
					dither:= True;
					end
				else
					begin
					run:= data shr 4;
					dither:= False;
					end;

//				color:= AMIGA_PALETTE[data and $0F];
				color:= data and $0F;
				if  run = 0 then
					begin
					run:= src^;
					Inc(src);
					end;
				end;

			if not dither then
				ptr_dither_table^:= color;

			if  (0 <= theX)
			and (theX < AWidth) then
				begin
				dst^:= ptr_dither_table^;
				Inc(ptr_dither_table);
				Inc(dst, AWidth);
				end;
			end;

		if  (0 <= theX)
		and (theX < AWidth) then
			begin
//			height * vs->pitch - 1 * vs->format.bytesPerPixel
			Dec(dst, AHeight * AWidth - 1);
			end;

		Inc(theX);
		end;

	ADest:= TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, AWidth, AHeight);

	dst:= @dstTmp[0];
	for theY:= 0 to AHeight - 1 do
		for theX:= 0 to AWidth - 1 do
			begin
			Assert(dst^ < 16, 'pixel colour out of range');

			ADest.Pixels[theX, theY]:= AMIGA_PALETTE[dst^];
			ADest.AlphaScanline[theY]^[theX]:= $FF;

			Inc(dst);
			end;
	end;
end.
