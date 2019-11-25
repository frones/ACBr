// *****************************************************************************
//  Title.............. :  GZIP / deflate / inflate Streams with PasZLib
//
//  Modulname ......... :  gziputils.pas
//  Type .............. :  Unit
//  Author ............ :  Udo Schmal
//  Development Status  :  20.03.2016
//  Operating System .. :  Win32/Win64
//  IDE ............... :  Delphi & Lazarus
//  https://www.gocher.me/GZIP
// *****************************************************************************

{
 Data: 19/08/2017
 [+] Compatibilização com D7 a XE
 [*] Ajustes para usar método Deflate nativo (Tcompressionstream, Tdecompressionstream)
     implementados em ACBrZLib
 [*] Ajustes para usar TStream e não apenas TMemoryStream
 (por: DSA)
}

{$I ACBr.inc}

unit GZIPUtils;

interface

uses
  Classes, SysUtils,
  {$IfDef FPC}
   zstream
  {$Else}
   ZLib
  {$EndIf};

const
  MAXWORD = 65535;
  MAX_WBITS = 15;         { 32K LZ77 window }
  RAW_WBITS = -MAX_WBITS; { deflate raw stream (without any header) }
  NMAX = 3854;            { code with signed 32 bit integer }
  BASE = cardinal(65521); { largest prime smaller than 65536 }

type
  TZStreamType = (
    zsNo,   // no compression
    zsRaw,  // raw stream (without any header)
    zsZLib, // standard zlib stream (deflate header)
    zsGZip  // gzip stream (with gzip header)
  );

  TGZipHeaderFLags = (
    FTEXT,     // bit 0 - indicates file is ASCII text (can be safely ignored)
    FHCRC,     // bit 1 - there is a CRC16 for the header immediately following the header
    FEXTRA,    // bit 2 - extra field is present
    FNAME,     // bit 3 - the zero-terminated filename is present. encoding; ISO-8859-1.
    FCOMMENT   // bit 4 - a zero-terminated file comment is present. encoding: ISO-8859-1
  );
  TFlags = set of TGZipHeaderFLags;

function zipStream( inStream, outStream: TStream;
                    level: Tcompressionlevel = cldefault;
                    streamType: TZStreamType = zsGZip): boolean;
function unzipStream(inStream, outStream: TStream): boolean;

procedure RawDeflateCompress(inStream, outStream: TStream;
                          level: Tcompressionlevel = cldefault);
procedure RawDeflateDeCompress(inStream, outStream: TStream);

function crc16(crc: word; S: TStream; len : Cardinal = 0): Word;
function crc32(thecrc:cardinal; S: TStream; len : Cardinal = 0): Cardinal;
function adler32(adler : cardinal; S: TStream; len : Cardinal = 0): Cardinal;
{$IfNDef FPC}
 function SwapEndian(const AValue: LongWord): LongWord;
{$EndIf}

implementation

Uses
  synacode;

function zipStream(inStream, outStream: TStream; level: Tcompressionlevel;
  streamType: TZStreamType): boolean;
var
  crc, size, adler, d: longword;
  w: Word;
  b: Byte;
begin
  result := false;
  adler := 0;
  inStream.Position := 0; // goto start of input stream
  outStream.Position := 0; // goto start of output stream
  if StreamType = zsGZip then //add GZip Header
  begin
    size := inStream.Size;
    crc := crc32(0, inStream, size);

    w := $8b1f;  //GZip IDentification
    outStream.WriteBuffer(w,2);

    // 00 - store (no compression)
    // 01 - compress
    // 02 - pack
    // 03 - lzh
    // 04..07 - reserved
    // 08 - deflate
    b := $08;    //Compression Method = deflate
    outStream.WriteBuffer(b,1);

    //FLags
    // bit 0   FTEXT - indicates file is ASCII text (can be safely ignored)
    // bit 1   FHCRC - there is a CRC16 for the header immediately following the header
              // continuation of multi-part gzip file, part number present
    // bit 2   FEXTRA - extra field is present
    // bit 3   FNAME - the zero-terminated filename is present. encoding; ISO-8859-1.
    // bit 4   FCOMMENT  - a zero-terminated file comment is present. encoding: ISO-8859-1
    // bit 5 -7   reserved
    b := $00;
    outStream.WriteBuffer(b,1);

    d := $00000000; //Modification TIME = no time stamp is available (UNIX time format)
    outStream.WriteBuffer(d,4);

    //eXtra FLags (depend on compression method)
    // 00 - default compression
    // 02 - compressor used maximum compression, slowest algorithm
    // 04 - compressor used fastest algorithm
    case level of
      clmax    : b := $02;
      clfastest: b := $04;
    else
      b := $00;
    end;
    outStream.WriteBuffer(b,1);

    // Operating System = NTFS filesystem (NT)
    // 00 - FAT filesystem (MS-DOS, OS/2, NT/Win32)
    // 01 - Amiga
    // 02 - VMS (or OpenVMS)
    // 03 - Unix
    // 04 - VM/CMS
    // 05 - Atari TOS
    // 06 - HPFS filesystem (OS/2, NT)
    // 07 - Macintosh
    // 08 - Z-System
    // 09 - CP/M
    // 0A - TOPS-20
    // 0B - NTFS filesystem (NT)
    // 0C - QDOS
    // 0D - Acorn RISCOS
    // FF - unknown
    b := $FF;
    outStream.WriteBuffer(b,1);
  end
  else if StreamType = zsZLib then
  begin
    w := $9c78; //ZLib Header
    outStream.WriteBuffer(w,2);
    adler := adler32(0, nil);
    adler := adler32(adler, inStream);
  end;

  outStream.Seek(0, soEnd);
  RawDeflateCompress(inStream, outStream, level);

  outStream.Seek(0, soEnd);
  if (StreamType = zsGZip) then // add checksum and size
  begin
    outStream.WriteBuffer(crc,4); // CRC32 (CRC-32)
    outStream.WriteBuffer(size,4); // ISIZE (Input SIZE)
  end
  else if (StreamType = zsZLib) then // add adler32 checksum
  begin
    d := SwapEndian(adler);
    outStream.WriteBuffer(d,4); // adler32 checksum
  end;

  outStream.Position := 0; // goto start of result stream
end;

function unzipStream(inStream, outStream: TStream): boolean;
var
  streamType: TZStreamType;
  hdr, crc, adler, adler32in, crcGZin, sizeGZin, headerSize, modificationtime, d: longword;
  len, crcH, crcHeader, w: word;
  b: byte;
  flags: TFlags;
begin
  result := false;
  inStream.Position := 0; // goto start of input stream
  outStream.Position := 0; // goto start of output stream
  sizeGZin := 0;
  adler32in := 0;
  modificationtime := 0;
  crcGZin := 0;
  crcHeader := 0;
  hdr := 0;
  len := 0;
  w := 0;
  b := 0;
  d := 0;
  inStream.ReadBuffer(hdr, 4);
  if (hdr and $00088B1F) = $00088B1F then // gzip header (deflate method)
  begin
    streamType := zsGZip; // GZIP format
    inStream.ReadBuffer(modificationtime, 4); //Modification TIME (UNIX time format)
    inStream.ReadBuffer(w, 2); // eXtra FLags & Operating System
    flags := TFlags(Byte(hdr shr 24)); // FLags
    if (FEXTRA in flags) then // extra field is present
    begin
      inStream.ReadBuffer(len, 2); // extra field length
      inStream.Seek(len, soCurrent);// jump over extra field
      // parse subfields
      // |SI1|SI2|  LEN  |... LEN bytes of subfield data ...|
      // SI1 and SI2 provide a subfield ID
      // LEN gives the length of the subfield data, excluding the 4 initial bytes
    end;
    if (FNAME in flags) then // the zero-terminated filename is present
    begin
      inStream.ReadBuffer(b, 1);
      while b <> 0 do
      begin
        // sFilename := sFilename + char(b); // if filename is used
        inStream.ReadBuffer(b, 1);
      end;
    end;
    if (FCOMMENT in flags) then // a zero-terminated comment is present
    begin
      inStream.ReadBuffer(b, 1);
      while b <> 0 do
      begin
        // sComment := sComment + char(b); // if comment is used
        inStream.ReadBuffer(b, 1);
      end;
    end;
    if (FHCRC in flags) then // there is a CRC16 for the header immediately following the header
    begin
      crcH := crc16(0, inStream, inStream.Position); // get crc16 checksum of the header
      inStream.ReadBuffer(crcHeader, 2); // 2 bytes CRC16 for the header
      if crcH<>crcHeader then
        ;// header checksum mistake
    end;
    headerSize := inStream.Position;
    inStream.Seek(-8, soEnd);
    inStream.ReadBuffer(crcGZin, 4); // CRC32 (CRC-32)
    inStream.ReadBuffer(sizeGZin, 4); // ISIZE (Input SIZE)
    //inStream.Size := inStream.Size-8; // cut the 4 byte crc32 and 4 byte input size
  end
  else if (hdr and $00009C78) = $00009C78 then // zlib header
  begin
    streamType := zsZLib; // deflate format (with header)
    headerSize := 2;
    inStream.Seek(-4, soEnd); // first byte is start of deflate header
    inStream.ReadBuffer(d, 4);
    adler32in := SwapEndian(d);
    //inStream.Size := inStream.Size-4; // cut the 4 byte adler32 code
  end
  else
  begin
    streamType := zsRaw; // deflate format (is without header)
    headerSize := 0;
  end;

  inStream.Position := headerSize; // jump over header
  RawDeflateDeCompress(inStream, outStream);

  inStream.Position := 0; // goto start of source stream
  outStream.Position := 0; // goto start of result stream

  if (streamType = zsGZip) then // can check crc32 and size
  begin
    crc := crc32(0, outStream, outStream.Size); // get result crc32 checksum
    result := (crc = crcGZin) and (outStream.Size = sizeGZin); // compare with input checksum and size
  end
  else if (streamType = zsZLib) then // can check adler32 checksum
  begin
    adler := adler32(0, nil);
    adler := adler32(adler, outStream, outStream.Size);
    result := (adler = adler32in);
  end;
end;

procedure RawDeflateCompress(inStream, outStream: TStream; level: Tcompressionlevel);
var
  cs: Tcompressionstream;
  SkipHeader: Boolean;
  {$IfNDef FPC}
   zlevel: TZCompressionLevel;
  {$EndIf}
begin
  SkipHeader := (outStream.Size > 0);

  {$IfDef FPC}
   cs := Tcompressionstream.create(level, outStream, SkipHeader );
  {$Else}
   case level of
     clNone   : zlevel := zcNone;
     clFastest: zlevel := zcFastest;
     clMax    : zlevel := zcMax;
   else
     zlevel := zcDefault;
   end;

   if SkipHeader then
     cs := Tcompressionstream.create(outStream, zlevel, RAW_WBITS)
   else
     cs := Tcompressionstream.create(level, outStream);
  {$EndIf}
  try
    cs.CopyFrom(inStream, inStream.Size);
  finally
    cs.Free;
  end;
end;

procedure RawDeflateDeCompress(inStream, outStream: TStream);
var
  ds: Tdecompressionstream;
  Buffer: Pointer;
  readCount: LongInt;
  SkipHeader: Boolean;
begin
  SkipHeader := (inStream.Position > 0);

  {$IfDef FPC}
   ds := Tdecompressionstream.Create(inStream, SkipHeader);
  {$Else}
   if SkipHeader then
     ds := Tdecompressionstream.Create(inStream, RAW_WBITS)
   else
     ds := Tdecompressionstream.Create(inStream);
  {$EndIf}
  GetMem(Buffer, MAXWORD);
  try
    repeat
      readCount := ds.Read(Buffer^, MAXWORD);
      if readCount > 0 then
        outStream.Write(Buffer^, readCount);

    until readCount < MAXWORD;
  finally
    Freemem(Buffer);
    ds.Free;
  end;
end;

function crc16(crc: word; S: TStream; len: Cardinal): Word;
var
  n, oldPos: Int64;
  b: Byte;
begin
  if not Assigned(S) then
  begin
    Result := 0;
    Exit;
  end;

  if len = 0 then
    len := S.Size;

  Result := $FFFF;
  oldPos := S.Position;
  n := oldPos;
  while n < len do
  begin
    b := 0;
    S.ReadBuffer(b, 1);
    Result := UpdateCrc16(b, Result);
    Inc(n);
  end;

  S.Position := oldPos;
end;

function crc32(thecrc: cardinal; S: TStream; len: Cardinal): Cardinal;
var
  n, oldPos: Int64;
  b: Byte;
begin
  if not Assigned(S) then
  begin
    Result := 0;
    Exit;
  end;

  if len = 0 then
    len := S.Size;

  Result := $FFFFFFFF;
  oldPos := S.Position;
  n := oldPos;
  while n < len do
  begin
    b := 0;
    S.ReadBuffer(b, 1);
    Result := UpdateCrc32(b, Result);
    Inc(n);
  end;

  S.Position := oldPos;
  Result := not Result;
end;

{ Adaptado de PasZLib. http://wiki.freepascal.org/paszlib }
function adler32(adler: cardinal; S: TStream; len: Cardinal): Cardinal;
var
  s1, s2 : cardinal;
  k : integer;
  b: Byte;
  oldPos: Int64;
begin
  if not Assigned(S) then
  begin
    Result := cardinal(1);
    Exit;
  end;

  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  oldPos := S.Position;
  if len = 0 then
    len := S.Size;

  while (len > 0) do
  begin
    if len < NMAX then
      k := len
    else
      k := NMAX;

    Dec(len, k);
    while (k > 0) do
    begin
      b := 0;
      S.ReadBuffer(b, 1);
      Inc(s1, b);
      Inc(s2, s1);
      Dec(k);
    end;

    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
  end;

  S.Position := oldPos;
  Result := (s2 shl 16) or s1;
end;

{$IfNDef FPC}
function SwapEndian(const AValue: LongWord): LongWord;
begin
  Result := ((AValue shl 8) and $FF00FF00) or ((AValue shr 8) and $00FF00FF);
  Result := (Result shl 16) or (Result shr 16);
end;
{$EndIf}

(*
// deflate raw stream
headerSize := outStream.Position;
zstream.next_in := inStream.Memory;
zstream.avail_in := inStream.Size;
outStream.SetSize(headerSize + ((inStream.Size + (inStream.Size div 10) + 12) + 255) and not 255);
//zstream.next_out := PByte(outStream.Memory) + headerSize;
zstream.next_out := outStream.Memory;
Inc(zstream.next_out, headerSize);
zstream.avail_out := outStream.Size - headerSize;
//if deflateInit2(zstream, ZLevels[level], Z_DEFLATED, RAW_WBITS, 8, Z_DEFAULT_STRATEGY) < Z_OK then Exit;
deflate(zstream, Z_FINISH);
result := not (deflateEnd(zstream) < 0);
outStream.SetSize(zstream.total_out + headerSize);
outStream.Position := zstream.total_out + headerSize;



// inflate raw stream
inStream.Position := headerSize; // jump over header
//zstream.next_in := PByte(inStream.Memory) + headerSize;
zstream.next_in := inStream.Memory;
Inc(zstream.next_in, headerSize);
zstream.avail_in := inStream.Size - headerSize;
delta := (inStream.Size + 255) and not 255;
if (streamType = zsGZip) then
  outStream.SetSize(sizeGZin)
else
  outStream.SetSize(delta);
zstream.next_out := outStream.Memory;
zstream.avail_out := outStream.Size;
//if inflateInit2(zstream, RAW_WBITS) < 0 then Exit;
while inflate(zstream, Z_NO_FLUSH) = Z_OK do
begin
  outStream.SetSize(outStream.Size + delta);
  //zstream.next_out := PByte(outStream.Memory) + zstream.total_out;
  zstream.next_out := outStream.Memory;
  Inc(zstream.next_out, zstream.total_out);
  zstream.avail_out := delta;
end;
result := not (inflateEnd(zstream) < 0);
outStream.SetSize(zstream.total_out);
*)

end.

