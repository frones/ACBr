{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrImage;

interface

uses
  Classes, SysUtils
  {$IfNDef NOGUI}
   {$IfDef FPC}
    ,LCLType, InterfaceBase
   {$Else}
    ,windows
   {$EndIf}
   ,Graphics
  {$EndIf};

const
  cErrImgPCXMono = 'Imagem não é PCX Monocromática';
  cErrImgBMPMono = 'Imagem não é BMP Monocromática';
  C_LUMINOSITY_THRESHOLD = 127;

type
  EACBrImage = class(Exception);

function IsPCX(S: TStream; CheckIsMono: Boolean = True): Boolean;
function IsBMP(S: TStream; CheckIsMono: Boolean = True): Boolean;

procedure RasterStrToAscII(const ARasterStr: AnsiString; AWidth: Integer;
  InvertImg: Boolean; AscIIArtLines: TStrings);
procedure AscIIToRasterStr(AscIIArtLines: TStrings; out AWidth: Integer;
  out AHeight: Integer; out ARasterStr: AnsiString);

procedure BMPMonoToRasterStr(ABMPStream: TStream; InvertImg: Boolean; out AWidth: Integer;
  out AHeight: Integer; out ARasterStr: AnsiString);
procedure RasterStrToBMPMono(ARasterStr: AnsiString; AWidth: Integer;
  InvertImg: Boolean; ABMPStream: TStream);

{$IfNDef NOGUI}
procedure BitmapToRasterStr(ABmpSrc: TBitmap; InvertImg: Boolean;
  out AWidth: Integer; out AHeight: Integer; out ARasterStr: AnsiString;
  LuminosityThreshold: Byte = C_LUMINOSITY_THRESHOLD);
{$EndIf}

implementation

uses
  math, strutils,
  ACBrUtil, ACBrConsts;

function IsPCX(S: TStream; CheckIsMono: Boolean): Boolean;
var
  p: Int64;
  b, bColorPlanes, bBitsPerPixel: Byte;
begin
  // https://stackoverflow.com/questions/1689715/image-data-of-pcx-file
  p := S.Position;
  S.Position := 0;
  b := 0;
  S.ReadBuffer(b,1);
  Result := (b = 10);

  if Result and CheckIsMono then
  begin
    // Lendo as cores
    bBitsPerPixel := 0; bColorPlanes := 0;
    S.Position := 3;
    S.ReadBuffer(bBitsPerPixel, 1);
    S.Position := 65;
    S.ReadBuffer(bColorPlanes, 1);
    Result := (bColorPlanes = 1) and (bBitsPerPixel = 1);
  end;

  S.Position := p;
end;

function IsBMP(S: TStream; CheckIsMono: Boolean): Boolean;
var
  Buffer: array[0..1] of AnsiChar;
  bColorPlanes, bBitsPerPixel: Word;
  p: Int64;
begin
  //https://en.wikipedia.org/wiki/BMP_file_format
  p := S.Position;
  S.Position := 0;
  Buffer[0] := ' ';
  S.ReadBuffer(Buffer, 2);
  Result := (Buffer = 'BM');

  if Result and CheckIsMono then
  begin
    // Lendo as cores
    bColorPlanes := 0; bBitsPerPixel := 0;
    S.Position := 26;
    S.ReadBuffer(bColorPlanes, 2);
    S.ReadBuffer(bBitsPerPixel, 2);
    Result := (bColorPlanes = 1) and (bBitsPerPixel = 1);
  end;

  S.Position := p;
end;

procedure BMPMonoToRasterStr(ABMPStream: TStream; InvertImg: Boolean; out
  AWidth: Integer; out AHeight: Integer; out ARasterStr: AnsiString);
var
  bPixelOffset, bSizePixelArr, bWidth, bHeight: LongWord;
  bPixel: Byte;
  StreamLastPos, RowStart, BytesPerRow, i, RealWidth: Int64;
  HasPadBits: Boolean;
  BytesPerWidth: Integer;
begin
  // Inspiração:
  // http://www.nonov.io/convert_bmp_to_ascii
  // https://en.wikipedia.org/wiki/BMP_file_format
  // https://github.com/asharif/img2grf/blob/master/src/main/java/org/orphanware/App.java

  if not IsBMP(ABMPStream, True) then
    raise EACBrImage.Create(ACBrStr(cErrImgBMPMono));

  // Lendo posição do Off-set da imagem
  ABMPStream.Position := 10;
  bPixelOffset := 0;
  ABMPStream.ReadBuffer(bPixelOffset, 4);

  // Lendo Tamanho do Pixel array
  ABMPStream.Position := 34;
  bSizePixelArr := 0;
  ABMPStream.ReadBuffer(bSizePixelArr, 4);

  // Lendo dimensões da imagem
  ABMPStream.Position := 18;
  bWidth := 0; bHeight := 0;
  ABMPStream.ReadBuffer(bWidth, 4);
  ABMPStream.ReadBuffer(bHeight, 4);

  AHeight := bHeight;
  //AWidth := bWidth;
  ARasterStr := '';

  // BMP é organizado da Esquerda para a Direita e de Baixo para cima.. serializando..
  BytesPerRow := ceil(bWidth / 8);
  HasPadBits := (BytesPerRow > (trunc(bWidth/8)));
  BytesPerRow := ceil(bWidth / 8);
  AWidth := BytesPerRow*8;
  RealWidth := trunc((bSizePixelArr*8)/bHeight);
  BytesPerWidth := ceil(RealWidth / 8);

  StreamLastPos := bPixelOffset + bSizePixelArr - 1; // ABMPStream.Size-1;
  while (StreamLastPos >= bPixelOffset) do
  begin
    RowStart := StreamLastPos - (BytesPerWidth - 1);
    i := 1;
    ABMPStream.Position := RowStart;
    while (i <= BytesPerRow) do
    begin
      bPixel := 0;
      ABMPStream.ReadBuffer(bPixel,1);
      if InvertImg then
      begin
        if (not HasPadBits) or (i < BytesPerRow) then
          bPixel := bPixel xor $FF
        else
          bPixel := bPixel xor bPixel;
      end;

      ARasterStr := ARasterStr + chr(bPixel);
      inc(i);
    end;
    StreamLastPos := RowStart-1;
  end;

  //DEBUG
  //WriteToFile('c:\temp\Raster.txt', ARasterStr);
end;

procedure RasterStrToAscII(const ARasterStr: AnsiString; AWidth: Integer;
  InvertImg: Boolean; AscIIArtLines: TStrings);
var
  BytesPerRow, LenRaster, i: Integer;
  ALine: String;
  AByte: Byte;
begin
  AscIIArtLines.Clear;
  if (AWidth <= 0) then
    raise EACBrImage.Create(ACBrStr('RasterStrToAscII: AWidth é obrigatório'));

  BytesPerRow := ceil(AWidth / 8);

  ALine := '';
  LenRaster := Length(ARasterStr);
  for i := 1 to LenRaster do
  begin
    AByte := ord(ARasterStr[i]);
    if InvertImg then
      AByte := AByte xor $FF;

    ALine := ALine + IntToBin(AByte, 8);
    if ((i mod BytesPerRow) = 0) then
    begin
      AscIIArtLines.Add(ALine);
      ALine := '';
    end;
  end;
end;

procedure AscIIToRasterStr(AscIIArtLines: TStrings; out AWidth: Integer; out
  AHeight: Integer; out ARasterStr: AnsiString);
var
  BytesPerRow, RealWidth, i, j: Integer;
  ALine, BinaryByte: String;
begin
  AWidth := 0;
  ARasterStr := '';
  AHeight := AscIIArtLines.Count;
  if AHeight < 1 then
    Exit;

  AWidth := Length(AscIIArtLines[1]);
  BytesPerRow := ceil(AWidth / 8);
  RealWidth := BytesPerRow * 8;

  for i := 0 to AHeight-1 do
  begin
    if AWidth = RealWidth then
      ALine := AscIIArtLines[i]
    else
      ALine := PadRight(AscIIArtLines[i], RealWidth, '0');

    j := 1;
    while J < RealWidth do
    begin
      BinaryByte := copy(ALine,j,8);
      ARasterStr := ARasterStr + chr(BinToInt(BinaryByte));
      inc(j,8);
    end;
  end;
end;

procedure RasterStrToBMPMono(ARasterStr: AnsiString; AWidth: Integer;
  InvertImg: Boolean; ABMPStream: TStream);
var
  AByte: Byte;
  AWord: Word;
  ALongWord: LongWord;
  LenPixArrIn, LenPixArrOut, BytesPerRowIn, BytesPerRowOut, RowStart: Integer;
  AHeight, i, p, b: Integer;
  PixArr: array of Byte;
begin
  BytesPerRowIn := ceil(AWidth / 8);
  LenPixArrIn := Length(ARasterStr);
  AHeight := Trunc(LenPixArrIn / BytesPerRowIn);

  // O BMP deve estar blocos de DWORD(32bits), calculando a Largura multiplo de 32
  BytesPerRowOut := ceil(AWidth / 32) * 4;
  LenPixArrOut := BytesPerRowOut * AHeight;
  SetLength(PixArr,LenPixArrOut);

  // BMP é organizado da Esquerda para a Direita e de Baixo para cima.. serializando..
  b := 0;
  p := LenPixArrIn-1;
  while (p > 0) do
  begin
    RowStart := p - (BytesPerRowIn - 1);
    i := 1;
    while (i <= BytesPerRowOut) do
    begin
      if i > BytesPerRowIn then
        AByte := 0
      else
      begin
        AByte := ord(ARasterStr[RowStart+i]);
        if InvertImg then
          AByte := AByte xor $FF;
      end;

      PixArr[b] := AByte;
      inc(i);
      inc(b);
    end;
    p := RowStart-1;
  end;

  with ABMPStream do
  begin
    Size := 0;  // Trunc Stream

    // BMP header   14 bytes
    Write('BM',2);    // BitMap signature
    ALongWord := 14 + 40 + 8 + LenPixArrOut ;
    WriteBuffer(ALongWord, 4);  // Tamanho do arquivo
    ALongWord := 0;
    WriteBuffer(ALongWord, 4);  // Reservado para a aplicação
    ALongWord := 62;
    WriteBuffer(ALongWord, 4);  // Inicio do Pixel array

    //DIB Header    40 bytes
    ALongWord := 40;
    WriteBuffer(ALongWord, 4);  // Tamanho do cabeçalho DIB
    ALongWord := AWidth;
    WriteBuffer(ALongWord, 4); // Largura
    ALongWord := AHeight;
    WriteBuffer(ALongWord, 4);  // Altura
    AWord := 1;
    WriteBuffer(AWord, 2);      // Numero de Planos
    WriteBuffer(AWord, 2);      // Numero de Bits (1bpp)
    ALongWord := 0;
    WriteBuffer(ALongWord, 4);  // Método de Compressão
    ALongWord := LenPixArrOut;
    WriteBuffer(ALongWord, 4);  // Tamanho do Pixel array
    ALongWord := 0 ;
    WriteBuffer(ALongWord, 4);  // Print resolution of the image, 72DPI - H
    WriteBuffer(ALongWord, 4);  // Print resolution of the image, 72DPI - V
    ALongWord := 2;
    WriteBuffer(ALongWord, 4);  // Numero de cores na paleta
    ALongWord := 0;
    WriteBuffer(ALongWord, 4);  // Numero de cores importantes

    // Tabela de Cores    8 bytes
    ALongWord := 0;
    WriteBuffer(ALongWord, 4);  // Cor Preta
    ALongWord := $ffffff;
    WriteBuffer(ALongWord, 4);  // Cor Branca

    // Pixel array
    Write(Pointer(PixArr)^, LenPixArrOut);
    //Write(PAnsiChar(ARasterStr)^, LenPixArrIn);
    Position := 0;
  end;
end;

{$IfNDef NOGUI}
{$IfNDef FPC}
procedure RedGreenBlue(rgb: TColorRef; out Red, Green, Blue: Byte);
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;
{$EndIf}

procedure BitmapToRasterStr(ABmpSrc: TBitmap; InvertImg: Boolean; out
  AWidth: Integer; out AHeight: Integer; out ARasterStr: AnsiString;
  LuminosityThreshold: Byte);
var
  MS: TMemoryStream;
  Row, Col: Integer;
  cRed, cGreen, cBlue: Byte;
  APixel: TColor;
  Luminosity: Int64;
  ByteStr: String;
  Bit: Boolean;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';

  if (ABmpSrc.PixelFormat = pf1bit) then  // Já é Mono ?
  begin
    MS := TMemoryStream.Create;
    try
      ABmpSrc.SaveToStream(MS);
      BMPMonoToRasterStr(MS, True, AWidth, AHeight, ARasterStr);
    finally
      MS.Free;
    end;

    Exit;
  end;

  AWidth := ABmpSrc.Width;
  AHeight := ABmpSrc.Height;
  for Row := 0 to AHeight - 1 do
  begin
    ByteStr := '';

    for Col := 0 to AWidth - 1 do
    begin
      APixel := ABmpSrc.Canvas.Pixels[Col, Row];
      cRed := 0; cGreen := 0; cBlue := 0;
      RedGreenBlue(APixel, cRed, cGreen, cBlue);
      Luminosity := Trunc( ( cRed * 0.3 ) + ( cGreen  * 0.59 ) + ( cBlue * 0.11 ) );
      Bit := ( Luminosity > LuminosityThreshold );
      if InvertImg then
        Bit := not Bit;

      ByteStr := ByteStr + ifthen(Bit,'1','0');
      if (Length(ByteStr) = 8) then
      begin
        ARasterStr := ARasterStr + chr(BinToInt(ByteStr));
        ByteStr := '';
      end;
    end;

    if (Length(ByteStr) > 0) then
      ARasterStr := ARasterStr + chr(BinToInt(PadRight(ByteStr, 8, '0')));
  end;
end;
{$EndIf}

end.

