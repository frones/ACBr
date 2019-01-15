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
  Classes, SysUtils;

const
  cErrImgPCXMono = 'Imagem não é PCX Monocromática';
  cErrImgBMPMono = 'Imagem não é BMP Monocromática';

type
  EACBrImage = class(Exception);

function IsPCX(S: TStream; CheckIsMono: Boolean = True): Boolean;
function IsBMP(S: TStream; CheckIsMono: Boolean = True): Boolean;

procedure BMPToRasterStr(AStream: TStream; InvertImg: Boolean; var AWidth: Integer;
  var AHeight: Integer; var RasterStr: AnsiString);



implementation

uses
  math,
  ACBrUtil;

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

procedure BMPToRasterStr(AStream: TStream; InvertImg: Boolean;
  var AWidth: Integer; var AHeight: Integer; var RasterStr: AnsiString);
var
  bPixelOffset, bWidth, bHeight: LongWord;
  bPixel: Byte;
  StreamLastPos, RowStart, BytesPerRow, i: Int64;
begin
  // Inspiração:
  // http://www.nonov.io/convert_bmp_to_ascii
  // https://en.wikipedia.org/wiki/BMP_file_format
  // https://github.com/asharif/img2grf/blob/master/src/main/java/org/orphanware/App.java

  if not IsBMP(AStream, True) then
    raise EACBrImage.Create(ACBrStr(cErrImgBMPMono));

  // Lendo posição do Off-set da imagem
  AStream.Position := 10;
  bPixelOffset := 0;
  AStream.ReadBuffer(bPixelOffset, 4);

  // Lendo dimensões da imagem
  AStream.Position := 18;
  bWidth := 0; bHeight := 0;
  AStream.ReadBuffer(bWidth, 4);
  AStream.ReadBuffer(bHeight, 4);

  AWidth := bWidth;
  AHeight := bHeight;
  RasterStr := '';

  BytesPerRow := ceil(bWidth / 8);
  StreamLastPos := AStream.Size-1;
  while (StreamLastPos >= bPixelOffset) do
  begin
    RowStart := StreamLastPos - (BytesPerRow - 1);
    i := 1;
    AStream.Position := RowStart;
    while (i <= BytesPerRow) do
    begin
      bPixel := 0;
      AStream.ReadBuffer(bPixel,1);
      if InvertImg then
        bPixel := bPixel xor $FF;

      RasterStr := RasterStr + chr(bPixel);
      inc(i);
    end;
    StreamLastPos := RowStart-1;
  end;
end;

end.

