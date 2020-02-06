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

unit ACBrCompress;

interface

uses
  Classes, SysUtils,
  {$IfDef USE_ZLibExGZ}
   ACBrZLibExGZ,
  {$Else}
   GZIPUtils,
  {$EndIf}
  {$IfDef FPC}
   zstream, zipper
  {$Else}
   {$IfDef MSWINDOWS}
    Windows,
   {$EndIf}
   ZLib
   {$IfDef DELPHIXE2_UP}
    ,System.Zip
   {$EndIf}
  {$EndIf};

type
  TCompressType = ( ctUnknown, ctZLib, ctGZip, ctZipFile );

{$IfDef NEXTGEN}
  AnsiString = RawByteString;
{$EndIf}

{$IfDef FPC}
  TACBrUnZipper = class(TUnZipper)
  private
    FInputStream: TStream;
    FOutputStream: TStream;
    Procedure ReadStream(Sender: TObject; var AStream: TStream);
    Procedure SetOutputStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure GetOutputStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    constructor Create;
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputStream: TStream read FOutputStream write FOutputStream;
  end;
{$EndIf}

function DetectCompressType(AStream: TStream): TCompressType;

// Decompress: Deflate, GZip or ZLib, ZipFile
function DeCompress(const ABinaryString: AnsiString): AnsiString; overload;
function DeCompress(AStream: TStream): AnsiString; overload;
function DeCompress(inStream, outStream: TStream): Boolean; overload;

// Compress: Deflate
function ZLibCompress(const ABinaryString: AnsiString;
                  level: Tcompressionlevel = cldefault): AnsiString; overload;
function ZLibCompress(AStream: TStream;
                  level: Tcompressionlevel = cldefault): AnsiString; overload;
function ZLibCompress(inStream, outStream: TStream;
                  level: Tcompressionlevel = cldefault): Boolean; overload;

// Compress: GZip
function GZipCompress(const ABinaryString: AnsiString): AnsiString; overload;
function GZipCompress(AStream: TStream): AnsiString; overload;
function GZipCompress(inStream, outStream: TStream): Boolean; overload;

// Compress: ZipFile
function ZipFileCompress(const ABinaryString: AnsiString; const AFileName: String = 'filename'): AnsiString; overload;
function ZipFileCompress(AStream: TStream; const AFileName: String = 'filename'): AnsiString; overload;
function ZipFileCompress(inStream, outStream: TStream; const AFileName: String = 'filename'): Boolean; overload;

// DeCompress: ZipFile
function ZipFileDeCompress(const ABinaryString: AnsiString): AnsiString; overload;
function ZipFileDeCompress(AStream: TStream): AnsiString; overload;
function ZipFileDeCompress(inStream, outStream: TStream): Boolean; overload;

implementation

uses
  synautil;

function DetectCompressType(AStream: TStream): TCompressType;
var
  hdr: LongWord;
  OldPos: Int64;
begin
  hdr := 0;
  OldPos := AStream.Position;
  AStream.Position := 0;
  AStream.ReadBuffer(hdr, 4);
  AStream.Position := OldPos;

  if (hdr and $88B1F) = $88B1F then
    Result := ctGZip
  else if (hdr and $9C78) = $9C78 then
    Result := ctZLib
  else if (hdr and $4034B50) = $4034B50 then
    Result := ctZipFile
  else
    Result := ctUnknown;
end;

function DeCompress(const ABinaryString: AnsiString): AnsiString;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, ABinaryString);
    MS.Position := 0;
    Result := DeCompress(MS);
  finally
    MS.Free;
  end;
end;

function DeCompress(AStream: TStream): AnsiString;
var
  outMemStream: TMemoryStream;
begin
  outMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    DeCompress(AStream, outMemStream);

    outMemStream.Position := 0;
    Result := ReadStrFromStream(outMemStream, outMemStream.Size);
  finally
    outMemStream.Free;
  end;
end;

{$IfNDef USE_ZLibExGZ}
function DeCompress(inStream, outStream: TStream): Boolean;
begin
  if (DetectCompressType(inStream) = ctZipFile) then
    Result := ZipFileDeCompress(inStream, outStream)
  else
    Result := GZIPUtils.unzipStream(inStream, outStream)
end;
{$Else}
function DeCompress(inStream, outStream: TStream): Boolean;
var
  ds: Tdecompressionstream;
  Buffer: Pointer;
  readCount: LongInt;
begin
  inStream.Position := 0;

  if (DetectCompressType(inStream) = ctGZip) then
    ACBrZLibExGZ.GZDecompressStream(inStream, outStream)
  else
  begin
    ds := Tdecompressionstream.Create(inStream);
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

  Result := True;
end;
{$EndIf}

function ZLibCompress(const ABinaryString: AnsiString; level: Tcompressionlevel): AnsiString;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, ABinaryString);
    MS.Position := 0;
    Result := ZLibCompress(MS, level);
  finally
    MS.Free;
  end;
end;

function ZLibCompress(AStream: TStream; level: Tcompressionlevel): AnsiString;
var
  outMemStream: TMemoryStream;
begin
  outMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    ZLibCompress(AStream, outMemStream, level);
    outMemStream.Position := 0;
    Result := ReadStrFromStream(outMemStream, outMemStream.Size);
  finally
    outMemStream.Free;
  end;
end;

function ZLibCompress(inStream, outStream: TStream; level: Tcompressionlevel): Boolean;
var
  cs: Tcompressionstream;
begin
  cs := Tcompressionstream.create(level, outStream);
  try
    cs.CopyFrom(inStream, inStream.Size);
  finally
    cs.Free;
  end;
  Result := True;
end;

function GZipCompress(const ABinaryString: AnsiString): AnsiString;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, ABinaryString);
    MS.Position := 0;
    Result := GZipCompress(MS);
  finally
    MS.Free;
  end;
end;

function GZipCompress(AStream: TStream): AnsiString;
var
  outMemStream: TMemoryStream;
begin
  outMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    GZipCompress(AStream, outMemStream);
    outMemStream.Position := 0;
    Result := ReadStrFromStream(outMemStream, outMemStream.Size);
  finally
    outMemStream.Free;
  end;
end;

function GZipCompress(inStream, outStream: TStream): Boolean;
begin
  {$IfNDef USE_ZLibExGZ}
   Result := GZIPUtils.zipStream(inStream, outStream, cldefault, zsGZip);
  {$Else}
   ACBrZLibExGZ.GZCompressStream(inStream, outStream);
   Result := True;
  {$EndIf}
end;

function ZipFileCompress(const ABinaryString: AnsiString; const AFileName: String): AnsiString;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, ABinaryString);
    MS.Position := 0;
    Result := ZipFileCompress(MS, AFileName);
  finally
    MS.Free;
  end;
end;

function ZipFileCompress(AStream: TStream; const AFileName: String): AnsiString;
var
  outMemStream: TMemoryStream;
begin
  outMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    ZipFileCompress(AStream, outMemStream, AFileName);
    outMemStream.Position := 0;
    Result := ReadStrFromStream(outMemStream, outMemStream.Size);
  finally
    outMemStream.Free;
  end;
end;

{$IfDef FPC}
function ZipFileCompress(inStream, outStream: TStream; const AFileName: String): Boolean;
var
  z: TZipper;
begin
  z := TZipper.Create;
  try
    z.Entries.AddFileEntry(inStream, AFileName);
    z.SaveToStream(outStream);
    Result := True;
  finally
    z.Free;
  end;
end;
{$Else}
{$IfDef DELPHIXE2_UP}
function ZipFileCompress(inStream, outStream: TStream; const AFileName: String): Boolean;
var
  z: TZipFile;
begin
  z := TZipFile.Create;
  try
    z.Open(outStream, zmWrite);
    z.Add(inStream, AFileName);
    z.Close;
    Result := True;
  finally
    z.Free;
  end;
end;
{$Else}
function ZipFileCompress(inStream, outStream: TStream; const AFileName: String): Boolean;
begin
  raise Exception.Create('O seu compilador não tem suporte nativo a ZipFile.');
end;
{$EndIf}
{$EndIf}

function ZipFileDeCompress(const ABinaryString: AnsiString): AnsiString;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, ABinaryString);
    MS.Position := 0;
    Result := ZipFileDeCompress(MS);
  finally
    MS.Free;
  end;
end;

function ZipFileDeCompress(AStream: TStream): AnsiString;
var
  outMemStream: TMemoryStream;
begin
  outMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    ZipFileDeCompress(AStream, outMemStream);
    outMemStream.Position := 0;
    Result := ReadStrFromStream(outMemStream, outMemStream.Size);
  finally
    outMemStream.Free;
  end;
end;

{$IfDef FPC}
constructor TACBrUnZipper.Create;
begin
  inherited;
  FOutputStream := Nil;
  FInputStream := Nil;
  Self.OnOpenInputStream := ReadStream;
  Self.OnCreateStream := SetOutputStream;
  Self.OnDoneStream := GetOutputStream;
end;

procedure TACBrUnZipper.ReadStream(Sender: TObject; var AStream: TStream);
begin
  if Assigned(FInputStream) then
    AStream := FInputStream;
end;

procedure TACBrUnZipper.SetOutputStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  if Assigned(FOutputStream) then
    AStream := FOutputStream;
end;

procedure TACBrUnZipper.GetOutputStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  if Assigned(FOutputStream) then
    AStream := FOutputStream;
end;

function ZipFileDeCompress(inStream, outStream: TStream): Boolean;
var
  u: TACBrUnZipper;
  ms: TMemoryStream;
begin
  Result := False;
  ms := TMemoryStream.Create;
  u := TACBrUnZipper.Create;
  try
    ms.CopyFrom(inStream, inStream.Size);
    ms.Position := 0;
    u.InputStream := ms;
    u.OutputStream := outStream;
    u.UnZipAllFiles;
    Result := True;
  finally
    u.Free;
    //ms.Free; TUnZipper cleans input Stream;
  end;
end;
{$Else}
{$IfDef DELPHIXE2_UP}
function ZipFileDeCompress(inStream, outStream: TStream): Boolean;
var
  Z: TZipFile;
  s: TStream;
  h: TZipHeader;
begin
  z := TZipFile.Create;
  try
    z.Open(inStream, zmRead);
    z.Read(0, s, h);
    try
      outStream.CopyFrom(s, s.Size);
      Result := True;
    finally
      s.Free;
    end;
  finally
    z.Free;
  end;
end;
{$Else}
function ZipFileDeCompress(inStream, outStream: TStream): Boolean;
begin
  raise Exception.Create('O seu compilador não tem suporte nativo a ZipFile.');
end;
{$EndIf}
{$EndIf}

end.


