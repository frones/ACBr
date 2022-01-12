{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrOpenSSLUtils;

interface

uses
  Classes, SysUtils, ctypes, StrUtils,
  OpenSSLExt,
  ACBrConsts, ACBrBase;

resourcestring
  sErrDigstNotFound = 'Digest %s not found in OpenSSL';
  sErrFileNotInformed = 'FileName is empty';
  sErrFileNotExists = 'File: %s does not exists';

const
  CBufferSize = 32768;

type
  TACBrOpenSSLDgst = (dgstMD2, dgstMD4, dgstMD5, dgstRMD160, dgstSHA, dgstSHA1,
    dgstSHA256, dgstSHA512);
  TACBrOpenSSLStrType = (outHexa, outBase64, outBinary);

  TACBrOpenSSLOnProgress = procedure(const PosByte, TotalSize: int64) of object;

  EACBrOpenSSLException = class(Exception);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  { TACBrOpenSSL }

  TACBrOpenSSL = class(TACBrComponent)
  private
    fVersion: String;
    fOldLib: Boolean;
    fCertificateFile: String;
    fCertificateStr: AnsiString;
    fPFXFile: String;
    fPFXStr: AnsiString;
    fPrivateKeyFile: String;
    fPrivateKeyStr: AnsiString;
    fPublicKeyFile: String;
    fPublicKeyStr: AnsiString;
    fBufferSize: Integer;
    fOnProgress: TACBrOpenSSLOnProgress;

    fEVP_PrivateKey: pEVP_PKEY;
    fEVP_PublicKey: pEVP_PKEY;

    function ACBrOpenSSLDgstToStr(ADigest: TACBrOpenSSLDgst): String;
    function ACBrOpenSSLDgstToPEVP_MD(ADigest: TACBrOpenSSLDgst): PEVP_MD;
    function GetVersion: String;
    procedure SetBufferSize(AValue: Integer);

  private
    procedure LoadKeys;
    procedure LoadPrivateKey;
    procedure LoadPublicKey;
    procedure FreeKeys;
    Procedure FreePrivateKey;
    Procedure FreePublicKey;

    function IsOldLib: Boolean;
    procedure CheckFileExists(const AFile: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CalcHashFromStream(AStream: TStream; ADigest: TACBrOpenSSLDgst;
      OutputType: TACBrOpenSSLStrType = outHexa; Sign: Boolean = False): AnsiString;
    function CalcHashFromString(const AStr: AnsiString; ADigest: TACBrOpenSSLDgst;
      OutputType: TACBrOpenSSLStrType = outHexa; Sign: Boolean = False): AnsiString;
    function CalcHashFromFile(const AFile: String; ADigest: TACBrOpenSSLDgst;
      OutputType: TACBrOpenSSLStrType = outHexa; Sign: Boolean = False): AnsiString;

    function VerifyHashFromStream(AStream : TStream; ADigest: TACBrOpenSSLDgst;
      const AHash: AnsiString; HashType: TACBrOpenSSLStrType = outHexa;
      Signed: Boolean = False): Boolean;
    function VerifyHashFromString(const AStr: AnsiString; ADigest: TACBrOpenSSLDgst;
      const AHash: AnsiString; HashType: TACBrOpenSSLStrType = outHexa;
      Signed: Boolean = False): Boolean;
    function VerifyHashFromFile(const AFile: String; ADigest: TACBrOpenSSLDgst;
      const AHash: AnsiString; HashType: TACBrOpenSSLStrType = outHexa;
      Signed: Boolean = False): Boolean;

    function ConvertToStrType(ABinaryStr: AnsiString;
      OutputType: TACBrOpenSSLStrType = outHexa): AnsiString;
    function ConvertFromStrType(ABinaryStr: AnsiString;
      InputType: TACBrOpenSSLStrType = outHexa): AnsiString;
  public
    property PFXFile: String read fPFXFile write fPFXFile;
    property CertificateFile: String read fCertificateFile write fCertificateFile;
    property PrivateKeyFile: String read fPrivateKeyFile write fPrivateKeyFile;
    property PublicKeyFile: String read fPublicKeyFile write fPublicKeyFile;

    property PFXStr: AnsiString read fPFXStr write fPFXStr;
    property CertificateStr: AnsiString read fCertificateStr write fCertificateStr;
    property PrivateKeyStr: AnsiString read fPrivateKeyStr write fPrivateKeyStr;
    property PublicKeyStr: AnsiString read fPublicKeyStr write fPublicKeyStr;

    property BufferSize: Integer read fBufferSize write SetBufferSize default CBufferSize;
    property Version: String read GetVersion;

    property OnProgress: TACBrOpenSSLOnProgress read fOnProgress write fOnProgress;
  end;

procedure InitOpenSSL;
procedure FreeOpenSSL;

var
  OpenSSLLoaded: Boolean;

implementation

uses
  Math, TypInfo,
  synacode,
  ACBrUtil;

procedure InitOpenSSL;
begin
  if OpenSSLLoaded then
    exit;

  OpenSSL_add_all_algorithms;
  OpenSSL_add_all_ciphers;
  OpenSSL_add_all_digests;
  ERR_load_crypto_strings;

  OpenSSLLoaded := True;
end;

procedure FreeOpenSSL;
begin
  if not OpenSSLLoaded then
    exit;

  EVPcleanup();
  OpenSSLLoaded := False;
end;

{ TACBrOpenSSL }

constructor TACBrOpenSSL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fVersion := '';
  fOldLib := False;
  fCertificateFile := '';
  fCertificateStr := '';
  fPFXFile := '';
  fPFXStr := '';
  fPrivateKeyFile := '';
  fPrivateKeyStr := '';
  fPublicKeyFile := '';
  fPublicKeyStr := '';
  fBufferSize := CBufferSize;

  fOnProgress := Nil;
  fEVP_PrivateKey := Nil;
  fEVP_PublicKey := Nil;
end;

destructor TACBrOpenSSL.Destroy;
begin
  inherited Destroy;
end;

function TACBrOpenSSL.CalcHashFromStream(AStream: TStream; ADigest: TACBrOpenSSLDgst;
  OutputType: TACBrOpenSSLStrType; Sign: Boolean): AnsiString;
var
  s: AnsiString;
  md: PEVP_MD;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
  pmd_ctx: PEVP_MD_CTX;
  md_value_bin: array [0..1023] of AnsiChar;
  buffer: Pointer;
  p: int64;
  b: longint;
begin
  InitOpenSSL;
  if Sign then
    LoadPrivateKey;

  pmd_ctx := nil;
  GetMem(buffer, fBufferSize);
  try
    md := ACBrOpenSSLDgstToPEVP_MD(ADigest);
    if IsOldLib then
      pmd_ctx := @md_ctx
    else
      pmd_ctx := EVP_MD_CTX_new();
    EVP_DigestInit(pmd_ctx, md);

    p := 0;
    AStream.Position := 0;
    if Assigned(fOnProgress) then
      fOnProgress(p, AStream.Size);
    while (p < AStream.Size) do
    begin
      b := AStream.Read(buffer^, BufferSize);
      if (b <= 0) then
        Break;
      EVP_DigestUpdate(pmd_ctx, buffer, b);
      Inc(p, b);
      if Assigned(fOnProgress) then
        fOnProgress(p, AStream.Size);
    end;

    md_len := 0;
    if Sign then
      EVP_SignFinal(pmd_ctx, @md_value_bin, md_len, fEVP_PrivateKey)
    else
      EVP_DigestFinal(pmd_ctx, @md_value_bin, @md_len);

    s := '';
    SetString(s, md_value_bin, md_len);
    Result := ConvertToStrType(s, OutputType);
  finally
    if (not IsOldLib) and (pmd_ctx <> nil) then
      EVP_MD_CTX_free(pmd_ctx);

    Freemem(buffer);
  end;
end;

function TACBrOpenSSL.CalcHashFromString(const AStr: AnsiString;
  ADigest: TACBrOpenSSLDgst; OutputType: TACBrOpenSSLStrType; Sign: Boolean
  ): AnsiString;
Var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Pointer(AStr)^, Length(AStr));
    Result := CalcHashFromStream(ms, ADigest, OutputType, Sign);
  finally
    ms.Free ;
  end ;
end;

function TACBrOpenSSL.CalcHashFromFile(const AFile: String;
  ADigest: TACBrOpenSSLDgst; OutputType: TACBrOpenSSLStrType; Sign: Boolean
  ): AnsiString;
Var
  fs: TFileStream ;
begin
  CheckFileExists(AFile);
  fs := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
  try
    Result := CalcHashFromStream(fs, ADigest, OutputType, Sign);
  finally
    fs.Free ;
  end ;
end;

function TACBrOpenSSL.VerifyHashFromStream(AStream: TStream; ADigest: TACBrOpenSSLDgst;
  const AHash: AnsiString; HashType: TACBrOpenSSLStrType; Signed: Boolean
  ): Boolean;
Var
  s, h: AnsiString;
  md : PEVP_MD ;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
  pmd_ctx: PEVP_MD_CTX;
  md_value_bin : array [0..1023] of AnsiChar;
  buffer: Pointer;
  p: Int64;
  b: LongInt;
begin
  InitOpenSSL;
  if Signed then
    LoadPublicKey;

  pmd_ctx := Nil;
  GetMem(buffer, CBufferSize);
  try
    md := ACBrOpenSSLDgstToPEVP_MD(ADigest);
    if IsOldLib then
      pmd_ctx := @md_ctx
    else
      pmd_ctx := EVP_MD_CTX_new();
    EVP_DigestInit(pmd_ctx, md);

    p := 0;
    AStream.Position := 0;
    if Assigned(fOnProgress) then
      fOnProgress(p, AStream.Size);
    while (p < AStream.Size) do
    begin
      b := AStream.Read(buffer^, CBufferSize);
      if (b <= 0) then
        Break;
      EVP_DigestUpdate(pmd_ctx, buffer, b);
      Inc(p, b);
      if Assigned(fOnProgress) then
        fOnProgress(p, AStream.Size);
    end;

    h := ConvertFromStrType(AHash, HashType);
    if Signed then
      Result := (EVP_VerifyFinal(pmd_ctx, PAnsiChar(h), Length(h), fEVP_PublicKey) = 1)
    else
    begin
      md_len := 0;
      EVP_DigestFinal(pmd_ctx, @md_value_bin, @md_len);
      SetString(s, md_value_bin, md_len);
      Result := (Pos(s, h) > 0);
    end;
  finally
    if (not IsOldLib) and (pmd_ctx <> nil) then
      EVP_MD_CTX_free(pmd_ctx);

    Freemem(buffer);
  end;
end;

function TACBrOpenSSL.VerifyHashFromString(const AStr: AnsiString;
  ADigest: TACBrOpenSSLDgst; const AHash: AnsiString;
  HashType: TACBrOpenSSLStrType; Signed: Boolean): Boolean;
Var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Pointer(AStr)^, Length(AStr));
    Result := VerifyHashFromStream(ms, ADigest, AHash, HashType, Signed);
  finally
    ms.Free ;
  end ;
end;

function TACBrOpenSSL.VerifyHashFromFile(const AFile: String;
  ADigest: TACBrOpenSSLDgst; const AHash: AnsiString;
  HashType: TACBrOpenSSLStrType; Signed: Boolean): Boolean;
Var
  fs: TFileStream ;
begin
  CheckFileExists(AFile);
  fs := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
  try
    Result := VerifyHashFromStream(fs, ADigest, AHash, HashType, Signed);
  finally
    fs.Free ;
  end ;
end;

function TACBrOpenSSL.ConvertToStrType(ABinaryStr: AnsiString;
  OutputType: TACBrOpenSSLStrType): AnsiString;
begin
  case OutputType of
    outBase64: Result := Trim(EncodeBase64(ABinaryStr));
    outHexa: Result := AsciiToHex(ABinaryStr);
  else
    Result := ABinaryStr;
  end;
end;

function TACBrOpenSSL.ConvertFromStrType(ABinaryStr: AnsiString;
  InputType: TACBrOpenSSLStrType): AnsiString;
begin
  case InputType of
    outBase64: Result := DecodeBase64(ABinaryStr);
    outHexa: Result := HexToAscii(ABinaryStr);
  else
    Result := ABinaryStr;
  end;
end;

function TACBrOpenSSL.ACBrOpenSSLDgstToStr(ADigest: TACBrOpenSSLDgst): String;
begin
  case ADigest of
    dgstMD2: Result := 'md2';
    dgstMD4: Result := 'md4';
    dgstMD5: Result := 'md5';
    dgstRMD160: Result := 'rmd160';
    dgstSHA: Result := 'sha';
    dgstSHA1: Result := 'sha1';
    dgstSHA256: Result := 'sha256';
    dgstSHA512: Result := 'sha512';
    else
      Result := '';
  end;
end;

function TACBrOpenSSL.ACBrOpenSSLDgstToPEVP_MD(ADigest: TACBrOpenSSLDgst
  ): PEVP_MD;
var
  s: AnsiString;
begin
  s := ACBrOpenSSLDgstToStr(ADigest);
  Result := EVP_get_digestbyname(PAnsiChar(s));
  if (Result = nil) then
    raise EACBrOpenSSLException.CreateFmt(sErrDigstNotFound,
      [GetEnumName(TypeInfo(TACBrOpenSSLDgst), Integer(ADigest))]);
end;

function TACBrOpenSSL.GetVersion: String;
var
  v: clong;
  s: AnsiString;
  p1, p2: Integer;
begin
  InitOpenSSL;
  if (fVersion = '') then
  begin
    v := OpenSSLExt.OpenSSLVersionNum;
    if (v > 0) then
    begin
      s := IntToHex(v, 9);
      fVersion := copy(s, 1, 2) + '.' + copy(s, 3, 2) + '.' + copy(s, 5, 2) + '.' + copy(s, 7, 10);
    end
    else
    begin
      s := OpenSSLExt.OpenSSLVersion(0);
      p1 := pos(' ', s);
      if (P1 > 0) then
      begin
        p2 := PosEx(' ', s, p1 + 1);
        if (P2 = 0) then
          p2 := Length(s);
        fVersion := Trim(copy(s, p1, p2 - p1));
      end;
    end;

    fOldLib := (CompareVersions(fVersion, '1.1.0') < 0);
  end;

  Result := fVersion;
end;

function TACBrOpenSSL.IsOldLib: Boolean;
begin
  GetVersion;
  Result := fOldLib;
end;

procedure TACBrOpenSSL.CheckFileExists(const AFile: String);
var
  s: String;
begin
  s := Trim(AFile);
  if (s = '') then
    raise EACBrOpenSSLException.Create(sErrFileNotInformed);

  if not FileExists(s) then
    raise EACBrOpenSSLException.CreateFmt(sErrFileNotExists, [s]);
end;

procedure TACBrOpenSSL.SetBufferSize(AValue: Integer);
begin
  if fBufferSize = AValue then
    Exit;

  fBufferSize := max(1024, AValue);
end;

procedure TACBrOpenSSL.LoadKeys;
begin
  LoadPrivateKey;
  LoadPublicKey;
end;

procedure TACBrOpenSSL.LoadPrivateKey;
begin

end;

procedure TACBrOpenSSL.LoadPublicKey;
begin

end;

procedure TACBrOpenSSL.FreeKeys;
begin
  FreePrivateKey;
  FreePublicKey;
end;

procedure TACBrOpenSSL.FreePrivateKey;
begin
  if (fEVP_PrivateKey <> Nil) then
  begin
    EVP_PKEY_free(fEVP_PrivateKey);
    fEVP_PrivateKey := Nil;
  end ;
end;

procedure TACBrOpenSSL.FreePublicKey;
begin
  if (fEVP_PublicKey <> Nil) then
  begin
    EVP_PKEY_free(fEVP_PublicKey);
    fEVP_PublicKey := Nil;
  end ;
end;

initialization
  OpenSSLLoaded := False;

finalization
  FreeOpenSSL;

end.
