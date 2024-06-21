{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira de Moraes                       }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeOpenSSL;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL,
  {$IfDef MSWINDOWS}ACBrDFeWinCrypt, ACBr_WinCrypt,{$EndIf}
  OpenSSLExt;

resourcestring
  sErrCarregarOpenSSL = 'Erro ao carregar bibliotecas do OpenSSL';
  sErrUtilizePFX = 'Utilize "ArquivoPFX" ou "DadosPFX"';
  sErrCertNaoInformado = 'Certificado não informado.' ;
  sErrCertNaoSuportado = 'TDFeOpenSSL não suporta Leitura de Certificado pelo Número de Série.';
  sErrCertNaoEncontrado = 'Arquivo: %s não encontrado, e DadosPFX não informado';
  sErrCertCarregar =  'Erro ao Carregar Certificado';
  sErrCertSenhaErrada = 'Erro ao ler informações do Certificado.'+sLineBreak+
                        'Provavelmente a senha está errada'; 

type
  { TDFeOpenSSL }

  TDFeOpenSSL = class(TDFeSSLCryptClass)
  private
    FStoreWinApi: Pointer;
    FCertContextWinApi: Pointer;
    FPrivKey: pEVP_PKEY;
    FCert: pX509;
    FVersion: String;
    FOldVersion: Boolean;

    procedure GetCertInfo(cert: pX509);
    procedure DestroyKey;
    procedure DestroyCert;
  protected

    function GetCertContextWinApi: Pointer; override;
    function LerPFXInfo(const PFXData: Ansistring): Boolean;
    procedure CarregarCertificadoDeDadosPFX; override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;
    procedure Clear; override;

    function Versao: String; override;
    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assina: Boolean =  False): AnsiString; override;

    function ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; override;

    procedure DescarregarCertificado; override;
    function CarregarCertificadoPublico(const DadosX509Base64: Ansistring): Boolean; override;

    function OpenSSLVersion: String;
    function OpenSSLOldVersion: Boolean;
    property Certificado: pX509 read FCert;
  end;

function CertToDERBase64(cert: pX509): AnsiString;
function GetCertExt(cert: pX509; const FlagExt: AnsiString): AnsiString;
function GetIssuerName(cert: pX509): String;
function GetNotAfter(cert: pX509): TDateTime;
function GetNotBefore(cert: pX509): TDateTime;
function GetSerialNumber(cert: pX509): String;
function GetThumbPrint( cert: pX509 ): String;
function GetSubjectName(cert: pX509): String;
function GetTaxIDFromExtensions(cert: pX509): String;

function X509NameToString(AX509Name: PX509_NAME): AnsiString;
function BioToStr(ABio: pBIO): AnsiString;

implementation

uses
  strutils, dateutils, typinfo, synautil, synacode,
  ACBrOpenSSLUtils,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.Math,
  ACBrUtil.DateTime,
  ACBrUtil.Base,
  ACBrDFeException;

function CertToDERBase64(cert: pX509): AnsiString;
var
  MemBio: PBIO;
  Buffer: AnsiString;
begin
  MemBio := BioNew(BioSMem());
  try
    i2dX509bio(MemBio, cert);
    Buffer := BioToStr( MemBio );
  finally
    BioFreeAll( MemBio );
  end;

  Result := EncodeBase64(Buffer);
end;

function GetNotAfter( cert: pX509 ): TDateTime;
var
  Validade: String;
  notAfter: PASN1_TIME;
begin
  notAfter := X509GetNotAfter(cert);
  if not Assigned(notAfter) then
  begin
    Result := 0;
    Exit;
  end;

  Validade := String(PAnsiChar(notAfter^.data));
  SetLength(Validade, notAfter^.length);
  Validade := OnlyNumber(Validade);
  if notAfter^.asn1_type = V_ASN1_UTCTIME then  // anos com 2 dígitos
    Validade :=  LeftStr(IntToStrZero(YearOf(Now),4),2) + Validade;

  Result := StoD(Validade);
  Result := IncMinute(Result, TimeZoneBias);

end;

function GetNotBefore( cert: pX509 ): TDateTime;
var
  Validade: String;
  notBefore: PASN1_TIME;
begin
  notBefore := X509GetNotBefore(cert);
  if not Assigned(notBefore) then
  begin
    Result := 0;
    Exit;
  end;

  Validade := String(PAnsiChar(notBefore^.data));
  SetLength(Validade, notBefore^.length);
  Validade := OnlyNumber(Validade);
  if notBefore^.asn1_type = V_ASN1_UTCTIME then  // anos com 2 dígitos
    Validade :=  LeftStr(IntToStrZero(YearOf(Now),4),2) + Validade;

  Result := StoD(Validade);
  Result := IncMinute(Result, TimeZoneBias);
end;

function GetSerialNumber( cert: pX509 ): String;
var
  SN: PASN1_STRING;
  s: AnsiString;
begin
  SN := X509GetSerialNumber(cert);
  s := AnsiString(PAnsiChar(SN^.data));
  SetLength(s,SN^.length);
  Result := AsciiToHex(s);
end;

function GetThumbPrint( cert: pX509 ): String;
var
  md_type: PEVP_MD;
  md_len: LongInt;
  md: AnsiString;
begin
  md_type := EVP_get_digestbyname( 'sha1' );
  md_len  := 0;
  SetLength(md, EVP_MAX_MD_SIZE);
  X509Digest(cert, md_type, md, md_len);
  SetLength(md, md_len);
  Result := AsciiToHex(md);
end;

function GetSubjectName( cert: pX509 ): String;
var
  X509SubjectName: PX509_NAME;
begin
  Result := '';
  X509SubjectName := X509GetSubjectName(cert);

  if Assigned(X509SubjectName) then
    Result := X509NameToString(X509SubjectName);
end;

function GetCertExt(cert: pX509; const FlagExt: AnsiString): AnsiString;
var
  ext: pX509_EXTENSION;
  ExtPos, P: Integer;
  prop: PASN1_STRING;
  propStr: AnsiString;

  procedure LoadExtension;
  begin
    ext := X509GetExt(cert, ExtPos);
  end;

  function AdjustAnsiOID(aOID: AnsiString): AnsiString;
  var
    LenOID: Integer;
  begin
    Result := aOID;
    LenOID := Length(aOID);
    if LenOID < 2 then Exit;
    if (ord(aOID[1]) <> 4) then Exit;   // Not ANSI

    LenOID := ord(aOID[2]);
    Result := copy(aOID,3,LenOID);
  end;

  function AdjustOID(aOID: AnsiString): AnsiString;
  var
    LenOID: Integer;
  begin
    Result := '';
    if aOID = '' then Exit;

    LenOID := ord(aOID[1]);
    Result := copy(aOID,2,LenOID);
    if (Result <> '') and (Result[1] = #4) then  // é ANSI
      Result := AdjustAnsiOID(Result);
  end;
begin
  Result := '';
  ExtPos := 0;
  LoadExtension;
  while (ext <> nil) do
  begin
    prop := X509ExtensionGetData(ext);
    if Assigned(prop) then
    begin
      propStr := PAnsiChar(prop^.data);
      SetLength(propStr, prop^.length);
      P := pos(FlagExt, propStr);
      if P > 0 then
      begin
        Result := AdjustOID( AnsiString( copy(propStr,P+Length(FlagExt),Length(propStr))));
        exit;
      end;
    end;

    inc(ExtPos);
    LoadExtension;
  end;
end;

function GetTaxIDFromExtensions(cert: pX509): String;
var
  aOID: AnsiString;
begin
  Result := '';
  // Procurando pela Extensão onde está o CNPJ
  aOID := GetCertExt( cert, #1#3#3#160 );
  if (aOID <> '') then
    Result := copy(Trim(aOID), 1, 14);

  // Ainda sem resposta, deve ser um eCPF, procure por CPF
  if Result = '' then
  begin
    aOID := GetCertExt( cert, #1#3#1#160 );
    if aOID <> ''then
      Result := copy( Trim(aOID), 9 ,11);  // Pula DataNascimento
  end;
end;

function GetIssuerName( cert: pX509 ): String;
var
  X509IssuerName: pX509_NAME;
begin
  Result := '';
  X509IssuerName := X509GetIssuerName(cert);

  if Assigned(X509IssuerName) then
    Result := X509NameToString(X509IssuerName);
end;

function X509NameToString(AX509Name: PX509_NAME): AnsiString;
var
  MemBio: PBIO;
begin
  MemBio := BioNew(BioSMem());
  try
    X509NAMEprintEx( MemBio, AX509Name, 0,
                     (XN_FLAG_SEP_CPLUS_SPC and XN_FLAG_SEP_MASK)
                     {$IfDef FPC} or ASN1_STRFLGS_UTF8_CONVERT{$EndIf}
                   );
    Result := BioToStr(MemBio);
  finally
    BioFreeAll(MemBio);
  end;
end;

{ Método clonado de ACBrEAD }
function BioToStr(ABio : pBIO) : AnsiString ;
Var
  Ret : Integer ;
  Lin : AnsiString ;
begin
  Result := '';
  repeat
    SetLength(Lin,1024);
    Ret := BioRead( ABio, Lin, 1024);
    if Ret > 0 then
    begin
      Lin := copy(Lin,1,Ret) ;
      Result := Result + Lin;
    end ;
  until (Ret <= 0);
end ;

{ TDFeOpenSSL }

constructor TDFeOpenSSL.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);
  FPrivKey := nil;
  FCert := nil;
  FStoreWinApi := Nil;
  FCertContextWinApi := Nil;
  Clear;
end;

destructor TDFeOpenSSL.Destroy;
begin
  DescarregarCertificado;
  inherited Destroy;
end;

procedure TDFeOpenSSL.Clear;
begin
  inherited Clear;
  FVersion := '';
  FOldVersion := False;
end;

function TDFeOpenSSL.Versao: String;
begin
  if not InitSSLInterface then
    Result := sErrCarregarOpenSSL 
  else
    Result := OpenSSLVersion;
end;

procedure TDFeOpenSSL.DestroyKey;
begin
  if (FPrivKey <> Nil) then
  begin
    EvpPkeyFree(FPrivKey);
    FPrivKey := nil;
  end;
end;

procedure TDFeOpenSSL.DestroyCert;
begin
  if (FCert <> Nil) then
  begin
    X509free(FCert);
    FCert := Nil;
  end;
end;

procedure TDFeOpenSSL.GetCertInfo(cert: pX509);
begin
  with FpDadosCertificado do
  begin
    Clear;
    NumeroSerie := GetSerialNumber( cert );
    ThumbPrint  := GetThumbPrint( cert );
    SubjectName := GetSubjectName( cert );
    if CNPJ = '' then  // Não tem CNPJ/CPF no SubjectName, lendo das Extensões
      CNPJ := GetTaxIDFromExtensions( cert );

    DataVenc := GetNotAfter( cert );
    DataInicioValidade := GetNotBefore( cert );
    IssuerName := GetIssuerName( cert );
    DERBase64 := CertToDERBase64( cert );
    Tipo := tpcA1;  // OpenSSL somente suporta A1
  end;
end;

function TDFeOpenSSL.GetCertContextWinApi: Pointer;
begin
  {$IfDef MSWINDOWS}
   CarregarCertificadoSeNecessario;
   if FCertContextWinApi = nil then
     PFXDataToCertContextWinApi( FpDFeSSL.DadosPFX, FpDFeSSL.Senha,
                                 FStoreWinApi, FCertContextWinApi);
  {$Else}
   FCertContextWinApi := Nil;
  {$EndIf}

  Result := FCertContextWinApi;
end;

procedure TDFeOpenSSL.DescarregarCertificado;
begin
  DestroyKey;
  DestroyCert;

  {$IfDef MSWINDOWS}
  if Assigned(FCertContextWinApi) then
    CertFreeCertificateContext(FCertContextWinApi);

  if Assigned(FStoreWinApi) then
    CertCloseStore(FStoreWinApi, CERT_CLOSE_STORE_FORCE_FLAG);
  {$EndIf}

  FCertContextWinApi := Nil;
  FStoreWinApi := Nil;

  inherited DescarregarCertificado;
end;

function TDFeOpenSSL.LerPFXInfo(const PFXData: Ansistring): Boolean;
var
  ca, p12: Pointer;
  b: PBIO;
begin
  Result := False;
  DestroyKey;

  b := BioNew(BioSMem);
  try
    BioWrite(b, PFXData, Length(PFXData));
    p12 := d2iPKCS12bio(b, nil);
    if not Assigned(p12) then
      Exit;

    try
      ca := nil;
      DestroyCert;
      DestroyKey;
      if (PKCS12parse(p12, FpDFeSSL.Senha, FPrivKey, FCert, ca) > 0) then
      begin
        if (FCert <> nil) then
        begin
          GetCertInfo( FCert );
          Result := True;
        end;
      end;
    finally
      PKCS12free(p12);
      OPENSSL_sk_pop_free(ca, @X509free);
    end;
  finally
    BioFreeAll(b);
  end;
end;

procedure TDFeOpenSSL.CarregarCertificadoDeDadosPFX;
begin
  if not InitSSLInterface then
    raise EACBrDFeException.Create(sErrCarregarOpenSSL);

  if not LerPFXInfo(FpDFeSSL.DadosPFX) then
    raise EACBrDFeException.Create(sErrCertSenhaErrada + sLineBreak + GetLastOpenSSLError);
end;

function TDFeOpenSSL.CarregarCertificadoPublico(const DadosX509Base64: Ansistring): Boolean;
var
  b: PBIO;
  BinaryX509: AnsiString;
begin
  Result := False;
  DescarregarCertificado;

  BinaryX509 := DecodeBase64( DadosX509Base64 );

  b := BioNew(BioSMem);
  try
    BioWrite(b, BinaryX509, Length(BinaryX509));
    FCert := d2iX509bio(b, FCert);
    if Assigned( FCert ) then
    begin
      GetCertInfo( FCert );
      Result := True;
    end;
  finally
    BioFreeAll(b);
  end;
end;

function TDFeOpenSSL.OpenSSLVersion: String;
begin
  OpenSSLOldVersion;
  Result := OpenSSLExt.OpenSSLVersion(0);
end;

function TDFeOpenSSL.OpenSSLOldVersion: Boolean;
var
  VersaoStr: String;
  VersaoNum: Integer;
  P1, P2: Integer;
begin
  if (FVersion = '') then
  begin
    VersaoNum := OpenSSLExt.OpenSSLVersionNum;
    if (VersaoNum > 0) then
    begin
      VersaoStr := IntToHex(VersaoNum, 9);
      FVersion := copy(VersaoStr,1,2)+'.'+copy(VersaoStr,3,2)+'.'+copy(VersaoStr,5,2)+'.'+copy(VersaoStr,7,10);
    end
    else
    begin
      VersaoStr := OpenSSLExt.OpenSSLVersion(0);

      P1 := pos(' ', VersaoStr);
      if P1 > 0 then
      begin
        P2 := PosEx(' ', VersaoStr, P1+1 );
        if P2 = 0 then
          P2 := Length(VersaoStr);

        FVersion := Trim(copy(VersaoStr, P1, P2-P1));
      end;
    end;

    FOldVersion := (CompareVersions(FVersion, '1.1.0') < 0);
  end;

  Result := FOldVersion;
end;

{ Método clonado de ACBrEAD }
function TDFeOpenSSL.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assina: Boolean): AnsiString;
Var
  md : PEVP_MD ;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
  pmd_ctx: PEVP_MD_CTX;
  md_value_bin : array [0..1023] of AnsiChar;
  NameDgst : PAnsiChar;
  ABinStr: AnsiString;
  Memory: Pointer;
  PosStream: Int64;
  BytesRead: LongInt;
begin
  NameDgst := '';
  case Digest of
    dgstMD2    : NameDgst := 'md2';
    dgstMD4    : NameDgst := 'md4';
    dgstMD5    : NameDgst := 'md5';
    dgstRMD160 : NameDgst := 'rmd160';
    dgstSHA    : NameDgst := 'sha';
    dgstSHA1   : NameDgst := 'sha1';
    dgstSHA256 : NameDgst := 'sha256';
    dgstSHA512 : NameDgst := 'sha512';
  end ;

  if Assina and (FPrivKey = Nil) then
    CarregarCertificado;

  pmd_ctx := Nil;
  PosStream := 0;
  AStream.Position := 0;
  GetMem(Memory, CBufferSize);
  try
    md_len := 0;
    md := EVP_get_digestbyname( NameDgst );
    if md = Nil then
      raise EACBrDFeException.Create('Erro ao carregar Digest: '+NameDgst + sLineBreak + GetLastOpenSSLError);

    if OpenSSLOldVersion then
      pmd_ctx := @md_ctx
    else
      pmd_ctx := EVP_MD_CTX_new();

    EVP_DigestInit( pmd_ctx, md );

    while (PosStream < AStream.Size) do
    begin
       BytesRead := AStream.Read(Memory^, CBufferSize);
       if BytesRead <= 0 then
          Break;

       EVP_DigestUpdate( pmd_ctx, Memory, BytesRead ) ;
       PosStream := PosStream + BytesRead;
    end;

    if Assina then
       EVP_SignFinal( pmd_ctx, @md_value_bin, md_len, FPrivKey)
    else
       EVP_DigestFinal( pmd_ctx, @md_value_bin, @md_len);

    SetString( ABinStr, md_value_bin, md_len);
    Result := ABinStr;
  finally
    if (not OpenSSLOldVersion) and (pmd_ctx <> nil) then
      EVP_MD_CTX_free( pmd_ctx );

    Freemem(Memory);
  end;
end;

function TDFeOpenSSL.ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean;
Var
  md : PEVP_MD ;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
  pmd_ctx: PEVP_MD_CTX;
  md_value_bin : array [0..1023] of AnsiChar;
  NameDgst : PAnsiChar;
  HashResult: AnsiString;
  Memory: Pointer;
  PosStream: Int64;
  Ret, BytesRead: LongInt;
  pubKey: pEVP_PKEY;
begin
{$IFNDEF COMPILER25_UP}
  Result := False;
{$ENDIF}
  NameDgst := '';
  case Digest of
    dgstMD2    : NameDgst := 'md2';
    dgstMD4    : NameDgst := 'md4';
    dgstMD5    : NameDgst := 'md5';
    dgstRMD160 : NameDgst := 'rmd160';
    dgstSHA    : NameDgst := 'sha';
    dgstSHA1   : NameDgst := 'sha1';
    dgstSHA256 : NameDgst := 'sha256';
    dgstSHA512 : NameDgst := 'sha512';
  end ;

  if Assinado and (FCert = nil) then
    CarregarCertificado;

  PosStream := 0;
  AStream.Position := 0;
  GetMem(Memory, CBufferSize);
  pmd_ctx := Nil;
  try
    md_len := 0;
    md := EVP_get_digestbyname( NameDgst );
    if md = Nil then
      raise EACBrDFeException.Create('Erro ao carregar Digest: '+NameDgst + sLineBreak + GetLastOpenSSLError);

    if OpenSSLOldVersion then
      pmd_ctx := @md_ctx
    else
      pmd_ctx := EVP_MD_CTX_new();

    EVP_DigestInit( pmd_ctx, md );

    while (PosStream < AStream.Size) do
    begin
      BytesRead := AStream.Read(Memory^, CBufferSize);
      if BytesRead <= 0 then
        Break;

      EVP_DigestUpdate( pmd_ctx, Memory, BytesRead ) ;
      PosStream := PosStream + BytesRead;
    end;

    if Assinado then
    begin
      pubKey := X509GetPubkey(FCert);
      Ret := EVP_VerifyFinal( pmd_ctx, PAnsiChar(Hash), Length(Hash), pubKey) ;
      Result := (Ret = 1);
    end
    else
    begin
      EVP_DigestFinal( pmd_ctx, @md_value_bin, @md_len);
      SetString( HashResult, md_value_bin, md_len);
      Result := (Pos( HashResult, Hash ) > 0) ;
    end;
  finally
    if (not OpenSSLOldVersion) and (pmd_ctx <> nil) then
      EVP_MD_CTX_free( pmd_ctx );

    Freemem(Memory);
  end;
end;

end.


