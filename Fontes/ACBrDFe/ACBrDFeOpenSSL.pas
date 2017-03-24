{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeOpenSSL;

interface

uses
  {$IFDEF DELPHIXE4_UP}
   AnsiStrings,
  {$ENDIF}
  Classes, SysUtils,
  ACBrDFeSSL,
  {$IfDef MSWINDOWS}ACBrDFeWinCrypt, ACBr_WinCrypt,{$EndIf}
  {$IFDEF USE_libeay32}libeay32{$ELSE} OpenSSLExt{$ENDIF};


type
  { TDFeOpenSSL }

  TDFeOpenSSL = class(TDFeSSLCryptClass)
  private
    FStoreWinApi: Pointer;
    FCertContextWinApi: Pointer;
    FPrivKey: pEVP_PKEY;
    FCert: pX509;

    procedure GetCertInfo(cert: pX509);

    procedure DestroyKey;
    procedure DestroyCert;
  protected

    function GetCertContextWinApi: Pointer; override;
    function LerPFXInfo(PFXData: Ansistring): Boolean;
  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assina: Boolean =  False): AnsiString; override;

    procedure CarregarCertificado; override;
    procedure DescarregarCertificado; override;
    function CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean; override;

    property Certificado: pX509 read FCert;
  end;

function CertToDERBase64(cert: pX509): AnsiString;
function GetCertExt(cert: pX509; FlagExt: AnsiString): String;
function GetIssuerName(cert: pX509): String;
function GetNotAfter(cert: pX509): TDateTime;
function GetSerialNumber(cert: pX509): String;
function GetSubjectName(cert: pX509): String;
function GetCNPJFromExtensions(cert: pX509): String;

function X509NameToString(AX509Name: PX509_NAME): AnsiString;
function BioToStr(ABio: pBIO): AnsiString;

implementation

uses
  strutils, dateutils, typinfo,
  ACBrUtil, ACBrDFeException,
  pcnAuxiliar,
  synautil, synacode;

function CertToDERBase64(cert: pX509): AnsiString;
var
  MemBio: PBIO;
  Buffer: AnsiString;
begin
  {$IFDEF USE_libeay32}
   MemBio := Bio_New(Bio_S_Mem());
   try
     i2d_X509_bio(MemBio, cert);
     Buffer := BioToStr( MemBio );
   finally
     BIO_free_all( MemBio );
   end;
  {$ELSE}
   MemBio := BioNew(BioSMem());
   try
     i2dX509bio(MemBio, cert);
     Buffer := BioToStr( MemBio );
   finally
     BioFreeAll( MemBio );
   end;
  {$ENDIF}

  Result := EncodeBase64(Buffer);
end;

function GetNotAfter( cert: pX509 ): TDateTime;
var
  Validade: String;
  notAfter: PASN1_TIME;
begin
  notAfter := cert^.cert_info^.validity^.notAfter;
  Validade := {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrPas( PAnsiChar(notAfter^.data) );
  SetLength(Validade, notAfter^.length);
  Validade := OnlyNumber(Validade);
    if notAfter^.asn1_type = V_ASN1_UTCTIME then  // anos com 2 dígitos
    Validade :=  LeftStr(IntToStrZero(YearOf(Now),4),2) + Validade;
    Result := StoD(Validade);
end;

function GetSerialNumber( cert: pX509 ): String;
var
  SN: PASN1_STRING;
  s: AnsiString;
begin
  {$IFDEF USE_libeay32}
   SN := X509_get_serialNumber(cert);
  {$ELSE}
   SN := X509GetSerialNumber(cert);
  {$ENDIF}
  s := {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrPas( PAnsiChar(SN^.data) );
  SetLength(s,SN^.length);
  Result := AsciiToHex(s);
end;

function GetSubjectName( cert: pX509 ): String;
var
  X509SubjectName: PX509_NAME;
begin
  Result := '';
  {$IFDEF USE_libeay32}
   X509SubjectName := X509_get_subject_name(cert);
  {$ELSE}
   X509SubjectName := X509GetSubjectName(cert);
  {$ENDIF}

  if Assigned(X509SubjectName) then
    Result := X509NameToString(X509SubjectName);
end;

function GetCNPJFromExtensions(cert: pX509): String;
begin
  // Procurando pela Extensão onde está o CNPJ
  Result := LeftStr(OnlyNumber(copy(GetCertExt( cert, #1#3#3#160#16 ),1,16)),14);

  // Ainda sem resposta, deve ser um eCPF, procure por CPF
  if Result = '' then
    Result := copy(GetCertExt( cert, #1#3#1#160#52 ),11 ,11);  // Pula DataNascimento
end;

function GetCertExt(cert: pX509; FlagExt: AnsiString): String;
var
  ext: pX509_EXTENSION;
  ExtPos, P: Integer;
  prop: PASN1_STRING;
  propStr: AnsiString;

  procedure LoadExtension;
  begin
    {$IFDEF USE_libeay32}
     ext := X509_get_ext( cert, ExtPos);
    {$ELSE}
     ext := X509GetExt( cert, ExtPos);
    {$ENDIF}
  end;

begin
  Result := '';
  ExtPos := 0;
  LoadExtension;
  while (ext <> nil) do
  begin
    prop := ext^.value;
    propStr := PAnsiChar(prop^.data);
    SetLength(propStr, prop^.length);
    P := pos(FlagExt, propStr);
    if P > 0 then
    begin
      Result := copy(propStr,P+Length(FlagExt),Length(propStr));
      exit;
    end;

    inc( ExtPos );
    LoadExtension;
  end;
end;

function GetIssuerName( cert: pX509 ): String;
var
  X509IssuerName: pX509_NAME;
begin
  Result := '';
  {$IFDEF USE_libeay32}
   X509IssuerName := X509_get_issuer_name(cert);
  {$ELSE}
   X509IssuerName := X509GetIssuerName(cert);
  {$ENDIF}

  if Assigned(X509IssuerName) then
    Result := X509NameToString(X509IssuerName);
end;

function X509NameToString(AX509Name: PX509_NAME): AnsiString;
var
  MemBio: PBIO;
begin
  {$IfDef USE_libeay32}
   MemBio := Bio_New(BIO_s_mem());
   try
    X509_NAME_print_ex(MemBio, AX509Name, 0,
    (XN_FLAG_SEP_CPLUS_SPC and XN_FLAG_SEP_MASK)
    {$IfDef FPC} or ASN1_STRFLGS_UTF8_CONVERT{$EndIf} );
     Result := BioToStr(MemBio);
   finally
     BIO_free_all(MemBio);
   end;
  {$Else}
   MemBio := BioNew(BioSMem());
   try
     X509NAMEprintEx(MemBio, AX509Name, 0,
     (XN_FLAG_SEP_CPLUS_SPC and XN_FLAG_SEP_MASK)
     {$IfDef FPC} or ASN1_STRFLGS_UTF8_CONVERT{$EndIf} );
     Result := BioToStr(MemBio);
   finally
     BioFreeAll(MemBio);
   end;
  {$EndIf}
end;

{ Método clonado de ACBrEAD }
function BioToStr(ABio : pBIO) : AnsiString ;
Var
  {$IFDEF USE_libeay32}
   Buf : array [0..1023] of AnsiChar;
  {$ENDIF}
  Ret : Integer ;
  Lin : String ;
begin
  Result := '';
    {$IFDEF USE_libeay32}
   while BIO_eof( ABio ) = 0 do
   begin
     Ret := BIO_gets( ABio, Buf, 1024 );
     SetString( Lin, Buf, Ret);
     Result := Result + Lin;
   end ;
  {$ELSE}
   repeat
      SetLength(Lin,1024);
      Ret := BioRead( ABio, Lin, 1024);
      if Ret > 0 then
      begin
         Lin := copy(Lin,1,Ret) ;
         Result := Result + Lin;
      end ;
   until (Ret <= 0);
  {$ENDIF}
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

procedure TDFeOpenSSL.DestroyKey;
begin
  if (FPrivKey <> Nil) then
  begin
    {$IfDef USE_libeay32}
     EVP_PKEY_free(FPrivKey);
    {$Else}
     EvpPkeyFree(FPrivKey);
    {$EndIf}
    FPrivKey := nil;
  end;
end;

procedure TDFeOpenSSL.DestroyCert;
begin
  if (FCert <> Nil) then
  begin
    {$IfDef USE_libeay32}
    X509_free(FCert);
    {$Else}
    X509free(FCert);
    {$EndIf}
    FCert := Nil;
  end;
end;

procedure TDFeOpenSSL.GetCertInfo(cert: pX509);
begin
  with FpDadosCertificado do
  begin
    Clear;
    NumeroSerie := GetSerialNumber( cert );
    SubjectName := GetSubjectName( cert );
    if CNPJ = '' then  // Não tem CNPJ/CPF no SubjectName, lendo das Extensões
      CNPJ := GetCNPJFromExtensions( cert );

    DataVenc := GetNotAfter( cert );
    IssuerName := GetIssuerName( cert );
    DERBase64 := CertToDERBase64( cert );
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

procedure TDFeOpenSSL.CarregarCertificado;
var
  LoadFromFile, LoadFromData: Boolean;
  FS: TFileStream;
begin
  DescarregarCertificado;

  with FpDFeSSL do
  begin
    // Verificando se possui parâmetros necessários //
    if EstaVazio(ArquivoPFX) and EstaVazio(DadosPFX) then
    begin
      if not EstaVazio(NumeroSerie) then
        raise EACBrDFeException.Create(
          'TDFeOpenSSL não suporta carga de Certificado pelo número de série.' +
          sLineBreak + 'Utilize "ArquivoPFX" ou "DadosPFX"')
      else
        raise EACBrDFeException.Create('Certificado não informado.' +
          sLineBreak + 'Utilize "ArquivoPFX" ou "DadosPFX"');
    end;

    LoadFromFile := (not EstaVazio(ArquivoPFX)) and FileExists(ArquivoPFX);
    LoadFromData := (not EstaVazio(DadosPFX));

    if not (LoadFromFile or LoadFromData) then
      raise EACBrDFeException.Create('Arquivo: ' + ArquivoPFX + ' não encontrado, e DadosPFX não informado');

    if LoadFromFile then
    begin
      FS := TFileStream.Create(ArquivoPFX, fmOpenRead or fmShareDenyNone);
      try
        DadosPFX := ReadStrFromStream(FS, FS.Size);
      finally
        FS.Free;
      end;
    end;

    if EstaVazio(DadosPFX) then
      raise EACBrDFeException.Create('Erro ao Carregar Certificado');

    if not LerPFXInfo(DadosPFX) then
      raise EACBrDFeException.Create('Erro ao ler informações do Certificado.'+sLineBreak+
                                     'Provavelmente a senha está errada' );
  end;

  FpCertificadoLido := True;
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

function TDFeOpenSSL.LerPFXInfo(PFXData: Ansistring): Boolean;
var
  ca, p12: Pointer;
  b: PBIO;
begin
  Result := False;
  DestroyKey;

  {$IFDEF USE_libeay32}
   b := Bio_New(BIO_s_mem);
  {$ELSE}
   b := BioNew(BioSMem);
  {$ENDIF}
  try
    {$IFDEF USE_libeay32}
     BIO_write(b, PAnsiChar(PFXData), Length(PFXData));
     p12 := d2i_PKCS12_bio(b, nil);
    {$ELSE}
     BioWrite(b, PFXData, Length(PFXData));
     p12 := d2iPKCS12bio(b, nil);
    {$ENDIF}
    if not Assigned(p12) then
      Exit;

    try
      DestroyCert;
      DestroyKey;
      ca := nil;
      {$IFDEF USE_libeay32}
      if PKCS12_parse(p12, PAnsiChar(FpDFeSSL.Senha), FPrivKey, FCert, ca) > 0 then
      {$ELSE}
      if PKCS12parse(p12, FpDFeSSL.Senha, FPrivKey, FCert, ca) > 0 then
      {$ENDIF}
      begin
        if (FCert <> nil) then
        begin
          GetCertInfo( FCert );
          Result := True;
        end;
      end;
    finally
      {$IFDEF USE_libeay32}
       PKCS12_free(p12);
      {$ELSE}
       PKCS12free(p12);
      {$ENDIF}
    end;
  finally
    {$IFDEF USE_libeay32}
     BIO_free_all(b);
    {$ELSE}
     BioFreeAll(b);
    {$ENDIF}
  end;
end;

function TDFeOpenSSL.CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean;
var
  b: PBIO;
  BinaryX509: AnsiString;
begin
  Result := False; 
  DescarregarCertificado;

  BinaryX509 := DecodeBase64( DadosX509Base64 );

  {$IFDEF USE_libeay32}
   b := Bio_New(BIO_s_mem);
  {$ELSE}
   b := BioNew(BioSMem);
  {$ENDIF}
  try
    {$IFDEF USE_libeay32}
     BIO_write(b, PAnsiChar(BinaryX509), Length(BinaryX509));
     FCert := d2i_X509_bio(b, FCert);
    {$ELSE}
     BioWrite(b, BinaryX509, Length(BinaryX509));
     FCert := d2iX509bio(b, FCert);
    {$ENDIF}
    if Assigned( FCert ) then
    begin
      GetCertInfo( FCert );
      Result := True;
    end;
  finally
    {$IFDEF USE_libeay32}
     BIO_free_all(b);
    {$ELSE}
     BioFreeAll(b);
    {$ENDIF}
  end;
end;

{ Método clonado de ACBrEAD }
function TDFeOpenSSL.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assina: Boolean): AnsiString;
Var
  md : PEVP_MD ;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
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

  PosStream := 0;
  AStream.Position := 0;
  GetMem(Memory, CBufferSize);
  try
    md_len := 0;
    md := EVP_get_digestbyname( NameDgst );
    EVP_DigestInit( @md_ctx, md );

    while (PosStream < AStream.Size) do
    begin
       BytesRead := AStream.Read(Memory^, CBufferSize);
       if BytesRead <= 0 then
          Break;

       EVP_DigestUpdate( @md_ctx, Memory, BytesRead ) ;
       PosStream := PosStream + BytesRead;
    end;

    if Assina then
       EVP_SignFinal( @md_ctx, @md_value_bin, md_len, FPrivKey)
    else
       EVP_DigestFinal( @md_ctx, @md_value_bin, {$IFNDEF USE_libeay32}@{$ENDIF}md_len);

    SetString( ABinStr, md_value_bin, md_len);
    Result := ABinStr;
  finally
    Freemem(Memory);
  end;
end;

end.


