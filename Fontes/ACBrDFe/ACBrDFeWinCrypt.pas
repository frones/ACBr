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

unit ACBrDFeWinCrypt;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL,
  ACBr_WinCrypt, Windows;

const
  sz_CERT_STORE_PROV_PKCS12 = 'PKCS12';
   SCARD_W_WRONG_CHV        = $8010006B;
   SCARD_W_CHV_BLOCKED      = $8010006C;

type
  { TDFeWinCrypt }

  TDFeWinCrypt = class(TDFeSSLCryptClass)
  private
    procedure GetCertContextInfo(ADadosCertificado: TDadosCertificado;
      ACertContext: PCCERT_CONTEXT; CheckIsHardware: Boolean);
    procedure OpenSystemStore;
  protected
    FpCertContext: PCCERT_CONTEXT;
    FpStore: HCERTSTORE;
    FpPFXData: AnsiString;

    function GetCertContextWinApi: Pointer; override;
    function GetCertPFXData: AnsiString; override;
  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assina: Boolean =  False): AnsiString; override;

    procedure CarregarCertificado; override;
    function SelecionarCertificado: String; override;
    procedure LerCertificadosStore; override;
    procedure DescarregarCertificado; override;
    function CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean; override;

    property Certificado: PCCERT_CONTEXT read FpCertContext;
  end;

function GetLastErrorAsHexaStr: String;
function MsgErroGetCryptProvider: String;
function MsgSetPINError: String;

function GetSerialNumber(ACertContext: PCCERT_CONTEXT): String;
function GetSubjectName(ACertContext: PCCERT_CONTEXT): String;
function GetIssuerName(ACertContext: PCCERT_CONTEXT): String;
function GetNotAfter(ACertContext: PCCERT_CONTEXT): TDateTime;
function GetCertIsHardware(ACertContext: PCCERT_CONTEXT): Boolean;
function GetProviderParamString(ACryptProvider: HCRYPTPROV; dwParam: DWORD): String;
function GetProviderParamDWord(ACryptProvider: HCRYPTPROV; dwParam: DWORD): DWORD;
function GetProviderIsHardware(ACryptProvider: HCRYPTPROV): Boolean;
function GetCNPJFromExtensions(ACertContext: PCCERT_CONTEXT): String;
function CertToDERBase64(ACertContext: PCCERT_CONTEXT): AnsiString;
procedure GetProviderInfo(ACertContext: PCCERT_CONTEXT;
   var ProviderType: DWORD; var ProviderName, ContainerName: String);

procedure PFXDataToCertContextWinApi( AData, APass: AnsiString; var AStore, ACertContext: Pointer);
function ExportCertContextToPFXData( ACertContext: PCCERT_CONTEXT; APass: AnsiString): AnsiString;
procedure SetCertContextPassword(ACertContext: PCCERT_CONTEXT; APass: AnsiString);



implementation

uses
  strutils, typinfo, comobj,
  ACBrUtil, ACBrDFeException, ACBrConsts,
  synautil, synacode;

function GetLastErrorAsHexaStr: String;
begin
  Result := IntToHex(GetLastError, 8);
end;

function MsgErroGetCryptProvider: String;
var
  WinErro: DWORD;
begin
  WinErro := GetLastError;
  if WinErro = DWORD( NTE_KEYSET_NOT_DEF ) then
    Result := 'Provedor de Cripotografia não encontrado!'
  else if WinErro = DWORD( NTE_KEYSET_ENTRY_BAD ) then
    Result := 'Estrura de Chave obtida no Provedor de Cripotografia está corrompida'
  else
    Result := 'Falha em obter Provedor de Cripotografia do Certificado. Erro: '+GetLastErrorAsHexaStr;
end;

function MsgSetPINError: String;
var
  WinError: DWORD;
begin
  WinError := GetLastError;
  if WinError = SCARD_W_WRONG_CHV then
    Result := 'O cartão não pode ser acessado porque o PIN errado foi apresentado.'
  else if WinError = SCARD_W_CHV_BLOCKED then
    Result := 'O cartão não pode ser acessado porque o número máximo de tentativas de entrada de PIN foi atingido'
  else
    Result := 'Falha ao Definir PIN do Certificado. Erro:'+GetLastErrorAsHexaStr;
end;

function GetSerialNumber(ACertContext: PCCERT_CONTEXT): String;
var
  I: Integer;
  ByteArr: array of byte;
begin
  Result := '';
  if Assigned(ACertContext) then
  begin
    SetLength(ByteArr, ACertContext^.pCertInfo^.SerialNumber.cbData);
    Move(ACertContext^.pCertInfo^.SerialNumber.pbData^,
         ByteArr[0],
         ACertContext^.pCertInfo^.SerialNumber.cbData);

    For I := 0 to ACertContext^.pCertInfo^.SerialNumber.cbData-1 do
      Result := IntToHex(ByteArr[I], 2) + Result;

    Result := Trim(UpperCase(Result));
  end;
end;

function GetSubjectName(ACertContext: PCCERT_CONTEXT): String;
var
  CertName: {$IfDef UNICODE}WideString{$Else}String{$EndIf};
  BytesRead: DWORD;
begin
  Result := '';
  if Assigned(ACertContext) then
  begin
    SetLength(CertName, 1024);
    BytesRead := CertNameToStr( ACertContext^.dwCertEncodingType,
                      @ACertContext^.pCertInfo^.Subject,
                      CERT_X500_NAME_STR,
                      {$IfDef UNICODE}LPWSTR{$Else}LPSTR{$EndIf}(CertName),
                      1024);
    if BytesRead > 0 then
      SetLength(CertName, BytesRead)
    else
      raise EACBrDFeException.Create( 'Falha ao executar "CertNameToStr" em "GetSubjectName". Erro:'+GetLastErrorAsHexaStr);

    Result := String( CertName );
  end;
end;

function GetIssuerName(ACertContext: PCCERT_CONTEXT): String;
var
  CertName: {$IfDef UNICODE}WideString{$Else}String{$EndIf};
  BytesRead: DWORD;
begin
  Result := '';
  if Assigned(ACertContext) then
  begin
    SetLength(CertName, 1024);
    BytesRead := CertNameToStr( ACertContext^.dwCertEncodingType,
                      @ACertContext^.pCertInfo^.Issuer,
                      CERT_X500_NAME_STR,
                      {$IfDef UNICODE}LPWSTR{$Else}LPSTR{$EndIf}(CertName),
                      1024);
    if BytesRead > 0 then
      SetLength(CertName, BytesRead)
    else
      raise EACBrDFeException.Create( 'Falha ao executar "CertNameToStr" em "GetIssuerName". Erro:'+GetLastErrorAsHexaStr);

    Result := String( CertName );
  end;
end;

function GetNotAfter(ACertContext: PCCERT_CONTEXT): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if Assigned(ACertContext) then
  begin
    FileTimeToLocalFileTime(TFILETIME(ACertContext^.pCertInfo^.NotAfter), LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  end;
end;

function GetCertIsHardware(ACertContext: PCCERT_CONTEXT): Boolean;
var
  mCryptProviderCert: HCRYPTPROV;
  dwKeySpec: DWORD;
  pfCallerFreeProv: LongBool;
begin
  mCryptProviderCert := 0;
  dwKeySpec := AT_KEYEXCHANGE;
  pfCallerFreeProv := False;

  // Obtendo o Contexto do Provedor de Criptografia do Certificado //
  if not CryptAcquireCertificatePrivateKey( ACertContext, 0, Nil,
                                            mCryptProviderCert, dwKeySpec,
                                            pfCallerFreeProv) then
    raise EACBrDFeException.Create( MsgErroGetCryptProvider );

  try
    Result := GetProviderIsHardware(mCryptProviderCert);
  finally
    if pfCallerFreeProv then
      CryptReleaseContext(mCryptProviderCert, 0);
  end;
end;

function GetProviderParamString(ACryptProvider: HCRYPTPROV; dwParam: DWORD): String;
var
  pdwDataLen: DWORD;
  pbData: PBYTE;
begin
  pdwDataLen := 0;
  if not CryptGetProvParam(ACryptProvider, dwParam, nil, pdwDataLen, 0) then
    raise EACBrDFeException.Create(
        'GetProviderParamAsString: Falha ao obter BufferSize. Erro:'+GetLastErrorAsHexaStr);

  pbData := AllocMem(pdwDataLen);
  try
    SetLength(Result, pdwDataLen);
    if not CryptGetProvParam(ACryptProvider, dwParam, pbData, pdwDataLen, 0) then
      raise EACBrDFeException.Create(
          'GetProviderParamAsString: Falha ao Ler Retorno. Erro:'+GetLastErrorAsHexaStr);

    SetLength(Result, pdwDataLen-1);
    Move(pbData^, Result[1], pdwDataLen-1);
  finally
    Freemem(pbData);
  end;
end;

function GetProviderParamDWord(ACryptProvider: HCRYPTPROV; dwParam: DWORD
  ): DWORD;
var
  pdwDataLen: DWORD;
begin
  pdwDataLen := SizeOf(DWORD);
  if not CryptGetProvParam(ACryptProvider, dwParam, @Result, pdwDataLen, 0) then
    raise EACBrDFeException.Create(
        'GetProviderParamDWord. Erro:'+GetLastErrorAsHexaStr);
end;

function GetProviderIsHardware(ACryptProvider: HCRYPTPROV): Boolean;
var
  ImpType: DWORD;
begin
  ImpType := GetProviderParamDWord(ACryptProvider, PP_IMPTYPE);
  Result := ((ImpType and CRYPT_IMPL_HARDWARE) = CRYPT_IMPL_HARDWARE);
end;

function GetCNPJFromExtensions(ACertContext: PCCERT_CONTEXT
  ): String;
type
  ArrCERT_ALT_NAME_ENTRY = array of CERT_ALT_NAME_ENTRY;
var
  pExtension: PCERT_EXTENSION;
  pNameInfo: PCERT_ALT_NAME_INFO ;
  BufferSize: DWORD;
  I: Int64;
  CertNameEntry: CERT_ALT_NAME_ENTRY;
begin
  Result := '';
  if Assigned(ACertContext) then
  begin
    pExtension := CertFindExtension( szOID_SUBJECT_ALT_NAME2,
                                     ACertContext^.pCertInfo^.cExtension,
                                     PCERT_EXTENSION(ACertContext^.pCertInfo^.rgExtension));

    if Assigned(pExtension) then
    begin
      BufferSize := 0;
      if not CryptDecodeObject( X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                         szOID_SUBJECT_ALT_NAME2,
                         pExtension^.Value.pbData,
                         pExtension^.Value.cbData,
                         0,
                         Nil, BufferSize) then  // Pega Tamanho do Retorno
        raise EACBrDFeException.Create(
           'Falha ao obter BufferSize com "CryptDecodeObject" de "GetCNPJFromExtensions". Erro:'+GetLastErrorAsHexaStr);

      pNameInfo := AllocMem(BufferSize);
      try
        if not CryptDecodeObject( X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                           szOID_SUBJECT_ALT_NAME2,
                           pExtension^.Value.pbData,
                           pExtension^.Value.cbData,
                           0,
                           pNameInfo, BufferSize) then
          raise EACBrDFeException.Create(
             'Falha ao executar "CryptDecodeObject" em "GetCNPJFromExtensions". Erro:'+GetLastErrorAsHexaStr);

        I := 0;
        while (Result = '') and (I <= pNameInfo^.cAltEntry) do
        begin
          CertNameEntry := ArrCERT_ALT_NAME_ENTRY(pNameInfo^.rgAltEntry)[I];
          if (CertNameEntry.dwAltNameChoice = 1) then
            if CertNameEntry.pOtherName^.pszObjId = '2.16.76.1.3.3' then
              Result := copy(PAnsiChar(CertNameEntry.pOtherName^.Value.pbData), 3, 14);

          Inc(I);
        end;
      finally
        Freemem(pNameInfo);
      end;
    end;
  end;
end;

function CertToDERBase64(ACertContext: PCCERT_CONTEXT ): AnsiString;
var
  Buffer: AnsiString;
begin
  Result := '';
  if Assigned(ACertContext) then
  begin
    SetLength(Buffer, ACertContext^.cbCertEncoded);
    Move(ACertContext^.pbCertEncoded^, Buffer[1], ACertContext^.cbCertEncoded);
    Result := EncodeBase64(Buffer);
  end;
end;

procedure GetProviderInfo(ACertContext: PCCERT_CONTEXT;
   var ProviderType: DWORD; var ProviderName, ContainerName: String);
var
  dwKeySpec: DWORD;
  mCryptProviderCert: HCRYPTPROV;
  pfCallerFreeProv: LongBool;
begin
  // Obtendo o Contexto do Provedor de Criptografia do Certificado //
  dwKeySpec := AT_KEYEXCHANGE;
  if not CryptAcquireCertificatePrivateKey( ACertContext,
                                            0, Nil, mCryptProviderCert,
                                            dwKeySpec, pfCallerFreeProv) then
    raise EACBrDFeException.Create( MsgErroGetCryptProvider );

  try
    ProviderType  := GetProviderParamDWord( mCryptProviderCert, PP_PROVTYPE);
    ProviderName  := GetProviderParamString( mCryptProviderCert, PP_NAME);
    ContainerName := GetProviderParamString( mCryptProviderCert, PP_CONTAINER);
  finally
    if pfCallerFreeProv then
      CryptReleaseContext(mCryptProviderCert, 0);
  end;
end;

procedure PFXDataToCertContextWinApi(AData, APass: AnsiString; var AStore,
  ACertContext: Pointer);
var
  PFXBlob: CRYPT_DATA_BLOB;
  PFXCert: PCCERT_CONTEXT;
  wsPass: WideString;
begin
  PFXBlob.cbData := Length(AData);
  PFXBlob.pbData := PBYTE(AData);
  if not PFXIsPFXBlob(PFXBlob) then
    raise EACBrDFeException.Create('PFXDataToCertContextWinApi: DadosPFX informado não são válidos');

  wsPass := WideString( APass );
  if not PFXVerifyPassword(PFXBlob, LPCWSTR(wsPass), 0) then
    raise EACBrDFeException.Create('PFXDataToCertContextWinApi: Senha informada está errada');

  AStore := PFXImportCertStore( PFXBlob, LPCWSTR(wsPass),
                                CRYPT_EXPORTABLE or
                                {PKCS12_PREFER_CNG_KSP or}
                                PKCS12_INCLUDE_EXTENDED_PROPERTIES);
  if AStore = nil then
    raise EACBrDFeException.Create(
      'PFXDataToCertContextWinApi: Falha em "PFXImportCertStore" Erro: '+GetLastErrorAsHexaStr);

  // Varre cadeia de certificados lidos, e procura por Certificado do Cliente //
  ACertContext := Nil;
  PFXCert := Nil;
  PFXCert := CertEnumCertificatesInStore(AStore, PCCERT_CONTEXT(PFXCert)^);
  while (PFXCert <> Nil) and (ACertContext = Nil) do
  begin
    // Se Tem numero de série, então é o certificado do cliente
    if PFXCert^.pCertInfo^.SerialNumber.cbData >= 16 then
      ACertContext := PFXCert
    else
      PFXCert := CertEnumCertificatesInStore(AStore, PCCERT_CONTEXT(PFXCert)^);
  end;

  if (ACertContext = Nil) then
    raise EACBrDFeException.Create(
      'PFXDataToCertContextWinApi: Falha ao localizar o Certificado com a Chave Privada.');
end;

function ExportCertContextToPFXData(ACertContext: PCCERT_CONTEXT; APass: AnsiString
  ): AnsiString;
type
  ArrPCERT_CHAIN_ELEMENT = array of PCERT_CHAIN_ELEMENT;
var
  PFXBlob: CRYPT_DATA_BLOB;
  dwFlags, I: DWORD;
  AStore: HCERTSTORE;
  Dummy: PCCERT_CONTEXT;
  AFileTime: TFileTime;
  ChainPara: CERT_CHAIN_PARA;
  pChainContext: PCCERT_CHAIN_CONTEXT;

  procedure AddCertContexToStoreMemory(NewCertContext: PCCERT_CONTEXT);
  begin
    // Adicionando o Certificado Atual, na nova Store
    Dummy := Nil;
    if not CertAddCertificateContextToStore( AStore, NewCertContext,
                                             CERT_STORE_ADD_REPLACE_EXISTING,
                                             Dummy ) then
      raise EACBrDFeException.Create(
        'ExportCertStoreToPFXData: Falha Importanto Certificado na Store. Erro: '+GetLastErrorAsHexaStr);
    CertFreeCertificateContext( Dummy );
  end;

begin
  // Criando uma Store em memória, para ser exportada
  AStore := CertOpenStore( CERT_STORE_PROV_MEMORY, 0, 0, 0, Nil );
  if (AStore = Nil) then
    raise EACBrDFeException.Create(
      'ExportCertStoreToPFXData: Falha Criando Store. Erro: '+GetLastErrorAsHexaStr);

  try
    // Adicionando o Certificado Recebido, no Store temporário  //
    AddCertContexToStoreMemory( ACertContext );

    // Verificando se o Certificado Informado, pode ter sua Chave Privada, Exportada //
    PFXBlob.cbData := 0;
    PFXBlob.pbData := Nil;
    dwFlags := (EXPORT_PRIVATE_KEYS or
                REPORT_NOT_ABLE_TO_EXPORT_PRIVATE_KEY or
                PKCS12_INCLUDE_EXTENDED_PROPERTIES);

    if not PFXExportCertStoreEx( AStore, PFXBlob,
                                 LPCWSTR(WideString( APass )),
                                 Nil, dwFlags) then
      raise EACBrDFeExceptionNoPrivateKey.Create('Certificado não permite Exportar Chave Privada.');

    // Obtendo a cadeia de Certificados ///
    Dummy := Nil;
    pChainContext := Nil;
    ZeroMemory(@ChainPara, SizeOf(ChainPara));
    ChainPara.cbSize := sizeof( CERT_CHAIN_PARA );
    ChainPara.RequestedUsage.dwType                     := USAGE_MATCH_TYPE_AND;
    ChainPara.RequestedUsage.Usage.cUsageIdentifier     := 0;
    ChainPara.RequestedUsage.Usage.rgpszUsageIdentifier := Nil;

    if not CertGetCertificateChain(
            HCCE_CURRENT_USER,  // use the default chain engine
            ACertContext,       // pointer to the end certificate
            AFileTime,          // use the default time
            Nil,                // ACertContext^.hCertStore,
            ChainPara,          // use AND logic and enhanced key usage as indicated in the ChainPara data structure
            0,                  // No Flags
            Dummy,              // currently reserved
            pChainContext) then
      raise EACBrDFeException.Create(
        'ExportCertStoreToPFXData: Falha obtendo a cadeia de Certificados. Erro: '+GetLastErrorAsHexaStr);

    // Adicionando todos Certificados da Cadeia, no Store temporário //
    try
      for I := 0 to pChainContext^.rgpChain^.cElement - 1 do
        AddCertContexToStoreMemory( ArrPCERT_CHAIN_ELEMENT(pChainContext^.rgpChain^.rgpElement)[I]^.pCertContext );
    finally
      CertFreeCertificateChain( pChainContext );
    end;

    // Adicionando o Certificado Recebido, no Store temporário (na ordem correta)  //
    AddCertContexToStoreMemory( ACertContext );

    // Exportando a Store, com todos os certificados //
    Result := '';
    PFXBlob.cbData := 0;
    PFXBlob.pbData := Nil;
    dwFlags := (EXPORT_PRIVATE_KEYS or
                PKCS12_INCLUDE_EXTENDED_PROPERTIES);

    if not PFXExportCertStoreEx( AStore, PFXBlob,
                                 LPCWSTR(WideString( APass )),
                                 Nil, dwFlags) then
      raise EACBrDFeException.Create(
        'ExportCertStoreToPFXData: Falha em calcular tamanho do buffer. Erro: '+GetLastErrorAsHexaStr);

    PFXBlob.pbData := AllocMem(PFXBlob.cbData);  // Aloca a memória para receber o Blob
    try
      if not PFXExportCertStoreEx( AStore, PFXBlob,
                                   LPCWSTR(WideString( APass )),
                                   Nil, dwFlags) then
        raise EACBrDFeException.Create(
          'ExportCertStoreToPFXData: Falha em "PFXExportCertStoreEx" Erro: '+GetLastErrorAsHexaStr);

      SetLength(Result, PFXBlob.cbData);
      Move(PFXBlob.pbData^, Result[1], PFXBlob.cbData);
    finally
      Freemem(PFXBlob.pbData);
    end;
  finally
    CertCloseStore(AStore, CERT_CLOSE_STORE_FORCE_FLAG);
  end;
end;

procedure SetCertContextPassword(ACertContext: PCCERT_CONTEXT; APass: AnsiString);
var
  mCryptProviderCert: HCRYPTPROV;
  dwKeySpec: DWORD;
  pfCallerFreeProv: LongBool;
begin
  mCryptProviderCert := 0;
  dwKeySpec := AT_KEYEXCHANGE;
  pfCallerFreeProv := False;

  // Obtendo o Contexto do Provedor de Criptografia do Certificado //
  if not CryptAcquireCertificatePrivateKey( ACertContext, 0, Nil,
                                            mCryptProviderCert, dwKeySpec,
                                            pfCallerFreeProv) then
    raise EACBrDFeException.Create( MsgErroGetCryptProvider );

  try
    if not GetProviderIsHardware(mCryptProviderCert) then
      Exit;

    if not CryptSetProvParam(mCryptProviderCert, PP_KEYEXCHANGE_PIN, PBYTE(APass), 0) then
      raise EACBrDFeException.Create( MsgSetPINError );

    if not CryptSetProvParam(mCryptProviderCert, PP_SIGNATURE_PIN, PBYTE(APass), 0) then
      raise EACBrDFeException.Create( MsgSetPINError );
  finally
    if pfCallerFreeProv then
      CryptReleaseContext(mCryptProviderCert, 0);
  end;
end;

{ TDFeWinCrypt }

constructor TDFeWinCrypt.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);

  FpCertContext := Nil;
  FpStore := Nil;
  FpPFXData := '';

  Clear;
end;

destructor TDFeWinCrypt.Destroy;
begin
  DescarregarCertificado;

  inherited Destroy;
end;

procedure TDFeWinCrypt.OpenSystemStore;
var
  StoreFlag: DWORD;
  StoreProvider: LPCSTR;
begin
  if (FpStore <> Nil) then
    Exit;

  //TODO: Mapeaer demais tipos
  case FpDFeSSL.StoreLocation of
    slLocalMachine : StoreFlag := CERT_SYSTEM_STORE_LOCAL_MACHINE;
  else
    StoreFlag := CERT_SYSTEM_STORE_CURRENT_USER;
  end;

  StoreProvider := CERT_STORE_PROV_SYSTEM_A;
  {$IfNDef FPC}{$IfDef UNICODE}
   StoreProvider := CERT_STORE_PROV_SYSTEM_W;
  {$EndIf}{$EndIf};

  FpStore := CertOpenStore(
      StoreProvider, 0, 0,
      StoreFlag or CERT_STORE_READONLY_FLAG,
      LPCTSTR( FpDFeSSL.StoreName ) );
  //FpStore := CertOpenSystemStore( 0, LPCTSTR(FpDFeSSL.StoreName) );

  if (FpStore = Nil) then
    raise EACBrDFeException.Create(
      'TDFeWinCrypt. Erro ao abrir StoreName: ' + FpDFeSSL.StoreName +
      ' Location: ' + IntToStr(Integer(FpDFeSSL.StoreLocation)));

  //BufferSize := 0;
  //CertGetStoreProperty(FpStore, CERT_STORE_LOCALIZED_NAME_PROP_ID, nil, BufferSize);
  //SetLength(AStoreName, BufferSize);
  //CertGetStoreProperty(FpStore, CERT_STORE_LOCALIZED_NAME_PROP_ID, @AStoreName[1], BufferSize)
end;

function TDFeWinCrypt.GetCertContextWinApi: Pointer;
begin
  CarregarCertificadoSeNecessario;
  Result := FpCertContext;
end;

function TDFeWinCrypt.GetCertPFXData: AnsiString;
begin
  if FpPFXData = '' then
  begin
    CarregarCertificadoSeNecessario;
    if (FpDFeSSL.DadosPFX = '') then
      FpPFXData := ExportCertContextToPFXData(FpCertContext, FpDFeSSL.Senha)
    else
      FpPFXData := FpDFeSSL.DadosPFX;
  end;

  //DEBUG
  //WriteToFile('c:\temp\CertACBr.pfx', FpPFXData );
  Result := FpPFXData;
end;

procedure TDFeWinCrypt.CarregarCertificado;
var
  ACertContext: PCCERT_CONTEXT;
  PFXStream: TFileStream;

  procedure CarregarCertificadoDeDadosPFX;
  begin
    OpenSystemStore;
    PFXDataToCertContextWinApi( FpDFeSSL.DadosPFX,
                                FpDFeSSL.Senha,
                                FpStore,
                                Pointer(FpCertContext))
  end;

begin
  DescarregarCertificado;

  Clear;
  ACertContext := Nil;


  if (not EstaVazio(FpDFeSSL.DadosPFX)) then
      CarregarCertificadoDeDadosPFX

  else if not EstaVazio(FpDFeSSL.ArquivoPFX) then
  begin
    if not FileExists(FpDFeSSL.ArquivoPFX) then
      raise EACBrDFeException.Create('Arquivo: ' + FpDFeSSL.ArquivoPFX + ' não encontrado');

    PFXStream := TFileStream.Create(FpDFeSSL.ArquivoPFX, fmOpenRead or fmShareDenyNone);
    try
      FpDFeSSL.DadosPFX := ReadStrFromStream(PFXStream, PFXStream.Size);
    finally
      PFXStream.Free;
    end;

    CarregarCertificadoDeDadosPFX;
  end

  else if NaoEstaVazio(FpDFeSSL.NumeroSerie) then
  begin
    OpenSystemStore;
    ACertContext := CertEnumCertificatesInStore(FpStore, ACertContext^);
    while (ACertContext <> nil) and (FpCertContext = nil) do
    begin
      if GetSerialNumber(ACertContext) = FpDFeSSL.NumeroSerie then
        FpCertContext := ACertContext
      else
      begin
        ACertContext := CertEnumCertificatesInStore(FpStore, ACertContext^);
      end
    end;

    if (FpCertContext = Nil) then
      raise EACBrDFeException.Create('Certificado "'+FpDFeSSL.NumeroSerie+'" não encontrado!');
  end

  else
  begin
    raise EACBrDFeException.Create( 'DadosPFX, ArquivoPFX ou NumeroSerie não especificados !');
  end;

  // Não Achou ? //
  if (FpCertContext = Nil) then
    raise EACBrDFeException.Create('Certificado Digital não encontrado!');

  // Obtendo propriedades do Certificado //
  GetCertContextInfo( FpDadosCertificado, FpCertContext, True );
  FpCertificadoLido := True;

  // Se necessário atribui a Senha para o Certificado //
  if (FpDadosCertificado.Tipo = tpcA3) and (FpDFeSSL.Senha <> '') then
  begin
    try
      SetCertContextPassword(FpCertContext, FpDFeSSL.Senha);
    except
      FpDFeSSL.Senha := '';  // A senha está errada... vamos remove-la para não tentar novamente...
      raise;
    end;
  end;
end;

procedure TDFeWinCrypt.GetCertContextInfo(ADadosCertificado: TDadosCertificado;
  ACertContext: PCCERT_CONTEXT; CheckIsHardware: Boolean);
begin
  with ADadosCertificado do
  begin
    Clear;
    if CheckIsHardware then
    begin
      if GetCertIsHardware(ACertContext) then
        Tipo := tpcA3
      else
        Tipo := tpcA1;
    end;

    NumeroSerie := GetSerialNumber(ACertContext);
    SubjectName := GetSubjectName(ACertContext);
    if CNPJ = '' then
      CNPJ := GetCNPJFromExtensions(ACertContext);

    DataVenc   := GetNotAfter(ACertContext);
    IssuerName := GetIssuerName(ACertContext);
    DERBase64  := CertToDERBase64(ACertContext);
  end;
end;

function TDFeWinCrypt.SelecionarCertificado: String;
var
  ACertContext: PCCERT_CONTEXT;
begin
  Result := '';
  DescarregarCertificado;
  OpenSystemStore;

  ACertContext := CryptUIDlgSelectCertificateFromStore(
      FpStore,
      0,
      'Selecione um Certificado',
      'Selecione o Certificado que deseja utilizar:',
      CRYPTUI_SELECT_LOCATION_COLUMN or CRYPTUI_SELECT_ISSUEDBY_COLUMN or CRYPTUI_SELECT_INTENDEDUSE_COLUMN,
      0,
      Nil);

  if ACertContext <> Nil then
     Result := GetSerialNumber(ACertContext);

  DescarregarCertificado;

  if (Result <> '') then
  begin
    FpDFeSSL.NumeroSerie := Result;
    CarregarCertificado;
  end;
end;

procedure TDFeWinCrypt.LerCertificadosStore;
var
  ACertContext: PCCERT_CONTEXT;
  ADadosCertificado: TDadosCertificado;
  FecharStore: Boolean;
begin
  FpListaCertificados.Clear;
  FecharStore := (FpStore = nil);
  OpenSystemStore;

  try
    ACertContext := nil;
    ACertContext := CertEnumCertificatesInStore(FpStore, ACertContext^);
    while (ACertContext <> nil) do
    begin
      ADadosCertificado := FpListaCertificados.New;
      GetCertContextInfo(ADadosCertificado, ACertContext, False);
      ACertContext := CertEnumCertificatesInStore(FpStore, ACertContext^);
    end;
  finally
    if FecharStore and Assigned(FpStore) then
    begin
      CertCloseStore(FpStore, CERT_CLOSE_STORE_CHECK_FLAG);
      FpStore := Nil;
    end;
  end;
end;

procedure TDFeWinCrypt.DescarregarCertificado;
begin
  // Limpando objetos da MS CryptoAPI //
  if Assigned(FpCertContext) then
    CertFreeCertificateContext(FpCertContext);

  if Assigned(FpStore) then
    CertCloseStore(FpStore, CERT_CLOSE_STORE_FORCE_FLAG);

  FpCertContext := Nil;
  FpStore := Nil;
  FpPFXData := '';

  inherited DescarregarCertificado;
end;

function TDFeWinCrypt.CarregarCertificadoPublico(DadosX509Base64: Ansistring
  ): Boolean;
var
  BinaryX509: AnsiString;
  mCryptProvider: HCRYPTPROV;
begin
  DescarregarCertificado;

  BinaryX509 := DecodeBase64( DadosX509Base64 );

  OpenSystemStore;

  mCryptProvider := 0;
  if not CryptAcquireContext( mCryptProvider, Nil, Nil,
                              PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
    raise EACBrDFeException.Create('CryptAcquireContext: '+MsgErroGetCryptProvider);

  FpCertContext := CertCreateCertificateContext( X509_ASN_ENCODING,
                                                 PBYTE(BinaryX509),
                                                 Length(BinaryX509) );
  GetCertContextInfo(FpDadosCertificado, FpCertContext, False);
  Result := (FpDadosCertificado.SubjectName <> '');
end;

function TDFeWinCrypt.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assina: Boolean): AnsiString;
const
  CALG_SHA_256 = $0000800c;
  CALG_SHA_512 = $0000800e;
  PROV_RSA_AES = 24;
  //MS_ENH_RSA_AES_PROV = 'Microsoft Enhanced RSA and AES Cryptographic Provider';

var
  mCryptProvider, mCryptProviderCert: HCRYPTPROV;
  mHash, aHashType: HCRYPTHASH;
  hRSAKey, hSessKey, hExpKey: HCRYPTKEY;
  I: Integer;
  mTotal: Int64;
  mBytesLen, mRead, dwKeySpec: DWORD;
  Memory: Pointer;
  mHashBuffer: array [0..1023] of AnsiChar;  // 1024 - Tamanho máximo do maior Hash atual
  pfCallerFreeProv: LongBool;
begin
  Result := '';

  case Digest of
    dgstMD2    : aHashType := CALG_MD2;
    dgstMD4    : aHashType := CALG_MD4;
    dgstMD5    : aHashType := CALG_MD5;
    dgstSHA    : aHashType := CALG_SHA;
    dgstSHA1   : aHashType := CALG_SHA1;
    dgstSHA256 : aHashType := CALG_SHA_256;
    dgstSHA512 : aHashType := CALG_SHA_512;
  else
    raise EACBrDFeException.Create( 'Digest '+GetEnumName(TypeInfo(TSSLDgst),Integer(Digest))+
                                    ' não suportado em '+ClassName);
  end ;

  mCryptProvider := 0;
  mCryptProviderCert := 0;
  mHash := 0;
  hRSAKey := 0;
  hExpKey := 0;
  hSessKey := 0;
  pfCallerFreeProv := False;
  dwKeySpec := AT_KEYEXCHANGE;

  try
    try
      // Obtendo Contexto de Provedor de Criptografia, com suporte a SHA256 //
      if not CryptAcquireContext( mCryptProvider, Nil, Nil, //PAnsiChar(MS_ENH_RSA_AES_PROV),
                                 PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
        raise EACBrDFeException.Create('CryptAcquireContext: '+MsgErroGetCryptProvider);

      if Assina then
      begin
        CarregarCertificadoSeNecessario;

        if not Assigned(FpCertContext) then
          raise EACBrDFeException.Create('Certificado não pode ser carregado po MS CryptoAPI');

        // Obtendo o Contexto do Provedor de Criptografia do Certificado //
        if CryptAcquireCertificatePrivateKey( FpCertContext, 0, Nil,
                                              mCryptProviderCert,
                                              dwKeySpec,
                                              pfCallerFreeProv) then
        begin
          if GetProviderIsHardware( mCryptProviderCert ) then
          begin
            CryptReleaseContext(mCryptProvider, 0);
            mCryptProvider := mCryptProviderCert;
            pfCallerFreeProv := False;
          end
          else
          begin
            // Obtendo as chaves do Certificado //
            if CryptGetUserKey(mCryptProviderCert, dwKeySpec, hRSAKey) then
            begin
              // Tentando copiar a chave do Certificado para o nosso Provedor de Criptografia //
              try
                mBytesLen := 0;
                if CryptExportKey( hRSAKey, hSessKey, PRIVATEKEYBLOB, 0, Nil, mBytesLen ) then  // Calcula mBytesLen
                begin
                  Memory := AllocMem(mBytesLen);  // Aloca a memória para receber o Blob
                  try
                    if CryptExportKey( hRSAKey, hSessKey, PRIVATEKEYBLOB, 0, Memory, mBytesLen ) then
                    begin
                      if not CryptImportKey(mCryptProvider, Memory, mBytesLen, hSessKey, 0, hExpKey ) then
                        raise EACBrDFeException.Create('CryptImportKey');
                    end
                    else
                      raise EACBrDFeException.Create('CryptExportKey');
                  finally
                    Freemem(Memory);
                  end;
                end
                else
                  raise EACBrDFeException.Create('CryptExportKey - len');
              except
                { Não foi capaz de Exportar/Copiar a Chave para o nosso Provedor
                  de Criptografia, então vamos usar o Provedor de Criptografia do
                  Certificado }

                CryptReleaseContext(mCryptProvider, 0);
                mCryptProvider := mCryptProviderCert;
                pfCallerFreeProv := False;
              end;
            end
            else
              raise EACBrDFeException.Create('CryptGetUserKey');
          end
        end
        else
          raise EACBrDFeException.Create( MsgErroGetCryptProvider );
      end;

      if CryptCreateHash(mCryptProvider, aHashType, 0, 0, mHash) then
      begin
        Memory := Allocmem(CBufferSize);
        try
          mTotal := AStream.Size;
          AStream.Position := 0;
          repeat
            mRead := AStream.Read(Memory^, CBufferSize);
            if mRead > 0 then
            begin
              if not CryptHashData(mHash, Memory, mRead, 0) then
                raise EACBrDFeException.Create('CryptHashData');
            end;

            mTotal := mTotal - mRead;
          until mTotal < 1;
        finally
          FreeMem(Memory);
        end;

        mBytesLen := Length(mHashBuffer);

        if Assina then
        begin
          if CryptSignHash(mHash, dwKeySpec, Nil, 0, @mHashBuffer, mBytesLen ) then
          begin
            // MS CryptoAPI retorna assinatura em "Little Endian bit string", invertendo...
            Result := '';
            for I := mBytesLen downto 1 do
              Result := Result + mHashBuffer[I-1];
          end
          else
            raise EACBrDFeException.Create('CryptSignHash');
        end
        else
        begin
          // Obtendo o Hash //
          if CryptGetHashParam(mHash, HP_HASHVAL, @mHashBuffer, mBytesLen, 0) then
            SetString( Result, mHashBuffer, mBytesLen)
          else
            raise EACBrDFeException.Create('CryptGetHashParam');
        end;
      end
      else
        raise EACBrDFeException.Create('CryptCreateHash');

    except
      On E: Exception do
      begin
        raise EACBrDFeException.Create(E.Message+' , erro: $'+ GetLastErrorAsHexaStr);
      end;
    end;
  finally
    if mHash <> 0 then
      CryptDestroyHash(mHash);

    if pfCallerFreeProv then
      CryptReleaseContext(mCryptProviderCert, 0);

    if mCryptProvider <> 0 then
      CryptReleaseContext(mCryptProvider, 0);

    if hRSAKey <> 0 then
      CryptDestroyKey( hRSAKey );

    if hExpKey <> 0 then
      CryptDestroyKey( hExpKey );
  end;
end;

end.

