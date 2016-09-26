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

unit ACBrDFeCapicom;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL, ACBrHTTPReqResp,
  ACBrCAPICOM_TLB, ACBrMSXML2_TLB,
  JwaWinCrypt,
  Windows, ActiveX, ComObj;

const
  DSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
  CAPICOM_STORE_NAME = 'My'; //My CA Root AddressBook
  CAPICOM_SIGNATURE_NODE = './/ds:Signature';

type
  { TDFeCapicom }

  TDFeCapicom = class(TDFeSSLClass)
  private
    FStoreLocation: CAPICOM_STORE_LOCATION;
    FNumCertCarregado: String;
    FRazaoSocial: String;
    FCNPJ: String;
    FCertificadora: String;
    FCertificado: ICertificate2;
    FCertStoreMem: IStore3;

    FpStore: HCERTSTORE;
    FpCertContext: PCCERT_CONTEXT;

    FReqResp: TACBrHTTPReqResp;

    procedure AtribuirSenhaA3;
    procedure CarregarCertificadoSeNecessario;
    procedure Clear;
    procedure VerificarValoresPadrao(var SignatureNode: String;
      var SelectionNamespaces: String);

  protected
    FMimeType: String;

    procedure ConfiguraReqResp(const URL, SoapAction: String); virtual;
    procedure Executar(const ConteudoXML: String; Resp: TStream); virtual;

    function GetCertDataVenc: TDateTime; override;
    function GetCertNumeroSerie: String; override;
    function GetCertIssuerName: String; override;
    function GetCertCertificadora: String; override;
    function GetCertSubjectName: String; override;
    function GetCertRazaoSocial: String; override;
    function GetCertCNPJ: String; override;
    function GetHTTPResultCode: Integer; override;
    function GetInternalErrorCode: Integer; override;
    function GetCertTipo: TSSLTipoCertificado; override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''): String; override;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String; const MimeType: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''): Boolean; override;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assinar: Boolean =  False): AnsiString; override;

    procedure CarregarCertificado; override;
    function SelecionarCertificado: String; override;
    procedure DescarregarCertificado; override;

    property Certificado: ICertificate2 read FCertificado;
    property StoreLocation: CAPICOM_STORE_LOCATION read FStoreLocation write FStoreLocation;
  end;

Var CertificadosA3ComPin: String;

implementation

uses
  strutils, typinfo,
  ACBrUtil, ACBrDFeException, ACBrDFeUtil, ACBrConsts,
  synautil;

{ TDFeCapicom }

constructor TDFeCapicom.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);
  FNumCertCarregado := '';
  FRazaoSocial := '';
  FCertificadora := '';
  FCNPJ := '';
  FCertificado := nil;
  FCertStoreMem := nil;
  FpCertContext := nil;
  FpStore := nil;
  FStoreLocation := CAPICOM_CURRENT_USER_STORE;
  FReqResp := TACBrHTTPReqResp.Create;
  FMimeType := '';
end;

destructor TDFeCapicom.Destroy;
begin
  FReqResp.Free;
  DescarregarCertificado;

  inherited Destroy;
end;

function TDFeCapicom.SelecionarCertificado: String;
var
  Store: IStore3;
  Certs: ICertificates2;
  Certs2: ICertificates2;
  Cert: ICertificate2;
begin
  Store := CoStore.Create;
  Store.Open(FStoreLocation, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);

  Certs := Store.Certificates as ICertificates2;
  Certs2 := Certs.Select('Certificado(s) Digital(is) disponível(is)',
    'Selecione o Certificado Digital para uso no aplicativo', False);

  if (Certs2.Count > 0) then
  begin
    Cert := IInterface(Certs2.Item[1]) as ICertificate2;
    FpDFeSSL.NumeroSerie := String(Cert.SerialNumber);
    CarregarCertificado;
    Result := GetCertNumeroSerie;
  end
  else
    Result := '';
end;

procedure TDFeCapicom.DescarregarCertificado;
begin
  Clear;
  FpCertificadoLido := False;
end;

procedure TDFeCapicom.CarregarCertificadoSeNecessario;
begin
  if FNumCertCarregado = '' then
    CarregarCertificado;
end;

procedure TDFeCapicom.AtribuirSenhaA3;
var
  PrivKey: IPrivateKey;
  XML: String;
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  SigKey: IXMLDSigKeyEx;
  hCryptProvider: Cardinal;
begin
  if (FpDFeSSL.Senha = '') or
     (not Assigned(FCertificado)) or
     (not Assigned(FCertStoreMem)) then
    exit ;

  // Se Atribuir novamente em outra instância causa conflito... //
  if (pos(FNumCertCarregado, CertificadosA3ComPin) > 0) then
    exit ;

  PrivKey := FCertificado.PrivateKey;

  // Atribuindo Senha para memória, apenas se o Certificado for A3 //
  if not PrivKey.IsHardwareDevice then
    exit;

  //CriarMutexParaCertificado

  // Criando objeto IXMLDSigKeyEx, para obter contexto do Certificado //
  try
    XML := SignatureElement('', False);

    xmldoc := CoDOMDocument50.Create;
    xmldoc.async := False;
    xmldoc.validateOnParse := False;
    xmldoc.preserveWhiteSpace := True;
    xmldoc.loadXML(WideString(XML));
    xmldoc.setProperty('SelectionNamespaces', DSIGNS);

    xmldsig := CoMXDigitalSignature50.Create;
    xmldsig.signature := xmldoc.selectSingleNode(CAPICOM_SIGNATURE_NODE);
    xmldsig.store := FCertStoreMem;

    try
      dsigKey := xmldsig.createKeyFromCSP(PrivKey.ProviderType,
        PrivKey.ProviderName, PrivKey.ContainerName, 0);

      if (dsigKey = nil) then
        raise Exception.Create('');
    except
      On E: Exception do
      begin
        raise EACBrDFeException.Create('Erro ao ler Chave do certificado: '+
                                       FNumCertCarregado+sLineBreak+ E.Message)
      end;
    end;

    SigKey := dsigKey as IXMLDSigKeyEx;
    SigKey.getCSPHandle(hCryptProvider);
    try
      CryptSetProvParam(hCryptProvider, PP_SIGNATURE_PIN, Windows.PBYTE(FpDFeSSL.Senha), 0);
    finally
      CryptReleaseContext(hCryptProvider, 0);
    end;

    CertificadosA3ComPin := CertificadosA3ComPin + FNumCertCarregado + ',';
  finally
    SigKey := nil;
    dsigKey := nil;
    xmldsig := nil;
    xmldoc := nil;
  end;
end;

procedure TDFeCapicom.Clear;
begin
  FCertificado := nil;
  if Assigned(FCertStoreMem) then
    FCertStoreMem.Close;

  FCertStoreMem := Nil;
  FNumCertCarregado := '';
  FCNPJ := '';
  FRazaoSocial := '';
  FCertificadora := '';

  // Limpando objetos da MS CryptoAPI //
  if Assigned(FpCertContext) then
    CertFreeCertificateContext(FpCertContext);

  if Assigned(FpStore) then
    CertCloseStore(FpStore, CERT_CLOSE_STORE_CHECK_FLAG);

  FpCertContext := Nil;
  FpStore := Nil;
end;

procedure TDFeCapicom.VerificarValoresPadrao(var SignatureNode: String;
  var SelectionNamespaces: String);
begin
  if SignatureNode = '' then
    SignatureNode := CAPICOM_SIGNATURE_NODE;

  if SelectionNamespaces = '' then
    SelectionNamespaces := DSIGNS
  else
  begin
    if LeftStr(SelectionNamespaces, Length(DSIGNS)) <> DSIGNS then
      SelectionNamespaces := DSIGNS + ' ' + SelectionNamespaces;
  end;
end;

procedure TDFeCapicom.CarregarCertificado;
var
  Store: IStore3;
  Certs: ICertificates2;
  Cert: ICertificate2;
  Extension: IExtension;
  i, j, p: integer;
  Propriedades, Propriedade: String;
  Lista: TStringList;
  KeyLocation: Integer;
  Inicializado: Boolean;
  ByteArr: array of byte;

begin
  // Certificado já foi carregado ??
  if ((FCertificado <> nil) and (FNumCertCarregado = FpDFeSSL.NumeroSerie)) then
  begin
    FpCertificadoLido := True;
    exit;
  end;

  Inicializado := (CoInitialize(nil) in [ S_OK, S_FALSE ]);
  try
    if NaoEstaVazio(FpDFeSSL.NumeroSerie) then
    begin
      // Lendo lista de Certificados //
      Store := CoStore.Create;
      try
        Store.Open(FStoreLocation, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);
        FCertificado := nil;
        Certs := Store.Certificates as ICertificates2;

        // Verificando se "FpDFeSSL.NumeroSerie" está na lista de certificados encontrados //;
        for i := 1 to Certs.Count do
        begin
          Cert := IInterface(Certs.Item[i]) as ICertificate2;
          if String(Cert.SerialNumber) = FpDFeSSL.NumeroSerie then
          begin
            FCertificado := Cert;
            Break;
          end;
        end;
      finally
        Store.Close;
      end;
    end

    else if not EstaVazio(FpDFeSSL.ArquivoPFX) then
    begin
      FCertificado := CoCertificate.Create;

      KeyLocation := CAPICOM_CURRENT_USER_KEY;
      if FStoreLocation = CAPICOM_LOCAL_MACHINE_STORE then
        KeyLocation := CAPICOM_LOCAL_MACHINE_KEY;

      FCertificado.Load( WideString(FpDFeSSL.ArquivoPFX), WideString(FpDFeSSL.Senha),
                         CAPICOM_KEY_STORAGE_DEFAULT, KeyLocation);

    end

    else if not EstaVazio(FpDFeSSL.DadosPFX) then
    begin
      raise EACBrDFeException.Create(ClassName +
        ' não suporta carga de Certificado por DadosPFX.' +
        sLineBreak + 'Utilize "NumeroSerie" ou "ArquivoPFX"')
    end

    else
    begin
      raise EACBrDFeException.Create(
      'Número de Série do Certificado Digital não especificado !');
    end;


    // Não Achou ? //
    if FCertificado = nil then
      raise EACBrDFeException.Create('Certificado Digital não encontrado!');

    // Salvando propriedades do Certificado //
    FNumCertCarregado := String(FCertificado.SerialNumber);

    // Criando memória de Store de Certificados para o ACBr, e adicionado certificado lido nela //
    FCertStoreMem := CoStore.Create;
    FCertStoreMem.Open(CAPICOM_MEMORY_STORE, 'MemoriaACBr', CAPICOM_STORE_OPEN_READ_ONLY);
    FCertStoreMem.Add(FCertificado);

    // Se necessário atribui a Senha para o FCertStoreMem //
    AtribuirSenhaA3;

    // Procurando pelo CNPJ nas propriedades do Certificado //
    for i := 1 to FCertificado.Extensions.Count do
    begin
      Extension := IInterface(FCertificado.Extensions.Item[i]) as IExtension;
      Propriedades := String(Extension.EncodedData.Format(True));

      if (Pos('2.16.76.1.3.3', Propriedades) > 0) then
      begin
        Lista := TStringList.Create;
        try
          Lista.Text := Propriedades;
          for j := 0 to Lista.Count - 1 do
          begin
            Propriedade := Lista.Strings[j];
            if (Pos('2.16.76.1.3.3', Propriedade) > 0) then
            begin
              p := Pos('=', Propriedade);
              FCNPJ := copy(Propriedade, p + 1, Length(Propriedade));
              FCNPJ := OnlyNumber(HexToAscii(RemoveString(' ', FCNPJ)));
              break;
            end;
          end;
        finally
          Lista.Free;
        end;
        break;
      end;
      Extension := nil;
    end;

    // Abrindo o Certificado, com MS CriptoAPI //

    { TODO: verificar porque  "CertOpenStore" não funciona...
            Preferir  usar "CertOpenStore" ao invez de "CertOpenSystemStore",
            pois permite definir o Local do Store }

    //FpStore := CertOpenStore(
    //     LPCSTR(sz_CERT_STORE_PROV_MEMORY),
    //     0,
    //     0,
    //     CERT_SYSTEM_STORE_CURRENT_USER,
    //     PAnsiChar(StoreName) );

    // Abrindo o Store do sistema //
    FpStore := CertOpenSystemStore( 0, LPCTSTR(CAPICOM_STORE_NAME) );

    if Assigned(FpStore) then
    begin
      { Abrindo Contexto do Certificado, para uso com MS CryptoAPI
        Vamos ler todos os Certificados e comparar o Num.Série. Esse modo funciona
        melhor que CertFindCertificateInStore();}
      repeat
        FpCertContext := CertEnumCertificatesInStore(FpStore, FpCertContext);

        if FpCertContext <> Nil then
        begin
          // Montando o número de série, em Hexa, para comparação
          Propriedade := '';
          SetLength(ByteArr, FpCertContext^.pCertInfo^.SerialNumber.cbData);
           Move(FpCertContext^.pCertInfo^.SerialNumber.pbData^,
                ByteArr[0],
                FpCertContext^.pCertInfo^.SerialNumber.cbData);

          For I := 0 to FpCertContext^.pCertInfo^.SerialNumber.cbData-1 do
            Propriedade := IntToHex(ByteArr[I], 2) + Propriedade;

          if Propriedade = FNumCertCarregado then
            break;
        end;
      until (FpCertContext = nil);  // não achou mais nenhum certificado
    end;

  finally
    if Inicializado then
      CoUninitialize;
  end;

  FpCertificadoLido := True;
end;

function TDFeCapicom.GetCertDataVenc: TDateTime;
begin
  CarregarCertificadoSeNecessario;
  if Assigned(FCertificado) then
    Result := FCertificado.ValidToDate
  else
    Result := inherited GetCertDataVenc;
end;

function TDFeCapicom.GetCertNumeroSerie: String;
begin
  CarregarCertificadoSeNecessario;
  if Assigned(FCertificado) then
    Result := String(FCertificado.SerialNumber)
  else
    Result := inherited GetCertNumeroSerie;
end;

function TDFeCapicom.GetCertSubjectName: String;
begin
  CarregarCertificadoSeNecessario;
  if Assigned(FCertificado) then
    Result := String(FCertificado.SubjectName)
  else
    Result := inherited GetCertSubjectName;
end;

function TDFeCapicom.GetCertRazaoSocial: String;
begin
  CarregarCertificadoSeNecessario;
  if (FRazaoSocial = '') and Assigned(FCertificado) then
    FRazaoSocial := GetRazaoSocialFromSubjectName( String(FCertificado.SubjectName) );

  Result := FRazaoSocial;
end;

function TDFeCapicom.GetCertIssuerName: String;
begin
  CarregarCertificadoSeNecessario;
  if Assigned(FCertificado) then
    Result := String(FCertificado.IssuerName)
  else
    Result := inherited GetCertIssuerName;
end;

function TDFeCapicom.GetCertCertificadora: String;
begin
  CarregarCertificadoSeNecessario;
  if (FCertificadora = '') and Assigned(FCertificado) then
    FCertificadora := GetCertificadoraFromSubjectName( String(FCertificado.IssuerName) );

  Result := FCertificadora;
end;

function TDFeCapicom.GetCertTipo: TSSLTipoCertificado;
begin
  if Assigned(FCertificado) and FCertificado.PrivateKey.IsHardwareDevice then
    Result := tpcA3
  else
    Result := tpcA1;
end;

function TDFeCapicom.GetCertCNPJ: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FCNPJ;
end;

function TDFeCapicom.GetHTTPResultCode: Integer;
begin
  Result := FReqResp.HTTPResultCode;
end;

function TDFeCapicom.GetInternalErrorCode: Integer;
begin
  Result := FReqResp.InternalErrorCode;
end;

function TDFeCapicom.Assinar(const ConteudoXML, docElement, infElement: String;
  SignatureNode: String; SelectionNamespaces: String; IdSignature: String
  ): String;
var
  AXml, XmlAss: AnsiString;
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  signedKey: IXMLDSigKey;
  PrivateKey: IPrivateKey;
  Inicializado: Boolean;
begin
  Inicializado := (CoInitialize(nil) in [ S_OK, S_FALSE ]);
  try
    CarregarCertificadoSeNecessario;

    // IXMLDOMDocument3 deve usar a String Nativa da IDE //
    {$IfDef FPC2}
     AXml := ACBrUTF8ToAnsi(ConteudoXML);
    {$Else}
     AXml := UTF8ToNativeString(ConteudoXML);
    {$EndIf}
    XmlAss := '';

    // Usa valores default, se não foram informados //
    VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

    // Inserindo Template da Assinatura digital //
    if (not XmlEstaAssinado(AXml)) or (SignatureNode <> CAPICOM_SIGNATURE_NODE) then
      AXml := AdicionarSignatureElement(AXml, False, docElement, IdSignature);

    try
      // Criando XMLDOC //
      xmldoc := CoDOMDocument50.Create;
      xmldoc.async := False;
      xmldoc.validateOnParse := False;
      xmldoc.preserveWhiteSpace := True;

      // Carregando o AXml em XMLDOC
      if (not xmldoc.loadXML( WideString(AXml) )) then
        raise EACBrDFeException.Create('Não foi possível carregar XML'+sLineBreak+ AXml);

      xmldoc.setProperty('SelectionNamespaces', SelectionNamespaces);

      //DEBUG
      //xmldoc.save('c:\temp\xmldoc.xml');

      // Criando Elemento de assinatura //
      xmldsig := CoMXDigitalSignature50.Create;

      // Lendo elemento de Assinatura de XMLDOC //
      xmldsig.signature := xmldoc.selectSingleNode( WideString(SignatureNode) );
      if (xmldsig.signature = nil) then
        raise EACBrDFeException.Create('É preciso carregar o template antes de assinar.');

      // Lendo Chave Privada do Certificado //
      OleCheck(IDispatch(FCertificado.PrivateKey).QueryInterface(IPrivateKey, PrivateKey));
      xmldsig.store := FCertStoreMem;
      dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType,
        PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
      if (dsigKey = nil) then
        raise EACBrDFeException.Create('Erro ao criar a chave do CSP.');

      // Assinando com MSXML e CryptoLib //
      signedKey := xmldsig.sign(dsigKey, $00000002);
      if (signedKey = nil) then
        raise EACBrDFeException.Create('Assinatura Falhou.');

      //DEBUG
      //xmldoc.save('c:\temp\ass.xml');
      XmlAss := AnsiString(xmldoc.xml);

      // Convertendo novamente para UTF8
      {$IfDef FPC2}
       XmlAss := ACBrAnsiToUTF8( XmlAss );
      {$Else}
       XmlAss := NativeStringToUTF8( String(XmlAss) );
      {$EndIf}

      // Ajustando o XML... CAPICOM insere um cabeçalho inválido
      XmlAss := AjustarXMLAssinado(XmlAss);
    finally
      dsigKey := nil;
      signedKey := nil;
      xmldoc := nil;
      xmldsig := nil;
    end;

    Result := XmlAss;
  finally
    if Inicializado then
      CoUninitialize;
  end;
end;

function TDFeCapicom.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String; const MimeType: String): String;
var
  Resp: TMemoryStream;
begin
  Result := '';

  if MimeType = '' then
    FMimeType := 'application/soap+xml'
  else
    FMimeType := MimeType;

  ConfiguraReqResp(URL, SoapAction);

  Resp := TMemoryStream.Create;
  try
    Executar(ConteudoXML, Resp);
    Resp.Position := 0;
    Result := ReadStrFromStream(Resp, Resp.Size);
    // DEBUG //
    //Resp.SaveToFile('c:\temp\ReqResp.xml');
  finally
    Resp.Free;
  end;
end;

procedure TDFeCapicom.Executar(const ConteudoXML: String; Resp: TStream);
begin
  try
    // Enviando, dispara exceptions no caso de erro //
    FReqResp.Execute(ConteudoXML, Resp);
  except
    On E: Exception do
    begin
      raise EACBrDFeException.CreateDef( Format( cACBrDFeSSLEnviarException,
                                         [InternalErrorCode, HTTPResultCode] ) + sLineBreak +
                                         E.Message ) ;
    end;

  end;
end;


function TDFeCapicom.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache;
  Inicializado: Boolean;
  AXml: String;
begin
  Result := False;
  Inicializado := (CoInitialize(nil) in [ S_OK, S_FALSE ]);
  try
    CarregarCertificadoSeNecessario;

    DOMDocument := CoDOMDocument50.Create;
    Schema := CoXMLSchemaCache50.Create;
    try
      DOMDocument.async := False;
      DOMDocument.resolveExternals := False;
      DOMDocument.validateOnParse := True;

      // Carregando ConteudoXML em XMLDOC. Nota: IXMLDOMDocument2 deve usar a String Nativa da IDE //
      {$IfDef FPC2}
       AXml := ACBrUTF8ToAnsi(ConteudoXML);
      {$Else}
       AXml := UTF8ToNativeString(ConteudoXML);
      {$EndIf}

      if (not DOMDocument.loadXML(WideString(AXml))) then
      begin
        ParseError := DOMDocument.parseError;
        MsgErro := ACBrStr('Não foi possível carregar o arquivo.')+sLineBreak+
                   'Err: '+IntToStr(ParseError.errorCode) + ', ' +
                   'Lin: '+IntToStr(ParseError.line) + ', ' +
                   'Pos: '+IntToStr(ParseError.linepos) + ' - ' +
                   String(ParseError.reason);
        exit;
      end;

      Schema.add(WideString(FpDFeSSL.NameSpaceURI), ArqSchema);

      DOMDocument.schemas := Schema;
      ParseError := DOMDocument.validate;

      Result := (ParseError.errorCode = 0);
      MsgErro := String(ParseError.reason);
    finally
      ParseError := nil;
      DOMDocument := nil;
      Schema := nil;
    end;
  finally
    if Inicializado then
      CoUninitialize;
  end;
end;

function TDFeCapicom.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String): Boolean;
var
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  pKeyInfo: IXMLDOMNode;
  pKey, pKeyOut: IXMLDSigKey;
  AXml: String;
begin
  // Usa valores default, se não foram informados //
  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);
  Result := False;
  xmldoc := CoDOMDocument50.Create;
  xmldsig := CoMXDigitalSignature50.Create;
  try
    xmldoc.async := False;
    xmldoc.validateOnParse := False;
    xmldoc.preserveWhiteSpace := True;

    // Carregando ConteudoXML em XMLDOC. Nota: IXMLDOMDocument2 deve usar a String Nativa da IDE //
    {$IfDef FPC2}
     AXml := ACBrUTF8ToAnsi(ConteudoXML);
    {$Else}
     AXml := UTF8ToNativeString(ConteudoXML);
    {$EndIf}

    if (not xmldoc.loadXML(WideString(AXml))) then
    begin
      MsgErro := 'Não foi possível carregar o arquivo.';
      exit;
    end;

    xmldoc.setProperty('SelectionNamespaces', SelectionNamespaces);
    xmldsig.signature := xmldoc.selectSingleNode( WideString(SignatureNode) );
    if (xmldsig.signature = nil) then
    begin
      MsgErro := 'Não foi possível carregar ou ler a assinatura ('+SignatureNode+')';
      exit;
    end;

    pKeyInfo := xmldoc.selectSingleNode('.//ds:KeyInfo/ds:X509Data');
    if (pKeyInfo = nil) then
    begin
      MsgErro := 'Erro ao ler KeyInfo.';
      exit;
    end;

    pKey := xmldsig.createKeyFromNode(pKeyInfo);
    if (pKey = nil) then
    begin
      MsgErro := 'Erro criar a Chave de KeyInfo.';
      exit;
    end;

    try
      pKeyOut := xmldsig.verify(pKey);
    except
      on E: Exception do
        MsgErro := 'Erro ao verificar assinatura do arquivo: ' + E.Message;
    end;
  finally
    MsgErro := ACBrStr(MsgErro);
    Result := (pKeyOut <> nil);

    pKeyOut := nil;
    pKey := nil;
    pKeyInfo := nil;
    xmldsig := nil;
    xmldoc := nil;
  end;
end;

function TDFeCapicom.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assinar: Boolean): AnsiString;
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
  mTotal: LargeInt;
  mBytesLen, mRead, dwKeySpec: DWORD;
  Memory: Pointer;
  mHashBuffer: array [0..1023] of AnsiChar;  // 1024 - Tamanho máximo do maior Hash atual
  pfCallerFreeProv: Boolean;
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
        raise EACBrDFeException.Create('CryptAcquireContext');


      if Assinar then
      begin
        CarregarCertificadoSeNecessario;

        if not Assigned(FpCertContext) then
          raise EACBrDFeException.Create('Certificado não pode ser carregado po MS CryptoAPI');

        // Obtendo o Contexto do Provedor de Criptografia do Certificado //
        if CryptAcquireCertificatePrivateKey( FpCertContext, 0, Nil, mCryptProviderCert, @dwKeySpec, @pfCallerFreeProv) then
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
        else
          raise EACBrDFeException.Create('CryptAcquireCertificatePrivateKey');
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

        if Assinar then
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
        raise EACBrDFeException.Create(E.Message+' , erro: $'+IntToHex(GetLastError, 8) );
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

procedure TDFeCapicom.ConfiguraReqResp(const URL, SoapAction: String);
begin
  with FpDFeSSL do
  begin
    if UseCertificateHTTP then
    begin
      CarregarCertificadoSeNecessario;
      FReqResp.SetCertificate(FCertificado);
    end
    else
      FReqResp.SetCertificate(nil);

    FReqResp.ProxyHost := ProxyHost;
    FReqResp.ProxyPort := ProxyPort;
    FReqResp.ProxyUser := ProxyUser;
    FReqResp.ProxyPass := ProxyPass;
    FReqResp.TimeOut   := TimeOut;
  end;

  FReqResp.Url := URL;
  FReqResp.SOAPAction := SoapAction;
  FReqResp.MimeType := FMimeType;
end;

initialization
  CertificadosA3ComPin := '';

finalization
  CertificadosA3ComPin := '';

end.

