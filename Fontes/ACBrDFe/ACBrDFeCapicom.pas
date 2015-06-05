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
  JwaWinCrypt, Windows, ActiveX, ComObj;

const
  DSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
  CAPICOM_STORE_NAME = 'My'; //My CA Root AddressBook

type
  { TDFeCapicom }

  TDFeCapicom = class(TDFeSSLClass)
  private
    FNumCertCarregado: String;
    FCNPJ: String;
    FCertificado: ICertificate2;
    FCertStoreMem: IStore3;

    FReqResp: TACBrHTTPReqResp;

    procedure CarregarCertificadoSeNecessario;
    procedure Clear;

  protected
    procedure ConfiguraReqResp(const URL, SoapAction: String); virtual;
    procedure Executar(const ConteudoXML: String; Resp: TStream); virtual;

    function GetCertDataVenc: TDateTime; override;
    function GetCertNumeroSerie: String; override;
    function GetCertSubjectName: String; override;
    function GetCertCNPJ: String; override;
    function GetHTTPResultCode: Integer; override;
    function GetInternalErrorCode: Integer; override;

  public
    property Certificado: ICertificate2 read FCertificado;

    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function Assinar(const ConteudoXML, docElement, infElement: String): String;
      override;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String;
      out MsgErro: String): Boolean; override;

    procedure CarregarCertificado; override;
    function SelecionarCertificado: String; override;
    procedure DescarregarCertificado; override;
  end;

implementation

uses
  strutils,
  ACBrUtil, ACBrDFeException, ACBrDFeUtil, ACBrConsts, synautil;

{ TDFeCapicom }

constructor TDFeCapicom.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);

  FNumCertCarregado := '';
  FCNPJ := '';
  FCertificado := nil;
  FCertStoreMem := nil;

  FReqResp := TACBrHTTPReqResp.Create;
end;

destructor TDFeCapicom.Destroy;
begin
  FReqResp.Free;
  DesInicializar;

  inherited Destroy;
end;

procedure TDFeCapicom.Inicializar;
begin
  if FpInicializado then
    exit;

  CoInitialize(nil); // PERMITE O USO DE THREAD
  FpInicializado := True;
  Clear;
end;

procedure TDFeCapicom.DesInicializar;
begin
  if FpInicializado {and Configuracoes.Geral.UnloadSSLLib} then
  begin
    DescarregarCertificado;
    CoUninitialize;
    FpInicializado := False;
  end;
end;

function TDFeCapicom.SelecionarCertificado: String;
var
  Store: IStore3;
  Certs: ICertificates2;
  Certs2: ICertificates2;
  Cert: ICertificate2;
begin
  Store := CoStore.Create;
  Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME,
    CAPICOM_STORE_OPEN_READ_ONLY);

  Certs := Store.Certificates as ICertificates2;
  Certs2 := Certs.Select(ACBrStr('Certificado(s) Digital(is) disponível(is)'),
    ACBrStr('Selecione o Certificado Digital para uso no aplicativo'), False);

  if not (Certs2.Count = 0) then
  begin
    Cert := IInterface(Certs2.Item[1]) as ICertificate2;
    FpDFeSSL.NumeroSerie := Cert.SerialNumber;
    CarregarCertificado;
  end;

  Result := GetCertNumeroSerie;
end;

procedure TDFeCapicom.DescarregarCertificado;
begin
  Clear;
end;

procedure TDFeCapicom.CarregarCertificadoSeNecessario;
begin
  if FCertificado = nil then
    CarregarCertificado;
end;

procedure TDFeCapicom.Clear;
begin
  FCertificado := nil;
  FCertStoreMem := nil;
  FNumCertCarregado := '';
  FCNPJ := '';
end;

procedure TDFeCapicom.CarregarCertificado;
var
  Store: IStore3;
  Certs: ICertificates2;
  Cert: ICertificate2;
  Extension: IExtension;
  i, j, p: integer;

  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  SigKey: IXMLDSigKeyEx;
  PrivateKey: IPrivateKey;
  hCryptProvider: Cardinal;
  XML, Propriedades, Propriedade: String;
  Lista: TStringList;
begin
  // Verificando se possui parâmetros necessários //
  with FpDFeSSL do
  begin
    if EstaVazio(FpDFeSSL.NumeroSerie) then
    begin
      if not EstaVazio(ArquivoPFX) then
        raise EACBrDFeException.Create(ClassName +
          ' não suporta carga de Certificado pelo ArquivoPFX.' +
          sLineBreak + 'Utilize o "NumeroSerie"')
      else if not EstaVazio(DadosPFX) then
        raise EACBrDFeException.Create(ClassName +
          ' não suporta carga de Certificado por DadosPFX.' +
          sLineBreak + 'Utilize o "NumeroSerie"')
      else
        raise EACBrDFeException.Create(
          'Número de Série do Certificado Digital não especificado !');
    end;

    Inicializar;

    // Certificado já foi carregado ??
    if (FCertificado <> nil) and (FNumCertCarregado = FpDFeSSL.NumeroSerie) then
      exit;

    // Lendo lista de Certificados //
    Store := CoStore.Create;
    Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME,
      CAPICOM_STORE_OPEN_READ_ONLY);
    FCertificado := nil;
    Certs := Store.Certificates as ICertificates2;

    // Verificando se "FpDFeSSL.NumeroSerie" está na lista de certificados encontrados //;
    for i := 1 to Certs.Count do
    begin
      Cert := IInterface(Certs.Item[i]) as ICertificate2;
      if Cert.SerialNumber = FpDFeSSL.NumeroSerie then
      begin
        FCertificado := Cert;
        Break;
      end;
    end;

    // Não Achou ? //
    if FCertificado = nil then
      raise EACBrDFeException.Create('Certificado Digital não encontrado!');

    // Salvando propriedades do Certificado //
    FNumCertCarregado := FCertificado.SerialNumber;
    PrivateKey := FCertificado.PrivateKey;

    // Criando memória de Store de Certificados para o ACBr, e adicionado certificado lido nela //[
    FCertStoreMem := CoStore.Create;
    FCertStoreMem.Open(CAPICOM_MEMORY_STORE, 'MemoriaACBr',
      CAPICOM_STORE_OPEN_READ_ONLY);
    FCertStoreMem.Add(FCertificado);

    // Atribuindo Senha para memória, se o Certificado for A3 //
    if (Senha <> '') and PrivateKey.IsHardwareDevice then
    begin
      XML := SignatureElement('', False);

      xmldoc := CoDOMDocument50.Create;
      xmldoc.async := False;
      xmldoc.validateOnParse := False;
      xmldoc.preserveWhiteSpace := True;
      xmldoc.loadXML(XML);
      xmldoc.setProperty('SelectionNamespaces', DSIGNS);

      xmldsig := CoMXDigitalSignature50.Create;
      xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');
      xmldsig.store := FCertStoreMem;

      dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType,
        PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
      if (dsigKey = nil) then
        raise EACBrDFeException.Create('Erro ao criar a chave do CSP.');

      SigKey := dsigKey as IXMLDSigKeyEx;
      SigKey.getCSPHandle(hCryptProvider);
      try
        CryptSetProvParam(hCryptProvider, PP_SIGNATURE_PIN, Windows.PBYTE(Senha), 0);
      finally
        CryptReleaseContext(hCryptProvider, 0);
      end;

      SigKey := nil;
      dsigKey := nil;
      xmldsig := nil;
      xmldoc := nil;
    end;
  end;

  // Procurando pelo CNPJ nas propriedades do Certificado //
  for i := 1 to FCertificado.Extensions.Count do
  begin
    Extension := IInterface(FCertificado.Extensions.Item[i]) as IExtension;
    Propriedades := Extension.EncodedData.Format(True);

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
end;

function TDFeCapicom.GetCertDataVenc: TDateTime;
begin
  CarregarCertificadoSeNecessario;
  Result := FCertificado.ValidToDate;
end;

function TDFeCapicom.GetCertNumeroSerie: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FCertificado.SerialNumber;
end;

function TDFeCapicom.GetCertSubjectName: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FCertificado.SubjectName;
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

function TDFeCapicom.Assinar(const ConteudoXML, docElement, infElement: String): String;
var
  PosIni, PosFim: integer;
  URI, AXml, TagEndDocElement, XmlAss: String;
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  signedKey: IXMLDSigKey;
  PrivateKey: IPrivateKey;
begin
  AXml := ConteudoXML;
  XmlAss := '';

  if not XmlEstaAssinado(AXml) then
  begin
    URI := ExtraiURI(AXml);

    TagEndDocElement := '</' + docElement + '>';
    AXml := copy(AXml, 1, PosLast(TagEndDocElement, AXml) - 1);

    AXml := AXml + SignatureElement(URI, False) + TagEndDocElement;
  end;

  try
    // Criando XMLDOC //
    xmldoc := CoDOMDocument50.Create;
    xmldoc.async := False;
    xmldoc.validateOnParse := False;
    xmldoc.preserveWhiteSpace := True;

    // Carregando o AXml em XMLDOC //
    if (not xmldoc.loadXML(AXml)) then
      raise EACBrDFeException.Create('Não foi possível carregar o arquivo: ' + AXml);

    xmldoc.setProperty('SelectionNamespaces', DSIGNS);

    // Criando Elemento de assinatura //
    xmldsig := CoMXDigitalSignature50.Create;

    // Lendo elemento de Assinatura de XMLDOC //
    xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');
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

    XmlAss := xmldoc.xml;

    // Removendo quebras de linha //
    XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
    XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

    // Removendo espaços desnecessários, do Elemento da Assinatura //
    PosIni := Pos('<SignatureValue>', XmlAss) + length('<SignatureValue>');
    XmlAss := copy(XmlAss, 1, PosIni - 1) + StringReplace(
      copy(XmlAss, PosIni, length(XmlAss)), ' ', '', [rfReplaceAll]);

    // Considerando apenas o último Certificado //
    PosIni := Pos('<X509Certificate>', XmlAss) - 1;
    PosFim := PosLast('<X509Certificate>', XmlAss);
    XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));
  finally
    dsigKey := nil;
    signedKey := nil;
    xmldoc := nil;
    xmldsig := nil;
  end;

  Result := XmlAss;
end;

function TDFeCapicom.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String): String;
var
  Resp: TStringStream;
begin
  Result := '';

  ConfiguraReqResp(URL, SoapAction);

  Resp := TStringStream.Create('');
  try
    Executar(ConteudoXML, Resp);
    Result := String(Resp.DataString);
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
      raise EACBrDFeException.Create( Format( cACBrDFeSSLEnviarException,
                                        [InternalErrorCode, HTTPResultCode] ) +
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
begin
  DOMDocument := CoDOMDocument50.Create;
  Schema := CoXMLSchemaCache50.Create;
  try
    DOMDocument.async := False;
    DOMDocument.resolveExternals := False;
    DOMDocument.validateOnParse := True;
    if (not DOMDocument.loadXML(ConteudoXML)) then
    begin
      MsgErro := ACBrStr('Não foi possível carregar o arquivo.');
      exit;
    end;

    Schema.add(FpDFeSSL.NameSpaceURI, ArqSchema);

    DOMDocument.schemas := Schema;
    ParseError := DOMDocument.validate;

    Result := (ParseError.errorCode = 0);
    MsgErro := ParseError.reason;
  finally
    ParseError := nil;
    DOMDocument := nil;
    Schema := nil;
  end;
end;

function TDFeCapicom.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String): Boolean;
var
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  pKeyInfo: IXMLDOMNode;
  pKey, pKeyOut: IXMLDSigKey;
begin
  xmldoc := CoDOMDocument50.Create;
  xmldsig := CoMXDigitalSignature50.Create;
  try
    xmldoc.async := False;
    xmldoc.validateOnParse := False;
    xmldoc.preserveWhiteSpace := True;

    if (not xmldoc.loadXML(ConteudoXML)) then
    begin
      MsgErro := 'Não foi possível carregar o arquivo.';
      exit;
    end;

    xmldoc.setProperty('SelectionNamespaces', DSIGNS);
    xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');
    if (xmldsig.signature = nil) then
    begin
      MsgErro := 'Não foi possível carregar ou ler a assinatura.';
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

procedure TDFeCapicom.ConfiguraReqResp(const URL, SoapAction: String);
begin
  with FpDFeSSL do
  begin
    if ProxyHost <> '' then
    begin
      FReqResp.ProxyHost := ProxyHost;
      FReqResp.ProxyPort := ProxyPort;
      FReqResp.ProxyUser := ProxyUser;
      FReqResp.ProxyPass := ProxyPass;
    end;

    FReqResp.TimeOut := TimeOut;
  end;

  FReqResp.SetCertificate(FCertificado);
  FReqResp.Url := URL;
  FReqResp.SOAPAction := SoapAction;

  if (pos('SCERECEPCAORFB', UpperCase(URL)) <= 0) and
    (pos('SCECONSULTARFB', UpperCase(URL)) <= 0) then
    FReqResp.MimeType := 'application/soap+xml'
  else
    FReqResp.MimeType := 'text/xml';
end;

end.
