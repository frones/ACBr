{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit ACBrGNREConfiguracoes;

interface

uses
{$IFNDEF ACBrGNREOpenSSL}
  Windows, ACBrCAPICOM_TLB, JwaWinCrypt, JwaWinType, ACBrMSXML2_TLB,
{$ENDIF}
  Classes, Sysutils, pgnreConversao, pcnConversao, ActiveX;

{$IFNDEF ACBrGNREOpenSSL}
  const CAPICOM_STORE_NAME = 'My'; //My CA Root AddressBook
{$ENDIF}

type
 TCertificadosConf = class(TComponent)
  private
    FAssinaLote: Boolean;
    FAssinaGNRE: Boolean;
    FSenhaCert: AnsiString;
    {$IFNDEF ACBrGNREOpenSSL}
       FNumeroSerie: AnsiString;
       FDataVenc: TDateTime;
       procedure SetNumeroSerie(const Value: AnsiString);
       function GetNumeroSerie: AnsiString;
       function GetDataVenc: TDateTime;
    {$ELSE}
       FCertificado: AnsiString;
    {$ENDIF}
  public
    {$IFNDEF ACBrGNREOpenSSL}
       function SelecionarCertificado:AnsiString;
       function GetCertificado: ICertificate2;
    {$ENDIF}
  published
    {$IFNDEF ACBrGNREOpenSSL}
       property NumeroSerie: AnsiString read GetNumeroSerie write SetNumeroSerie;
       property DataVenc: TDateTime     read GetDataVenc;
    {$ELSE}
       property Certificado: AnsiString read FCertificado write FCertificado;
    {$ENDIF}
    property AssinaGNRE: Boolean  read FAssinaGNRE;
    property AssinaLote: Boolean read FAssinaLote;
    property Senha: AnsiString   read FSenhaCert write FSenhaCert;
  end;

 TWebServicesConf = Class(TComponent)
  private
    FSalvar: Boolean;
    FVisualizar : Boolean;
    FUF: String;
    FAmbiente: TpcnTipoAmbiente;
    FAmbienteCodigo: Integer;
    FProxyHost: String;
    FProxyPort: String;
    FProxyUser: String;
    FProxyPass: String;
    FAguardarConsultaRet : Cardinal;
    FTentativas : Integer;
    FIntervaloTentativas : Cardinal;
    FAjustaAguardaConsultaRet : Boolean;

    procedure SetAmbiente(AValue: TpcnTipoAmbiente);
    procedure SetTentativas(const Value: Integer);
    procedure SetIntervaloTentativas(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Salvar: Boolean               read FSalvar     write FSalvar     default False;
    property Visualizar: Boolean           read FVisualizar write FVisualizar default False;
    property UF: String                    read FUF         write FUF;    
    property Ambiente: TpcnTipoAmbiente    read FAmbiente   write SetAmbiente default taHomologacao;
    property AmbienteCodigo: Integer       read FAmbienteCodigo;
    property ProxyHost: String             read FProxyHost  write FProxyHost;
    property ProxyPort: String             read FProxyPort  write FProxyPort;
    property ProxyUser: String             read FProxyUser  write FProxyUser;
    property ProxyPass: String             read FProxyPass  write FProxyPass;
    property AguardarConsultaRet: Cardinal read FAguardarConsultaRet write FAguardarConsultaRet;
    property Tentativas : Integer read FTentativas write SetTentativas default 18;
    property IntervaloTentativas : Cardinal read FIntervaloTentativas write SetIntervaloTentativas;
    property AjustaAguardaConsultaRet : Boolean read FAjustaAguardaConsultaRet write FAjustaAguardaConsultaRet;
  end;

 TGeralConf = class(TComponent)
  private
    FSalvar: Boolean;
    FPathSalvar: String;
    FPathSchemas: String;
    function GetPathSalvar: String;
  public
    constructor Create(AOwner: TComponent); override;
    function Save(AXMLName: String; AXMLFile: WideString; aPath: String = ''): Boolean;
  published
    property Salvar: Boolean read FSalvar write FSalvar default False;
    property PathSalvar: String read GetPathSalvar write FPathSalvar;
    property PathSchemas: String read FPathSchemas write FPathSchemas;
  end;

 TArquivosConf = class(TComponent)
  private
    FSalvar   : Boolean;
    FMensal   : Boolean;
    FLiteral  : Boolean;
    FEmissaoPathGNRE  : Boolean;
    FPathGNRE : String;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPathGNRE(Data : TDateTime = 0): String;
  published
    property Salvar     : Boolean read FSalvar  write FSalvar  default False;
    property PastaMensal: Boolean read FMensal  write FMensal  default False;
    property AdicionarLiteral: Boolean read FLiteral write FLiteral default False;
    property EmissaoPathGNRE: Boolean read FEmissaoPathGNRE write FEmissaoPathGNRE default False;
    property PathGNRE : String read FPathGNRE  write FPathGNRE;
  end;

 TConfiguracoes = class(TComponent)
  private
    FGeral: TGeralConf;
    FWebServices: TWebServicesConf;
    FCertificados: TCertificadosConf;
    FArquivos: TArquivosConf;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Geral: TGeralConf read FGeral;
    property WebServices: TWebServicesConf read FWebServices;
    property Certificados: TCertificadosConf read FCertificados;
    property Arquivos: TArquivosConf read FArquivos;
  end;

implementation

uses
 IniFiles, DateUtils, Math, StrUtils, ACBrUtil, {ACBrGNRE,} ACBrGNREUtil,  ACBrDFeUtil;

{ TConfiguracoes }

constructor TConfiguracoes.Create(AOwner: TComponent);
begin
 inherited Create( AOwner );

 FGeral      := TGeralConf.Create(Self);
 FGeral.Name := 'GeralConf';

{$IFDEF COMPILER6_UP}
  FGeral.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FCertificados      := TCertificadosConf.Create(self);
 FCertificados.Name := 'CertificadosConf';

{$IFDEF COMPILER6_UP}
  FCertificados.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FWebServices      := TWebServicesConf.Create(self);
 FWebServices.Name := 'WebServicesConf';

{$IFDEF COMPILER6_UP}
  FWebServices.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}

 FArquivos      := TArquivosConf.Create(self);
 FArquivos.Name := 'ArquivosConf';

{$IFDEF COMPILER6_UP}
  FArquivos.SetSubComponent( true ); { para gravar no DFM/XFM }
{$ENDIF}
end;

destructor TConfiguracoes.Destroy;
begin
 FGeral.Free;
 FWebServices.Free;
 FCertificados.Free;
 FArquivos.Free;

 inherited;
end;

{ TGeralConf }

constructor TGeralConf.Create(AOwner: TComponent);
begin
 Inherited Create( AOwner );

 FSalvar      := False;
 FPathSalvar  := '';
 FPathSchemas := '';
end;

function TGeralConf.GetPathSalvar: String;
begin
 if EstaVazio(FPathSalvar)
  then Result := ApplicationPath
  else Result := FPathSalvar;

 Result := GNREUtil.PathWithDelim( Trim(Result) );
end;

function TGeralConf.Save(AXMLName: String; AXMLFile: WideString; aPath: String = ''): Boolean;
var
 vSalvar: TStrings;
begin
 Result := False;
 vSalvar := TStringList.Create;
 try
  try
   if NaoEstaVazio(ExtractFilePath(AXMLName))
    then begin
     aPath := ExtractFilePath(AXMLName);
     AXMLName := StringReplace(AXMLName,aPath,'',[rfIgnoreCase]);
    end
    else begin
     if EstaVazio(aPath)
      then aPath := PathSalvar
      else aPath := PathWithDelim(aPath);
    end;

   vSalvar.Text := AXMLFile;
   if not DirectoryExists( aPath )
    then ForceDirectories( aPath );

   vSalvar.SaveToFile( aPath + AXMLName);
   Result := True;
  except on E: Exception do
   raise Exception.Create('Erro ao salvar. '+E.Message);
  end;
 finally
  vSalvar.Free;
 end;
end;

{ TWebServicesConf }

constructor TWebServicesConf.Create(AOwner: TComponent);
begin
 Inherited Create( AOwner );

 FSalvar     := False;
 FVisualizar := False;
 FAmbiente   := taHomologacao;
 if FAmbiente=taProducao
  then FAmbienteCodigo := 1
  else FAmbienteCodigo := 2;
end;

procedure TWebServicesConf.SetAmbiente(AValue: TpcnTipoAmbiente);
begin
 FAmbiente := AValue;
 if AValue=taProducao
  then FAmbienteCodigo := 1
  else FAmbienteCodigo := 2;
end;

{ TCertificadosConf }

{$IFNDEF ACBrGNREOpenSSL}
function TCertificadosConf.GetCertificado: ICertificate2;
var
 Store          : IStore3;
 Certs          : ICertificates2;
 Cert           : ICertificate2;
 i              : Integer;
 xmldoc         : IXMLDOMDocument3;
 xmldsig        : IXMLDigitalSignature;
 dsigKey        : IXMLDSigKey;
 SigKey         : IXMLDSigKeyEx;
 PrivateKey     : IPrivateKey;
 hCryptProvider : HCRYPTPROV;
 XML            : String;
begin
 CoInitialize(nil); // PERMITE O USO DE THREAD
 try
 if EstaVazio( FNumeroSerie )
  then raise Exception.Create('Número de Série do Certificado Digital não especificado !');

 Result := nil;
 Store := CoStore.Create;
 Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);

 Certs := Store.Certificates as ICertificates2;
 for i:= 1 to Certs.Count do
  begin
   Cert := IInterface(Certs.Item[i]) as ICertificate2;
   if Cert.SerialNumber = FNumeroSerie
    then begin
     if EstaVazio(NumCertCarregado)
      then NumCertCarregado := Cert.SerialNumber;

     if CertStoreMem = nil
      then begin
       CertStoreMem := CoStore.Create;
       CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'Memoria', CAPICOM_STORE_OPEN_READ_ONLY);
       CertStoreMem.Add(Cert);
      end;

     PrivateKey := Cert.PrivateKey;

     if (FSenhaCert <> '') and PrivateKey.IsHardwareDevice
      then begin
       PrivateKey := Cert.PrivateKey;

       XML := XML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">'+
                      '<SignedInfo>'+
                       '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>'+
                       '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />'+
                       '<Reference URI="#">'+
                        '<Transforms>'+
                         '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />'+
                         '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />'+
                        '</Transforms>'+
                        '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />'+
                        '<DigestValue>'+
                        '</DigestValue>'+
                       '</Reference>'+
                      '</SignedInfo>'+
                      '<SignatureValue>'+
                      '</SignatureValue>'+
                      '<KeyInfo>'+
                      '</KeyInfo>'+
                     '</Signature>';

       xmldoc := CoDOMDocument50.Create;
       xmldoc.async              := False;
       xmldoc.validateOnParse    := False;
       xmldoc.preserveWhiteSpace := True;
       xmldoc.loadXML(XML);
       xmldoc.setProperty('SelectionNamespaces', DSIGNS);

       xmldsig := CoMXDigitalSignature50.Create;
       xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');
       xmldsig.store := CertStoreMem;

       dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
       if (dsigKey = nil)
        then raise Exception.Create('Erro ao criar a chave do CSP.');

       SigKey := dsigKey as IXMLDSigKeyEx;
       SigKey.getCSPHandle( hCryptProvider );

       try
        CryptSetProvParam( hCryptProvider , PP_SIGNATURE_PIN, LPBYTE(FSenhaCert), 0 );
       finally
        CryptReleaseContext(hCryptProvider, 0);
       end;

       SigKey  := nil;
       dsigKey := nil;
       xmldsig := nil;
       xmldoc  := nil;
      end;

     Result    := Cert;
     FDataVenc := Cert.ValidToDate;
     break;
    end;
  end;

 if not(Assigned(Result))
  then raise Exception.Create('Certificado Digital não encontrado!');
 finally
   CoUninitialize;
 end;
end;

function TCertificadosConf.GetNumeroSerie: AnsiString;
begin
 Result := Trim(UpperCase(StringReplace(FNumeroSerie,' ','',[rfReplaceAll] )));
end;

procedure TCertificadosConf.SetNumeroSerie(const Value: AnsiString);
begin
 FNumeroSerie := Trim(UpperCase(StringReplace(Value,' ','',[rfReplaceAll] )));
end;

function TCertificadosConf.SelecionarCertificado: AnsiString;
var
 Store  : IStore3;
 Certs  : ICertificates2;
 Certs2 : ICertificates2;
 Cert   : ICertificate2;
begin
 Store := CoStore.Create;
 Store.Open(CAPICOM_CURRENT_USER_STORE, CAPICOM_STORE_NAME, CAPICOM_STORE_OPEN_READ_ONLY);

 Certs  := Store.Certificates as ICertificates2;
 Certs2 := Certs.Select('Certificado(s) Digital(is) disponível(is)', 'Selecione o Certificado Digital para uso no aplicativo', false);

 if not(Certs2.Count = 0)
  then begin
   Cert         := IInterface(Certs2.Item[1]) as ICertificate2;
   FNumeroSerie := Cert.SerialNumber;
   FDataVenc    := Cert.ValidToDate;
  end;

 Result := FNumeroSerie;
end;

function TCertificadosConf.GetDataVenc: TDateTime;
begin
 if NaoEstaVazio(FNumeroSerie)
  then begin
   if FDataVenc = 0
    then GetCertificado;
   Result := FDataVenc;
  end
 else Result := 0;
end;
{$ENDIF}

procedure TWebServicesConf.SetIntervaloTentativas(const Value: Cardinal);
begin
 if (Value > 0) and (Value < 1000)
  then FIntervaloTentativas := 1000
  else FIntervaloTentativas := Value;
end;

procedure TWebServicesConf.SetTentativas(const Value: Integer);
begin
 if Value <= 0
  then FTentativas := 18
  else FTentativas := Value;
end;

{ TArquivosConf }

constructor TArquivosConf.Create(AOwner: TComponent);
begin
 inherited;

end;

function TArquivosConf.GetPathGNRE(Data: TDateTime): String;
var
 wDia, wMes, wAno : Word;
 Dir : String;
begin
 if EstaVazio(FPathGNRE)
  then Dir := TConfiguracoes( Self.Owner ).Geral.PathSalvar
  else Dir := FPathGNRE;

 if FMensal
  then begin
   if Data = 0
    then Data := Now;
   DecodeDate(Data, wAno, wMes, wDia);
   if Pos(IntToStr(wAno)+IntToStrZero(wMes,2),Dir) <= 0
    then Dir := PathWithDelim(Dir)+IntToStr(wAno)+IntToStrZero(wMes,2);
  end;

 if FLiteral
  then begin
   if copy(Dir,length(Dir)-2,3) <> 'GNRE'
    then Dir := PathWithDelim(Dir)+'GNRE';
  end;

 if not DirectoryExists(Dir)
  then ForceDirectories(Dir);

 Result := Dir;
end;

end.
