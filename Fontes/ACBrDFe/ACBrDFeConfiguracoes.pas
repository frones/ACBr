{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
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

unit ACBrDFeConfiguracoes;

interface

uses
  Classes, SysUtils, types, IniFiles,
  pcnConversao, pcnAuxiliar,
  blcksock,
  ACBrDFeSSL;

const
  CDFeSessaoIni = 'DFe';

type

  TTagOrdenacaoPath = (opNenhum, opCNPJ, opModelo, opData, opLiteral);

  TConfiguracoes = class;

  { TCertificadosConf }

  TCertificadosConf = class(TComponent)
  private
    FDadosPFX: AnsiString;
    FSenha: AnsiString;
    FK: String;
    FNumeroSerie: String;
    FArquivoPFX: String;
    FVerificarValidade: Boolean;

    function GetSenha: AnsiString;
    procedure SetArquivoPFX(AValue: String);
    procedure SetDadosPFX(AValue: AnsiString);
    procedure SetNumeroSerie(const AValue: String);
    procedure SetSenha(AValue: AnsiString);
  protected
    fpConfiguracoes: TConfiguracoes;

  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload;
    procedure Assign(DeCertificadosConf: TCertificadosConf); reintroduce; virtual;
    procedure GravarIni( const AIni: TCustomIniFile ); virtual;
    procedure LerIni( const AIni: TCustomIniFile ); virtual;

  published
    property ArquivoPFX: String read FArquivoPFX write SetArquivoPFX;
    property DadosPFX: AnsiString read FDadosPFX write SetDadosPFX;
    property NumeroSerie: String read FNumeroSerie write SetNumeroSerie;
    property Senha: AnsiString read GetSenha write SetSenha;
    property VerificarValidade: Boolean read FVerificarValidade write
      FVerificarValidade default True;
  end;

  { TWebServicesConf }

  TWebServicesConf = class(TComponent)
  private
    FResourceName: String;
    FSSLType: TSSLType;
    FTimeOut: Integer;
    FTimeZoneConf: TTimeZoneConf;
    FVisualizar: Boolean;
    FUF: String;
    FUFCodigo: integer;
    FAmbiente: TpcnTipoAmbiente;
    FProxyHost: String;
    FProxyPort: String;
    FProxyUser: String;
    FProxyPass: String;
    FAguardarConsultaRet: cardinal;
    FTentativas: integer;
    FIntervaloTentativas: cardinal;
    FAjustaAguardaConsultaRet: Boolean;
    FSalvar: Boolean;
    FParams: TStrings;
    FQuebradeLinha: String;

    function GetAmbienteCodigo: integer;
    procedure SetProxyHost(AValue: String);
    procedure SetProxyPass(AValue: String);
    procedure SetProxyPort(AValue: String);
    procedure SetProxyUser(AValue: String);
    procedure SetSSLType(AValue: TSSLType);
    procedure SetTimeOut(AValue: Integer);
    procedure SetUF(AValue: String);
    procedure SetTentativas(const Value: integer);
    procedure SetIntervaloTentativas(const Value: cardinal);
    procedure SetParams(const AValue: TStrings);

  protected
    fpConfiguracoes: TConfiguracoes;
    function LerParamsIniServicos: AnsiString; virtual;
    function LerParamsInterno: AnsiString; virtual;

  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(DeWebServicesConf: TWebServicesConf); reintroduce; virtual;
    procedure GravarIni( const AIni: TCustomIniFile ); virtual;
    procedure LerIni( const AIni: TCustomIniFile ); virtual;

    procedure LerParams; virtual;

    property ResourceName: String read FResourceName write FResourceName;
  published
    property Visualizar: Boolean read FVisualizar write FVisualizar default False;
    property UF: String read FUF write SetUF;
    property UFCodigo: integer read FUFCodigo;
    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente default taHomologacao;
    property AmbienteCodigo: integer read GetAmbienteCodigo;
    property ProxyHost: String read FProxyHost write SetProxyHost;
    property ProxyPort: String read FProxyPort write SetProxyPort;
    property ProxyUser: String read FProxyUser write SetProxyUser;
    property ProxyPass: String read FProxyPass write SetProxyPass;
    property AguardarConsultaRet: cardinal read FAguardarConsultaRet
      write FAguardarConsultaRet;
    property Tentativas: integer read FTentativas write SetTentativas default 5;
    property IntervaloTentativas: cardinal read FIntervaloTentativas
      write SetIntervaloTentativas default 1000;
    property AjustaAguardaConsultaRet: Boolean
      read FAjustaAguardaConsultaRet write FAjustaAguardaConsultaRet default False;
    // WebService.Salvar - trata-se de arquivos gerais envelopados, ou seja,
    // arquivos de envio e de retorno sem validade jurídica
    property Salvar: Boolean read FSalvar write FSalvar default False;
    property Params: TStrings read FParams write SetParams;
    property TimeOut: Integer read FTimeOut write SetTimeOut default 5000;
    property QuebradeLinha: String read FQuebradeLinha write FQuebradeLinha;
    property TimeZoneConf: TTimeZoneConf read FTimeZoneConf write FTimeZoneConf;
    property SSLType: TSSLType read FSSLType write SetSSLType;
  end;

  TOrdenacaoPathItem = class;

  TOrdenacaoPath = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TOrdenacaoPathItem;
    procedure SetItem(Index: Integer; Value: TOrdenacaoPathItem);
  public
    function Add: TOrdenacaoPathItem;
    property Items[Index: Integer]: TOrdenacaoPathItem read GetItem write SetItem; default;
  end;

  TOrdenacaoPathItem = class(TCollectionItem)
  private
    FItem: TTagOrdenacaoPath;
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Item: TTagOrdenacaoPath read FItem write FItem;
  end;

  { TGeralConf }

  TGeralConf = class(TComponent)
  private
    FIdentarXML: Boolean;
    FSSLLib: TSSLLib;
    FSSLCryptLib: TSSLCryptLib;
    FSSLHttpLib: TSSLHttpLib;
    FFormaEmissao: TpcnTipoEmissao;
    FSalvar: Boolean;
    FExibirErroSchema: Boolean;
    FFormatoAlerta: String;
    FRetirarAcentos: Boolean;
    FRetirarEspacos: Boolean;
    FSSLXmlSignLib: TSSLXmlSignLib;
    FValidarDigest: Boolean;
    FCalcSSLLib: Boolean;

    function GetFormaEmissaoCodigo: integer;
    procedure SetSSLLib(AValue: TSSLLib);
    procedure SetSSLCryptLib(AValue: TSSLCryptLib);
    procedure SetSSLHttpLib(AValue: TSSLHttpLib);
    procedure SetSSLXmlSignLib(AValue: TSSLXmlSignLib);
    procedure CalcSSLLib;

    function GetFormatoAlerta: String;
  protected
    fpConfiguracoes: TConfiguracoes;

  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload; virtual;
    procedure Assign(DeGeralConf: TGeralConf); reintroduce; virtual;
    procedure GravarIni( const AIni: TCustomIniFile ); virtual;
    procedure LerIni( const AIni: TCustomIniFile ); virtual;

  published
    property SSLLib: TSSLLib read FSSLLib write SetSSLLib;
    property SSLCryptLib: TSSLCryptLib read FSSLCryptLib write SetSSLCryptLib;
    property SSLHttpLib: TSSLHttpLib read FSSLHttpLib write SetSSLHttpLib;
    property SSLXmlSignLib: TSSLXmlSignLib read FSSLXmlSignLib write SetSSLXmlSignLib;

    property FormaEmissao: TpcnTipoEmissao read FFormaEmissao
      write FFormaEmissao default teNormal;
    property FormaEmissaoCodigo: integer read GetFormaEmissaoCodigo;
    // Geral.Salvar - trata-se de arquivos gerais, ou seja, arquivos de envio e
    // de retorno sem validade jurídica.
    property Salvar: Boolean read FSalvar write FSalvar default True;
    property ExibirErroSchema: Boolean read FExibirErroSchema
      write FExibirErroSchema default True;
    property FormatoAlerta: String read GetFormatoAlerta write FFormatoAlerta;
    property RetirarAcentos: Boolean read FRetirarAcentos
      write FRetirarAcentos default True;
    property RetirarEspacos: Boolean read FRetirarEspacos
      write FRetirarEspacos default True;
    property IdentarXML: Boolean read FIdentarXML write FIdentarXML default False;
    property ValidarDigest: Boolean
      read FValidarDigest write FValidarDigest default True;
  end;

  { TArquivosConf }

  TArquivosConf = class(TComponent)
  private
    FPathSalvar: String;
    FPathSchemas: String;
    FIniServicos: String;

    FSalvar: Boolean;
    FAdicionarLiteral: Boolean;
    FSepararPorCNPJ: Boolean;
    FSepararPorModelo: Boolean;
    FOrdenacaoPath: TOrdenacaoPath;
    FSepararPorAno: Boolean;
    FSepararPorMes: Boolean;
    FSepararPorDia: Boolean;
  private
    function GetIniServicos: String;
    function GetPathSalvar: String;
    function GetPathSchemas: String;
    procedure SetSepararPorDia(const Value: Boolean);
    procedure SetSepararPorMes(const Value: Boolean);
    procedure SetSepararPorAno(const Value: Boolean);
  protected
    fpConfiguracoes: TConfiguracoes;
  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Assign(DeArquivosConf: TArquivosConf); reintroduce; virtual;
    procedure GravarIni( const AIni: TCustomIniFile ); virtual;
    procedure LerIni( const AIni: TCustomIniFile ); virtual;

    function GetPath(APath: String; ALiteral: String; CNPJ: String = '';
      Data: TDateTime = 0; ModeloDescr: String = ''): String; virtual;
  published
    property PathSalvar: String read GetPathSalvar write FPathSalvar;
    property PathSchemas: String read GetPathSchemas write FPathSchemas;
    property IniServicos: String read GetIniServicos write FIniServicos;
    // Arquivos.Salvar - trata-se de arquivos com validade jurídica.
    property Salvar: Boolean read FSalvar write FSalvar default True;
    property AdicionarLiteral: Boolean read FAdicionarLiteral write FAdicionarLiteral default False;
    property SepararPorCNPJ: Boolean read FSepararPorCNPJ write FSepararPorCNPJ default False;
    property SepararPorModelo: Boolean read FSepararPorModelo write FSepararPorModelo default False;
    property OrdenacaoPath: TOrdenacaoPath read FOrdenacaoPath write FOrdenacaoPath;
    property SepararPorAno: Boolean read FSepararPorAno write SetSepararPorAno default False;
    property SepararPorMes: Boolean read FSepararPorMes write SetSepararPorMes default False;
    property SepararPorDia: Boolean read FSepararPorDia write SetSepararPorDia default False;
  end;

  { TConfiguracoes }

  TConfiguracoes = class(TComponent)
  protected
    FPGeral: TGeralConf;
    FPWebServices: TWebServicesConf;
    FPCertificados: TCertificadosConf;
    FPArquivos: TArquivosConf;
    FPSessaoIni: String;
  protected
    procedure CreateGeralConf; virtual;
    procedure CreateWebServicesConf; virtual;
    procedure CreateCertificadosConf; virtual;
    procedure CreateArquivosConf; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(DeConfiguracoes: TConfiguracoes); reintroduce; virtual;
    procedure GravarIni( const AIni: TCustomIniFile ); virtual;
    procedure LerIni( const AIni: TCustomIniFile ); virtual;

    procedure LerParams(NomeArqParams: String = '');

    property Geral: TGeralConf read FPGeral;
    property WebServices: TWebServicesConf read FPWebServices;
    property Certificados: TCertificadosConf read FPCertificados;
    property Arquivos: TArquivosConf read FPArquivos;
    property SessaoIni: String read FPSessaoIni;
  end;

implementation

uses
  Math, strutils, DateUtils,
  synautil, synacode,
  ACBrDFe, ACBrDFeException, ACBrUtil;

{ TConfiguracoes }

constructor TConfiguracoes.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrDFe) then
    raise EACBrDFeException.Create('Owner de TConfiguracoes deve ser do tipo TACBrDFe');

  inherited Create(AOwner);

  FPSessaoIni := '';

  CreateGeralConf;
  FPGeral.Name := 'GeralConf';
  {$IFDEF COMPILER6_UP}
  FPGeral.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  CreateWebServicesConf;
  FPWebServices.Name := 'WebServicesConf';
  {$IFDEF COMPILER6_UP}
  FPWebServices.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  CreateCertificadosConf;
  FPCertificados.Name := 'CertificadosConf';
  {$IFDEF COMPILER6_UP}
  FPCertificados.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  CreateArquivosConf;
  FPArquivos.Name := 'ArquivosConf';
  {$IFDEF COMPILER6_UP}
  FPArquivos.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}
end;

procedure TConfiguracoes.CreateGeralConf;
begin
  FPGeral := TGeralConf.Create(Self);
end;

procedure TConfiguracoes.CreateWebServicesConf;
begin
  FPWebServices := TWebServicesConf.Create(self);
end;

procedure TConfiguracoes.CreateCertificadosConf;
begin
  FPCertificados := TCertificadosConf.Create(self);
end;

procedure TConfiguracoes.CreateArquivosConf;
begin
  FPArquivos := TArquivosConf.Create(self);
end;

destructor TConfiguracoes.Destroy;
begin
  FPGeral.Free;
  FPWebServices.Free;
  FPCertificados.Free;
  FPArquivos.Free;

  inherited;
end;

procedure TConfiguracoes.Assign(DeConfiguracoes: TConfiguracoes);
begin
  Geral.Assign(DeConfiguracoes.Geral);
  WebServices.Assign(DeConfiguracoes.WebServices);
  Certificados.Assign(DeConfiguracoes.Certificados);
  Arquivos.Assign(DeConfiguracoes.Arquivos);
end;

procedure TConfiguracoes.LerParams(NomeArqParams: String);
var
  SL: TStringList;
begin
  if not FileExists(NomeArqParams) then
    raise EACBrDFeException.Create('Arquivo de Parâmetro não encontrado: ' +
      NomeArqParams);

  SL := TStringList.Create;
  try
    FPWebServices.Params := SL;
  finally
    SL.Free;
  end;
end;

procedure TConfiguracoes.GravarIni(const AIni: TCustomIniFile);
begin
  Geral.GravarIni( AIni );
  WebServices.GravarIni( AIni );
  Certificados.GravarIni( AIni );
  Arquivos.GravarIni( AIni );
end;

procedure TConfiguracoes.LerIni(const AIni: TCustomIniFile);
begin
  Geral.LerIni( AIni );
  WebServices.LerIni( AIni );
  Certificados.LerIni( AIni );
  Arquivos.LerIni( AIni );
end;

{ TGeralConf }

constructor TGeralConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  fpConfiguracoes := AConfiguracoes;
  FSSLLib := libNone;
  FSSLCryptLib := cryNone;
  FSSLHttpLib := httpNone;
  FFormaEmissao := teNormal;
  FSalvar := True;
  FExibirErroSchema := True;
  FFormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';
  // O Formato da mensagem de erro pode ser alterado pelo usuario alterando-se a property FFormatoAlerta: onde;
  // %TAGNIVEL%  : Representa o Nivel da TAG; ex: <transp><vol><lacres>
  // %TAG%       : Representa a TAG; ex: <nLacre>
  // %ID%        : Representa a ID da TAG; ex X34
  // %MSG%       : Representa a mensagem de alerta
  // %DESCRICAO% : Representa a Descrição da TAG
  FRetirarAcentos := True;
  FRetirarEspacos := True;
  FIdentarXML := False;
  FValidarDigest := True;
  FCalcSSLLib := True;
end;

procedure TGeralConf.Assign(DeGeralConf: TGeralConf);
begin
  SSLLib           := DeGeralConf.SSLLib;
  SSLCryptLib      := DeGeralConf.SSLCryptLib;
  SSLHttpLib       := DeGeralConf.SSLHttpLib;
  SSLXmlSignLib    := DeGeralConf.SSLXmlSignLib;
  FormaEmissao     := DeGeralConf.FormaEmissao;
  Salvar           := DeGeralConf.Salvar;
  ExibirErroSchema := DeGeralConf.ExibirErroSchema;
  FormatoAlerta    := DeGeralConf.FormatoAlerta;
  RetirarAcentos   := DeGeralConf.RetirarAcentos;
  RetirarEspacos   := DeGeralConf.RetirarEspacos;
  ValidarDigest    := DeGeralConf.ValidarDigest;
end;

procedure TGeralConf.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CDFeSessaoIni, 'SSLCryptLib', Integer(SSLCryptLib));
  AIni.WriteInteger(CDFeSessaoIni, 'SSLHttpLib', Integer(SSLHttpLib));
  AIni.WriteInteger(CDFeSessaoIni, 'SSLXmlSignLib', Integer(SSLXmlSignLib));

  if NaoEstaVazio(fpConfiguracoes.SessaoIni) then
  begin
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'FormaEmissao', Integer(FormaEmissao));
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'ExibirErroSchema', ExibirErroSchema);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'FormatoAlerta', FormatoAlerta);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'RetirarAcentos', RetirarAcentos);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'RetirarEspacos', RetirarEspacos);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'IdentarXML', IdentarXML);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'ValidarDigest', ValidarDigest);
  end;
end;

procedure TGeralConf.LerIni(const AIni: TCustomIniFile);
begin
  SSLCryptLib := TSSLCryptLib(AIni.ReadInteger(CDFeSessaoIni, 'SSLCryptLib', Integer(SSLCryptLib)));
  SSLHttpLib := TSSLHttpLib(AIni.ReadInteger(CDFeSessaoIni, 'SSLHttpLib', Integer(SSLHttpLib)));
  SSLXmlSignLib := TSSLXmlSignLib(AIni.ReadInteger(CDFeSessaoIni, 'SSLXmlSignLib', Integer(SSLXmlSignLib)));

  if NaoEstaVazio(fpConfiguracoes.SessaoIni) then
  begin
    FormaEmissao := TpcnTipoEmissao(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'FormaEmissao', Integer(FormaEmissao)));
    Salvar := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
    ExibirErroSchema := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'ExibirErroSchema', ExibirErroSchema);
    FormatoAlerta := AIni.ReadString(fpConfiguracoes.SessaoIni, 'FormatoAlerta', FormatoAlerta);
    RetirarAcentos := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'RetirarAcentos', RetirarAcentos);
    RetirarEspacos := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'RetirarEspacos', RetirarEspacos);
    IdentarXML := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'IdentarXML', IdentarXML);
    ValidarDigest := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'ValidarDigest', ValidarDigest);
  end;
end;

function TGeralConf.GetFormatoAlerta: String;
begin
  if (FFormatoAlerta = '') or ((pos('%TAGNIVEL%', FFormatoAlerta) <= 0) and
    (pos('%TAG%', FFormatoAlerta) <= 0) and (pos('%ID%', FFormatoAlerta) <= 0) and
    (pos('%MSG%', FFormatoAlerta) <= 0) and
    (pos('%DESCRICAO%', FFormatoAlerta) <= 0)) then
    Result := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
  else
    Result := FFormatoAlerta;
end;

procedure TGeralConf.SetSSLLib(AValue: TSSLLib);
begin
  FCalcSSLLib := False;
  try
    case AValue of
      libNone:
      begin
        SSLCryptLib := cryNone;
        SSLHttpLib := httpNone;
        SSLXmlSignLib := xsNone;
      end;

      libOpenSSL:
      begin
        SSLCryptLib := cryOpenSSL;
        SSLHttpLib := httpOpenSSL;
        SSLXmlSignLib := xsXmlSec;
      end;

      libCapicom:
      begin
        SSLCryptLib := cryCapicom;
        SSLHttpLib := httpWinINet;
        SSLXmlSignLib := xsMsXmlCapicom;
      end;

      libCapicomDelphiSoap:
      begin
        SSLCryptLib := cryCapicom;
        SSLHttpLib := httpIndy;
        SSLXmlSignLib := xsMsXmlCapicom;
      end;

      libWinCrypt:
      begin
        SSLCryptLib := cryWinCrypt;
        SSLHttpLib := httpWinHttp;
        SSLXmlSignLib := xsMsXml;
      end;
    end;
  finally
    FCalcSSLLib := True;
  end;

  FSSLLib := AValue;
end;

function TGeralConf.GetFormaEmissaoCodigo: integer;
begin
  Result := StrToInt(TpEmisToStr(FFormaEmissao));
end;

procedure TGeralConf.SetSSLCryptLib(AValue: TSSLCryptLib);
begin
  TACBrDFe(fpConfiguracoes.Owner).SSL.SSLCryptLib := AValue;
  FSSLCryptLib := AValue;
  CalcSSLLib;
end;

procedure TGeralConf.SetSSLHttpLib(AValue: TSSLHttpLib);
begin
  TACBrDFe(fpConfiguracoes.Owner).SSL.SSLHttpLib := AValue;
  FSSLHttpLib := AValue;
  CalcSSLLib;
end;

procedure TGeralConf.SetSSLXmlSignLib(AValue: TSSLXmlSignLib);
begin
  TACBrDFe(fpConfiguracoes.Owner).SSL.SSLXmlSignLib := AValue;
  FSSLXmlSignLib := AValue;
  CalcSSLLib;
end;


procedure TGeralConf.CalcSSLLib;
begin
  if not FCalcSSLLib then Exit;

  if (SSLCryptLib = cryOpenSSL) and (SSLHttpLib = httpOpenSSL) and (SSLXmlSignLib = xsXmlSec) then
    FSSLLib := libOpenSSL

  else if (SSLCryptLib = cryNone) and (SSLHttpLib = httpNone) and (SSLXmlSignLib = xsNone)then
    FSSLLib := libNone

  else if (SSLCryptLib = cryCapicom) and (SSLHttpLib = httpWinINet) and (SSLXmlSignLib = xsMsXmlCapicom) then
    FSSLLib := libCapicom

  else if (SSLCryptLib = cryCapicom) and (SSLHttpLib = httpIndy) and (SSLXmlSignLib = xsMsXmlCapicom) then
    FSSLLib := libCapicomDelphiSoap

  else if (SSLCryptLib = cryWinCrypt) and (SSLHttpLib = httpWinHttp) and (SSLXmlSignLib = xsMsXml) then
    FSSLLib := libWinCrypt

  else
    FSSLLib := libCustom;
end;


{ TWebServicesConf }

constructor TWebServicesConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  fpConfiguracoes := AConfiguracoes;
  FParams := TStringList.Create;
  FTimeZoneConf := TTimeZoneConf.Create;

  FUF := DFeUF[24];
  FUFCodigo := DFeUFCodigo[24];
  FAmbiente := taHomologacao;
  FVisualizar := False;
  FProxyHost := '';
  FProxyPort := '';
  FProxyUser := '';
  FProxyPass := '';
  FAguardarConsultaRet := 0;
  FTentativas := 5;
  FIntervaloTentativas := 1000;
  FAjustaAguardaConsultaRet := False;
  FSalvar := False;
  FTimeOut := 5000;
  FResourceName := 'ACBrServicos';
  FQuebradeLinha := '|';
end;

destructor TWebServicesConf.Destroy;
begin
  FParams.Free;
  FTimeZoneConf.Free;
  inherited;
end;

procedure TWebServicesConf.Assign(DeWebServicesConf: TWebServicesConf);
begin
  ResourceName             := DeWebServicesConf.ResourceName;
  Visualizar               := DeWebServicesConf.Visualizar;
  UF                       := DeWebServicesConf.UF;
  Ambiente                 := DeWebServicesConf.Ambiente;
  ProxyHost                := DeWebServicesConf.ProxyHost;
  ProxyPort                := DeWebServicesConf.ProxyPort;
  ProxyUser                := DeWebServicesConf.ProxyUser;
  ProxyPass                := DeWebServicesConf.ProxyPass;
  AguardarConsultaRet      := DeWebServicesConf.AguardarConsultaRet;
  Tentativas               := DeWebServicesConf.Tentativas;
  IntervaloTentativas      := DeWebServicesConf.IntervaloTentativas;
  AjustaAguardaConsultaRet := DeWebServicesConf.AjustaAguardaConsultaRet;
  Salvar                   := DeWebServicesConf.Salvar;
  Params.Assign(DeWebServicesConf.Params);
end;

procedure TWebServicesConf.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CDFeSessaoIni, 'UF', UF);
  AIni.WriteInteger(CDFeSessaoIni, 'TimeZone.Modo', Integer(TimeZoneConf.ModoDeteccao));
  AIni.WriteString(CDFeSessaoIni, 'TimeZone.Str', TimeZoneConf.TimeZoneStr);
  AIni.WriteString(CDFeSessaoIni, 'Proxy.Host', ProxyHost);
  AIni.WriteString(CDFeSessaoIni, 'Proxy.Port', ProxyPort);
  AIni.WriteString(CDFeSessaoIni, 'Proxy.User', ProxyUser);
  AIni.WriteString(CDFeSessaoIni, 'Proxy.Pass', EncodeBase64(StrCrypt(ProxyPass, ProxyHost)));

  if NaoEstaVazio(fpConfiguracoes.SessaoIni) then
  begin
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'Ambiente', Integer(Ambiente));
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'Timeout', TimeOut);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Visualizar', Visualizar);
    AIni.WriteBool(fpConfiguracoes.SessaoIni, 'AjustaAguardaConsultaRet', AjustaAguardaConsultaRet);
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'AguardarConsultaRet', AguardarConsultaRet);
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'IntervaloTentativas', IntervaloTentativas);
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'Tentativas', Tentativas);
    AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'SSLType', Integer(SSLType));
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'QuebradeLinha', QuebradeLinha);
  end;
end;

procedure TWebServicesConf.LerIni(const AIni: TCustomIniFile);
begin
  UF := AIni.ReadString(CDFeSessaoIni, 'UF', UF);
  TimeZoneConf.ModoDeteccao := TTimeZoneModoDeteccao(AIni.ReadInteger(CDFeSessaoIni, 'TimeZone.Modo', Integer(TimeZoneConf.ModoDeteccao)));
  TimeZoneConf.TimeZoneStr := AIni.ReadString(CDFeSessaoIni, 'TimeZone.Str', TimeZoneConf.TimeZoneStr);
  ProxyHost := AIni.ReadString(CDFeSessaoIni, 'Proxy.Host', ProxyHost);
  ProxyPort := AIni.ReadString(CDFeSessaoIni, 'Proxy.Port', ProxyPort);
  ProxyUser := AIni.ReadString(CDFeSessaoIni, 'Proxy.User', ProxyUser);
  ProxyPass := StrCrypt( DecodeBase64(AIni.ReadString(CDFeSessaoIni, 'Proxy.Pass', '')), ProxyHost);

  if NaoEstaVazio(fpConfiguracoes.SessaoIni) then
  begin
    Ambiente := TpcnTipoAmbiente( AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'Ambiente', Integer(Ambiente)));
    Salvar := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
    TimeOut := AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'Timeout', TimeOut);
    Visualizar := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Visualizar', Visualizar);
    AjustaAguardaConsultaRet := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'AjustaAguardaConsultaRet', AjustaAguardaConsultaRet);
    AguardarConsultaRet := AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'AguardarConsultaRet', AguardarConsultaRet);
    IntervaloTentativas := AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'IntervaloTentativas', IntervaloTentativas);
    Tentativas := AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'Tentativas', Tentativas);
    SSLType := TSSLType(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'SSLType', Integer(SSLType)));
    QuebradeLinha := AIni.ReadString(fpConfiguracoes.SessaoIni, 'QuebradeLinha', QuebradeLinha);
  end;
end;

procedure TWebServicesConf.LerParams;
var
  ConteudoParams: AnsiString;
begin
  ConteudoParams := LerParamsIniServicos;

  if ConteudoParams = '' then
    ConteudoParams := LerParamsInterno;

  FParams.Text := ConteudoParams;
end;

procedure TWebServicesConf.SetIntervaloTentativas(const Value: cardinal);
begin
  FIntervaloTentativas := max(Value, 1000);
end;

procedure TWebServicesConf.SetParams(const AValue: TStrings);
begin
  FParams.Assign(AValue);
end;

function TWebServicesConf.LerParamsIniServicos: AnsiString;
var
  SL: TStringList;
begin
  Result := '';

  if (fpConfiguracoes.Arquivos.IniServicos <> '') and
    FileExists(fpConfiguracoes.Arquivos.IniServicos) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(fpConfiguracoes.Arquivos.IniServicos);
      Result := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

function TWebServicesConf.LerParamsInterno: AnsiString;
var
  RS: TResourceStream;
begin
  Result := '';

  RS := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  try
    RS.Position := 0;
    Result := ReadStrFromStream(RS, RS.Size);
  finally
    RS.Free;
  end;
end;

procedure TWebServicesConf.SetTentativas(const Value: integer);
begin
  if Value <= 0 then
    FTentativas := 5
  else
    FTentativas := Value;
end;

procedure TWebServicesConf.SetUF(AValue: String);
var
  Codigo, i: integer;
begin
  Codigo := -1;
  for i := 0 to High(DFeUF) do
  begin
    if DFeUF[I] = AValue then
    begin
      Codigo := DFeUFCodigo[I];
      Break;
    end;
  end;

  if Codigo > 0 then
  begin
    FUF := AValue;
    FUFCodigo := Codigo;
  end;
end;

procedure TWebServicesConf.SetProxyHost(AValue: String);
begin
  if FProxyHost = AValue then Exit;
  FProxyHost := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.ProxyHost := AValue;
end;

function TWebServicesConf.GetAmbienteCodigo: integer;
begin
  Result := StrToInt(TpAmbToStr(FAmbiente));
end;

procedure TWebServicesConf.SetProxyPass(AValue: String);
begin
  if FProxyPass = AValue then Exit;
  FProxyPass := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.ProxyPass := AValue;
end;

procedure TWebServicesConf.SetProxyPort(AValue: String);
begin
  if FProxyPort = AValue then Exit;
  FProxyPort := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.ProxyPort := AValue;
end;

procedure TWebServicesConf.SetProxyUser(AValue: String);
begin
  if FProxyUser = AValue then Exit;
  FProxyUser := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.ProxyUser := AValue;
end;

procedure TWebServicesConf.SetSSLType(AValue: TSSLType);
begin
  TACBrDFe(fpConfiguracoes.Owner).SSL.SSLType := AValue;
  FSSLType := AValue;
end;

procedure TWebServicesConf.SetTimeOut(AValue: Integer);
begin
  if FTimeOut = AValue then Exit;
  FTimeOut := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.TimeOut := AValue;
end;

{ TCertificadosConf }

constructor TCertificadosConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  fpConfiguracoes := AConfiguracoes;
  FSenha := '';
  FK := '';
  FArquivoPFX := '';
  FDadosPFX := '';
  FNumeroSerie := '';
  FVerificarValidade := True;
end;

procedure TCertificadosConf.Assign(DeCertificadosConf: TCertificadosConf);
begin
  DadosPFX := DeCertificadosConf.DadosPFX;
  Senha := DeCertificadosConf.Senha;
  NumeroSerie := DeCertificadosConf.NumeroSerie;
  ArquivoPFX := DeCertificadosConf.ArquivoPFX;
end;

procedure TCertificadosConf.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CDFeSessaoIni, 'ArquivoPFX', ArquivoPFX);
  AIni.WriteString(CDFeSessaoIni, 'DadosPFX', EncodeBase64(DadosPFX));
  AIni.WriteString(CDFeSessaoIni, 'NumeroSerie', NumeroSerie);
  AIni.WriteString(CDFeSessaoIni, 'Senha', EncodeBase64(FSenha));
  AIni.WriteString(CDFeSessaoIni, 'FK', EncodeBase64(FK));
  AIni.WriteBool(CDFeSessaoIni, 'VerificarValidade', VerificarValidade);
end;

procedure TCertificadosConf.LerIni(const AIni: TCustomIniFile);
begin
  ArquivoPFX := AIni.ReadString(CDFeSessaoIni, 'ArquivoPFX', ArquivoPFX);
  DadosPFX := DecodeBase64( AIni.ReadString(CDFeSessaoIni, 'DadosPFX', EncodeBase64(DadosPFX)));
  NumeroSerie := AIni.ReadString(CDFeSessaoIni, 'NumeroSerie', NumeroSerie);
  FSenha := DecodeBase64( AIni.ReadString(CDFeSessaoIni, 'Senha', EncodeBase64(FSenha)));
  FK := DecodeBase64( AIni.ReadString(CDFeSessaoIni, 'FK', EncodeBase64(FK)));
  VerificarValidade := AIni.ReadBool(CDFeSessaoIni, 'VerificarValidade', VerificarValidade);
end;

procedure TCertificadosConf.SetNumeroSerie(const AValue: String);
begin
  if FNumeroSerie = AValue then Exit;
  FNumeroSerie := Trim(UpperCase(StringReplace(AValue, ' ', '', [rfReplaceAll])));
  TACBrDFe(fpConfiguracoes.Owner).SSL.NumeroSerie := FNumeroSerie;
end;

procedure TCertificadosConf.SetSenha(AValue: AnsiString);
begin
  if (FK <> '') and (FSenha = StrCrypt(AValue, FK)) then
    Exit;

  FK := FormatDateTime('hhnnsszzz',Now);
  FSenha := StrCrypt(AValue, FK);  // Salva Senha de forma Criptografada, para evitar "Inspect"

  TACBrDFe(fpConfiguracoes.Owner).SSL.Senha := AValue;
end;

procedure TCertificadosConf.SetArquivoPFX(AValue: String);
begin
  if FArquivoPFX = AValue then Exit;
  FArquivoPFX := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.ArquivoPFX := AValue;
end;

function TCertificadosConf.GetSenha: AnsiString;
begin
  Result := StrCrypt(FSenha, FK)  // Descritografa a Senha
end;

procedure TCertificadosConf.SetDadosPFX(AValue: AnsiString);
begin
  if FDadosPFX = AValue then Exit;
  FDadosPFX := AValue;
  TACBrDFe(fpConfiguracoes.Owner).SSL.DadosPFX := AValue;
end;


{ TArquivosConf }

constructor TArquivosConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  FOrdenacaoPath := TOrdenacaoPath.Create(Self, TOrdenacaoPathItem);

  fpConfiguracoes := AConfiguracoes;
  FSalvar := True;
  FPathSalvar := '';
  FPathSchemas := '';
  FIniServicos := '';

  FSepararPorDia := False;
  FSepararPorMes := False;
  FSepararPorAno := False;
  FAdicionarLiteral := False;
  FSepararPorCNPJ := False;
  FSepararPorModelo := False;
end;

destructor TArquivosConf.Destroy;
begin
  FOrdenacaoPath.Free;
  inherited;
end;

procedure TArquivosConf.Assign(DeArquivosConf: TArquivosConf);
var
  I: Integer;
begin
  PathSalvar       := DeArquivosConf.PathSalvar;
  PathSchemas      := DeArquivosConf.PathSchemas;
  IniServicos      := DeArquivosConf.IniServicos;
  Salvar           := DeArquivosConf.Salvar;
  AdicionarLiteral := DeArquivosConf.AdicionarLiteral;
  SepararPorCNPJ   := DeArquivosConf.SepararPorCNPJ;
  SepararPorModelo := DeArquivosConf.SepararPorModelo;
  SepararPorMes    := DeArquivosConf.SepararPorMes;
  SepararPorDia    := DeArquivosConf.SepararPorDia;
  OrdenacaoPath.Clear;
  for I := 0 to DeArquivosConf.OrdenacaoPath.Count-1 do
    OrdenacaoPath.Add.Item := DeArquivosConf.OrdenacaoPath.Items[I].Item;
end;

procedure TArquivosConf.GravarIni(const AIni: TCustomIniFile);
begin
  if EstaVazio(fpConfiguracoes.SessaoIni) then
    Exit;

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathSalvar', PathSalvar);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathSchemas', PathSchemas);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'IniServicos', IniServicos);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'AdicionarLiteral', AdicionarLiteral);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SepararPorCNPJ', SepararPorCNPJ);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SepararPorModelo', SepararPorModelo);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SepararPorAno', SepararPorAno);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SepararPorMes', SepararPorMes);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SepararPorDia', SepararPorDia);
end;

procedure TArquivosConf.LerIni(const AIni: TCustomIniFile);
begin
  if EstaVazio(fpConfiguracoes.SessaoIni) then
    Exit;

  PathSalvar := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathSalvar', PathSalvar);
  PathSchemas := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathSchemas', PathSchemas);
  IniServicos := AIni.ReadString(fpConfiguracoes.SessaoIni, 'IniServicos', IniServicos);
  Salvar := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Salvar', Salvar);
  AdicionarLiteral := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'AdicionarLiteral', AdicionarLiteral);
  SepararPorCNPJ := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SepararPorCNPJ', SepararPorCNPJ);
  SepararPorModelo := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SepararPorModelo', SepararPorModelo);
  SepararPorAno := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SepararPorAno', SepararPorAno);
  SepararPorMes := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SepararPorMes', SepararPorMes);
  SepararPorDia := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SepararPorDia', SepararPorDia);
end;

function TArquivosConf.GetPathSalvar: String;
begin
  if FPathSalvar = '' then
    if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
      FPathSalvar := ApplicationPath + 'Docs';

  FPathSalvar := PathWithDelim(Trim(FPathSalvar));
  Result := FPathSalvar;
end;

function TArquivosConf.GetPathSchemas: String;
begin
  if FPathSchemas = '' then
    if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
      FPathSchemas := ApplicationPath + 'Schemas';

  FPathSchemas := PathWithDelim(Trim(FPathSchemas));
  Result := FPathSchemas;
end;

procedure TArquivosConf.SetSepararPorAno(const Value: Boolean);
begin
  FSepararPorAno := Value;
end;

procedure TArquivosConf.SetSepararPorDia(const Value: Boolean);
begin
  FSepararPorDia := Value;
  if FSepararPorDia then
    FSepararPorMes := True;
end;

procedure TArquivosConf.SetSepararPorMes(const Value: Boolean);
begin
  FSepararPorMes := Value;
  if not FSepararPorMes then
    FSepararPorDia := False;
end;

function TArquivosConf.GetIniServicos: String;
begin
  if FIniServicos = '' then
    if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
      FIniServicos := ApplicationPath + fpConfiguracoes.WebServices.ResourceName+'.ini';

  Result := FIniServicos;
end;

function TArquivosConf.GetPath(APath: String; ALiteral: String; CNPJ: String;
  Data: TDateTime; ModeloDescr: String): String;

  procedure AddPathOrder(AAdicionar: Boolean; AItemOrdenacaoPath: TTagOrdenacaoPath);
  begin
    if AAdicionar then
      FOrdenacaoPath.Add.Item := AItemOrdenacaoPath;
  end;
  
var
  wDia, wMes, wAno: word;
  Dir, Modelo, sAno, sMes, sDia: String;
  LenLiteral, i: integer;
begin
  if EstaVazio(APath) then
    Dir := PathSalvar
  else
    Dir := APath;

  //se nao foi informada nenhuma ordenação, cria ordenação na ordem anterior (compatibilidade)
  if (FOrdenacaoPath.Count = 0) then
  begin
    AddPathOrder(SepararPorCNPJ, opCNPJ);
    AddPathOrder(SepararPorModelo, opModelo);
    AddPathOrder((SepararPorAno or SepararPorMes or SepararPorDia), opData);
    AddPathOrder(AdicionarLiteral, opLiteral);
  end;

  for i := 0 to FOrdenacaoPath.Count - 1 do
  begin
    case FOrdenacaoPath[i].Item of
      opCNPJ:
        begin
          CNPJ := OnlyNumber(CNPJ);

          if EstaVazio(CNPJ) then
            CNPJ := OnlyNumber(TACBrDFe(fpConfiguracoes.Owner).SSL.CertCNPJ);

          if NaoEstaVazio(CNPJ) then
            Dir := PathWithDelim(Dir) + CNPJ;
        end;

      opModelo:
        begin
          if ModeloDescr = '' then
            Modelo := TACBrDFe(fpConfiguracoes.Owner).GetNomeModeloDFe
          else
            Modelo := ModeloDescr;

          Dir := PathWithDelim(Dir) + Modelo;

        end;

      opData:
        begin
          if Data = 0 then
            Data := Now;

          DecodeDate(Data, wAno, wMes, wDia);
          sDia := IntToStrZero(wDia, 2);
          sMes := IntToStrZero(wMes, 2);
          sAno := IntToStrZero(wAno, 4);

          if SepararPorAno then
            Dir := PathWithDelim(Dir) + sAno;

          if SepararPorMes then
          begin
            if SepararPorAno then
              Dir := PathWithDelim(Dir) + sMes
            else
              Dir := PathWithDelim(Dir) + sAno + sMes;

            if SepararPorDia then
              Dir := PathWithDelim(Dir) + sDia;
          end;
        end;

      opLiteral:
        begin
          LenLiteral := Length(ALiteral);

          if (LenLiteral > 0) then
          begin
            if RightStr(Dir, LenLiteral) <> ALiteral then
              Dir := PathWithDelim(Dir) + ALiteral;
          end;
        end;
    end;
  end;

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

{ TOrdenacaoPath }

function TOrdenacaoPath.Add: TOrdenacaoPathItem;
begin
  Result := TOrdenacaoPathItem(inherited Add);
end;

function TOrdenacaoPath.GetItem(Index: Integer): TOrdenacaoPathItem;
begin
  Result := TOrdenacaoPathItem(inherited GetItem(Index));
end;

procedure TOrdenacaoPath.SetItem(Index: Integer; Value: TOrdenacaoPathItem);
begin
  inherited SetItem(Index, Value);
end;

{ TOrdenacaoPathItem }

procedure TOrdenacaoPathItem.Assign(Source: TPersistent);
begin
  if Source is TOrdenacaoPathItem then
    FItem := TOrdenacaoPathItem(Source).FItem
  else
    inherited Assign(Source);
end;

function TOrdenacaoPathItem.GetDisplayName: String;
begin
  case FItem of
    opNenhum:   Result := '(nenhum)';
    opCNPJ:     Result := 'CNPJ';
    opModelo:   Result := 'Modelo';
    opData:     Result := 'Data';
    opLiteral:  Result := 'Literal';
  end;
end;

end.

