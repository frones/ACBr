{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit ACBreSocialWebServices;

interface

uses
  Classes, SysUtils,
  ACBrUtil, ACBrDFe, ACBrDFeWebService,
  pcnLeitor,
  ACBreSocialLoteEventos,  ACBreSocialEventos, ACBreSocialConfiguracoes,
  eSocial_Conversao, eSocial_Common;

type

  TeSocialWebService = class(TDFeWebService)
  private
    FPStatus: TStatusACBreSocial;
    FPLayout: TLayOut;
    FPConfiguracoeseSocial: TConfiguracoeseSocial;
    procedure ConfigurarSoapDEPC;
    function ExtrairModeloChaveAcesso(AChaveeSocial: String): String;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    property Status: TStatusACBreSocial read FPStatus;
    property Layout: TLayOut read FPLayout;
  end;

  { TOcorrencia }
  TOcorrencia = class(TCollectionItem)
  private
    FCodigo : Integer;
    FDescricao : String;
    FTipo : Byte;
    FLocalizacao : String;
    Fxml: AnsiString;
    FLeitor: TLeitor;
  public
    constructor Create; reintroduce;
    procedure BeforeDestruction; override;
    property Codigo : Integer read FCodigo write FCodigo;
    property Descricao : String read FDescricao write FDescricao;
    property Tipo : Byte read FTipo write FTipo;
    property Localizacao : String read FLocalizacao write FLocalizacao;
    property xml: AnsiString read Fxml write FXml;
    property Leitor: TLeitor read FLeitor;
    procedure LerXml;
  end;

  { TOcorrencias }
  TOcorrencias = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TOcorrencia;
    procedure SetItem(Index: Integer; Value: TOcorrencia);
  public
    function Add: TOcorrencia;
    property Items[Index: Integer]: TOcorrencia read GetItem write SetItem; default;
  end;

  { TStatusEnvLote }
  TStatusRetorno = class
  private
    FcdResposta : Integer;
	  FdescResposta : String;
  	FOcorrencias : TOcorrencias;
  public
    property cdResposta : Integer read FcdResposta write FcdResposta;
    property descResposta : String read FdescResposta write FdescResposta;
    property Ocorrencias : TOcorrencias read FOcorrencias write FOcorrencias;
  end;

  TStatusEnvLote = class(TStatusRetorno);

  { TDadosRecepcaoLote }
  TDadosRecepcaoLote = class
  private
   	FdhRecepcao : TDateTime;
  	FversaoAplicRecepcao : String;
  	FProtocolo : String;
  public
    property dhRecepcao : TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicRecepcao : String read FversaoAplicRecepcao write FversaoAplicRecepcao;
    property Protocolo : String read FProtocolo write FProtocolo;
  end;

  TStatusProcLote = class(TStatusRetorno)
  private
  	FTmpConclusao : Integer;
  public
  	property TmpConclusao : Integer read FTmpConclusao write FTmpConclusao;
  end;

  { TRetEnvLote }

  TRetEnvLote = class(TeSocialWebService)
  private
    FACBreSocial : TComponent;
    FIdeEmpregador : TIdeEmpregador;
	  FIdeTransmissor : TIdeTransmissor;
	  FStatusEnvLote : TStatusEnvLote;
	  FDadosRecepcaoLote : TDadosRecepcaoLote;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    property IdeEmpregador : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTransmissor : TIdeTransmissor read FIdeTransmissor write FIdeTransmissor;
    property StatusEnvLote : TStatusEnvLote read FStatusEnvLote write FStatusEnvLote;
  end;

  TdadosProcLote = class
  private
  	FversaoAplicProcLote : String;
  public
  	property versaoAplicProcLote : String read FversaoAplicProcLote write FversaoAplicProcLote;
  end;

  TRecepcao = class
  private
  	FtpAmb : TptpAmb;
  	FdhRecepcao : TDateTime;
  	FversaoAplicRecepcao : String;
  	Fprotocolo : String;
  end;

  TProcessamento = class
  private
  	FcdResposta: string;
  	FdescResposta: string;
  	FversaoAplicProcLote : string;
  	FdhProcessamento : TDateTime;
  	FOcorrencias : TOcorrencias;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  	property cdResposta: string read FcdResposta write FcdResposta;
  	property descResposta: string read FdescResposta write FdescResposta;
  	property versaoAplicProcLote : string read FversaoAplicProcLote write FversaoAplicProcLote;
  	property dhProcessamento : TDateTime read FdhProcessamento write FdhProcessamento;
  	property Ocorrencias : TOcorrencias read FOcorrencias write  FOcorrencias;
  end;

  TRecibo = class
  public
  	FnrRecibo : String;
  	FHash : String;
  end;

  TretEvento = class(TCollectionItem)
  private
  	FIDEvento: string;
  	FRecepcao : TRecepcao;
  	FProcessamento : TProcessamento;
  	FRecibo : TRecibo;
  	FSignature : AnsiString;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property  Processamento : TProcessamento read FProcessamento;
    property IDEvento: string read FIDEvento write FIDEvento;
    property Recibo: TRecibo read FRecibo;
  end;

  TretEventos = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TretEvento;
    procedure SetItem(Index: Integer; Value: TretEvento);
  public
    function Add: TretEvento;
    property Items[Index: Integer]: TretEvento read GetItem write SetItem; default;
  end;

  TRetProcLote = class(TeSocialWebService)
  private
  	FIdeEmpregador : TIdeEmpregador;
  	FIdeTransmissor : TIdeTransmissor;
  	FStatus : Integer;
    FDescricao: string;
  	FdadosRecLote : TDadosRecepcaoLote;
   	FdadosProcLote : TdadosProcLote;
  	FretEventos : TretEventos;
  protected
  	procedure LerXML(const AXml: string);
  public
  	constructor Create(AOwner : TACBrDFe);
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  	property IdeEmpregador : TIdeEmpregador read FIdeEmpregador;
  	property IdeTransmissor : TIdeTransmissor read FIdeTransmissor;
  	property Status : Integer read FStatus write FStatus;
    property Descricao: string read FDescricao write FDescricao;
  	property dadosRecLote : TDadosRecepcaoLote read FdadosRecLote;
   	property dadosProcLote : TdadosProcLote read FdadosProcLote;
  	property retEventos : TretEventos read FretEventos;
  end;

   { TeSocialConsulta }

  TConsultaLote = class(TeSocialWebService)
  private
    FACBreSocial : TACBrDFe;
    FProtocolo : string;
    FXMLEnvio: AnsiString;
    FXMLAssinado: AnsiString;
    FXMLOriginal: AnsiString;
    FXMlRet: AnsiString;
    FRetProcLote: TRetProcLote;
    function Assinar(Axml: string; ANomeEvento: string): AnsiString;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
    procedure DefinirEnvelopeSoap; override;
    procedure SalvarResposta; override;
  public
    constructor Create(AOwner : TACBrDFe);
    procedure GerarXML;
    procedure BeforeDestruction; override;
    property Protocolo: string read FProtocolo write FProtocolo;
    property XMLEnvio: AnsiString read FXMLEnvio;
    property XMlRet: AnsiString read FXMlRet;
    property RetProcLote: TRetProcLote read FRetProcLote;
  end;

  { TEnvioLote }

  TEnvioLote = class(TeSocialWebService)
  private
    FVersao : String;
    FGrupo : Integer;
    FIdeEmpregador : TIdeEmpregador;
    FIdeTransmissor : TIdeTransmissor;
    FLote : TLoteEventos;
    FPURLEnvio : string;
    FXMLEnvio : string;
    FRetProcLote: TRetProcLote;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure SalvarEnvio; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
    function GerarMsgErro(E: Exception): String;
    function GerarVersaoDadosSoap: String;
    procedure DefinirEnvelopeSoap; override;
  public
    constructor Create(AOwner: TACBrDFe);
    property RetProcLote: TRetProcLote read FRetProcLote;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBreSocial: TACBrDFe;
    FEnvioLote  : TEnvioLote;
    FConsultaLote : TConsultaLote;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;
    function Envia(ALote: integer; const ASincrono: Boolean = True): Boolean; overload;
    function Envia(ALote: String; const ASincrono: Boolean = True): Boolean; overload;
    function Consultar(const AProtocolo: string): Boolean;
    property ACBreSocial: TACBrDFe read FACBreSocial write FACBreSocial;
    property EnvioLote : TEnvioLote read FEnvioLote write FEnvioLote;
    property ConsultaLote : TConsultaLote read FConsultaLote write FConsultaLote;
  end;

implementation

uses
  StrUtils, Math, blcksock, DateUtils,
  pcnConversao, pcnGerador,
  ACBreSocial;

{ TeSocialWebService }

constructor TeSocialWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPConfiguracoeseSocial := TConfiguracoeseSocial(FPConfiguracoes);
  FPStatus := stIdle;
  FPSoapVersion := 'soap';
  FPHeaderElement := '';
  FPBodyElement := '';

  FPCabMsg := '';
  FPURL := '';
  FPVersaoServico := '';
  FPArqEnv := '';
  FPArqResp := '';
  FPServico := '';
  FPSoapAction := '';
  FPMimeType := 'text/xml';  // Vazio, usará por default: 'application/soap+xml'
end;

procedure TeSocialWebService.ConfigurarSoapDEPC;
begin
  FPSoapVersion := 'soap';
  FPHeaderElement := 'Header';
  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1"';
  FPBodyElement := 'Body';
end;

function TeSocialWebService.ExtrairModeloChaveAcesso(AChaveeSocial: String): String;
begin
  Result := '';
end;

procedure TeSocialWebService.DefinirURL ;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBreSocial(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;


function TeSocialWebService.GerarPrefixoArquivo: String;
begin
  Result := 'eSocial';
end;

function TeSocialWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := 'v2.1.0';//TACBreSocial(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := ''; //'<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TeSocialWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  FPDFeOwner.SSL.SSLType := LT_TLSv1;
end;

procedure TeSocialWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBreSocial(FPDFeOwner).SetStatus(stIdle);
end;

{ TEnvioLote }

constructor TEnvioLote.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPLayout := LayEnvLoteEventos;
  FLote := TLoteEventos.Create(nil);
  FRetProcLote := TRetProcLote.Create(AOwner);
  FPStatus := stIdle;
  FVersao := '';
  ConfigurarSoapDEPC;
end;

procedure TEnvioLote.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  TACBreSocial(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TEnvioLote.DefinirServicoEAction;
begin
  FPServico := FPDFeOwner.GetNameSpaceURI + '/ServicoEnviarLoteEventos/EnviarLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TEnvioLote.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
  LeitorXML := TLeitor.Create;
  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;
  FPDadosMsg := FXMLEnvio;
end;

procedure TEnvioLote.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  { Sobrescrever apenas se necessário }

  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:EnviarLoteEventos>';
  Texto := Texto + '<' +  'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' +  '/v1:loteEventos>';
  Texto := Texto + '<' +  '/v1:EnviarLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  Texto := '<?xml version="1.0" encoding="utf-8"?>' + Texto;
  FPEnvelopeSoap := Texto;
end;

function TEnvioLote.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  i: integer;
  Processamento: TProcessamento;
  retEvento: TretEvento;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'retornoEnvioLoteEventos');
  Result := FPRetWS <> EmptyStr;
  retEvento := FRetProcLote.retEventos.Add;
  Processamento  := retEvento.FProcessamento;

  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := FPRetWS;
    if Leitor.rExtrai(1, 'ideEmpregador') = EmptyStr then {msg de errs}
    begin
      Leitor.Arquivo := FPRetWS;
      Leitor.Grupo := Leitor.rExtrai(1, 'cdResposta');
      FRetProcLote.Status := StrToInt64Def(Leitor.rCampo(tcStr, 'cdResposta'), -1);
      Processamento.cdResposta := Leitor.rCampo(tcStr, 'cdResposta');
      Leitor.Grupo := Leitor.rExtrai(1, 'descResposta');
      Processamento.FdescResposta := UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));
      Leitor.Arquivo := Leitor.rExtrai(1, 'ocorrencias');
      Leitor.Grupo := Leitor.Arquivo;
      i:=0;
      while Leitor.rExtrai(1, 'ocorrencia', '', i + 1) <> '' do
      begin
        Processamento.Ocorrencias.Add;
        Processamento.Ocorrencias.Items[i].xml := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].FLeitor.Arquivo := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].FLeitor.Grupo := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].LerXml;
        inc(i);
      end;
    end
    else
    begin
      Leitor.Arquivo := FPRetWS;
      Leitor.Grupo := Leitor.rExtrai(1, 'ideEmpregador');
      FRetProcLote.FIdeEmpregador.TpInsc := tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
      FRetProcLote.FIdeEmpregador.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

      Leitor.Grupo := Leitor.rExtrai(1, 'ideTransmissor');
      FRetProcLote.FIdeTransmissor.TpInsc := tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
      FRetProcLote.FIdeTransmissor.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

      Leitor.Grupo := Leitor.rExtrai(1, 'status');
      FRetProcLote.Status := StrToInt64Def(Leitor.rCampo(tcStr, 'cdResposta'), -1);
      FRetProcLote.Descricao := UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));

      Leitor.Grupo := Leitor.rExtrai(1, 'dadosRecepcaoLote');
      try
//Compatibilizar        FRetProcLote.dadosRecLote.dhRecepcao := ISO8601ToDate(Leitor.rCampo(tcStr, 'dhRecepcao'), False);
      except //'2017-07-20T22:14:51.1569524-03:00'
        FRetProcLote.dadosRecLote.dhRecepcao := 0;
      end;
      FRetProcLote.dadosRecLote.versaoAplicRecepcao := Leitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
      FRetProcLote.dadosRecLote.Protocolo  := Leitor.rCampo(tcStr, 'protocoloEnvio');
    end;
  finally
    Leitor.Free;
  end;

end;

function TEnvioLote.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + #13#10 + '- Inativo ou Inoperante tente novamente.');
end;

function TEnvioLote.GerarMsgLog: String;
begin
end;

function TEnvioLote.GerarPrefixoArquivo: String;
begin
end;

function TEnvioLote.GerarVersaoDadosSoap: String;
begin
  Result := ''; //'<versaoDados>' + FVersao + '</versaoDados>';
end;

procedure TEnvioLote.SalvarEnvio;
var
  Path: string;
begin
  inherited;
  if TACBreSocial(Self.FPDFeOwner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBreSocial(Self.FPDFeOwner).Configuracoes.Arquivos.PathSalvar;
    with TStringList.Create do
    try
      Text := FPEnvelopeSoap;
      SaveToFile(Path+'\'+'Envio_Soap'+'-'+ IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
  end;
end;

procedure TEnvioLote.SalvarResposta;
var
  Path: string;
begin
  inherited;
  if TACBreSocial(Self.FPDFeOwner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBreSocial(Self.FPDFeOwner).Configuracoes.Arquivos.PathSalvar;
    with TStringList.Create do
    try
      Text := FPRetornoWS;
      SaveToFile(Path+'\'+'Resp_Soap'+'-' + IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
      Text := FPRetWS;
      SaveToFile(Path+'\'+'Resp'+'-'+ IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
  end;
end;

{ TWebServices }

function TWebServices.Consultar(const AProtocolo: string): Boolean;
begin
  FConsultaLote.FProtocolo := AProtocolo;
  FConsultaLote.GerarXML;
  try
    if not FConsultaLote.Executar then
      FConsultaLote.GerarException( FConsultaLote.Msg );
    Result := True;
  except
    raise;
  end;
end;

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBreSocial := TACBrDFe(AOwner);
  FConsultaLote := TConsultaLote.Create(FACBreSocial);
  FEnvioLote := TEnvioLote.Create(FACBreSocial);
end;

destructor TWebServices.Destroy;
begin
  FConsultaLote.Free;
  FEnvioLote.Free;

  inherited Destroy;
end;

function TWebServices.Envia(ALote: integer; const ASincrono: Boolean): Boolean;
begin
  Result := Envia(IntToStr(ALote), ASincrono);
end;

function TWebServices.Envia(ALote: String; const ASincrono: Boolean): Boolean;
begin
  FEnvioLote.FLote.XML := ALote;
  EnvioLote.FXMLEnvio := ALote;
  try
    if not EnvioLote.Executar then
      EnvioLote.GerarException( EnvioLote.Msg );
    Result := True;
  except
    raise;
  end;
end;

{ TOcorrencias }

function TOcorrencias.Add: TOcorrencia;
begin
  Result := TOcorrencia(inherited Add);
  Result.Create;
end;

function TOcorrencias.GetItem(Index: Integer): TOcorrencia;
begin
  Result := TOcorrencia(inherited GetItem(Index));
end;

procedure TOcorrencias.SetItem(Index: Integer; Value: TOcorrencia);
begin
  inherited SetItem(Index, Value);
end;

{ TOcorrencia }

procedure TOcorrencia.BeforeDestruction;
begin
  inherited;
  FLeitor.Free;
end;

constructor TOcorrencia.Create;
begin
  FLeitor := TLeitor.Create;
end;

procedure TOcorrencia.LerXml;
begin
  FCodigo := FLeitor.rCampo(tcInt, 'codigo');
  FDescricao := UTF8ToNativeString(FLeitor.rCampo(tcStr, 'descricao'));
  FTipo := FLeitor.rCampo(tcInt, 'tipo');
  FLocalizacao := UTF8ToNativeString(FLeitor.rCampo(tcStr, 'localizacao'));
end;

{ TRetEnvLote }

constructor TRetEnvLote.Create(AOwner: TACBrDFe);
begin
  inherited;
  FACBreSocial := AOwner;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FStatusEnvLote := TStatusEnvLote.Create;
  FDadosRecepcaoLote := TDadosRecepcaoLote.Create;
end;

destructor TRetEnvLote.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FStatusEnvLote.Free;
  FDadosRecepcaoLote.Free;
  inherited;
end;

{ TretEventos }

function TretEventos.Add: TretEvento;
begin
  Result := TretEvento(inherited Add);
end;

function TretEventos.GetItem(Index: Integer): TretEvento;
begin
   Result := TretEvento(inherited GetItem(Index));
end;

procedure TretEventos.SetItem(Index: Integer; Value: TretEvento);
begin
  inherited SetItem(Index, Value);
end;

{ TRetProcLote }

procedure TRetProcLote.AfterConstruction;
begin
  inherited;
  FStatus := 0;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FdadosRecLote := TDadosRecepcaoLote.Create;
  FdadosProcLote := TdadosProcLote.Create;
  FretEventos := TretEventos.Create(nil, TretEvento);
end;

procedure TRetProcLote.BeforeDestruction;
begin
  inherited;
  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FdadosRecLote.Free;
  FdadosProcLote.Free;
  FretEventos.Free;
end;

constructor TRetProcLote.Create(AOwner : TACBrDFe);
begin
  Inherited Create(AOwner);
end;

destructor TRetProcLote.Destroy;
begin

end;

procedure TRetProcLote.LerXML(const AXml: string);
begin
end;

{ TConsultaLote }

function TConsultaLote.Assinar(Axml, ANomeEvento: string): AnsiString;
var
  XMLAss: String;
  ArqXML: String;
//  Leitor: TLeitor;
//  i: Integer;
begin
  Result := '';

  ArqXML := Axml;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);

  with TACBreSocial(FACBreSocial) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'eSocial', ANomeEvento, '', '', 'id');
    FXMLAssinado := XMLAss;
    FXMLOriginal := XMLAss;

    XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    if Configuracoes.Arquivos.Salvar then
       Gravar(ANomeEvento + '.xml', XMLAss,Configuracoes.Arquivos.PathSalvar);

    Result := XMLAss;
  end;
end;

procedure TConsultaLote.BeforeDestruction;
begin
  inherited;
  FRetProcLote.Free;
end;

constructor TConsultaLote.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
  FACBreSocial := AOwner;
  FPLayout := LayConsResultProcessamento;
  FPStatus := stIdle;
  ConfigurarSoapDEPC;
  FPArqEnv := 'Consul';
  FRetProcLote := TRetProcLote.Create(AOwner);
end;

procedure TConsultaLote.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
  LeitorXML := TLeitor.Create;
  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
  finally
    LeitorXML.Free;
  end;
  FPDadosMsg := FXMLEnvio;
end;

procedure TConsultaLote.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  { Sobrescrever apenas se necessário }

  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:EnviarLoteEventos>';
  Texto := Texto + '<' +  'v1:loteEventos>';
  Texto := Texto + StringReplace(DadosMsg , '<?xml version="1.0" encoding="utf-8"?>', '', []);
  Texto := Texto + '<' +  '/v1:loteEventos>';
  Texto := Texto + '<' +  '/v1:EnviarLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  Texto := '<?xml version="1.0" encoding="utf-8"?>' + Texto;
  FPEnvelopeSoap := Texto;
end;

procedure TConsultaLote.DefinirServicoEAction;
begin
  FPServico := 'http://www.esocial.gov.br/servicos/empregador/lote/eventos/envio/consulta/retornoProcessamento/v1_1_0/ServicoConsultarLoteEventos/ConsultarLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TConsultaLote.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  TACBreSocial(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');end;

function TConsultaLote.GerarMsgLog: String;
begin
  // Falta implementar
  Result := FPRetornoWS;
end;

function TConsultaLote.GerarPrefixoArquivo: String;
begin
  // Falta implementar
  Result := '';
end;

procedure TConsultaLote.GerarXML;
var  XML: AnsiString;
begin
  XML :=
              '<?xml version="1.0" encoding="utf-8"?>' +
              '<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/consulta/retornoProcessamento/v1_0_0">' +
              '  <consultaLoteEventos>' +
              '    <protocoloEnvio>' + FProtocolo + '</protocoloEnvio>' +
              '  </consultaLoteEventos>'+
              '</eSocial>';
 FXMLEnvio := XML; //Assinar(XML, 'consultaLoteEventos');
end;


procedure TConsultaLote.SalvarResposta;
var
  Path: string;
begin
  inherited;
  if TACBreSocial(Self.FPDFeOwner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBreSocial(Self.FPDFeOwner).Configuracoes.Arquivos.PathSalvar;
    with TStringList.Create do
    try
      Text := FPRetornoWS;
      SaveToFile(Path+'\'+'RespConsulta_Soap'+'-' + IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
      Text := FPRetWS;
      SaveToFile(Path+'\'+'RespConsulta'+'-'+  IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
  end;
end;

function TConsultaLote.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  i, j: integer;
  Processamento: TProcessamento;
  retEvento: TretEvento;
  Reader: TLeitor;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultarLoteEventosResponse');
  FXMlRet := FPRetWS;
  Result := FPRetWS <> EmptyStr;

  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := FPRetWS;

    Leitor.Grupo := Leitor.rExtrai(1, 'ideEmpregador');
    FRetProcLote.FIdeEmpregador.TpInsc := tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
    FRetProcLote.FIdeEmpregador.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

    Leitor.Grupo := Leitor.rExtrai(1, 'ideTransmissor');
    FRetProcLote.FIdeTransmissor.TpInsc := tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
    FRetProcLote.FIdeTransmissor.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

    Leitor.Grupo := Leitor.rExtrai(1, 'status');
    FRetProcLote.Status := StrToInt64Def(Leitor.rCampo(tcStr, 'cdResposta'), -1);
    FRetProcLote.Descricao := UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));
    if (FRetProcLote.Status in [200, 201]) then
    begin
      Leitor.Grupo := Leitor.rExtrai(1, 'dadosRecepcaoLote');
      try
//Compatibilizar        FRetProcLote.dadosRecLote.dhRecepcao := ISO8601ToDate(Leitor.rCampo(tcStr, 'dhRecepcao'));
      except
        FRetProcLote.dadosRecLote.dhRecepcao := 0;
      end;
      FRetProcLote.dadosRecLote.versaoAplicRecepcao := Leitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
      FRetProcLote.dadosRecLote.Protocolo  := Leitor.rCampo(tcStr, 'protocoloEnvio');

      Leitor.Arquivo := FPRetWS;
      Leitor.Grupo := Leitor.Arquivo;
      i:=0;
      while Leitor.rExtrai(1, 'retornoEventos', '', i + 1) <> '' do
      begin
        //recepcao
        Reader := TLeitor.Create;
        try
          Reader.Arquivo := Leitor.Grupo;
          retEvento := FRetProcLote.retEventos.Add;
          retEvento.IDEvento := Leitor.rAtributo('Id', 'evento');
          Reader.Grupo := Reader.rExtrai(1, 'recepcao');
          retEvento.FRecepcao.FtpAmb := TpTpAmb(Integer(Leitor.rCampo(tcInt, 'tpAmb')));
//Compatibilizar          retEvento.FRecepcao.FdhRecepcao :=  ISO8601ToDate(Leitor.rCampo(tcStr, 'dhRecepcao',''));
          retEvento.FRecepcao.FversaoAplicRecepcao := Leitor.rCampo(tcStr, 'versaoAppRecepcao');
          retEvento.FRecepcao.Fprotocolo := Leitor.rCampo(tcStr, 'protocoloEnvioLote');
          //processamento
          Reader.Grupo := Reader.rExtrai(1, 'processamento');
          retEvento.FProcessamento.FcdResposta :=  Leitor.rCampo(tcStr, 'cdResposta');
          retEvento.FProcessamento.FdescResposta := UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));
          retEvento.FProcessamento.versaoAplicProcLote := Leitor.rCampo(tcStr, 'versaoAppProcessamento');
//Compatibilizar          retEvento.FProcessamento.FdhProcessamento := ISO8601ToDate(Leitor.rCampo(tcStr, 'dhProcessamento'));
          //recibo
          Reader.Grupo := Reader.rExtrai(1, 'recibo');
          retEvento.FRecibo.FnrRecibo := Leitor.rCampo(tcStr, 'nrRecibo');
          retEvento.FRecibo.FHash := Leitor.rCampo(tcStr, 'hash');
          j := 0;
          Reader.Grupo := Reader.rExtrai(1, 'ocorrencias');
          Processamento := retEvento.FProcessamento;
          while Reader.rExtrai(1, 'ocorrencia', '', j + 1) <> '' do
          begin
            Processamento.Ocorrencias.Add;
            Processamento.Ocorrencias.Items[j].xml := Leitor.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Arquivo := Leitor.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Grupo := Leitor.Grupo;
            Processamento.Ocorrencias.Items[j].LerXml;
            inc(j);
          end;
          inc(i);
        finally
          Reader.Free;
        end;
      end;
    end
    else
    begin
      Leitor.Arquivo := Leitor.rExtrai(1, 'ocorrencias');
      Leitor.Grupo := Leitor.Arquivo;
      retEvento := FRetProcLote.retEventos.Add;
      Processamento := retEvento.FProcessamento;
      i:=0;
      while Leitor.rExtrai(1, 'ocorrencia', '', i + 1) <> '' do
      begin
        Processamento.Ocorrencias.Add;
        Processamento.Ocorrencias.Items[i].xml := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].FLeitor.Arquivo := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].FLeitor.Grupo := Leitor.Grupo;
        Processamento.Ocorrencias.Items[i].LerXml;
        inc(i);
      end;
    end;
  finally
    Leitor.Free;
  end;
end;

{ TProcessamento }

procedure TProcessamento.AfterConstruction;
begin
  inherited;
  FOcorrencias := TOcorrencias.Create(nil, TOcorrencia);
end;

procedure TProcessamento.BeforeDestruction;
begin
  inherited;
  FOcorrencias.Free;
end;

{ TretEvento }

procedure TretEvento.AfterConstruction;
begin
  inherited;
	FIDEvento := '';
  FRecepcao := TRecepcao.Create;
  FProcessamento := TProcessamento.Create;
  FRecibo := TRecibo.Create;
  FSignature := '';
end;

procedure TretEvento.BeforeDestruction;
begin
  inherited;
  FRecepcao.Free;
  FProcessamento.Free;
  FRecibo.free;
end;

end.
