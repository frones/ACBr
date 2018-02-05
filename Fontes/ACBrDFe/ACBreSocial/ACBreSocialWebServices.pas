{ ****************************************************************************** }
{ Projeto: Componente ACBreSocial }
{ Biblioteca multiplataforma de componentes Delphi para envio dos eventos do }
{ eSocial - http://www.esocial.gov.br/ }
{ }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto }
{ Daniel Simoes de Almeida }
{ André Ferreira de Moraes }
{ }
{ Colaboradores nesse arquivo: }
{ }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr }
{ }
{ }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }
{ }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }
{ }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }
{ }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
  |*  - Doação do componente para o Projeto ACBr
  |* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
  |*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
  ****************************************************************************** }

{$I ACBr.inc}

unit ACBreSocialWebServices;

interface

uses
  Classes, SysUtils,
  ACBrUtil, ACBrDFe, ACBrDFeWebService,
  pcnLeitor,
  ACBreSocialLoteEventos, ACBreSocialConfiguracoes,
  pcesConversaoeSocial, pcesCommon, pcesRetEnvioLote,
  pcesS5001, pcesS5002, pcesS5011, pcesS5012;

type

  TeSocialWebService = class(TDFeWebService)
  private
    FPStatus: TStatusACBreSocial;
    FPLayout: TLayOut;
    FPConfiguracoeseSocial: TConfiguracoeseSocial;
    procedure ConfigurarSoapDEPC;

  protected
    procedure InicializarServico; override;
    procedure FinalizarServico; override;
    procedure Clear; override;
    procedure DefinirURL; override;

    function GerarVersaoDadosSoap: String; override;
    function GerarPrefixoArquivo: String; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBreSocial read FPStatus;
    property Layout: TLayOut read FPLayout;

  end;

  { TStatusEnvLote }
  TStatusRetorno = class
  private
    FcdResposta: Integer;
    FdescResposta: String;
//    FOcorrencias: TOcorrencias;

  public
    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: String read FdescResposta write FdescResposta;
//    property Ocorrencias: TOcorrencias read FOcorrencias write FOcorrencias;

  end;

  TStatusEnvLote = class(TStatusRetorno);

  TStatusProcLote = class(TStatusRetorno)
  private
    FTmpConclusao: Integer;

  public
    property TmpConclusao: Integer read FTmpConclusao write FTmpConclusao;

  end;

  TRecepcao = class
  private
    FtpAmb: TptpAmb;
    FdhRecepcao: TDateTime;
    FversaoAplicRecepcao: String;
    FProtocolo: String;

  end;

  TRecibo = class
  public
    FnrRecibo: String;
    FHash: String;
  end;

  { TeSocialConsulta }

  TConsultaLote = class(TeSocialWebService)
  private
    FACBreSocial: TACBrDFe;
    FProtocolo: string;
    FXMLEnvio: AnsiString;
    FXMlRet: AnsiString;
    FRetProcLote: TRetProcLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;
    procedure SalvarResposta; override;
    procedure Clear; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

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
    FVersao: String;
    FLote: TLoteEventos;
    FRetEnvioLote: TRetEnvioLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
//    procedure SalvarEnvio; override;
//    procedure SalvarResposta; override;
    procedure DefinirEnvelopeSoap; override;
    procedure Clear; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBreSocial: TACBrDFe;
    FEnvioLote: TEnvioLote;
    FConsultaLote: TConsultaLote;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(AGrupo: TeSocialGrupo): Boolean; reintroduce;
    function Consultar(const AProtocolo: string): Boolean;

    property ACBreSocial: TACBrDFe read FACBreSocial write FACBreSocial;
    property EnvioLote: TEnvioLote read FEnvioLote write FEnvioLote;
    property ConsultaLote: TConsultaLote read FConsultaLote write FConsultaLote;
  end;

implementation

uses
  StrUtils, blcksock, DateUtils,
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
  FPMimeType := 'text/xml'; // Vazio, usará por default: 'application/soap+xml'
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

procedure TeSocialWebService.DefinirURL;
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
    FPVersaoServico := 'v2.1.0';
  // TACBreSocial(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := ''; // '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TeSocialWebService.Clear;
begin
  inherited Clear;

  FPStatus := stIdle;
  if Assigned(FPDFeOwner) and Assigned(FPDFeOwner.SSL) then
    FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TeSocialWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  // FPDFeOwner.SSL.SSLType := LT_TLSv1;
  TACBreSocial(FPDFeOwner).SetStatus(FPStatus);
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

  FLote := TLoteEventos.Create(AOwner);
end;

procedure TEnvioLote.Clear;
begin
  inherited Clear;

  FPStatus := stEnvLoteEventos;
  FPLayout := LayEnvioLoteEventos;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';
  FVersao := '';
  ConfigurarSoapDEPC;

  if Assigned(FRetEnvioLote) then
    FRetEnvioLote.Free;

  FRetEnvioLote := TRetEnvioLote.Create;
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
  FPServico := FPDFeOwner.GetNameSpaceURI +
    '/ServicoEnviarLoteEventos/EnviarLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TEnvioLote.DefinirDadosMsg;
begin
  FPDadosMsg := FLote.Xml;
end;

procedure TEnvioLote.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  { Sobrescrever apenas se necessário }

{$IFDEF FPC}
  Texto := '<' + ENCODING_UTF8 + '>'; // Envelope já está sendo montado em UTF8
{$ELSE}
  Texto := ''; // Isso forçará a conversão para UTF8, antes do envio
{$ENDIF}

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' +
    FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:EnviarLoteEventos>';
  Texto := Texto + '<' + 'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' + '/v1:loteEventos>';
  Texto := Texto + '<' + '/v1:EnviarLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

//  Texto := '<?xml version="1.0" encoding="utf-8"?>' + Texto;
  FPEnvelopeSoap := Texto;
end;

function TEnvioLote.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'EnviarLoteEventosResult');

  FRetEnvioLote.Leitor.Arquivo := ParseText(FPRetWS);
  FRetEnvioLote.LerXml;

  Result := True; //(FRetEnvioLote.cdResposta in [201, 202]);
end;

function TEnvioLote.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: ' + FPServico + #13#10 +
    '- Inativo ou Inoperante tente novamente.');
end;

function TEnvioLote.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
//                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 ['2.4.01',
//                  TpAmbToStr(FEventoRetorno.tpAmb),
                  FRetEnvioLote.RetProcLote.dadosRecLote.versaoAplicRecepcao,
                  IntToStr(FRetEnvioLote.cdResposta),
                  FRetEnvioLote.descResposta]);

//  if FEventoRetorno.retEvento.Count > 0 then
//    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
//       [IfThen(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento = 0, '',
//               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento))]);

  Result := aMsg;
end;

function TEnvioLote.GerarPrefixoArquivo: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TEnvioLote.GerarVersaoDadosSoap: String;
begin
  Result := ''; // '<versaoDados>' + FVersao + '</versaoDados>';
end;
(*
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
        SaveToFile(Path + '\' + 'Envio_Soap' + '-' + IntTostr(HourOf(Now)) +
          IntTostr(MinuteOf(Now)) + IntTostr(SecondOf(Now)) + '_' +
          IntTostr(MilliSecondOf(Now)) + '.xml');
      finally
        Free;
      end;
  end;
end;
*)
(*
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
        SaveToFile(Path + '\' + 'Resp_Soap' + '-' + IntTostr(HourOf(Now)) +
          IntTostr(MinuteOf(Now)) + IntTostr(SecondOf(Now)) + '_' +
          IntTostr(MilliSecondOf(Now)) + '.xml');
        Text := FPRetWS;
        SaveToFile(Path + '\' + 'Resp' + '-' + IntTostr(HourOf(Now)) +
          IntTostr(MinuteOf(Now)) + IntTostr(SecondOf(Now)) + '_' +
          IntTostr(MilliSecondOf(Now)) + '.xml');
      finally
        Free;
      end;
  end;
end;
*)
{ TWebServices }

function TWebServices.Consultar(const AProtocolo: string): Boolean;
begin
  FConsultaLote.FProtocolo := AProtocolo;
  FConsultaLote.GerarXML;

  if Assigned(TACBreSocial(FACBreSocial).OnTransmissaoEventos) then
    TACBreSocial(FACBreSocial).OnTransmissaoEventos(ConsultaLote.XMLEnvio,
      eseEnvioConsulta);

  try
    if not FConsultaLote.Executar then
      FConsultaLote.GerarException(FConsultaLote.Msg);
    Result := True;
  except
    raise;
  end;

  if Assigned(TACBreSocial(FACBreSocial).OnTransmissaoEventos) then
    TACBreSocial(FACBreSocial).OnTransmissaoEventos(ConsultaLote.XMlRet,
      eseRetornoConsulta);
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

function TWebServices.Envia(AGrupo: TeSocialGrupo): Boolean;
begin

  with EnvioLote.FLote.IdeEmpregador do
  begin
    TpInsc := tiCNPJ;
    NrInsc := TACBreSocial(FACBreSocial).Configuracoes.Geral.IdEmpregador;
  end;

  with EnvioLote.FLote.IdeTransmissor do
  begin
    TpInsc := tiCNPJ;
    NrInsc := TACBreSocial(FACBreSocial).Configuracoes.Geral.IdTransmissor;
  end;

  EnvioLote.FLote.GerarXML(AGrupo);

  if Assigned(TACBreSocial(FACBreSocial).OnTransmissaoEventos) then
    TACBreSocial(FACBreSocial).OnTransmissaoEventos(EnvioLote.FLote.xml,
      eseEnvioLote);

  if not EnvioLote.Executar then
    EnvioLote.GerarException(EnvioLote.Msg);

  if Assigned(TACBreSocial(FACBreSocial).OnTransmissaoEventos) then
    TACBreSocial(FACBreSocial).OnTransmissaoEventos(EnvioLote.RetornoWS,
      eseRetornoLote);

  Result := True;
end;

{ TConsultaLote }

constructor TConsultaLote.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
  FACBreSocial := AOwner;
  FPLayout := LayConsultaLoteEventos;
  FPStatus := stIdle;
  ConfigurarSoapDEPC;
  FPArqEnv := 'Consul';
//  FRetProcLote := TRetProcLote.Create(AOwner);
end;

procedure TConsultaLote.Clear;
begin
  inherited Clear;

  FPStatus := stConsultaLote;
end;

procedure TConsultaLote.BeforeDestruction;
begin
  inherited;
//  FRetProcLote.Free;
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
  Texto := '<' + ENCODING_UTF8 + '>'; // Envelope já está sendo montado em UTF8
{$ELSE}
  Texto := ''; // Isso forçará a conversão para UTF8, antes do envio
{$ENDIF}

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' +
    FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:EnviarLoteEventos>';
  Texto := Texto + '<' + 'v1:loteEventos>';
  Texto := Texto + StringReplace(DadosMsg,
    '<?xml version="1.0" encoding="utf-8"?>', '', []);
  Texto := Texto + '<' + '/v1:loteEventos>';
  Texto := Texto + '<' + '/v1:EnviarLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

//  Texto := '<?xml version="1.0" encoding="utf-8"?>' + Texto;
  FPEnvelopeSoap := Texto;
end;

procedure TConsultaLote.DefinirServicoEAction;
begin
  FPServico :=
    'http://www.esocial.gov.br/servicos/empregador/lote/eventos/envio/consulta/retornoProcessamento/v1_1_0/ServicoConsultarLoteEventos/ConsultarLoteEventos';
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
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

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
var
  xml: AnsiString;
begin
  xml := '<?xml version="1.0" encoding="utf-8"?>' +
         '<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/consulta/retornoProcessamento/v1_0_0">' +
          '<consultaLoteEventos>' +
           '<protocoloEnvio>' + FProtocolo + '</protocoloEnvio>' +
          '</consultaLoteEventos>' +
         '</eSocial>';

  FXMLEnvio := xml;
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
        SaveToFile(Path + '\' + 'RespConsulta_Soap' + '-' + IntTostr(HourOf(Now)
          ) + IntTostr(MinuteOf(Now)) + IntTostr(SecondOf(Now)) + '_' +
          IntTostr(MilliSecondOf(Now)) + '.xml');
        Text := FPRetWS;
        SaveToFile(Path + '\' + 'RespConsulta' + '-' + IntTostr(HourOf(Now)) +
          IntTostr(MinuteOf(Now)) + IntTostr(SecondOf(Now)) + '_' +
          IntTostr(MilliSecondOf(Now)) + '.xml');
      finally
        Free;
      end;
  end;
end;

function TConsultaLote.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  i, j: Integer;
//  Processamento: TProcessamento;
//  retEvento: TretEvento;
  Reader: TLeitor;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultarLoteEventosResponse');
  FXMlRet := FPRetWS;
  Result := FPRetWS <> EmptyStr;
  Leitor := TLeitor.Create;
  (*
  try
    FRetProcLote.retEventos.Clear;
    Leitor.Arquivo := FPRetWS;

    Leitor.Grupo := Leitor.rExtrai(1, 'ideEmpregador');
    FRetProcLote.FIdeEmpregador.TpInsc :=
      tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
    FRetProcLote.FIdeEmpregador.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

    Leitor.Grupo := Leitor.rExtrai(1, 'ideTransmissor');
    FRetProcLote.FIdeTransmissor.TpInsc :=
      tpTpInsc(Leitor.rCampo(tcInt, 'tpInsc') - 1);
    FRetProcLote.FIdeTransmissor.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');

    Leitor.Grupo := Leitor.rExtrai(1, 'status');
    FRetProcLote.Status :=
      StrToInt64Def(Leitor.rCampo(tcStr, 'cdResposta'), -1);
    FRetProcLote.Descricao := UTF8ToNativeString
      (Leitor.rCampo(tcStr, 'descResposta'));
    if (FRetProcLote.Status in [200, 201]) then
    begin
      Leitor.Grupo := Leitor.rExtrai(1, 'dadosRecepcaoLote');
      try
        FRetProcLote.dadosRecLote.dhRecepcao :=
          Leitor.rCampo(tcDatHor, 'dhRecepcao');
      except
        FRetProcLote.dadosRecLote.dhRecepcao := 0;
      end;
      FRetProcLote.dadosRecLote.versaoAplicRecepcao :=
        Leitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
      FRetProcLote.dadosRecLote.Protocolo :=
        Leitor.rCampo(tcStr, 'protocoloEnvio');

      Leitor.Grupo := Leitor.rExtrai(1, 'dadosProcessamentoLote');
      FRetProcLote.dadosProcLote.versaoAplicProcLote :=
        Leitor.rCampo(tcStr, 'versaoAplicativoProcessamentoLote');

      Leitor.Arquivo := Leitor.rExtrai(1, 'retornoEventos');
      i := 0;
      while Leitor.rExtrai(1, 'evento', '', i + 1) <> '' do
      begin
        // recepcao
        Reader := TLeitor.Create;
        try
          Reader.Arquivo := Leitor.Grupo;
          retEvento := FRetProcLote.retEventos.Add;
          retEvento.IDEvento := Leitor.rAtributo('Id', 'evento');
          Reader.Grupo := Reader.rExtrai(1, 'recepcao');
          retEvento.FRecepcao.FtpAmb :=
            TptpAmb(Integer(Leitor.rCampo(tcInt, 'tpAmb')));
          retEvento.FRecepcao.FdhRecepcao :=
            Leitor.rCampo(tcDatHor, 'dhRecepcao', '');
          retEvento.FRecepcao.FversaoAplicRecepcao :=
            Leitor.rCampo(tcStr, 'versaoAppRecepcao');
          retEvento.FRecepcao.FProtocolo :=
            Leitor.rCampo(tcStr, 'protocoloEnvioLote');
          // processamento
          Reader.Grupo := Reader.rExtrai(1, 'processamento');
          retEvento.FProcessamento.FcdResposta :=
            Leitor.rCampo(tcStr, 'cdResposta');
          retEvento.FProcessamento.FdescResposta :=
            UTF8ToNativeString(Leitor.rCampo(tcStr, 'descResposta'));
          retEvento.FProcessamento.versaoAplicProcLote :=
            Leitor.rCampo(tcStr, 'versaoAppProcessamento');
          retEvento.FProcessamento.FdhProcessamento :=
            Leitor.rCampo(tcDatHor, 'dhProcessamento');
          // recibo
          Reader.Grupo := Reader.rExtrai(1, 'recibo');
          retEvento.FRecibo.FnrRecibo := Leitor.rCampo(tcStr, 'nrRecibo');
          retEvento.FRecibo.FHash := Leitor.rCampo(tcStr, 'hash');
          Processamento := retEvento.FProcessamento;

          j := 0;
          Reader.Arquivo := Reader.rExtrai(1, 'ocorrencias');
          while Reader.rExtrai(1, 'ocorrencia', '', j + 1) <> '' do
          begin
            Processamento.Ocorrencias.Add;
            Processamento.Ocorrencias.Items[j].xml := Reader.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Arquivo := Reader.Grupo;
            Processamento.Ocorrencias.Items[j].FLeitor.Grupo := Reader.Grupo;
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
      i := 0;
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
  *)
end;

end.
