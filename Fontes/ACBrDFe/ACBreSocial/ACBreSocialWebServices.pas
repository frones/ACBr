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
  pcesConversaoeSocial, pcesCommon, pcesRetEnvioLote, pcesRetConsultaLote,
  pcesS5001, pcesS5002, pcesS5011, pcesS5012;

type

  { TeSocialWebService }

  TeSocialWebService = class(TDFeWebService)
  private
    FPStatus: TStatusACBreSocial;
    FPLayout: TLayOut;
    FPConfiguracoeseSocial: TConfiguracoeseSocial;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBreSocial read FPStatus;
    property Layout: TLayOut read FPLayout;
  end;

  { TEnvioLote }

  TEnvioLote = class(TeSocialWebService)
  private
    AGrupo: TeSocialGrupo;
    FVersao: String;
    FLote: TLoteEventos;
    FRetEnvioLote: TRetEnvioLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

    procedure Clear; override;
    procedure BeforeDestruction; override;

    property Grupo: TeSocialGrupo read AGrupo write AGrupo;
    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
  end;

  { TConsultaLote }

  TConsultaLote = class(TeSocialWebService)
  private
    FProtocolo: string;
    FRetConsultaLote: TRetConsultaLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    procedure Clear; override;
    procedure BeforeDestruction; override;

    property Protocolo: string read FProtocolo write FProtocolo;
    property RetConsultaLote: TRetConsultaLote read FRetConsultaLote;

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

    function Envia(AGrupo: TeSocialGrupo): Boolean;
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

  FPSoapVersion   := 'soap';
  FPHeaderElement := 'Header';
  FPBodyElement   := 'Body';

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1"';

  FPMimeType := 'text/xml'; // Vazio, usará por default: 'application/soap+xml'
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

  FPLayout := LayEnvioLoteEventos;
  FPStatus := stEnvLoteEventos;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';
  FVersao := '';

  if Assigned(FRetEnvioLote) then
    FRetEnvioLote.Free;

  FRetEnvioLote := TRetEnvioLote.Create;
end;

procedure TEnvioLote.BeforeDestruction;
begin
  inherited;

  FRetEnvioLote.Free;
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

  with FLote.IdeEmpregador do
  begin
    TpInsc := tiCNPJ;
    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdEmpregador;
  end;

  with FLote.IdeTransmissor do
  begin
    TpInsc := tiCNPJ;
    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdTransmissor;
  end;

  FLote.GerarXML(AGrupo);

  FPDadosMsg := FLote.Xml;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, eseEnvioLote);
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

  FPEnvelopeSoap := Texto;
end;

function TEnvioLote.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'EnviarLoteEventosResult');

  FRetEnvioLote.Leitor.Arquivo := ParseText(FPRetWS);
  FRetEnvioLote.LerXml;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPRetWS, eseRetornoLote);

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
                  FRetEnvioLote.dadosRecLote.versaoAplicRecepcao,
                  IntToStr(FRetEnvioLote.Status.cdResposta),
                  FRetEnvioLote.Status.descResposta]);

    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FRetEnvioLote.dadosRecLote.dhRecepcao = 0, '',
               FormatDateTimeBr(FRetEnvioLote.dadosRecLote.dhRecepcao))]);

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

{ TConsultaLote }

constructor TConsultaLote.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
end;

procedure TConsultaLote.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaLoteEventos;
  FPStatus := stConsultaLote;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  if Assigned(FRetConsultaLote) then
    FRetConsultaLote.Free;

  FRetConsultaLote := TRetConsultaLote.Create;
end;

procedure TConsultaLote.BeforeDestruction;
begin
  inherited;
  
  FRetConsultaLote.Free;
end;

procedure TConsultaLote.DefinirDadosMsg;
begin
  FPDadosMsg :=
         '<eSocial xmlns="' + ACBRESOCIAL_NAMESPACE_CON + '">' +
          '<consultaLoteEventos>' +
           '<protocoloEnvio>' + FProtocolo + '</protocoloEnvio>' +
          '</consultaLoteEventos>' +
         '</eSocial>';

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, eseEnvioConsulta);
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
  Texto := Texto + '<' + 'v1:ConsultarLoteEventos>';
  Texto := Texto + '<' + 'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' + '/v1:loteEventos>';
  Texto := Texto + '<' + '/v1:ConsultarLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

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
                  FRetConsultaLote.dadosRecLote.versaoAplicRecepcao,
                  IntToStr(FRetConsultaLote.Status.cdResposta),
                  FRetConsultaLote.Status.descResposta]);

    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FRetConsultaLote.dadosRecLote.dhRecepcao = 0, '',
               FormatDateTimeBr(FRetConsultaLote.dadosRecLote.dhRecepcao))]);

  Result := aMsg;
end;

function TConsultaLote.GerarPrefixoArquivo: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TConsultaLote.TratarResposta: Boolean;
var
  I, J: Integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultarLoteEventosResult');

  FRetConsultaLote.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsultaLote.LerXml;

  for I := 0 to FRetConsultaLote.RetEventos.Count - 1 do
  begin
    for J := 0 to FRetConsultaLote.RetEventos.Items[I].tot.Count -1 do
    begin
      AXML := FRetConsultaLote.RetEventos.Items[I].tot.Items[J].XML;

      if AXML <> '' then
      begin
        NomeArq := FRetConsultaLote.RetEventos.Items[I].tot.Items[J].tipo + '.xml';

        if (FPConfiguracoeseSocial.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
          FPDFeOwner.Gravar(NomeArq, AXML);
      end;
    end;
  end;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPRetWS, eseRetornoConsulta);

  Result := True; //(FRetEnvioLote.cdResposta in [201, 202]);
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBreSocial := TACBrDFe(AOwner);

  FEnvioLote    := TEnvioLote.Create(FACBreSocial);
  FConsultaLote := TConsultaLote.Create(FACBreSocial);
end;

destructor TWebServices.Destroy;
begin
  FEnvioLote.Free;
  FConsultaLote.Free;

  inherited Destroy;
end;

function TWebServices.Envia(AGrupo: TeSocialGrupo): Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}

  EnvioLote.Grupo := AGrupo;

  if not EnvioLote.Executar then
    EnvioLote.GerarException(EnvioLote.Msg);

  Result := True;
end;

function TWebServices.Consultar(const AProtocolo: string): Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultaLote.FProtocolo := AProtocolo;

  if not FConsultaLote.Executar then
      FConsultaLote.GerarException(FConsultaLote.Msg);

  Result := True;
end;

end.
