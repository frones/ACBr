{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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
  ACBrDFe, ACBrDFeWebService,
  pcnLeitor,
  ACBrDFeConsts,
  ACBreSocialLoteEventos, ACBreSocialConfiguracoes,
  pcesConversaoeSocial, pcesCommon, pcesRetEnvioLote, pcesRetConsultaLote,
  pcesConsultaIdentEvt, pcesRetConsultaIdentEvt, pcesRetDownloadEvt;

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
    FGrupo: TeSocialGrupo;
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

    property Grupo: TeSocialGrupo read FGrupo write FGrupo;
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

  public
    constructor Create(AOwner: TACBrDFe); override;

    procedure Clear; override;
    procedure BeforeDestruction; override;

    property Protocolo: string read FProtocolo write FProtocolo;
    property RetConsultaLote: TRetConsultaLote read FRetConsultaLote;

  end;

  { TConsultaIdentEventos }

  TConsultaIdentEventos = class(TeSocialWebService)
  private
    FtipoConsulta: tpConsulta;
    FCnpj: String;
    FEvento: TTipoEvento;
    FPerApur: TDateTime;
    FchEvt: String;
    FdtIni: TDateTime;
    FdtFim: TDateTime;
    FcpfTrab: String;
    FmetodoConsulta: String;

    FRetConsultaIdentEvt: TRetConsultaIdentEvt;
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

    property tipoConsulta: tpConsulta read FtipoConsulta write FtipoConsulta;
    property Cnpj: String read FCnpj write FCnpj;
    property Evento: TTipoEvento read FEvento write FEvento;
    property PerApur: TDateTime read FPerApur write FPerApur;
    property chEvt: String read FchEvt write FchEvt;
    property dtIni: TDateTime read FdtIni write FdtIni;
    property dtFim: TDateTime read FdtFim write FdtFim;
    property cpfTrab: String read FcpfTrab write FcpfTrab;

    property RetConsultaIdentEvt: TRetConsultaIdentEvt read FRetConsultaIdentEvt;
  end;

  { TDownloadEventos }

  TDownloadEventos = class(TeSocialWebService)
  private
    FTipoDownload: string;
    FCnpj : String;
    FPorID: String;
    FPorNrRecibo: String;
    FRetDownloadEvt: TRetDownloadEvt;
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
    property Cnpj: String read FCnpj write FCnpj;

    property PorID: String read FPorID write FPorID;
    property PorNrRecibo: String read FPorNrRecibo write FPorNrRecibo;
    property RetDownloadEvt: TRetDownloadEvt read FRetDownloadEvt;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBreSocial: TACBrDFe;
    FEnvioLote: TEnvioLote;
    FConsultaLote: TConsultaLote;
    FConsultaIdentEventos: TConsultaIdentEventos;
    FDownloadEventos: TDownloadEventos;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(AGrupo: TeSocialGrupo): Boolean;
    function GeraLote(AGrupo: TeSocialGrupo;FlSalvar:boolean = True): Boolean;


    function Consultar(const AProtocolo: string): Boolean;
    function ConsultaIdentificadoresEventosEmpregador(const CnpjEstab: String;
        tpEvt : TTipoEvento; PerApur : TDateTime): boolean;
    function ConsultaIdentificadoresEventosTabela(const CnpjEstab: String;
        tpEvt: TTipoEvento; const AchEvt: string; AdtIni, AdtFim: TDateTime): boolean;
    function ConsultaIdentificadoresEventosTrabalhador(const CnpjEstab: String;
        const AcpfTrab: string; AdtIni, AdtFim: TDateTime): boolean;
    function DownloadEvento(const ACnpjEmpr, APorID, APorNrRecibo: String): boolean;

    property ACBreSocial: TACBrDFe read FACBreSocial write FACBreSocial;
    property EnvioLote: TEnvioLote read FEnvioLote write FEnvioLote;
    property ConsultaLote: TConsultaLote read FConsultaLote write FConsultaLote;
    property ConsultaIdentEventos: TConsultaIdentEventos read FConsultaIdentEventos write FConsultaIdentEventos;
    property DownloadEventos: TDownloadEventos read FDownloadEventos write FDownloadEventos;
  end;

implementation

uses
  StrUtils, blcksock, DateUtils,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
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
  Result := FormatDateTime('yyyymmddhhnnss', Now);
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
  FLote.Free;
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
    if ( Length(TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdEmpregador) = 14 ) or
       ( TACBreSocial(FPDFeOwner).Configuracoes.Geral.TipoEmpregador <> tePessoaFisica ) then
      TpInsc := tiCNPJ
    else
      TpInsc := tiCPF;

    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdEmpregador;

    OrgaoPublico := TACBreSocial(FPDFeOwner).Configuracoes.Geral.TipoEmpregador in [tePessoaFisica,
                                                                                    teOrgaoPublicoExecutivoFederal, teOrgaoPublicoLegislativoFederal,
                                                                                    teOrgaoPublicoJudiciarioFederal, teOrgaoPublicoAutonomoFederal];
  end;

  with FLote.IdeTransmissor do
  begin
    if Length(TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdTransmissor) = 14 then
      TpInsc := tiCNPJ
    else
      TpInsc := tiCPF;

    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdTransmissor;
  end;

  FLote.GerarXML(FGrupo);

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

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1"';

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
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoeSocialToStrSchemas(TACBreSocial(FPDFeOwner).Configuracoes.Geral.VersaoDF),
                  TpAmbToStr(TACBreSocial(FPDFeOwner).Configuracoes.WebServices.Ambiente),
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
  Result := FormatDateTime('yymmddhhnnss', Now);
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

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1"';

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
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoeSocialToStrSchemas(TACBreSocial(FPDFeOwner).Configuracoes.Geral.VersaoDF),
                  TpAmbToStr(TACBreSocial(FPDFeOwner).Configuracoes.WebServices.Ambiente),
                  FRetConsultaLote.dadosRecLote.versaoAplicRecepcao,
                  IntToStr(FRetConsultaLote.Status.cdResposta),
                  FRetConsultaLote.Status.descResposta]);

    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FRetConsultaLote.dadosRecLote.dhRecepcao = 0, '',
               FormatDateTimeBr(FRetConsultaLote.dadosRecLote.dhRecepcao))]);

  Result := aMsg;
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
        NomeArq := FRetConsultaLote.RetEventos.Items[I].Id + '-' +
                   FRetConsultaLote.RetEventos.Items[I].tot.Items[J].tipo + '.xml';

        if (FPConfiguracoeseSocial.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
          FPDFeOwner.Gravar(NomeArq, AXML);
      end;
    end;
  end;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPRetWS, eseRetornoConsulta);

  Result := True;
end;

{ TConsultaIdentEventos }

procedure TConsultaIdentEventos.BeforeDestruction;
begin
  inherited;
  FRetConsultaIdentEvt.Free;
end;

procedure TConsultaIdentEventos.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaIdentEventos;
  FPStatus := stConsultaIdentEvt;
  FPArqEnv := 'ped-con';
  FPArqResp := 'con';

  if Assigned(FRetConsultaIdentEvt) then
    FRetConsultaIdentEvt.Free;

  FRetConsultaIdentEvt := TRetConsultaIdentEvt.Create;
end;

constructor TConsultaIdentEventos.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
end;

procedure TConsultaIdentEventos.DefinirDadosMsg;
var
  Consulta: TConsultaIdentEvt;
  ArqXSD: string;
  TpInsc : tpTpInsc;
//  DadosMsg: AnsiString;
  Erro : string;
  EhValido : Boolean;
begin

  // Deve ser enviado somente a raiz do CNPJ
  if Length(FCnpj) = 14 then
    TpInsc := tiCNPJ
  else
    TpInsc := tiCPF;

  Consulta := TConsultaIdentEvt.Create;
  try
//    Consulta.SoapEnvelope := ACBRESOCIAL_NAMESPACE_RETEVT;

    case tipoConsulta of
        tcEmpregador:
          begin
            Consulta.SoapEnvelope := ACBRESOCIAL_NAMESPACE_CONS_EMP;
          end;
          tcTabela:
          begin
            Consulta.SoapEnvelope := ACBRESOCIAL_NAMESPACE_CONS_TAB;
          end;
          tcTrabalhador:
          begin
            Consulta.SoapEnvelope := ACBRESOCIAL_NAMESPACE_CONS_TRA;
          end;
    end;

    Consulta.tipoConsulta := tipoConsulta;

    Consulta.tpInsc := eSTpInscricaoToStr(TpInsc);

    case TpInsc of
      tiCNPJ:
        begin
          Consulta.nrInsc := copy(Cnpj, 0, 8);
        end;
      tiCPF:
        begin
          Consulta.nrInsc := Cnpj;
        end;
    end;
    Consulta.TipoEvento := Evento;
    Consulta.perApur := FormatDateTime('yyyy-mm', FPerApur);
    Consulta.chEvt := chEvt;
    Consulta.dtIni := dtIni;
    Consulta.dtFim := dtFim;
    Consulta.cpfTrab := cpfTrab;

    AjustarOpcoes( Consulta.Gerador.Opcoes );
    Consulta.GerarXML;

    // Atribuindo o XML para propriedade interna //
    FPDadosMsg := Consulta.Gerador.ArquivoFormatoXML;
  finally
    Consulta.Free;
  end;

  (*
  FPDadosMsg :=
         '<eSocial xmlns="' + ACBRESOCIAL_NAMESPACE_RETEVT + '">' +
          '<consultaIdentificadoresEvts>' +
           '<ideEmpregador>' +
            '<tpInsc>' + eSTpInscricaoToStr(TpInsc) + '</tpInsc>' +
            '<nrInsc>' + FCnpj + '</nrInsc>' +
           '</ideEmpregador>';

  if FtpConsulta = 'Empregador' then
    FPDadosMsg := FPDadosMsg +
             '<consultaEvtsEmpregador>' +
               '<tpEvt>' + TipoEventoToStr(FEvento) + '</tpEvt>' +
               '<perApur>' + FormatDateTime('yyyy-mm', FPerApur) + '</perApur>' +
             '</consultaEvtsEmpregador>';

  if FtpConsulta = 'Tabela' then
    FPDadosMsg := FPDadosMsg +
             '<consultaEvtsTabela>' +
               '<tpEvt>' + TipoEventoToStr(FEvento) + '</tpEvt>' +
               '<chEvt>' + FchEvt + '</chEvt>' +
               '<dtIni>' + FormatDateTime('yyyy-mm-dd', FdtIni) + '</dtIni>' +
               '<dtFim>' + FormatDateTime('yyyy-mm-dd', FdtFim) + '</dtFim>' +
             '</consultaEvtsTabela>';

  FPDadosMsg := FPDadosMsg +
          '</consultaIdentificadoresEvts>' +
         '</eSocial>';
  *)
//  FPDadosMsg := AnsiToUtf8(DadosMsg);

  with TACBreSocial(FPDFeOwner) do
  begin
    FPDadosMsg := SSL.Assinar(String(FPDadosMsg),
                    'eSocial', 'consultaIdentificadoresEvts', '', '', '', 'ID');

    case tipoConsulta of
      tcEmpregador: ArqXSD := 'ConsultaIdentificadoresEventosEmpregador-v1_0_0.xsd';
      tcTabela: ArqXSD := 'ConsultaIdentificadoresEventosTabela-v1_0_0.xsd';
      tcTrabalhador: ArqXSD := 'ConsultaIdentificadoresEventosTrabalhador-v1_0_0.xsd';
    end;

    EhValido := SSL.Validar(String(FPDadosMsg),
                   Configuracoes.Arquivos.PathSchemas + ArqXSD, Erro);

    if not EhValido then
      raise EACBreSocialException.CreateDef(Erro);

    if Assigned(OnTransmissaoEventos) then
      OnTransmissaoEventos(FPDadosMsg, eseEnvioConsultaIdentEvt);
  end;
end;

procedure TConsultaIdentEventos.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  { Sobrescrever apenas se necessário }

{$IFDEF FPC}
  Texto := '<' + ENCODING_UTF8 + '>'; // Envelope já está sendo montado em UTF8
{$ELSE}
  Texto := ''; // Isso forçará a conversão para UTF8, antes do envio
{$ENDIF}

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/servicos/empregador/consulta/identificadores-eventos/v1_0_0"';

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' +
    FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<v1:ConsultarIdentificadoresEventos' + FmetodoConsulta + '>';
  Texto := Texto + '<v1:consultaEventos' + FmetodoConsulta + '>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '</v1:consultaEventos' + FmetodoConsulta + '>';
  Texto := Texto + '</v1:ConsultarIdentificadoresEventos' + FmetodoConsulta + '>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TConsultaIdentEventos.DefinirServicoEAction;
begin
  FPServico :=
    'http://www.esocial.gov.br/servicos/empregador/consulta/identificadores-eventos/v1_0_0/ServicoConsultarIdentificadoresEventos/ConsultarIdentificadoresEventos' + FmetodoConsulta;
  FPSoapAction := Trim(FPServico);
end;

procedure TConsultaIdentEventos.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  TACBreSocial(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TConsultaIdentEventos.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoeSocialToStrSchemas(TACBreSocial(FPDFeOwner).Configuracoes.Geral.VersaoDF),
                  TpAmbToStr(TACBreSocial(FPDFeOwner).Configuracoes.WebServices.Ambiente),
                  IntToStr(FRetConsultaIdentEvt.Status.cdResposta),
                  FRetConsultaIdentEvt.Status.descResposta]);

  Result := aMsg;
end;

function TConsultaIdentEventos.GerarPrefixoArquivo: String;
begin
  Result := TipoEventoToStr(FEvento) + '-' +
            fCnpj + '-' + FormatDateTime('mm-yyyy', FPerApur) + '-' +
            FormatDateTime('yyyymmddhhnnss', Now);
end;

function TConsultaIdentEventos.TratarResposta: Boolean;
var
  i: Integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultarIdentificadoresEventos' + FmetodoConsulta + 'Result');

  FRetConsultaIdentEvt.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsultaIdentEvt.LerXml;

  for i := 0 to FRetConsultaIdentEvt.RetIdentEvts.Count - 1 do
  begin
    AXML := FPRetWS;

    if AXML <> '' then
    begin
      NomeArq := FRetConsultaIdentEvt.RetIdentEvts.Items[i].Id + '-' +
                 TipoEventoToStr(FEvento) +  Cnpj + FormatDateTime('mm-yyyy', FPerApur) + '.xml';

      if (FPConfiguracoeseSocial.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML);
    end;
  end;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPRetWS, eseRetornoConsultaIdentEvt);

  Result := True;
end;

{ TDownloadEventos }

procedure TDownloadEventos.BeforeDestruction;
begin
  inherited;

  FRetDownloadEvt.Free;
end;

procedure TDownloadEventos.Clear;
begin
  inherited Clear;

  FPLayout := LayDownloadEventos;
  FPStatus := stDownloadEvt;
  FPArqEnv := 'ped-dow';
  FPArqResp := 'dow';

  if Assigned(FRetDownloadEvt) then
    FRetDownloadEvt.Free;

  FRetDownloadEvt := TRetDownloadEvt.Create;
end;

constructor TDownloadEventos.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
end;

procedure TDownloadEventos.DefinirDadosMsg;
var
  TpInsc: tpTpInsc;
  NameSpace, Erro, ArqXSD: string;
  EhValido : boolean;
begin

  if Length(FCnpj) = 14 then
  begin
    TpInsc := tiCNPJ;
    FCnpj  := Copy(FCnpj, 1, 8);
  end
  else
    TpInsc := tiCPF;

  if FPorID <> '' then
  begin
    NameSpace := ACBRESOCIAL_NAMESPACE_DOWEVTID;
    ArqXSD    := 'SolicitacaoDownloadEventosPorId-v1_0_0.xsd';
  end
  else
  begin
    NameSpace := ACBRESOCIAL_NAMESPACE_DOWEVTREC;
    ArqXSD    := 'SolicitacaoDownloadEventosPorNrRecibo-v1_0_0.xsd';
  end;

  FPDadosMsg :=
         '<eSocial xmlns="' + NameSpace + '">' +
          '<download>' +
           '<ideEmpregador>' +
            '<tpInsc>' + eSTpInscricaoToStr(TpInsc) + '</tpInsc>' +
            '<nrInsc>' + FCnpj + '</nrInsc>' +
           '</ideEmpregador>';

  if FPorID <> '' then
    FPDadosMsg := FPDadosMsg +
                  '<solicDownloadEvtsPorId>' +
                    '<id>' + FPorID + '</id>' + // Pode ser uma lista
                  '</solicDownloadEvtsPorId>'
  else
    FPDadosMsg := FPDadosMsg +
                  '<solicDownloadEventosPorNrRecibo>' +
                    '<nrRec>' + FPorNrRecibo + '</nrRec>' + // Pode ser uma lista
                  '</solicDownloadEventosPorNrRecibo>';

  FPDadosMsg := FPDadosMsg +
          '</download>' +
         '</eSocial>';

  with TACBreSocial(FPDFeOwner) do
  begin
    FPDadosMsg := SSL.Assinar(String(FPDadosMsg),
                                        'eSocial', 'eSocial', '', '', '', 'ID');

    EhValido := SSL.Validar(String(FPDadosMsg),
                   Configuracoes.Arquivos.PathSchemas + ArqXSD, Erro);

    if not EhValido then
      raise EACBreSocialException.CreateDef(Erro);

    if Assigned(OnTransmissaoEventos) then
      OnTransmissaoEventos(FPDadosMsg, eseEnvioDownloadEvt);
  end;
end;

procedure TDownloadEventos.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  { Sobrescrever apenas se necessário }

{$IFDEF FPC}
  Texto := '<' + ENCODING_UTF8 + '>'; // Envelope já está sendo montado em UTF8
{$ELSE}
  Texto := ''; // Isso forçará a conversão para UTF8, antes do envio
{$ENDIF}

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://www.esocial.gov.br/servicos/empregador/download/solicitacao/v1_0_0"';

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' +
    FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:SolicitarDownloadEventos' + FTipoDownload + '>';
  Texto := Texto + '<' + 'v1:solicitacao>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' + '/v1:solicitacao>';
  Texto := Texto + '<' + '/v1:SolicitarDownloadEventos' + FTipoDownload + '>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TDownloadEventos.DefinirServicoEAction;
begin
  FPServico :=
     'http://www.esocial.gov.br/servicos/empregador/download/solicitacao/v1_0_0/ServicoSolicitarDownloadEventos/SolicitarDownloadEventos' + FTipoDownload;

  FPSoapAction := Trim(FPServico);
end;

procedure TDownloadEventos.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  TACBreSocial(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TDownloadEventos.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoeSocialToStrSchemas(TACBreSocial(FPDFeOwner).Configuracoes.Geral.VersaoDF),
                  TpAmbToStr(TACBreSocial(FPDFeOwner).Configuracoes.WebServices.Ambiente),
                  IntToStr(FRetDownloadEvt.Status.cdResposta),
                  FRetDownloadEvt.Status.descResposta]);

  Result := aMsg;
end;

function TDownloadEventos.GerarPrefixoArquivo: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TDownloadEventos.TratarResposta: Boolean;
var
  i: Integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'SolicitarDownloadEventos' +FTipoDownload + 'Result');

  FRetDownloadEvt.Leitor.Arquivo := ParseText(FPRetWS);
  FRetDownloadEvt.LerXml;

  for i := 0 to FRetDownloadEvt.Arquivo.Count - 1 do
  begin
    AXML := FRetDownloadEvt.Arquivo.Items[i].XML;

    if AXML <> '' then
    begin
      if FRetDownloadEvt.Arquivo.Items[i].Id <> '' then
        NomeArq := FRetDownloadEvt.Arquivo.Items[i].Id + '-down.xml';

      if FRetDownloadEvt.Arquivo.Items[i].nrRec <> '' then
        NomeArq := FRetDownloadEvt.Arquivo.Items[i].nrRec + '-down.xml';

      if (FPConfiguracoeseSocial.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML);
    end;

  end;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPRetWS, eseRetornoDownloadEvt);

  Result := True;
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBreSocial := TACBrDFe(AOwner);

  FEnvioLote    := TEnvioLote.Create(FACBreSocial);
  FConsultaLote := TConsultaLote.Create(FACBreSocial);
  FConsultaIdentEventos := TConsultaIdentEventos.Create(FACBreSocial);
  FDownloadEventos := TDownloadEventos.Create(FACBreSocial);

end;

destructor TWebServices.Destroy;
begin
  FEnvioLote.Free;
  FConsultaLote.Free;
  FConsultaIdentEventos.Free;
  FDownloadEventos.Free;

  inherited Destroy;
end;

function TWebServices.Envia(AGrupo: TeSocialGrupo): Boolean;
begin
  FEnvioLote.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  EnvioLote.Grupo := AGrupo;

  if not EnvioLote.Executar then
    EnvioLote.GerarException(EnvioLote.Msg);

  Result := True;
end;

function TWebServices.GeraLote(AGrupo: TeSocialGrupo;FlSalvar:boolean = True): boolean;
  var
  ErroMsg: String;
begin

  Result :=False;

  EnvioLote.FazerLog('Inicio '+ClassName, False);

  try
    EnvioLote.DefinirDadosMsg;
    if Assigned(EnvioLote.FPDFeOwner.Integrador) then
       EnvioLote.DefinirDadosIntegrador;

    EnvioLote.DefinirEnvelopeSoap;
    if FlSalvar then
    begin
       EnvioLote.SalvarEnvio;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      ErroMsg := EnvioLote.GerarMsgErro(E);
      EnvioLote.GerarException(ErroMsg, E);
    end;
  end;

end;

function TWebServices.Consultar(const AProtocolo: string): Boolean;
begin
  FConsultaLote.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultaLote.FProtocolo := AProtocolo;

  if not FConsultaLote.Executar then
      FConsultaLote.GerarException(FConsultaLote.Msg);

  Result := True;
end;

function TWebServices.ConsultaIdentificadoresEventosEmpregador(const CnpjEstab: String;
  tpEvt: TTipoEvento; PerApur: TDateTime): boolean;
begin
  FConsultaIdentEventos.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultaIdentEventos.FtipoConsulta := tcEmpregador;
  FConsultaIdentEventos.FCnpj := CnpjEstab;
  FConsultaIdentEventos.FEvento := tpEvt;
  FConsultaIdentEventos.FPerApur := PerApur;
  FConsultaIdentEventos.FmetodoConsulta := 'Empregador';

  if not FConsultaIdentEventos.Executar then
    FConsultaIdentEventos.GerarException(FConsultaIdentEventos.Msg);

  Result := True;
end;

function TWebServices.ConsultaIdentificadoresEventosTabela(
  const CnpjEstab: String; tpEvt: TTipoEvento; const AchEvt: string; AdtIni,
  AdtFim: TDateTime): boolean;
begin
  FConsultaIdentEventos.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultaIdentEventos.FtipoConsulta := tcTabela;
  FConsultaIdentEventos.FCnpj := CnpjEstab;
  FConsultaIdentEventos.FEvento := tpEvt;
  FConsultaIdentEventos.FchEvt := AchEvt;
  FConsultaIdentEventos.FdtIni := AdtIni;
  FConsultaIdentEventos.FdtFim := AdtFim;
  FConsultaIdentEventos.FmetodoConsulta := 'Tabela';

  if not FConsultaIdentEventos.Executar then
    FConsultaIdentEventos.GerarException(FConsultaIdentEventos.Msg);

  Result := True;
end;

function TWebServices.ConsultaIdentificadoresEventosTrabalhador(
  const CnpjEstab: String; const AcpfTrab: string; AdtIni,
  AdtFim: TDateTime): boolean;
begin
  FConsultaIdentEventos.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultaIdentEventos.FtipoConsulta := tcTrabalhador;
  FConsultaIdentEventos.FCnpj := CnpjEstab;
  FConsultaIdentEventos.FcpfTrab := AcpfTrab;
  FConsultaIdentEventos.FdtIni := AdtIni;
  FConsultaIdentEventos.FdtFim := AdtFim;
  FConsultaIdentEventos.FmetodoConsulta := 'Trabalhador';

  if not FConsultaIdentEventos.Executar then
    FConsultaIdentEventos.GerarException(FConsultaIdentEventos.Msg);

  Result := True;
end;

function TWebServices.DownloadEvento(const ACnpjEmpr, APorID, APorNrRecibo: String): boolean;
begin
  FDownloadEventos.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  if APorID <> '' then
    FDownloadEventos.FTipoDownload := 'PorId'
  else
    FDownloadEventos.FTipoDownload := 'PorNrRecibo';

  FDownloadEventos.FCnpj := ACnpjEmpr;
  FDownLoadEventos.FPorID := Trim(APorID);
  FDownLoadEventos.FPorNrRecibo := Trim(APorNrRecibo);

  if not FDownLoadEventos.Executar then
      FDownLoadEventos.GerarException(FDownLoadEventos.Msg);

  Result := True;
end;

end.
