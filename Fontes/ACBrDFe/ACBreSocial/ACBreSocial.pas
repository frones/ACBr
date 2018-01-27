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

unit ACBreSocial;

interface

uses
  Classes, SysUtils, ACBrUtil,
  ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBreSocialConfiguracoes, ACBreSocialWebServices, ACBreSocialEventos,
  ACBreSocialLoteEventos,
  pcnConversao, pcesConversaoeSocial;

resourcestring
  ACBRESOCIAL_CErroAmbienteDiferente = 'Ambiente do XML (tpAmb) é diferente do configurado no Componente (Configuracoes.WebServices.Ambiente)';
  ACBRESOCIAL_CErroSignLib = 'Necessário DigestMethod Algorithm SHA256. use xsXmlSec ou xsLibXml2 na propriedade SSLXmlSignLib.';
  ACBRESOCIAL_CErroCryptLib = 'Necessário DigestMethod Algorithm SHA256. use cryOpenSSL ou cryWinCrypt na propriedade SSLCryptLib.';

const
  ACBRESOCIAL_VERSAO = '2.4.1';
  ACBRESOCIAL_NAMESPACE = ' http://www.esocial.gov.br/servicos/empregador/lote/eventos/envio/v1_1_0';
  ACBRESOCIAL_NAMESPACE_URI = 'http://www.esocial.gov.br/schema/evt/';
  ACBRESOCIAL_VERSAO_URI = '/v02_04_01';
  ACBRESOCIAL_MODELODF = 'eSocial';

type

  EACBreSocialException = class(EACBrDFeException);

  TNotifyEventoseSocial = procedure(const AXML: AnsiString; ATipo: TeSocialEventos) of object;

  { TACBreSocial }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBreSocial = class(TACBrDFe)
  private
    FEventos: TEventos;
    FLoteEventos : TLoteEventos;

    FStatus : TStatusACBreSocial;
    FWebServices: TWebServices;

    FIdTransmissor: string;
    FIdEmpregador: string;
    FOnTransmissaoEventos: TNotifyEventoseSocial;

    function GetConfiguracoes: TConfiguracoeseSocial;
    procedure SetConfiguracoes(AValue: TConfiguracoeseSocial);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    function GetAbout: String; override;
    procedure GerarXMLEventos; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Enviar(AGrupo: TeSocialGrupo): boolean;
    function Consultar(const AProtocolo: string): boolean;

    procedure AssinarEventos;

    function NomeServicoToNomeSchema(const NomeServico: String): String; override;
    procedure LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double; var URL: String); reintroduce;
    function LerVersaoDeParams(LayOutServico: TLayOut): String; reintroduce; overload;
    procedure SetStatus(const stNewStatus: TStatusACBreSocial);

    function GetNomeModeloDFe: string; override;
    function GetNameSpaceURI: string; override;


    property Eventos: TEventos read FEventos write FEventos;
    property Status: TStatusACBreSocial read FStatus;
    property WebServices: TWebServices read FWebServices write FWebServices;
    property IdEmpregador: string read FIdEmpregador write FIdEmpregador;
    property IdTransmissor: string read FIdTransmissor write FIdTransmissor;

  published
    property Configuracoes: TConfiguracoeseSocial read GetConfiguracoes write SetConfiguracoes;
    property OnTransmissaoEventos: TNotifyEventoseSocial read FOnTransmissaoEventos write FOnTransmissaoEventos;

  end;


implementation

{ TACBreSocial }

uses
  ACBrDFeSSL;

constructor TACBreSocial.Create(AOwner: TComponent);
begin
  inherited;

  SSL.SSLDgst := dgstSHA256;
  FEventos := TEventos.Create(Self);
  FWebServices := TWebServices.Create(Self);
  FLoteEventos := TLoteEventos.Create(Self);
end;

destructor TACBreSocial.Destroy;
begin
  FEventos.Free;
  FWebServices.Free;
  FLoteEventos.Free;

  inherited;
end;

function TACBreSocial.GetConfiguracoes: TConfiguracoeseSocial;
begin
  Result := TConfiguracoeseSocial(FPConfiguracoes);
end;

procedure TACBreSocial.SetConfiguracoes(AValue: TConfiguracoeseSocial);
begin
  FPConfiguracoes := AValue;
end;

function TACBreSocial.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoeseSocial.Create(Self);
end;

function TACBreSocial.GetAbout: String;
begin
  Result := 'ACBreSocial Ver: ' + ACBRESOCIAL_VERSAO;
end;

procedure TACBreSocial.GerarXMLEventos;
var
  i: Integer;
begin
  //Limpando
  FLoteEventos.Clear;

{Iniciais}
  {S1000}
  for i := 0 to FEventos.Iniciais.S1000.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Iniciais.S1000[i].evtInfoEmpregador.XML);
  {S1005}
  for i := 0 to FEventos.Iniciais.S1005.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Iniciais.S1005[i].evtTabEstab.XML);
  {S2100}
  for i := 0 to FEventos.Iniciais.S2100.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Iniciais.S2100[i].evtCadInicial.XML);
{Iniciais}

{Tabelas}
  {S1010}
  for i := 0 to FEventos.Tabelas.S1010.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1010[i].EvtTabRubrica.XML);
  {S1020}
  for i := 0 to FEventos.Tabelas.S1020.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1020[i].EvtTabLotacao.XML);
  {S2100}
  for i := 0 to FEventos.Tabelas.S1030.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1030[i].EvtTabCargo.XML);
  {S1040}
  for i := 0 to FEventos.Tabelas.S1040.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1040[i].EvtTabFuncao.XML);
  {S1050}
  for i := 0 to FEventos.Tabelas.S1050.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1050[i].EvtTabHorContratual.XML);
  {S1060}
  for i := 0 to FEventos.Tabelas.S1060.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1060[i].EvtTabAmbiente.XML);
  {S1070}
  for i := 0 to FEventos.Tabelas.S1070.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1070[i].EvtTabProcesso.XML);
  {S1080}
  for i := 0 to FEventos.Tabelas.S1080.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Tabelas.S1080[i].EvtTabOperPortuario.XML);
{Tabelas}

{NaoPeriodicos}
  {S2190}
  for i := 0 to FEventos.NaoPeriodicos.S2190.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2190[i].EvtAdmPrelim.XML);
  {S2200}
  for i := 0 to FEventos.NaoPeriodicos.S2200.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2200[i].EvtAdmissao.XML);
  {S2205}
  for i := 0 to FEventos.NaoPeriodicos.S2205.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2205[i].EvtAltCadastral.XML);
  {S2206}
  for i := 0 to FEventos.NaoPeriodicos.S2206.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2206[i].EvtAltContratual.XML);
  {S2210}
  for i := 0 to FEventos.NaoPeriodicos.S2210.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2210[i].EvtCAT.XML);
  {S2220}
  for i := 0 to FEventos.NaoPeriodicos.S2220.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2220[i].EvtASO.XML);
  {S2230}
  for i := 0 to FEventos.NaoPeriodicos.S2230.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2230[i].EvtAfastTemp.XML);
  {S2240}
  for i := 0 to FEventos.NaoPeriodicos.S2240.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2240[i].EvtExpRisco.XML);
  {S2241}
  for i := 0 to FEventos.NaoPeriodicos.S2241.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2241[i].EvtInsApo.XML);
  {S2250}
  for i := 0 to FEventos.NaoPeriodicos.S2250.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2250[i].EvtAvPrevio.XML);
  {S2260}
  for i := 0 to FEventos.NaoPeriodicos.S2260.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2260[i].EvtConvInterm.XML);
  {S2298}
  for i := 0 to FEventos.NaoPeriodicos.S2298.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2298[i].EvtReintegr.XML);
  {S2299}
  for i := 0 to FEventos.NaoPeriodicos.S2299.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2299[i].EvtDeslig.XML);
  {S2300}
  for i := 0 to FEventos.NaoPeriodicos.S2300.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2300[i].EvtTSVInicio.XML);
  {S2306}
  for i := 0 to FEventos.NaoPeriodicos.S2306.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2306[i].EvtTSVAltContr.XML);
  {S2399}
  for i := 0 to FEventos.NaoPeriodicos.S2399.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2399[i].EvtTSVTermino.XML);
  {S2400}
  for i := 0 to FEventos.NaoPeriodicos.S2400.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S2400[i].evtCdBenPrRP.XML);
  {S2400}
  for i := 0 to FEventos.NaoPeriodicos.S3000.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S3000[i].EvtExclusao.XML);
  {S4000}
  for i := 0 to FEventos.NaoPeriodicos.S4000.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S4000[i].EvtSolicTotal.XML);
  {S4999}
  for i := 0 to FEventos.NaoPeriodicos.S4999.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.NaoPeriodicos.S4999[i].EvtAdesao.XML);
{NaoPeriodicos}

{Periodicos}
  for i := 0 to FEventos.Periodicos.S1200.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1200[i].evtRemun.XML);
  for i := 0 to FEventos.Periodicos.S1202.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1202[i].evtRmnRPPS.XML);
  for i := 0 to FEventos.Periodicos.S1207.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1207[i].evtBenPrRP.XML);
  for i := 0 to FEventos.Periodicos.S1210.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1210[i].evtPgtos.XML);
  for i := 0 to FEventos.Periodicos.S1220.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1220[i].EvtPgtosNI.XML);
  for i := 0 to FEventos.Periodicos.S1250.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1250[i].EvtAqProd.XML);
  for i := 0 to FEventos.Periodicos.S1260.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1260[i].EvtComProd.XML);
  for i := 0 to FEventos.Periodicos.S1270.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1270[i].EvtContratAvNP.XML);
  for i := 0 to FEventos.Periodicos.S1280.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1280[i].EvtInfoComplPer.XML);
  for i := 0 to FEventos.Periodicos.S1295.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1295[i].evtTotConting.XML);
  for i := 0 to FEventos.Periodicos.S1298.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1298[i].EvtReabreEvPer.XML);
  for i := 0 to FEventos.Periodicos.S1299.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1299[i].EvtFechaEvPer.XML);
  for i := 0 to FEventos.Periodicos.S1300.Count - 1 do
    FLoteEventos.LoadFromString(FEventos.Periodicos.S1300[i].EvtContrSindPatr.XML);
{Periodicos}
end;

function TACBreSocial.Enviar(AGrupo: TeSocialGrupo): boolean;
begin
  if not (SSL.SSLCryptLib in ([cryOpenSSL, cryWinCrypt])) then
    raise EACBreSocialException.Create(ACBRESOCIAL_CErroCryptLib);

  if not (SSL.SSLXmlSignLib in ([xsXmlSec, xsLibXml2])) then
    raise EACBreSocialException.Create(ACBRESOCIAL_CErroSignLib);

  with FLoteEventos.IdeEmpregador do
  begin
    TpInsc := tiCNPJ;
    NrInsc := Self.IdEmpregador;
  end;

  with FLoteEventos.IdeTransmissor do
  begin
    TpInsc := tiCNPJ;
    NrInsc := Self.IdTransmissor;
  end;

  GerarXMLEventos;

  FLoteEventos.GerarXML(Inttostr(ord(AGrupo)));

  if Assigned(FOnTransmissaoEventos) then
    FOnTransmissaoEventos(FLoteEventos.XML, eseEnvioLote);


  result := WebServices.Envia(FLoteEventos.XML);

  if Assigned(FOnTransmissaoEventos) then
    FOnTransmissaoEventos(WebServices.EnvioLote.RetornoWS, eseRetornoLote);
end;

function TACBreSocial.Consultar(const AProtocolo: string): boolean;
begin
  if Assigned(FOnTransmissaoEventos) then
     FOnTransmissaoEventos(WebServices.ConsultaLote.XMLEnvio, eseEnvioConsulta);

  Result := WebServices.Consultar(AProtocolo);

  if Assigned(FOnTransmissaoEventos) then
    FOnTransmissaoEventos(WebServices.ConsultaLote.XMlRet, eseRetornoConsulta);
end;

procedure TACBreSocial.AssinarEventos;
begin
  Eventos.GerarXMLs;
  if Configuracoes.Geral.Salvar then
    Eventos.SaveToFiles;
end;

function TACBreSocial.NomeServicoToNomeSchema(const NomeServico: String): String;
var
  ok: Boolean;
  ALayout: TLayOut;
begin
  ALayout := ServicoToLayOut(ok, NomeServico);
  if ok then
    Result := SchemaESocialToStr( LayOutToSchema( ALayout ) )
  else
    Result := '';
end;

procedure TACBreSocial.LerServicoDeParams(LayOutServico: TLayOut; var Versao: Double; var URL: String);
begin
 {TODO: Implementar com URI}
  if Configuracoes.WebServices.Ambiente = taHomologacao then
  begin
    case LayOutServico of
      LayEnvLoteEventos:             URL := 'https://webservices.producaorestrita.esocial.gov.br/servicos/empregador/enviarloteeventos/WsEnviarLoteEventos.svc';
      LayRetEnvLoteEventos:          URL := '';
      LayConsResultProcessamento:    URL := 'https://webservices.producaorestrita.esocial.gov.br/servicos/empregador/consultarloteeventos/WsConsultarLoteEventos.svc';
      LayRetConsResultProcessamento: URL := '';
    end;
  end
  else
  begin
    case LayOutServico of
      LayEnvLoteEventos:             URL := 'https://webservices.envio.esocial.gov.br/servicos/empregador/enviarloteeventos/WsEnviarLoteEventos.svc';
      LayRetEnvLoteEventos:          URL := '';
      LayConsResultProcessamento:    URL := 'https://webservices.consulta.esocial.gov.br/servicos/empregador/consultarloteeventos/WsConsultarLoteEventos.svc';
      LayRetConsResultProcessamento: URL := '';
    end;
  end;
end;

function TACBreSocial.LerVersaoDeParams(LayOutServico: TLayOut): String;
var
  Versao: Double;
begin
  Versao := LerVersaoDeParams(GetNomeModeloDFe, Configuracoes.WebServices.UF,
    Configuracoes.WebServices.Ambiente, LayOuteSocialToServico(LayOutServico),
    VersaoeSocialToDbl(Configuracoes.Geral.VersaoDF));

  Result := FloatToString(Versao, '.', '0.00');
end;

procedure TACBreSocial.SetStatus(const stNewStatus: TStatusACBreSocial);
begin
  if stNewStatus <> FStatus then
  begin
    FStatus := stNewStatus;
    if Assigned(OnStatusChange) then
      OnStatusChange(Self);
  end;
end;

function TACBreSocial.GetNomeModeloDFe: string;
begin
  Result := 'eSocial';
end;

function TACBreSocial.GetNameSpaceURI: string;
begin
  Result := ACBRESOCIAL_NAMESPACE;
end;

end.
