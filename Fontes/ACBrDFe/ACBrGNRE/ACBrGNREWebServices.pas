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

unit ACBrGNREWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pcnAuxiliar, pcnConversao,
  pgnreGNRE, pgnreConversao, pgnreRetEnvLoteGNRE, pgnreConsResLoteGNRE,
  pgnreRetConsResLoteGNRE, pgnreConsConfigUF, pgnreRetConsConfigUF,
  ACBrGNREGuias, ACBrGNREConfiguracoes;

type

  { TGNReWebService }

  TGNReWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrGNRe;
    FPLayout: TLayOut;
    FPConfiguracoesGNRe: TConfiguracoesGNRe;

    function ExtrairModeloChaveAcesso(AChaveGNRe: String): String;
    function ExtrairUFChaveAcesso(AChaveGNRe: String): Integer;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrGNRe read FPStatus;
    property Layout: TLayOut read FPLayout;
  end;

  { TGNReRecepcao }

  TGNReRecepcao = class(TGNReWebService)
  private
    Fnumero: String;
    FtempoEstimadoProc: Integer;
    Fcodigo: Integer;
    Fdescricao: String;
    FdataHoraRecibo: TDateTime;
    FAmbiente: TpcnTipoAmbiente;
    FGuias: TGuias;

    FGNReRetorno: TretEnvGNRe;

    function GetLote: String;
    function GetRecibo: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; AGuias: TGuias);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
    property numero: String read Fnumero write Fnumero;
    property dataHoraRecibo: TDateTime read FdataHoraRecibo write FdataHoraRecibo;
    property tempoEstimadoProc: Integer read FtempoEstimadoProc write FtempoEstimadoProc;
  end;

  { TGNReRetRecepcao }

  TGNReRetRecepcao = class(TGNReWebService)
  private
    FAmbiente: TpcnTipoAmbiente;
    FnumeroRecibo: String;
    Fcodigo: Integer;
    Fresultado: String;
    Fdescricao: String;
    Fprotocolo: String;
    FGuias: TGuias;

    FGNReRetorno: TRetConsReciGNRe;

    function GetRecibo: String;
    function TratarRespostaFinal: Boolean;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; AGuias: TGuias);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property numeroRecibo: String read FnumeroRecibo write FnumeroRecibo;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
    property protocolo: String read Fprotocolo write Fprotocolo;
    property resultado: String read Fresultado write Fresultado;

    property GNReRetorno: TRetConsReciGNRe read FNFeRetorno;
  end;

  { TGNReRecibo }

  TGNReRecibo = class(TGNReWebService)
  private
    FAmbiente: TpcnTipoAmbiente;
    FnumeroRecibo: String;
    Fcodigo: Integer;
    Fresultado: String;
    Fdescricao: String;
    Fprotocolo: String;
    FGuias: TGuias;

    FGNReRetorno: TRetConsReciGNRe;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirURL; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; AGuias: TGuias);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property numeroRecibo: String read FnumeroRecibo write FnumeroRecibo;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
    property protocolo: String read Fprotocolo write Fprotocolo;
    property resultado: String read Fresultado write Fresultado;

    property GNReRetorno: TRetConsReciGNRe read FGNReRetorno;
  end;

  { TGNReConsultaUF }

  TGNReConsultaUF = class(TGNReWebService)
  private
    Fcodigo: Integer;
    Fdescricao: String;
    Freceita: Integer;
    FexigeReceita: String;
    FexigeDataVencimento: String;
    FexigeDataPagamento: String;
    FexigeContribuinteEmitente: String;
    FexigeUfFavorecida: String;
    FexigeConvenio: String;
    FUf: String;
    FAmbiente: TpcnTipoAmbiente;

    FGNRERetorno: TTConfigUf;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property Uf: String read FUf write FUf;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
    property receita: Integer read Freceita write Freceita;
    property exigeReceita: String read FexigeReceita write FexigeReceita;
    property exigeUfFavorecida: String read FexigeUfFavorecida write FexigeUfFavorecida;
    property exigeContribuinteEmitente: String read FexigeContribuinteEmitente write FexigeContribuinteEmitente;
    property exigeDataVencimento: String read FexigeDataVencimento write FexigeDataVencimento;
    property exigeConvenio: String read FexigeConvenio write FexigeConvenio;
    property exigeDataPagamento: String read FexigeDataPagamento write FexigeDataPagamento;

    property GNRERetorno: TTConfigUf read FGNRERetorno write FGNRERetorno;
  end;

  { TGNReEnvioWebService }

  TGNReEnvioWebService = class(TGNReWebService)
  private
    FXMLEnvio: String;
    FPURLEnvio: String;
    FVersao: String;
    FSoapActionEnvio: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property Versao: String read FVersao;
    property XMLEnvio: String read FXMLEnvio write FXMLEnvio;
    property URLEnvio: String read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrGNRe: TACBrDFe;
    FEnviar: TGNReRecepcao;
    FRetorno: TGNReRetRecepcao;
    FRecibo: TGNReRecibo;
    FConsulta: TGNReConsulta;
    FConsultaUF: TGNReConsultaUF;
    FEnvioWebService: TGNReEnvioWebService;
    FEnvEvento: TGNReEnvEvento;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia: Boolean;
    function ConsultaResultadoLote(ANumRecibo: String): Boolean;

    property ACBrGNRe: TACBrDFe read FACBrGNRe write FACBrGNRe;
    property Enviar: TGNReRecepcao read FEnviar write FEnviar;
    property Retorno: TGNReRetRecepcao read FRetorno write FRetorno;
    property Recibo: TGNReRecibo read FRecibo write FRecibo;
    property Consulta: TGNReConsulta read FConsulta write FConsulta;
    property ConsultaUF: TGNReConsultaUF read FConsultaUF write FConsultaUF;
    property EnvEvento: TGNReEnvEvento read FEnvEvento write FEnvEvento;
    property EnvioWebService: TGNReEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil, ACBrGNRE,
  pcnGerador, pcnLeitor;

{ TGNReWebService }

constructor TGNReWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesGNRe := TConfiguracoesGNRe(FPConfiguracoes);
  FPLayout := LayGNReStatusServico;

  FPHeaderElement := 'gnreCabecMsg';
  FPBodyElement := 'gnreDadosMsg';
end;

procedure TGNReWebService.Clear;
begin
  inherited Clear;

  FPStatus := stIdle;
end;

procedure TGNReWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrGNRe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TGNReWebService.ExtrairModeloChaveAcesso(
  AChaveGNRe: String): String;
begin
  AChaveGNRe := OnlyNumber(AChaveGNRe);
  if ValidarChave(AChaveGNRe) then
    Result := copy(AChaveGNRe, 21, 2)
  else
    Result := '';
end;

function TGNReWebService.ExtrairUFChaveAcesso(AChaveGNRe: String): Integer;
begin
  Result := StrToIntDef(Copy(AChaveGNRe, 1, 2), 0);
end;

procedure TGNReWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrGNRe(FPDFeOwner).SetStatus(FPStatus);
end;

function TGNReWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrGNRe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TGNReWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrGNRe(FPDFeOwner).SetStatus(stIdle);
end;

{ TGNReRecepcao }

constructor TGNReRecepcao.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FGuias := AGuias;
end;

destructor TGNReRecepcao.Destroy;
begin
  FGNReRetorno.Free;

  inherited Destroy;
end;

procedure TGNReRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stGNReRecepcao;
  FPLayout := LayGNReRecepcao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  Fnumero := '';
  FtempoEstimadoProc := 0;
  Fcodigo := 0;
  Fdescricao := '';
  FdataHoraRecibo := 0;

  if Assigned(FPConfiguracoesGNRe) then
  begin
    FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRe.WebServices.UFCodigo;
  end;

  if Assigned(FGNReRetorno) then
    FGNReRetorno.Free;

  FGNReRetorno := TretEnvGNRe.Create;
end;

procedure TGNReRecepcao.DefinirDadosMsg;
var
  i: Integer;
  vGuias: WideString;
begin
  vGuias := '';
  for i := 0 to TGNRERecepcaoLote(Self).FGuias.Count - 1 do
    vGuias := vGuias + FGuias.Items[i].XML;

  FDPadosMsg := '<TLote_GNRE xmlns="http://www.gnre.pe.gov.br">' +
                '<guias>' + vGuias + '</guias>' +
               '</TLote_GNRE>';


  if Length(FPDadosMsg) > (300 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 300 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

procedure TGNReRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreLoteRecepcao';

  FPSoapAction := FPServico;
end;

procedure TGNReRecepcao.DefinirURL;
var
  xUF: String;
  ok: Boolean;
  VerServ: Double;
  Modelo: TpcnModeloDF;
begin
  if FGuias.Count > 0 then    // Tem GNRe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FGuias.Items[0].GNRe.Ide.modelo));
    FcUF    := FGuias.Items[0].GNRe.Ide.cUF;
    VerServ := FGuias.Items[0].GNRe.infGNRe.Versao;

    if FPConfiguracoesGNRe.WebServices.Ambiente <> FGuias.Items[0].GNRe.Ide.Ambiente then
      raise EACBrGNReException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRe, use as configurações do componente
    Modelo  := FPConfiguracoesGNRe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesGNRe.WebServices.UFCodigo;
    VerServ := VersaoDFToDbl(FPConfiguracoesGNRe.Geral.VersaoDF);
  end;

  FAmbiente  := FPConfiguracoesGNRe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';
  FPLayout := LayGNReRecepcao;

  xUF := CUFtoUF(FcUF);

  TACBrGNRe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNReRecepcao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + sLineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Tempo Médio: %s ' + LineBreak),
                   [FGNReRetorno.versao,
                    TpAmbToStr(FGNReRetorno.Ambiente),
                    FGNReRetorno.verAplic,
                    IntToStr(FGNReRetorno.cStat),
                    FGNReRetorno.xMotivo,
                    CodigoParaUF(FGNReRetorno.cUF),
                    FGNReRetorno.infRec.nRec,
                    IfThen(FGNReRetorno.InfRec.dhRecbto = 0, '',
                           FormatDateTimeBr(FGNReRetorno.InfRec.dhRecbto)),
                    IntToStr(FGNReRetorno.InfRec.TMed)]);
  {*)}
end;

function TGNReRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Lote;
end;

function TGNReRecepcao.GetLote: String;
begin
  Result := Trim(FLote);
end;

function TGNReRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TGNReRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'processarResponse');

  FGNReRetorno.Leitor.Arquivo := FPRetWS;
  FGNReRetorno.LerXml;

  Fcodigo            := FGNRERetorno.codigo;
  Fdescricao         := FGNRERetorno.descricao;
  Fnumero            := FGNRERetorno.numero;
  FdataHoraRecibo    := FGNRERetorno.dataHoraRecibo;
  FtempoEstimadoProc := FGNRERetorno.tempoEstimadoProc;
  FPMsg              := FGNRERetorno.descricao;

  Result := (FGNRERetorno.codigo = 100); //Lote recebido com Sucesso
end;

{ TGNReRetRecepcao }

constructor TGNReRetRecepcao.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FGuias := AGuias;
end;

destructor TGNReRetRecepcao.Destroy;
begin
  FGNReRetorno.Free;

  inherited Destroy;
end;

procedure TGNReRetRecepcao.Clear;
var
  i, j: Integer;
begin
  inherited Clear;

  FPStatus := stGNReRetRecepcao;
  FPLayout := LayGNReRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  FnumeroRecibo := '';
  Fcodigo := 0;
  Fresultado := '';
  Fdescricao := '';
  Fprotocolo := '';

  if Assigned(FPConfiguracoesGNRe) then
  begin
    FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRe.WebServices.UFCodigo;
  end;

  if Assigned(FGNReRetorno) and Assigned(FGuias) then
  begin
    // Limpa Dados dos retornos das guias;
    for i := 0 to FGNReRetorno.ProtGNRe.Count - 1 do
    begin
      for j := 0 to FGuias.Count - 1 do
      begin
        if OnlyNumber(FGNReRetorno.ProtGNRe.Items[i].chGNRe) = FGuias.Items[J].NumID then
        begin
          FGuias.Items[j].GNRe.procGNRe.verAplic := '';
          FGuias.Items[j].GNRe.procGNRe.chGNRe   := '';
          FGuias.Items[j].GNRe.procGNRe.dhRecbto := 0;
          FGuias.Items[j].GNRe.procGNRe.nProt    := '';
          FGuias.Items[j].GNRe.procGNRe.digVal   := '';
          FGuias.Items[j].GNRe.procGNRe.cStat    := 0;
          FGuias.Items[j].GNRe.procGNRe.xMotivo  := '';
        end;
      end;
    end;

    FreeAndNil(FGNReRetorno);
  end;

  FGNReRetorno := TRetConsReciGNRe.Create;
end;

procedure TGNReRetRecepcao.DefinirDadosMsg;
var
  ConsReciGNRe: TConsReciGNRe;
begin
  ConsReciGNRe := TConsReciGNRe.Create;
  try
    ConsReciGNRe.Ambiente := FAmbiente;
    ConsReciGNRe.numeroRecibo := FnumeroRecibo;
    ConsReciGNRe.GerarXML;

    FPDadosMsg := ConsReciGNRe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciGNRe.Free;
  end;
end;

procedure TGNReRetRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreResultadoLote';

  FPSoapAction := FPServico;
end;

procedure TGNReRetRecepcao.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  ok: Boolean;
  Modelo: TpcnModeloDF;
begin
  if FGuias.Count > 0 then    // Tem GNRe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FGuias.Items[0].GNRe.Ide.modelo));
    FcUF    := FGuias.Items[0].GNRe.Ide.cUF;
    VerServ := FGuias.Items[0].GNRe.infGNRe.Versao;

    if FPConfiguracoesGNRe.WebServices.Ambiente <> FGuias.Items[0].GNRe.Ide.Ambiente then
      raise EACBrGNReException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRe, use as configurações do componente
    Modelo  := FPConfiguracoesGNRe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesGNRe.WebServices.UFCodigo;
    VerServ := VersaoDFToDbl(FPConfiguracoesGNRe.Geral.VersaoDF);
  end;

  FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';
  FPLayout := LayGNReRetRecepcao;

  xUF := CUFtoUF(FcUF);

  TACBrGNRe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNReRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: Integer;
begin
  Result := False;

  TACBrGNRe(FPDFeOwner).SetStatus(stGNReRetRecepcao);
  try
    Sleep(FPConfiguracoesGNRe.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesGNRe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesGNRe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrGNRe(FPDFeOwner).SetStatus(stIdle);
  end;

  if FGNRERetorno.codigo = 402 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TGNReRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TGNReRetRecepcao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'cMsg: %s ' + LineBreak +
                           'xMsg: %s ' + LineBreak),
                   [FGNReRetorno.versao, TpAmbToStr(FGNReRetorno.Ambiente),
                    FGNReRetorno.verAplic, FGNReRetorno.nRec,
                    IntToStr(FGNReRetorno.cStat), FGNReRetorno.xMotivo,
                    CodigoParaUF(FGNReRetorno.cUF), IntToStr(FGNReRetorno.cMsg),
                    FGNReRetorno.xMsg]);
  {*)}
end;

function TGNReRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := numeroRecibo;
end;

function TGNReRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TGNReRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  FGNReRetorno.Leitor.Arquivo := FPRetWS;
  FGNReRetorno.LerXML;

  FAmbiente  := FGNRERetorno.Ambiente;
  Fcodigo    := FGNRERetorno.codigo;
  Fdescricao := FGNRERetorno.descricao;
  Fresultado := FGNRERetorno.resultado;
  FPMsg      := FGNRERetorno.descricao;

  Result := (FGNRERetorno.codigo = 401); // Lote em Processamento
end;

function TGNReRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: Integer;
  AProcGNRe: TProcGNRe;
  AInfProt: TProtGNReCollection;
  SalvarXML: Boolean;
begin
  Result := False;

  AInfProt := FGNReRetorno.ProtGNRe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FGNReRetorno.ProtGNRe.Items[0].xMotivo;
    FxMotivo := FGNReRetorno.ProtGNRe.Items[0].xMotivo;
  end;

  //Setando os retornos das notas fiscais;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FGuias.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chGNRe) = FGuias.Items[J].NumID then
      begin
        if (FPConfiguracoesGNRe.Geral.ValidarDigest) and
           (AInfProt.Items[I].digVal <> '') and
           (FGuias.Items[J].GNRe.signature.DigestValue <> AInfProt.Items[I].digVal) then
        begin
          raise EACBrGNReException.Create('DigestValue do documento ' +
            FGuias.Items[J].NumID + ' não confere.');
        end;

        with FGuias.Items[J] do
        begin
          GNRe.procGNRe.Ambiente := AInfProt.Items[I].Ambiente;
          GNRe.procGNRe.verAplic := AInfProt.Items[I].verAplic;
          GNRe.procGNRe.chGNRe := AInfProt.Items[I].chGNRe;
          GNRe.procGNRe.dhRecbto := AInfProt.Items[I].dhRecbto;
          GNRe.procGNRe.nProt := AInfProt.Items[I].nProt;
          GNRe.procGNRe.digVal := AInfProt.Items[I].digVal;
          GNRe.procGNRe.cStat := AInfProt.Items[I].cStat;
          GNRe.procGNRe.xMotivo := AInfProt.Items[I].xMotivo;
        end;

        // Monta o XML da Guia assinada e com o protocolo de Autorização ou Denegação
        if (AInfProt.Items[I].cStat = 100) or (AInfProt.Items[I].cStat = 110) or
           (AInfProt.Items[I].cStat = 150) or (AInfProt.Items[I].cStat = 301) or
           (AInfProt.Items[I].cStat = 302) or (AInfProt.Items[I].cStat = 303) then
        begin
          AProcGNRe := TProcGNRe.Create;
          try
            AProcGNRe.XML_GNRe := StringReplace(FGuias.Items[J].XMLAssinado,
                                       '<' + ENCODING_UTF8 + '>', '',
                                       [rfReplaceAll]);
            AProcGNRe.XML_Prot := AInfProt.Items[I].XMLprotGNRe;
            AProcGNRe.Versao := FPVersaoServico;
            AProcGNRe.GerarXML;

            with FGuias.Items[J] do
            begin
              XMLOriginal := AProcGNRe.Gerador.ArquivoFormatoXML;

              if FPConfiguracoesGNRe.Arquivos.Salvar then
              begin
                SalvarXML := (not FPConfiguracoesGNRe.Arquivos.SalvarApenasGNReProcessadas) or
                             Processada;

                // Salva o XML da Guia assinada e protocolada
                if SalvarXML then
                begin
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado

                  GravarXML; // Salva na pasta baseado nas configurações do PathGNRe
                end;
              end;
            end;
          finally
            AProcGNRe.Free;
          end;
        end;

        break;
      end;
    end;
  end;


(*
function TWebServicesBase.Confirma(AResultado: String): Boolean;
var SL, SLAux: TStringList;
  i, GuiasOk: Integer;
  Cabec, RepresentacaoNumerica, SituacaoGuia: String;
begin
  SL := TStringList.Create;
  SLAux := TStringList.Create;
  SL.Text := AResultado;
  GuiasOk := 0;

  try
    Cabec := SL.Strings[0];
    for i := 0 to SL.Count - 1 do
    begin
      if SameText(Copy(SL.Strings[i], 1, 1), '1') then
      begin
        SituacaoGuia := Trim(Copy(SL.Strings[i], 6, 1));
        if SameText(SituacaoGuia, '0') then
        begin
          SLAux.Add(Cabec);
          SLAux.Add(SL.Strings[i]);
          Inc(GuiasOk);
          RepresentacaoNumerica := Copy(SL.Strings[i], 979, 48);

          if FConfiguracoes.Geral.Salvar then
            SLAux.SaveToFile(PathWithDelim(FConfiguracoes.Geral.PathSalvar)+RepresentacaoNumerica+'-gnre.txt');
        end;
      end;

      SLAux.Clear;
      SituacaoGuia := '';
      RepresentacaoNumerica := '';
    end;
  finally
    FreeAndNil(SL);
    FreeAndNil(SLAux);
    Result := GuiasOk > 0;
  end;
end;
*)



  //Verificando se existe alguma guia confirmada
  for I := 0 to FGuias.Count - 1 do
  begin
    if FGuias.Items[I].Confirmada then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe alguma guia nao confirmada
  for I := 0 to FGuias.Count - 1 do
  begin
    if not FGuias.Items[I].Confirmada then
    begin
      FPMsg := ACBrStr('Guia(s) não confirmadas:') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para as guias nao confirmadas
  for I := 0 to FGuias.Count - 1 do
  begin
    if not FGuias.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FGuias.Items[I].GNRe.Ide.nNF) +
        '->' + FGuias.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveGNRe := AInfProt.Items[0].chGNRe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

{ TGNReRecibo }

constructor TGNReRecibo.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FGuias := AGuias;
end;

destructor TGNReRecibo.Destroy;
begin
  FGNReRetorno.Free;

  inherited Destroy;
end;

procedure TGNReRecibo.Clear;
begin
  inherited Clear;

  FPStatus := stGNReRecibo;
  FPLayout := LayGNReRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  FnumeroRecibo := '';
  Fcodigo := 0;
  Fresultado := '';
  Fdescricao := '';
  Fprotocolo := '';

  if Assigned(FPConfiguracoesGNRe) then
  begin
    FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRe.WebServices.UFCodigo;
  end;

  if Assigned(FGNReRetorno) then
    FGNReRetorno.Free;

  FGNReRetorno := TRetConsReciGNRe.Create;
end;

procedure TGNReRecibo.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreResultadoLote';
  FPSoapAction := FPServico;
end;

procedure TGNReRecibo.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  ok: Boolean;
  Modelo: TpcnModeloDF;
begin
  if FGuias.Count > 0 then    // Tem GNRe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FGuias.Items[0].GNRe.Ide.modelo));
    FcUF    := FGuias.Items[0].GNRe.Ide.cUF;
    VerServ := FGuias.Items[0].GNRe.infGNRe.Versao;

    if FPConfiguracoesGNRe.WebServices.Ambiente <> FGuias.Items[0].GNRe.Ide.Ambiente then
      raise EACBrGNReException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRe, use as configurações do componente
    Modelo  := FPConfiguracoesGNRe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesGNRe.WebServices.UFCodigo;
    VerServ := VersaoDFToDbl(FPConfiguracoesGNRe.Geral.VersaoDF);
  end;

  FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  FPLayout := LayGNReRetRecepcao;

  xUF := CUFtoUF(FcUF);

  TACBrGNRe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TGNReRecibo.DefinirDadosMsg;
var
  ConsResLoteGNRE: TConsResLoteGNRE;
begin
  ConsResLoteGNRE := TConsResLoteGNRE.Create;
  try
    ConsResLoteGNRE.Ambiente := FAmbiente;
    ConsResLoteGNRE.numeroRecibo := FnumeroRecibo;

    ConsResLoteGNRE.GerarXML;

    FPDadosMsg := ConsResLoteGNRE.Gerador.ArquivoFormatoXML;
  finally
    ConsResLoteGNRE.Free;
  end;
end;

function TGNReRecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  FGNReRetorno.Leitor.Arquivo := FPRetWS;
  FGNReRetorno.LerXML;

  FAmbiente  := FGNRERetorno.Ambiente;
  Fcodigo    := FGNRERetorno.codigo;
  Fdescricao := FGNRERetorno.descricao;
  Fresultado := FGNRERetorno.resultado;
  FPMsg      := FGNRERetorno.descricao;

  Result := (FGNRERetorno.codigo = 402); // 402 = Lote processado com sucesso.
end;

function TGNReRecibo.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FGNReRetorno.versao, TpAmbToStr(FGNRERetorno.Ambiente),
                   FGNReRetorno.verAplic, FGNReRetorno.nRec,
                   IntToStr(FGNRERetorno.codigo),
                   FGNRERetorno.descricao,
                   CodigoParaUF(FGNReRetorno.cUF)]);
  {*)}
end;

function TGNReRecibo.GerarPrefixoArquivo: String;
begin
  Result := numeroRecibo;
end;

{ TGNReConsultaUF }

constructor TGNReConsultaUF.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

end;

destructor TGNReConsultaUF.Destroy;
begin
  FGNReRetorno.Free;

  inherited Destroy;
end;

procedure TGNReConsultaUF.Clear;
begin
  inherited Clear;

  FPStatus := stGNReRecibo;
  FPLayout := LayGNReRetRecepcao;
  FPArqEnv := 'ped-cfg';
  FPArqResp := 'cfg';

  Fcodigo := 0;
  Fdescricao := '';
  Freceita := 0;
  FexigeReceita := '';
  FexigeDataVencimento := '';
  FexigeDataPagamento := '';
  FexigeContribuinteEmitente := '';
  FexigeUfFavorecida := '';
  FexigeConvenio := '';
  FUf := '';

  if Assigned(FPConfiguracoesGNRe) then
  begin
    FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRe.WebServices.UFCodigo;
  end;

  if Assigned(FGNReRetorno) then
    FGNReRetorno.Free;

  FGNReRetorno := TTConfigUf.Create;
end;

procedure TGNReConsultaUF.DefinirDadosMsg;
var
 ConsConfigUF: TConsConfigUF;
begin
  ConsConfigUF := TConsConfigUF.Create;
  try
    ConsConfigUF.UF       := FUF;
    ConsConfigUF.Ambiente := FAmbiente;
    ConsConfigUF.Receita  := Freceita;

    ConsConfigUF.GerarXML;

    FPDadosMsg := ConsConfigUF.Gerador.ArquivoFormatoXML;
  finally
    ConsConfigUF.Free;
  end;
end;

procedure TGNReConsultaUF.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreConfigUF';
  FPSoapAction := FPServico;
end;

procedure TGNReConsultaUF.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  ok: Boolean;
  Modelo: TpcnModeloDF;
begin
  if FGuias.Count > 0 then    // Tem GNRe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FGuias.Items[0].GNRe.Ide.modelo));
    FcUF    := FGuias.Items[0].GNRe.Ide.cUF;
    VerServ := FGuias.Items[0].GNRe.infGNRe.Versao;

    if FPConfiguracoesGNRe.WebServices.Ambiente <> FGuias.Items[0].GNRe.Ide.Ambiente then
      raise EACBrGNReException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRe, use as configurações do componente
    Modelo  := FPConfiguracoesGNRe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesGNRe.WebServices.UFCodigo;
    VerServ := VersaoDFToDbl(FPConfiguracoesGNRe.Geral.VersaoDF);
  end;

  FAmbiente := FPConfiguracoesGNRe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  FPLayout := LayGNReRetRecepcao;

  xUF := CUFtoUF(FcUF);

  TACBrGNRe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNReConsultaUF.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  FGNReRetorno.Leitor.Arquivo := FPRetWS;
  FGNReRetorno.LerXML;

  FAmbiente                   := FGNRERetorno.Ambiente;
  Fcodigo                     := FGNRERetorno.codigo;
  Fdescricao                  := UTF8Decode(FGNRERetorno.descricao);
  FexigeReceita               := FGNRERetorno.exigeReceita;
  FexigeDataVencimento        := FGNRERetorno.exigeDataVencimento;
  FexigeDataPagamento         := FGNRERetorno.exigeDataPagamento;
  FexigeContribuinteEmitente  := FGNRERetorno.exigeContribuinteEmitente;
  FexigeUfFavorecida          := FGNRERetorno.exigeUfFavorecida;
  FexigeConvenio              := FGNRERetorno.exigeConvenio;
  FUf                         := FGNRERetorno.Uf;
  FPMsg                       := UTF8Decode(FGNRERetorno.descricao);

  Result := (FGNRERetorno.codigo = 450); // 450 = Consulta da configuração da UF realizada com sucesso.
end;

function TGNReConsultaUF.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FGNReRetorno.versao, TpAmbToStr(FGNRERetorno.Ambiente),
                   FGNReRetorno.verAplic, FGNReRetorno.nRec,
                   IntToStr(FGNRERetorno.codigo),
                   FGNRERetorno.descricao,
                   CodigoParaUF(FGNReRetorno.cUF)]);
  {*)}
end;

{ TGNReEnvioWebService }

constructor TGNReEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stEnvioWebService;
end;

destructor TGNReEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TGNReEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TGNReEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TGNReEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TGNReEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TGNReEnvioWebService.DefinirDadosMsg;
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

function TGNReEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TGNReEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: ' + FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TGNReEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrGNRe := TACBrGNRe(AOwner);

  FEnviar       := TGNReRecepcao.Create(FACBrGNRe, TACBrGNRe(FACBrGNRe).Guias);
  FRetorno      := TGNReRetRecepcao.Create(FACBrGNRe, TACBrGNRe(FACBrGNRe).Guias);
  FConsResLote  := TGNReRecibo.Create(FACBrGNRe, TACBrGNRe(FACBrGNRe).Guias);
  FConsConfigUF := TGNReConsultaUF.Create(FACBrGNRe);
end;

destructor TWebServices.Destroy;
begin
  FEnviar.Free;
  FRetorno.Free;
  FConsResLote.Free;
  FConsConfigUF.Free;

  inherited Destroy;
end;

function TWebServices.Envia: Boolean;
begin
  if not FEnviar.Executar then
    FEnviar.GerarException( FEnviar.Msg );

  FRetorno.numeroRecibo := FEnviar.numeroRecibo;
  if not FRetorno.Executar then
    FRetorno.GerarException( FRetorno.Msg );

  Result := True;
end;

function TWebServices.ConsultaResultadoLote(ANumRecibo: String): Boolean;
begin
  FConsResLote.numeroRecibo := ANumRecibo;

  if not FConsResLote.Executar then
    FConsResLote.GerarException( FConsResLote.Msg );

  Result := true;
end;

end.
