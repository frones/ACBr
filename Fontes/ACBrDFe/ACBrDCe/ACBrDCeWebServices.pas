{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrDCeWebServices;

interface

uses
  Classes, SysUtils, synacode,
  ACBrXmlBase,
  pcnConversao,
  ACBrDFe,
  ACBrDFeWebService,
  ACBrDFecomum.Proc,
  ACBrDCe.Classes,
  ACBrDCe.Conversao,
  ACBrDCe.RetConsSit,
  ACBrDCe.EnvEvento,
  ACBrDCe.RetEnvEvento,
  ACBrDCeDeclaracoes,
  ACBrDCeConfiguracoes;

type

  { TDCeWebService }

  TDCeWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusDCe;
    FPLayout: TLayOutDCe;
    FPConfiguracoesDCe: TConfiguracoesDCe;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: string; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusDCe read FPStatus;
    property Layout: TLayOutDCe read FPLayout;
  end;

  { TDCeStatusServico }

  TDCeStatusServico = class(TDCeWebService)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FTMed: Integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarMsgErro(E: Exception): string; override;
  public
    procedure Clear; override;

    property versao: string read Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb;
    property verAplic: string read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: string read FxMotivo;
    property cUF: Integer read FcUF;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: Integer read FTMed;
    property dhRetorno: TDateTime read FdhRetorno;
    property xObs: string read FxObs;
  end;

  { TDCeRecepcao }

  TDCeRecepcao = class(TDCeWebService)
  private
    FLote: string;
    FZipado: Boolean;
    FDeclaracoes: TDeclaracoes;
    Fversao: string;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FcUF: Integer;
    FxMotivo: string;
    FdhRecbto: TDateTime;
    FTMed: Integer;
    FProtocolo: string;
    FVersaoDF: TVersaoDCe;
    FMsgUnZip: string;

    FDCeRetornoSincrono: TRetConsSitDCe;

    function GetLote: string;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: string read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: string read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property xMotivo: string read FxMotivo;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: Integer read FTMed;
    property Protocolo: string read FProtocolo;

    property Lote: string read GetLote write FLote;
    property Zipado: Boolean read FZipado write FZipado;
    property MsgUnZip: string read FMsgUnZip write FMsgUnZip;
  end;

  { TDCeConsulta }

  TDCeConsulta = class(TDCeWebService)
  private
    FOwner: TACBrDFe;
    FDeclaracoes: TDeclaracoes;
    FDCeChave: string;
    FExtrairEventos: Boolean;
    FProtocolo: string;
    FDhRecbto: TDateTime;
    FXMotivo: string;
    Fversao: string;
    FTpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FcUF: Integer;
    FRetDCeDFe: string;

    FprotDCe: TProcDFe;
    FprocEventoDCe: TRetEventoDCeCollection;
    FDCeRetorno: TRetConsSitDCe;

    procedure SetDCeChave(const AValue: string);
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function GerarUFSoap: string; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property DCeChave: string read FDCeChave write SetDCeChave;
    property ExtrairEventos: Boolean read FExtrairEventos write FExtrairEventos;
    property Protocolo: string read FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto;
    property XMotivo: string read FXMotivo;
    property versao: string read Fversao;
    property TpAmb: TACBrTipoAmbiente read FTpAmb;
    property verAplic: string read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property RetDCeDFe: string read FRetDCeDFe;

    property protDCe: TProcDFe read FprotDCe;
    property procEventoDCe: TRetEventoDCeCollection read FprocEventoDCe;
  end;

  { TDCeEnvEvento }

  TDCeEnvEvento = class(TDCeWebService)
  private
    FidLote: Integer;
//    FEvento: TEventoDCe;
    FcStat: Integer;
    FxMotivo: string;
    FTpAmb: TpcnTipoAmbiente;
    FCNPJ: string;

//    FEventoRetorno: TRetEventoDCe;

    function GerarPathEvento(const ACNPJ: string = ''; const AIE: string = ''): string;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create; //(AOwner: TACBrDFe; AEvento: TEventoDCe);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property idLote: Integer read FidLote write FidLote;
    property cStat: Integer read FcStat;
    property xMotivo: string read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

//    property EventoRetorno: TRetEventoDCe read FEventoRetorno;
  end;

 { TDCeEnvioWebService }

  TDCeEnvioWebService = class(TDCeWebService)
  private
    FXMLEnvio: string;
    FPURLEnvio: string;
    FVersao: string;
    FSoapActionEnvio: string;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgErro(E: Exception): string; override;
    function GerarVersaoDadosSoap: string; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property Versao: string read FVersao;
    property XMLEnvio: string read FXMLEnvio write FXMLEnvio;
    property URLEnvio: string read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: string read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrDCe: TACBrDFe;
    FStatusServico: TDCeStatusServico;
    FEnviar: TDCeRecepcao;
    FConsulta: TDCeConsulta;
    FEnvEvento: TDCeEnvEvento;
    FEnvioWebService: TDCeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(const ALote: string; Zipado: Boolean = True): Boolean;

    property ACBrDCe: TACBrDFe read FACBrDCe write FACBrDCe;
    property StatusServico: TDCeStatusServico read FStatusServico write FStatusServico;
    property Enviar: TDCeRecepcao read FEnviar write FEnviar;
    property Consulta: TDCeConsulta read FConsulta write FConsulta;
    property EnvEvento: TDCeEnvEvento read FEnvEvento write FEnvEvento;
    property EnvioWebService: TDCeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrCompress,
  ACBrConsts,
  ACBrDFeUtil,
  ACBrDFeConsts,
  ACBrDFeComum.ConsStatServ,
  ACBrDFeComum.RetConsStatServ,
  ACBrDCe,
  ACBrDCe.Consts,
  ACBrDCe.ConsSit;

{ TDCeWebService }

constructor TDCeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesDCe := TConfiguracoesDCe(FPConfiguracoes);
  FPLayout := LayDCeStatusServico;

  FPHeaderElement := '';
  FPBodyElement := 'dceDadosMsg';
end;

procedure TDCeWebService.Clear;
begin
  inherited Clear;

  FPStatus := stDCeIdle;
  FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TDCeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrDCe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TDCeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TDCeWebService.GerarVersaoDadosSoap: string;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrDCe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TDCeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrDCe(FPDFeOwner).SetStatus(stDCeIdle);
end;

{ TDCeStatusServico }

procedure TDCeStatusServico.Clear;
begin
  inherited Clear;

  FPStatus := stDCeStatusServico;
  FPLayout := LayDCeStatusServico;
  FPArqEnv := 'ped-sta';
  FPArqResp := 'sta';

  Fversao := '';
  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FdhRecbto := 0;
  FTMed := 0;
  FdhRetorno := 0;
  FxObs := '';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesDCe.WebServices.Ambiente);
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end
end;

procedure TDCeStatusServico.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeStatusServico';
  FPSoapAction := FPServico + '/dceStatusServico';
end;

procedure TDCeStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe', True);
  try
    ConsStatServ.TpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesDCe.WebServices.UFCodigo;

    FPDadosMsg := ConsStatServ.GerarXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TDCeStatusServico.TratarResposta: Boolean;
var
  DCeRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'dceStatusServicoResult');

  DCeRetorno := TRetConsStatServ.Create('DCe');
  try
    DCeRetorno.XmlRetorno := ParseText(FPRetWS);
    DCeRetorno.LerXml;

    Fversao := DCeRetorno.versao;
    FtpAmb := DCeRetorno.tpAmb;
    FverAplic := DCeRetorno.verAplic;
    FcStat := DCeRetorno.cStat;
    FxMotivo := DCeRetorno.xMotivo;
    FcUF := DCeRetorno.cUF;
    FdhRecbto := DCeRetorno.dhRecbto;
    FTMed := DCeRetorno.TMed;
    FdhRetorno := DCeRetorno.dhRetorno;
    FxObs := DCeRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if FPConfiguracoesDCe.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesDCe.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);
  finally
    DCeRetorno.Free;
  end;
end;

function TDCeStatusServico.GerarMsgLog: string;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s' + LineBreak +
                           'Status Descrição: %s' + LineBreak +
                           'UF: %s' + LineBreak +
                           'Recebimento: %s' + LineBreak +
                           'Tempo Médio: %s' + LineBreak +
                           'Retorno: %s' + LineBreak +
                           'Observação: %s' + LineBreak),
                   [Fversao, TipoAmbienteToStr(FtpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoUFParaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto)),
                    IntToStr(FTMed),
                    IfThen(FdhRetorno = 0, '', FormatDateTimeBr(FdhRetorno)),
                    FxObs]);
end;

function TDCeStatusServico.GerarMsgErro(E: Exception): string;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TDCeRecepcao }

constructor TDCeRecepcao.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FDeclaracoes := ADeclaracoes;

  FZipado := True;
end;

destructor TDCeRecepcao.Destroy;
begin
  FDCeRetornoSincrono.Free;

  inherited Destroy;
end;

procedure TDCeRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stDCeAutorizacao;
  FPLayout := LayDCeAutorizacao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  Fversao    := '';
  FTMed      := 0;
  FverAplic  := '';
  FcStat     := 0;
  FxMotivo   := '';
  FdhRecbto  := 0;
  FProtocolo := '';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  if Assigned(FDCeRetornoSincrono) then
    FDCeRetornoSincrono.Free;

  FDCeRetornoSincrono := TRetConsSitDCe.Create;
end;

function TDCeRecepcao.GetLote: string;
begin
  Result := Trim(FLote);
end;

procedure TDCeRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDCe({ok,} FDeclaracoes.Items[0].DCe.infDCe.Versao)
  else
    FVersaoDF := FPConfiguracoesDCe.Geral.VersaoDF;

  inherited InicializarServico;
end;

procedure TDCeRecepcao.DefinirURL;
var
  Modelo: string;
  VerServ: Double;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
  begin
    FcUF := FDeclaracoes.Items[0].DCe.Ide.cUF;

    if Integer(FPConfiguracoesDCe.WebServices.Ambiente) <> Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb) then
      raise EACBrDCeException.Create( ACBRDCe_CErroAmbDiferente );
  end
  else
  begin                              // Se não tem DCe, use as configurações do componente
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  FPLayout := LayDCeAutorizacao;

  VerServ := VersaoDCeToDbl(FVersaoDF);
  Modelo := 'DCe';
  FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeAutorizacao';

  if Zipado then
  begin
    FPBodyElement := 'dceDadosMsgZip';
    FPSoapAction := FPServico + '/dceAutorizacaoZip';
  end
  else
  begin
    FPBodyElement := 'dceDadosMsg';
    FPSoapAction := FPServico + '/dceAutorizacao';
  end;
end;

procedure TDCeRecepcao.DefinirDadosMsg;
var
  I: Integer;
  vDCe: string;
begin
  if FDeclaracoes.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de DC-e transmitidos (máximo de 1 DC-e)' +
           ' excedido. Quantidade atual: ' + IntToStr(FDeclaracoes.Count)));

  if FDeclaracoes.Count > 0 then
    FPDadosMsg := '<DCe' +
      RetornarConteudoEntre(FDeclaracoes.Items[0].XMLAssinado, '<DCe', '</DCe>') +
      '</DCe>';

  FMsgUnZip := FPDadosMsg;

  if Zipado then
//    FPDadosMsg := EncodeBase64(GZipCompress(FPDadosMsg));
    FPDadosMsg := EncodeBase64(GZipCompress(CUTF8DeclaracaoXML+FPDadosMsg));

  // FPDadosMsg tem mais de 1024kb ? //
  if Length(FPDadosMsg) > (1024 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 1024 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TDCeRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chDCe, AXML, NomeXMLSalvo: string;
  AProcDCe: TProcDFe;
  SalvarXML: Boolean;
begin
  FPRetWS := SeparaDadosArray(['dceAutorizacaoResult'], FPRetornoWS );

  if pos('retDCe', FPRetWS) > 0 then
    AXML := StringReplace(FPRetWS, 'retDCe', 'retConsSitDCe',
                                   [rfReplaceAll, rfIgnoreCase])
  else
    AXML := FPRetWS;

  FDCeRetornoSincrono.XmlRetorno := ParseText(AXML);
  FDCeRetornoSincrono.LerXml;

  Fversao := FDCeRetornoSincrono.versao;
  FTpAmb := TpcnTipoAmbiente(FDCeRetornoSincrono.TpAmb);
  FverAplic := FDCeRetornoSincrono.verAplic;

  FcUF := FDCeRetornoSincrono.cUF;
  chDCe := FDCeRetornoSincrono.ProtDCe.chDFe;

  if (FDCeRetornoSincrono.protDCe.cStat > 0) then
    FcStat := FDCeRetornoSincrono.protDCe.cStat
  else
    FcStat := FDCeRetornoSincrono.cStat;

  if (FDCeRetornoSincrono.protDCe.xMotivo <> '') then
  begin
    FPMsg := FDCeRetornoSincrono.protDCe.xMotivo;
    FxMotivo := FDCeRetornoSincrono.protDCe.xMotivo;
  end
  else
  begin
    FPMsg := FDCeRetornoSincrono.xMotivo;
    FxMotivo := FDCeRetornoSincrono.xMotivo;
  end;

  // Verificar se a DC-e foi autorizado com sucesso
  Result := (FDCeRetornoSincrono.cStat = 100) and
    (TACBrDCe(FPDFeOwner).CstatProcessado(FDCeRetornoSincrono.protDCe.cStat));

  if Result then
  begin
    // Pega o numero do protocolo
    FProtocolo := FDCeRetornoSincrono.protDCe.nProt;

    for I := 0 to TACBrDCe(FPDFeOwner).Declaracoes.Count - 1 do
    begin
      with TACBrDCe(FPDFeOwner).Declaracoes.Items[I] do
      begin
        if OnlyNumber(chDCe) = NumID then
        begin

          if (FPConfiguracoesDCe.Geral.ValidarDigest) and
             (FDCeRetornoSincrono.protDCe.digVal <> '') and
             (DCe.signature.DigestValue <> FDCeRetornoSincrono.protDCe.digVal) then
          begin
            raise EACBrDCeException.Create('DigestValue do documento ' + NumID + ' não confere.');
          end;

          DCe.procDCe.cStat := FDCeRetornoSincrono.protDCe.cStat;
          DCe.procDCe.tpAmb := FDCeRetornoSincrono.tpAmb;
          DCe.procDCe.verAplic := FDCeRetornoSincrono.verAplic;
          DCe.procDCe.chDFe := FDCeRetornoSincrono.protDCe.chDFe;
          DCe.procDCe.dhRecbto := FDCeRetornoSincrono.protDCe.dhRecbto;
          DCe.procDCe.nProt := FDCeRetornoSincrono.protDCe.nProt;
          DCe.procDCe.digVal := FDCeRetornoSincrono.protDCe.digVal;
          DCe.procDCe.xMotivo := FDCeRetornoSincrono.protDCe.xMotivo;

          AProcDCe := TProcDFe.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe');
          try
            // Processando em UTF8, para poder gravar arquivo corretamente //
            AProcDCe.XML_DFe := RemoverDeclaracaoXML(XMLAssinado);
            AProcDCe.XML_Prot := FDCeRetornoSincrono.XMLprotDCe;
            XMLOriginal := AProcDCe.GerarXML;

            if FPConfiguracoesDCe.Arquivos.Salvar then
            begin
              SalvarXML := (not FPConfiguracoesDCe.Arquivos.SalvarApenasDCeProcessados) or
                           Processado;

              // Salva o XML do DC-e assinado e protocolado
              if SalvarXML then
              begin
                NomeXMLSalvo := '';
                if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                begin
                  FPDFeOwner.Gravar( NomeArq, XMLOriginal ); // Atualiza o XML carregado
                  NomeXMLSalvo := NomeArq;
                end;

                if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                  GravarXML; // Salva na pasta baseado nas configurações do PathDCe
              end;
            end ;
          finally
            AProcDCe.Free;
          end;

          Break;
        end;
      end;
    end;
  end;
end;

function TDCeRecepcao.GerarMsgLog: string;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak +
                         'UF: %s ' + sLineBreak +
                         'dhRecbto: %s ' + sLineBreak +
                         'chDCe: %s ' + LineBreak),
                   [FDCeRetornoSincrono.versao,
                    TipoAmbienteToStr(FDCeRetornoSincrono.TpAmb),
                    FDCeRetornoSincrono.verAplic,
                    IntToStr(FDCeRetornoSincrono.cStat),
                    FDCeRetornoSincrono.xMotivo,
                    CodigoUFParaUF(FDCeRetornoSincrono.cUF),
                    FormatDateTimeBr(FDCeRetornoSincrono.protDCe.dhRecbto),
                    FDCeRetornoSincrono.chDCe])
end;

function TDCeRecepcao.GerarPrefixoArquivo: string;
begin
  Result := Lote;
  FPArqResp := 'pro-lot';
end;

{ TDCeConsulta }

constructor TDCeConsulta.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FDeclaracoes := ADeclaracoes;

  if Assigned(FDCeRetorno) then
    FDCeRetorno.Free;
end;

destructor TDCeConsulta.Destroy;
begin
  FprotDCe.Free;
  FprocEventoDCe.Free;

  inherited Destroy;
end;

procedure TDCeConsulta.Clear;
begin
  inherited Clear;

  FPStatus := stDCeConsulta;
  FPLayout := LayDCeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FProtocolo := '';
  FDhRecbto := 0;
  Fversao := '';
  FRetDCeDFe := '';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesDCe.WebServices.Ambiente);
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  if Assigned(FprotDCe) then
    FprotDCe.Free;

  if Assigned(FprocEventoDCe) then
    FprocEventoDCe.Free;

  FprotDCe       := TProcDFe.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe');
  FprocEventoDCe := TRetEventoDCeCollection.Create;
end;

procedure TDCeConsulta.SetDCeChave(const AValue: string);
var
  NumChave: string;
begin
  if FDCeChave = AValue then Exit;
    NumChave := OnlyNumber(AValue);

  if not ValidarChave(NumChave) then
    raise EACBrDCeException.Create(Format('Chave "%s" inválida.',[AValue]));

  FDCeChave := NumChave;
end;

procedure TDCeConsulta.DefinirURL;
var
  VerServ: Double;
  Modelo: string;
  Ambiente: Integer;
begin
  FPVersaoServico := '';
  FPURL   := '';
  Modelo  := 'DCe';
  FcUF    := ExtrairUFChaveAcesso(FDCeChave);
  VerServ := VersaoDCeToDbl(FPConfiguracoesDCe.Geral.VersaoDF);
  {
  if FDeclaracoes.Count > 0 then
    FTpAmb := FDeclaracoes.Items[0].DCe.Ide.tpAmb
  else
    FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  }
  if FDeclaracoes.Count > 0 then
    Ambiente := Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb)
  else
    Ambiente := Integer(FPConfiguracoesDCe.WebServices.Ambiente);

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    TpcnTipoAmbiente(Ambiente),
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeConsulta.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeConsulta';
  FPSoapAction := FPServico + '/dceConsulta';
end;

procedure TDCeConsulta.DefinirDadosMsg;
var
  ConsSitDCe: TConsSitDCe;
begin
  ConsSitDCe := TConsSitDCe.Create;
  try
    ConsSitDCe.TpAmb := TpAmb;
    ConsSitDCe.chDCe := FDCeChave;
    ConsSitDCe.Versao := FPVersaoServico;

    FPDadosMsg := ConsSitDCe.GerarXML;
  finally
    ConsSitDCe.Free;
  end;
end;

function TDCeConsulta.GerarUFSoap: string;
begin
  Result := '<cUF>' + IntToStr(FcUF) + '</cUF>';
end;

function TDCeConsulta.TratarResposta: Boolean;

procedure SalvarEventos(Retorno: string);
var
  aEvento, aProcEvento, aIDEvento, sPathEvento, sCNPJ: string;
  Inicio, Fim: Integer;
  TipoEvento: TpcnTpEvento;
  Ok: Boolean;
begin
  while Retorno <> '' do
  begin
    Inicio := Pos('<procEventoDCe', Retorno);
    Fim    := Pos('</procEventoDCe>', Retorno) + 15;

    aEvento := Copy(Retorno, Inicio, Fim - Inicio + 1);

    Retorno := Copy(Retorno, Fim + 1, Length(Retorno));

    aProcEvento := '<procEventoDCe versao="' + FVersao + '" ' + NAME_SPACE_DCE + '>' +
                      SeparaDados(aEvento, 'procEventoDCe') +
                   '</procEventoDCe>';

    Inicio := Pos('Id=', aProcEvento) + 6;
    Fim    := 52;

    if Inicio = 6 then
      aIDEvento := FormatDateTime('yyyymmddhhnnss', Now)
    else
      aIDEvento := Copy(aProcEvento, Inicio, Fim);

//    TipoEvento  := StrToTpEventoDCe(Ok, SeparaDados(aEvento, 'tpEvento'));
    sCNPJ       := SeparaDados(aEvento, 'CNPJ');
    sPathEvento := PathWithDelim(FPConfiguracoesDCe.Arquivos.GetPathEvento(TipoEvento, sCNPJ));

    if (aProcEvento <> '') then
      FPDFeOwner.Gravar( aIDEvento + '-procEventoDCe.xml', aProcEvento, sPathEvento);
  end;
end;

var
//  DCeRetorno: TRetConsSitDCe;
  SalvarXML, DCeCancelado, Atualiza: Boolean;
  aEventos, sPathDCe, NomeXMLSalvo: string;
  AProcDCe: TProcDFe;
  I, J, Inicio, Fim: Integer;
  dhEmissao: TDateTime;
begin
  FDCeRetorno := TRetConsSitDCe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'dceConsultaResult');

    FDCeRetorno.XmlRetorno := ParseText(FPRetWS);
    FDCeRetorno.LerXML;

    DCeCancelado := False;
    aEventos := '';

    // <retConsSitDCe> - Retorno da consulta da situação da DC-e
    // Este é o status oficial da DC-e
    Fversao := FDCeRetorno.versao;
    FTpAmb := FDCeRetorno.tpAmb;
    FverAplic := FDCeRetorno.verAplic;
    FcStat := FDCeRetorno.cStat;
    FXMotivo := FDCeRetorno.xMotivo;
    FcUF := FDCeRetorno.cUF;
    FDCeChave := FDCeRetorno.chDCe;
    FPMsg := FXMotivo;

    // <protDCe> - Retorno dos dados do ENVIO da DC-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotDCe.PathDFe            := FDCeRetorno.protDCe.PathDFe;
    FprotDCe.PathRetConsReciDFe := FDCeRetorno.protDCe.PathRetConsReciDFe;
    FprotDCe.PathRetConsSitDFe  := FDCeRetorno.protDCe.PathRetConsSitDFe;
    FprotDCe.tpAmb              := FDCeRetorno.protDCe.tpAmb;
    FprotDCe.verAplic           := FDCeRetorno.protDCe.verAplic;
    FprotDCe.chDFe              := FDCeRetorno.protDCe.chDFe;
    FprotDCe.dhRecbto           := FDCeRetorno.protDCe.dhRecbto;
    FprotDCe.nProt              := FDCeRetorno.protDCe.nProt;
    FprotDCe.digVal             := FDCeRetorno.protDCe.digVal;
    FprotDCe.cStat              := FDCeRetorno.protDCe.cStat;
    FprotDCe.xMotivo            := FDCeRetorno.protDCe.xMotivo;

    if Assigned(FDCeRetorno.procEventoDCe) and (FDCeRetorno.procEventoDCe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da DC-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(FDCeRetorno.procEventoDCe.Count);

      FprocEventoDCe.Clear;
      for I := 0 to FDCeRetorno.procEventoDCe.Count - 1 do
      begin
        with FprocEventoDCe.New.RetEventoDCe do
        begin
        {
          idLote := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.idLote;
          tpAmb := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.tpAmb;
          verAplic := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.verAplic;
          cOrgao := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.cOrgao;
          cStat := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.cStat;
          xMotivo := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.xMotivo;
          XML := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.XML;

          Infevento.ID              := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.ID;
          Infevento.tpAmb           := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.tpAmb;
          InfEvento.CNPJCPF         := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.CNPJCPF;
          InfEvento.chDCe          := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.chDCe;
          InfEvento.dhEvento        := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.dhEvento;
          InfEvento.TpEvento        := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.TpEvento;
          InfEvento.nSeqEvento      := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.nSeqEvento;
          InfEvento.VersaoEvento    := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.VersaoEvento;
          InfEvento.DetEvento.nProt := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.nProt;
          InfEvento.DetEvento.xJust := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.xJust;
          InfEvento.DetEvento.xNome := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.xNome;
          InfEvento.DetEvento.CPF   := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.CPF;
          InfEvento.DetEvento.cUF   := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.cUF;
          InfEvento.DetEvento.cMun  := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.cMun;
          InfEvento.DetEvento.dtEnc := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.dtEnc;

          retEvento.Clear;
          for J := 0 to FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Count-1 do
          begin
            with retEvento.New.RetInfEvento do
            begin
              Id          := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.Id;
              tpAmb       := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.tpAmb;
              verAplic    := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.verAplic;
              cOrgao      := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.cOrgao;
              cStat       := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.cStat;
              xMotivo     := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.xMotivo;
              chDCe       := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.chDCe;
              tpEvento    := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.tpEvento;
              xEvento     := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.xEvento;
              nSeqEvento  := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.nSeqEvento;
              CNPJDest    := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.CNPJDest;
              emailDest   := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.emailDest;
              dhRegEvento := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.dhRegEvento;
              nProt       := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.nProt;
              XML         := FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.XML;
            end;
          end;
          }
        end;
        {
        with FDCeRetorno.procEventoDCe.Items[I].RetEventoDCe do
        begin
          for j := 0 to retEvento.Count -1 do
          begin
            aEventos := aEventos + LineBreak + LineBreak +
              Format(ACBrStr('Número de sequência: %s ' + LineBreak +
                             'Código do evento: %s ' + LineBreak +
                             'Descrição do evento: %s ' + LineBreak +
                             'Status do evento: %s ' + LineBreak +
                             'Descrição do status: %s ' + LineBreak +
                             'Protocolo: %s ' + LineBreak +
                             'Data/Hora do registro: %s '),
                     [IntToStr(InfEvento.nSeqEvento),
                      TpEventoToStr(InfEvento.TpEvento),
                      InfEvento.DescEvento,
                      IntToStr(retEvento.Items[J].RetInfEvento.cStat),
                      retEvento.Items[J].RetInfEvento.xMotivo,
                      retEvento.Items[J].RetInfEvento.nProt,
                      FormatDateTimeBr(retEvento.Items[J].RetInfEvento.dhRegEvento)]);

            if retEvento.Items[J].RetInfEvento.tpEvento = teCancelamento then
            begin
              DCeCancelado := True;
              FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
              FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
              FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
            end;

            if retEvento.Items[J].RetInfEvento.tpEvento = teEncerramento then
            begin
              DCencerrado := True;
              FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
              FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
              FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
            end;
          end;
        end;
        }
      end;
    end;

    if (not DCeCancelado) and (NaoEstaVazio(FDCeRetorno.protDCe.nProt))  then
    begin
      FProtocolo := FDCeRetorno.protDCe.nProt;
      FDhRecbto := FDCeRetorno.protDCe.dhRecbto;
      FPMsg := FDCeRetorno.protDCe.xMotivo;
    end;

    if Result then
    begin
      if TACBrDCe(FPDFeOwner).Declaracoes.Count > 0 then
      begin
        for i := 0 to TACBrDCe(FPDFeOwner).Declaracoes.Count - 1 do
        begin
          with TACBrDCe(FPDFeOwner).Declaracoes.Items[i] do
          begin
            if (OnlyNumber(FDCeChave) = NumID) then
            begin
              Atualiza := (NaoEstaVazio(FDCeRetorno.XMLprotDCe));

              if Atualiza and
                 TACBrDCe(FPDFeOwner).cStatCancelado(FDCeRetorno.cStat) then
                Atualiza := False;

              if (FPConfiguracoesDCe.Geral.ValidarDigest) and
                 (FDCeRetorno.protDCe.digVal <> '') and (DCe.signature.DigestValue <> '') and
                 (UpperCase(DCe.signature.DigestValue) <> UpperCase(FDCeRetorno.protDCe.digVal)) then
              begin
                raise EACBrDCeException.Create('DigestValue do documento ' +
                    NumID + ' não confere.');
              end;

              // Atualiza o Status da DCe interna //
              DCe.procDCe.cStat := FDCeRetorno.cStat;

              if Atualiza then
              begin
                DCe.procDCe.tpAmb := FDCeRetorno.tpAmb;
                DCe.procDCe.verAplic := FDCeRetorno.verAplic;
                DCe.procDCe.chDFe := FDCeRetorno.chDCe;
                DCe.procDCe.dhRecbto := FDhRecbto;
                DCe.procDCe.nProt := FProtocolo;
                DCe.procDCe.digVal := FDCeRetorno.protDCe.digVal;
                DCe.procDCe.cStat := FDCeRetorno.cStat;
                DCe.procDCe.xMotivo := FDCeRetorno.xMotivo;

                AProcDCe := TProcDFe.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe');
                try
                  AProcDCe.XML_DFe := RemoverDeclaracaoXML(XMLOriginal);
                  AProcDCe.XML_Prot := FDCeRetorno.XMLprotDCe;
                  XMLOriginal := AProcDCe.GerarXML;

                  FRetDCeDFe := '';

                  if (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoDCe'))) then
                  begin
                    Inicio := Pos('<procEventoDCe', FPRetWS);
                    Fim    := Pos('</retConsSitDCe', FPRetWS) -1;

                    aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                    FRetDCeDFe := '<DCeDFe>' +
                                    '<procDCe versao="' + FVersao + '">' +
                                      SeparaDados(XMLOriginal, 'DCeProc') +
                                    '</procDCe>' +
                                    '<procEventoDCe versao="' + FVersao + '">' +
                                      aEventos +
                                    '</procEventoDCe>' +
                                   '</DCeDFe>';

                  end;
                finally
                  AProcDCe.Free;
                end;

                SalvarXML := Result and
                           FPConfiguracoesDCe.Arquivos.Salvar and
                           ((not FPConfiguracoesDCe.Arquivos.SalvarApenasDCeProcessados) or
                             Processado);

                if SalvarXML then
                begin
                  if FPConfiguracoesDCe.Arquivos.EmissaoPathDCe then
                    dhEmissao := DCe.Ide.dhEmi
                  else
                    dhEmissao := Now;

                  sPathDCe := PathWithDelim(FPConfiguracoesDCe.Arquivos.GetPathDCe(dhEmissao, DCe.Emit.CNPJCPF, ''));

                  if (FRetDCeDFe <> '') then
                    FPDFeOwner.Gravar( FDCeChave + '-DCeDFe.xml', FRetDCeDFe, sPathDCe);

                  // Salva o XML do DC-e assinado e protocolado
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  // Salva na pasta baseado nas configurações do PathCTe
                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML;

                  // Salva o XML de eventos retornados ao consultar um DC-e
                  if ExtrairEventos then
                    SalvarEventos(aEventos);
                end;
              end;

              break;
            end;
          end;
        end;
      end
      else
      begin
        if ExtrairEventos and FPConfiguracoesDCe.Arquivos.Salvar and
           (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoDCe'))) then
        begin
          Inicio := Pos('<procEventoDCe', FPRetWS);
          Fim    := Pos('</retConsSitDCe', FPRetWS) -1;

          aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

          // Salva o XML de eventos retornados ao consultar um DC-e
          SalvarEventos(aEventos);
        end;
      end;
    end;
  finally
    FDCeRetorno.Free;
  end;
end;

function TDCeConsulta.GerarMsgLog: string;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Identificador: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Chave Acesso: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Protocolo: %s ' + LineBreak +
                           'Digest Value: %s ' + LineBreak),
                   [Fversao, FDCeChave, TipoAmbienteToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoUFParaUF(FcUF), FDCeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotDCe.digVal]);
end;

function TDCeConsulta.GerarPrefixoArquivo: string;
begin
  Result := Trim(FDCeChave);
end;

{ TDCeEnvEvento }

constructor TDCeEnvEvento.Create; //(AOwner: TACBrDFe; AEvento: TEventoDCe);
begin
//  inherited Create(AOwner);

//  FEvento := AEvento;
end;

destructor TDCeEnvEvento.Destroy;
begin
//  if Assigned(FEventoRetorno) then
//    FEventoRetorno.Free;

  inherited;
end;

procedure TDCeEnvEvento.Clear;
begin
  inherited Clear;

  FPStatus := stDCeEvento;
  FPLayout := LayDCeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';

  FcStat   := 0;
  FxMotivo := '';
  FCNPJ := '';

  if Assigned(FPConfiguracoesDCe) then
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  {
  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  FEventoRetorno := TRetEventoDCe.Create;
  }
end;

function TDCeEnvEvento.GerarPathEvento(const ACNPJ, AIE: string): string;
begin
 {
  with FEvento.Evento.Items[0].InfEvento do
  begin
    Result := FPConfiguracoesDCe.Arquivos.GetPathEvento(tpEvento, ACNPJ, AIE);
  end;
  }
end;

procedure TDCeEnvEvento.DefinirURL;
var
  UF, Modelo : string;
  VerServ: Double;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }
  {
  VerServ := VersaoDCeToDbl(FPConfiguracoesDCe.Geral.VersaoDF);
  FCNPJ   := FEvento.Evento.Items[0].InfEvento.CNPJCPF;
  FTpAmb  := FEvento.Evento.Items[0].InfEvento.tpAmb;
  Modelo  := 'DCe';
  UF      := CUFtoUF(ExtrairUFChaveAcesso(FEvento.Evento.Items[0].InfEvento.chDCe));
  }
  FPLayout := LayDCeEvento;

  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    UF,
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeEnvEvento.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeRecepcaoEvento';
  FPSoapAction := FPServico + '/dceRecepcaoEvento';
end;

procedure TDCeEnvEvento.DefinirDadosMsg;
var
//  EventoDCe: TEventoDCe;
  I, J, k, F: Integer;
  Evento, Eventos, EventosAssinados, AXMLEvento: AnsiString;
  FErroValidacao: string;
  EventoEhValido: Boolean;
  SchemaEventoDCe: TSchemaDCe;
begin
(*
  EventoDCe := TEventoDCe.Create;
  try
    EventoDCe.idLote := FidLote;
    SchemaEventoDCe := schErro;
    
    for I := 0 to FEvento.Evento.Count - 1 do
    begin
      with EventoDCe.Evento.New do
      begin
        infEvento.tpAmb      := FTpAmb;
        infEvento.CNPJCPF    := FEvento.Evento[i].InfEvento.CNPJCPF;
        infEvento.chDCe     := FEvento.Evento[i].InfEvento.chDCe;
        infEvento.dhEvento   := FEvento.Evento[i].InfEvento.dhEvento;
        infEvento.tpEvento   := FEvento.Evento[i].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[i].InfEvento.nSeqEvento;
        infEvento.versaoEvento := FEvento.Evento[i].InfEvento.versaoEvento;

        case InfEvento.tpEvento of
          teCancelamento:
          begin
            SchemaEventoDCe := schevCancDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[i].InfEvento.detEvento.xJust;
          end;

          teEncerramento:
          begin
            SchemaEventoDCe := schevEncDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.dtEnc := FEvento.Evento[i].InfEvento.detEvento.dtEnc;
            infEvento.detEvento.cUF   := FEvento.Evento[i].InfEvento.detEvento.cUF;
            infEvento.detEvento.cMun  := FEvento.Evento[i].InfEvento.detEvento.cMun;
          end;

          teInclusaoCondutor:
          begin
            SchemaEventoDCe := schevIncCondutorDCe;
            infEvento.detEvento.xNome := FEvento.Evento[i].InfEvento.detEvento.xNome;
            infEvento.detEvento.CPF   := FEvento.Evento[i].InfEvento.detEvento.CPF;
          end;

          teInclusaoDFe:
          begin
            SchemaEventoDCe := schevInclusaoDFeDCe;
            infEvento.detEvento.nProt       := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.cMunCarrega := FEvento.Evento[i].InfEvento.detEvento.cMunCarrega;
            infEvento.detEvento.xMunCarrega := FEvento.Evento[i].InfEvento.detEvento.xMunCarrega;

            for j := 0 to FEvento.Evento[i].InfEvento.detEvento.infDoc.Count - 1 do
            begin
              with EventoDCe.Evento[i].InfEvento.detEvento.infDoc.New do
              begin
                cMunDescarga := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].cMunDescarga;
                xMunDescarga := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].xMunDescarga;
                chNFe        := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].chNFe;
              end;
            end;
          end;

          tePagamentoOperacao:
          begin
            SchemaEventoDCe := schevPagtoOperDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;

            infEvento.detEvento.infViagens.qtdViagens := FEvento.Evento[i].InfEvento.detEvento.infViagens.qtdViagens;
            infEvento.detEvento.infViagens.nroViagem  := FEvento.Evento[i].InfEvento.detEvento.infViagens.nroViagem;

            for j := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag.Count - 1 do
            begin
              with EventoDCe.Evento[i].InfEvento.detEvento.infPag.New do
              begin
                xNome         := FEvento.Evento[i].InfEvento.detEvento.infPag[j].xNome;
                idEstrangeiro := FEvento.Evento[i].InfEvento.detEvento.infPag[j].idEstrangeiro;
                CNPJCPF       := FEvento.Evento[i].InfEvento.detEvento.infPag[j].CNPJCPF;

                for k := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp.Count - 1 do
                begin
                  with EventoDCe.Evento[i].InfEvento.detEvento.infPag[j].Comp.New do
                  begin
                    tpComp := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].tpComp;
                    vComp  := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].vComp;
                    xComp  := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].xComp;
                  end;
                end;

                vContrato := FEvento.Evento[i].InfEvento.detEvento.infPag[j].vContrato;
                indPag    := FEvento.Evento[i].InfEvento.detEvento.infPag[j].indPag;
                vAdiant   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].vAdiant;

                if indPag = ipPrazo then
                begin
                  for k := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo.Count - 1 do
                  begin
                    with EventoDCe.Evento[i].InfEvento.detEvento.infPag[j].infPrazo.New do
                    begin
                      nParcela := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].nParcela;
                      dVenc    := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].dVenc;
                      vParcela := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].vParcela;
                    end;
                  end;
                end;

                infBanc.PIX        := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.PIX;
                infBanc.CNPJIPEF   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.CNPJIPEF;
                infBanc.codBanco   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.codBanco;
                infBanc.codAgencia := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.codAgencia;
              end;
            end;
          end;
        end;
      end;
    end;

    EventoDCe.Versao := FPVersaoServico;

    AjustarOpcoes( EventoDCe.Gerador.Opcoes );

    EventoDCe.GerarXML;

    Eventos := NativeStringToUTF8( EventoDCe.Gerador.ArquivoFormatoXML );
    EventosAssinados := '';

    // Realiza a assinatura para cada evento
    while Eventos <> '' do
    begin
      F := Pos('</eventoDCe>', Eventos);

      if F > 0 then
      begin
        Evento := Copy(Eventos, 1, F + 12);
        Eventos := Copy(Eventos, F + 13, length(Eventos));

        AssinarXML(Evento, 'eventoDCe', 'infEvento', 'Falha ao assinar o Envio de Evento ');
        EventosAssinados := EventosAssinados + FPDadosMsg;
      end
      else
        Break;
    end;

    // Separa o XML especifico do Evento para ser Validado.
    AXMLEvento := SeparaDados(FPDadosMsg, 'detEvento');

    case SchemaEventoDCe of
      schevCancDCe:
        begin
          AXMLEvento := '<evCancDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evCancDCe>', '</evCancDCe>')) +
                        '</evCancDCe>';
        end;

      schevEncDCe:
        begin
          AXMLEvento := '<evEncDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evEncDCe>', '</evEncDCe>')) +
                        '</evEncDCe>';
        end;

      schevIncCondutorDCe:
        begin
          AXMLEvento := '<evIncCondutorDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evIncCondutorDCe>', '</evIncCondutorDCe>')) +
                        '</evIncCondutorDCe>';
        end;

      schevInclusaoDFeDCe:
        begin
          AXMLEvento := '<evIncDFeDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evIncDFeDCe>', '</evIncDFeDCe>')) +
                        '</evIncDFeDCe>';
        end;

      schevPagtoOperDCe:
        begin
          AXMLEvento := '<evPagtoOperDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evPagtoOperDCe>', '</evPagtoOperDCe>')) +
                        '</evPagtoOperDCe>';
        end;
    end;

    AXMLEvento := '<' + ENCODING_UTF8 + '>' + AXMLEvento;

    with TACBrDCe(FPDFeOwner) do
    begin
      EventoEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                                       StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg) and
                        SSL.Validar(AXMLEvento,
                                    GerarNomeArqSchemaEvento(SchemaEventoDCe,
                                                             StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg);
    end;

    if not EventoEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Evento: ') +
        FPMsg;

      raise EACBrDCeException.CreateDef(FErroValidacao);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoDCe.Evento[I].InfEvento.id;
  finally
    EventoDCe.Free;
  end;
  *)
end;

function TDCeEnvEvento.TratarResposta: Boolean;
var
//italo  Leitor: TLeitor;
  I, J: Integer;
  NomeArq, PathArq, VersaoEvento, Texto: string;
begin
 (*
  FEvento.idLote := idLote;

  FPRetWS := SeparaDados(FPRetornoWS, 'dceRecepcaoEventoResult');

  EventoRetorno.Leitor.Arquivo := ParseText(FPRetWS);
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.cStat;
  FxMotivo := EventoRetorno.xMotivo;
  FPMsg := EventoRetorno.xMotivo;
  FTpAmb := EventoRetorno.tpAmb;

  Result := (FcStat in [135, 136]);

  //gerar arquivo proc de evento
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].InfEvento.chDCe =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chDCe then
          begin
            FEvento.Evento.Items[I].RetInfEvento.tpAmb :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.tpAmb;
            FEvento.Evento.Items[I].RetInfEvento.nProt :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.nProt;
            FEvento.Evento.Items[I].RetInfEvento.dhRegEvento :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.dhRegEvento;
            FEvento.Evento.Items[I].RetInfEvento.cStat :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.cStat;
            FEvento.Evento.Items[I].RetInfEvento.xMotivo :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.xMotivo;

            VersaoEvento := TACBrDCe(FPDFeOwner).LerVersaoDeParams(LayDCeEvento);


            Leitor.Arquivo := FPDadosMsg;
            Texto := '<procEventoDCe versao="' + VersaoEvento + '" xmlns="' + ACBRDCe_NAMESPACE + '">' +
                      '<eventoDCe versao="' + VersaoEvento + '">' +
                       Leitor.rExtrai(1, 'infEvento', '', I + 1) +
                       '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
                        Leitor.rExtrai(1, 'SignedInfo', '', I + 1) +
                        Leitor.rExtrai(1, 'SignatureValue', '', I + 1) +
                        Leitor.rExtrai(1, 'KeyInfo', '', I + 1) +
                       '</Signature>' +
                      '</eventoDCe>';

            Leitor.Arquivo := FPRetWS;
            Texto := Texto +
                       '<retEventoDCe versao="' + VersaoEvento + '">' +
                        Leitor.rExtrai(1, 'infEvento', '', J + 1) +
                       '</retEventoDCe>' +
                      '</procEventoDCe>';

            if FPConfiguracoesDCe.Arquivos.Salvar then
            begin
              NomeArq := OnlyNumber(FEvento.Evento.Items[I].InfEvento.Id) + '-procEventoDCe.xml';
              PathArq := PathWithDelim(GerarPathEvento(FEvento.Evento.Items[I].InfEvento.CNPJCPF));

              FPDFeOwner.Gravar(NomeArq, Texto, PathArq);
              FEventoRetorno.retEvento.Items[J].RetInfEvento.NomeArquivo := PathArq + NomeArq;
              FEvento.Evento.Items[I].RetInfEvento.NomeArquivo := PathArq + NomeArq;
            end;

            Texto := ParseText(Texto);
            FEventoRetorno.retEvento.Items[J].RetInfEvento.XML := Texto;
            FEvento.Evento.Items[I].RetInfEvento.XML := Texto;

            break;
          end;
        end;
      end;
    finally
      Leitor.Free;
    end;
  end;
  *)
end;

function TDCeEnvEvento.GerarMsgLog: string;
var
  aMsg: string;
begin
(*
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [FEventoRetorno.versao, TpAmbToStr(FEventoRetorno.tpAmb),
                  FEventoRetorno.verAplic, IntToStr(FEventoRetorno.cStat),
                  FEventoRetorno.xMotivo]);

  if FEventoRetorno.retEvento.Count > 0 then
    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento = 0, '',
               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento))]);
  *)
  Result := aMsg;
end;

function TDCeEnvEvento.GerarPrefixoArquivo: string;
begin
//  Result := IntToStr(FEvento.idLote);
  Result := IntToStr(FidLote);
end;

{ TDCeEnvioWebService }

constructor TDCeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stDCeEnvioWebService;
end;

destructor TDCeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TDCeEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TDCeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TDCeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TDCeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TDCeEnvioWebService.DefinirDadosMsg;
//var
//  LeitorXML: TLeitor;
begin
{
  LeitorXML := TLeitor.Create;
  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;
}
  FPDadosMsg := FXMLEnvio;
end;

function TDCeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TDCeEnvioWebService.GerarMsgErro(E: Exception): string;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TDCeEnvioWebService.GerarVersaoDadosSoap: string;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrDCe := TACBrDCe(AOwner);

  FStatusServico := TDCeStatusServico.Create(FACBrDCe);
  FEnviar := TDCeRecepcao.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
  FConsulta := TDCeConsulta.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
//  FEnvEvento := TDCeEnvEvento.Create(FACBrDCe, TACBrDCe(FACBrDCe).EventoDCe);
  FEnvioWebService := TDCeEnvioWebService.Create(FACBrDCe);
end;

destructor TWebServices.Destroy;
begin
  FStatusServico.Free;
  FEnviar.Free;
  FConsulta.Free;
//  FEnvEvento.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia(const ALote: string; Zipado: Boolean = True): Boolean;
begin
  FEnviar.Clear;

  FEnviar.Lote := ALote;
  FEnviar.Zipado := Zipado;

  if not Enviar.Executar then
    Enviar.GerarException( Enviar.Msg );

  Result := True;
end;

end.
