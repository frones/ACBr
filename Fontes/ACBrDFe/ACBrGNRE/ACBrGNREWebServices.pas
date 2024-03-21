{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit ACBrGNREWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pcnConversao,
  pgnreGNRE, pgnreConversao, pgnreRetEnvLoteGNRE, pgnreConsResLoteGNRE,
  pgnreRetConsResLoteGNRE, pgnreConsConfigUF, pgnreRetConsConfigUF,
  ACBrGNREGuias, ACBrGNREConfiguracoes;

type

  { TGNREWebService }

  TGNREWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrGNRE;
    FPLayout: TLayOutGNRE;
    FPConfiguracoesGNRE: TConfiguracoesGNRE;
    FtempoEstimadoProc: Integer;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;
    function GetUrlWsd: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrGNRE read FPStatus;
    property Layout: TLayOutGNRE read FPLayout;
    property tempoEstimadoProc: Integer read FtempoEstimadoProc write FtempoEstimadoProc;
  end;

  { TGNRERecepcao }

  TGNRERecepcao = class(TGNREWebService)
  private
    Fnumero: String;
    Fcodigo: Integer;
    Fdescricao: String;
    FdataHoraRecibo: TDateTime;
    FAmbiente: TpcnTipoAmbiente;
    FGuias: TGuias;

    FGNRERetorno: TTretLote_GNRE;
    FcUF: Integer;

//    function GetLote: String;
//    function GetRecibo: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
//    function GerarPrefixoArquivo: String; override;
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
    property cUF: Integer read FcUF;
  end;

  { TGNRERetRecepcao }

  TGNRERetRecepcao = class(TGNREWebService)
  private
    FAmbiente: TpcnTipoAmbiente;
    FnumeroRecibo: String;
    FIncluirPDFGuias: Boolean;
    Fcodigo: Integer;
    Fresultado: String;
    Fdescricao: String;
    Fprotocolo: String;
    FGuias: TGuias;

    FGNRERetorno: TTResultLote_GNRE;
    FcUF: Integer;

//    function GetRecibo: String;
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
    function SalvarTXT(AResultado: String; Item: Integer): Boolean;
    function SalvarXML(AGuia, ANumero: String; aData: TDateTime;
      aCNPJ, aIE: string; Item: Integer): Boolean;
    function SalvarPDF(AGuia, ANumero: String; aData: TDateTime;
      aCNPJ, aIE: string): Boolean;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property numeroRecibo: String read FnumeroRecibo write FnumeroRecibo;
    property IncluirPDFGuias: Boolean read FIncluirPDFGuias write FIncluirPDFGuias;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: String read Fdescricao write Fdescricao;
    property protocolo: String read Fprotocolo write Fprotocolo;
    property resultado: String read Fresultado write Fresultado;

    property GNRERetorno: TTResultLote_GNRE read FGNRERetorno;
    property cUF: Integer read FcUF;
  end;

  { TGNRERecibo }

  TGNRERecibo = class(TGNREWebService)
  private
    FAmbiente: TpcnTipoAmbiente;
    FnumeroRecibo: String;
    Fcodigo: Integer;
    Fresultado: String;
    Fdescricao: String;
    Fprotocolo: String;
    FGuias: TGuias;

    FGNRERetorno: TTResultLote_GNRE;
    FcUF: Integer;

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

    property GNRERetorno: TTResultLote_GNRE read FGNRERetorno;
    property cUF: Integer read FcUF;
  end;

  { TGNREConsultaUF }

  TGNREConsultaUF = class(TGNREWebService)
  private
    Fcodigo: Integer;
    Fdescricao: String;
    Freceita: Integer;
    FexigeReceita: String;
    FexigeUfFavorecida: String;
    FUf: String;
    FAmbiente: TpcnTipoAmbiente;
    FGNRERetorno: TTConfigUf;
    FcUF: Integer;
    FexigeContribuinteEmitente: String;
    FexigeDataVencimento: String;
    FexigeDocumentoOrigem: String;
    FexigePeriodoApuracao: String;
    FexigeDetalhamentoReceita: String;
    FexigeDataPagamento: String;
    FexigePeriodoReferencia: String;
    FexigeContribuinteDestinatario: String;
    FexigeParcela: String;
    FexigeProduto: String;
    FexigeConvenio: String;

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
    property GNRERetorno: TTConfigUf read FGNRERetorno write FGNRERetorno;
    property cUF: Integer read FcUF;
    property exigeDetalhamentoReceita : String read FexigeDetalhamentoReceita write FexigeDetalhamentoReceita;
    property exigeContribuinteEmitente : String read FexigeContribuinteEmitente write FexigeContribuinteEmitente;
    property exigeProduto : String read FexigeProduto write FexigeProduto;
    property exigePeriodoReferencia : String read FexigePeriodoReferencia write FexigePeriodoReferencia;
    property exigePeriodoApuracao : String read FexigePeriodoApuracao write FexigePeriodoApuracao;
    property exigeParcela : String read FexigeParcela write FexigeParcela;
    property exigeDocumentoOrigem : String read FexigeDocumentoOrigem write FexigeDocumentoOrigem;
    property exigeContribuinteDestinatario : String read FexigeContribuinteDestinatario write FexigeContribuinteDestinatario;
    property exigeDataVencimento : String read FexigeDataVencimento write FexigeDataVencimento;
    property exigeDataPagamento : String read FexigeDataPagamento write FexigeDataPagamento;
    property exigeConvenio : String read FexigeConvenio write FexigeConvenio;

  end;

  { TGNREEnvioWebService }

  TGNREEnvioWebService = class(TGNREWebService)
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
    FACBrGNRE: TACBrDFe;
    FEnviar: TGNRERecepcao;
    FRetorno: TGNRERetRecepcao;
    FRecibo: TGNRERecibo;
    FConsultaUF: TGNREConsultaUF;
    FEnvioWebService: TGNREEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia: Boolean;
    function ConsultaResultadoLote(ANumRecibo: String): Boolean;

    property ACBrGNRE: TACBrDFe read FACBrGNRE write FACBrGNRE;
    property Enviar: TGNRERecepcao read FEnviar write FEnviar;
    property Retorno: TGNRERetRecepcao read FRetorno write FRetorno;
    property Recibo: TGNRERecibo read FRecibo write FRecibo;
    property ConsultaUF: TGNREConsultaUF read FConsultaUF write FConsultaUF;
    property EnvioWebService: TGNREEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math, synacode,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrGNRE2,
  pcnGerador, pcnLeitor;

{ TGNREWebService }

constructor TGNREWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesGNRE := TConfiguracoesGNRE(FPConfiguracoes);
  FPLayout := LayGNRERecepcao;

  FPHeaderElement := 'gnreCabecMsg';
  FPBodyElement := 'gnreDadosMsg';
end;

procedure TGNREWebService.Clear;
begin
  inherited Clear;

  FPStatus := stGNREIdle;
  FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TGNREWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrGNRE(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TGNREWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrGNRE(FPDFeOwner).SetStatus(FPStatus);
end;

function TGNREWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }
  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrGNRE(FPDFeOwner).LerVersaoDeParams(FPLayout);
  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TGNREWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }
  TACBrGNRE(FPDFeOwner).SetStatus(stGNREIdle);
end;

function TGNREWebService.GetUrlWsd: String;
begin
//  if FPConfiguracoesGNRE.Geral.VersaoDF = ve100 then
    Result := FPDFeOwner.GetNameSpaceURI + '/webservice/';
//  else
//    Result := FPDFeOwner.GetNameSpaceURI + '/wsdl/';
end;

{ TGNRERecepcao }

constructor TGNRERecepcao.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FGuias := AGuias;
end;

destructor TGNRERecepcao.Destroy;
begin
  FGNRERetorno.Free;

  inherited Destroy;
end;

procedure TGNRERecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stGNRERecepcao;
  FPLayout := LayGNRERecepcao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  Fnumero := '';
  FtempoEstimadoProc := 0;
  Fcodigo := 0;
  Fdescricao := '';
  FdataHoraRecibo := 0;

  if Assigned(FPConfiguracoesGNRE) then
  begin
    FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRE.WebServices.UFCodigo;
  end;

  if Assigned(FGNRERetorno) then
    FGNRERetorno.Free;

  FGNRERetorno := TTretLote_GNRE.Create;
end;

procedure TGNRERecepcao.DefinirDadosMsg;
var
  i: Integer;
  vGuias, Versao: String;
begin
  vGuias := '';
  for i := 0 to FGuias.Count - 1 do
    vGuias := vGuias + RemoverDeclaracaoXML(FGuias.Items[i].XMLAssinado);

  if FPConfiguracoesGNRE.Geral.VersaoDF = ve200 then
    Versao := 'versao="2.00" '
  else
    Versao := '';

  FPDadosMsg := '<TLote_GNRE ' + Versao + 'xmlns="http://www.gnre.pe.gov.br">' +
                '<guias>' + vGuias + '</guias>' +
               '</TLote_GNRE>';

  if Length(FPDadosMsg) > (300 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 300 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

procedure TGNRERecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreLoteRecepcao';

  FPSoapAction := FPServico;
end;

procedure TGNRERecepcao.DefinirURL;
var
//  xUF: String;
//  ok: Boolean;
  VerServ: Double;
begin
  (*
  if FGuias.Count > 0 then    // Tem GNRE ? Se SIM, use as informações do XML
  begin
    VerServ := 1.00; //FGuias.Items[0].GNRE.infGNRE.Versao;

  end
  else
  begin                   // Se não tem GNRE, use as configurações do componente
    VerServ := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  end;
  *)

  VerServ   := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';
  FPLayout := LayGNRERecepcao;

  TACBrGNRE(FPDFeOwner).LerServicoDeParams(
    'GNRE',
    'PE',
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNRERecepcao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Tempo Médio: %s ' + LineBreak),
                   [TpAmbToStr(FGNRERetorno.Ambiente),
                    IntToStr(FGNRERetorno.codigo),
                    FGNRERetorno.descricao,
                    FGNRERetorno.numero,
                    IfThen(FGNRERetorno.dataHoraRecibo = 0, '',
                           FormatDateTimeBr(FGNRERetorno.dataHoraRecibo)),
                    IntToStr(FGNRERetorno.tempoEstimadoProc)]);
  {*)}
end;
{
function TGNRERecepcao.GerarPrefixoArquivo: String;
begin
  Result := numero;
end;

function TGNRERecepcao.GetLote: String;
begin
  Result := Trim(Numero);
end;

function TGNRERecepcao.GetRecibo: String;
begin
  Result := Trim(numero);
end;
}
function TGNRERecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'processarResponse');

  //A função UTF8ToNativeString deve ser removida quando refatorado para usar ACBrXmlDocument
  FGNRERetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(AnsiString(FPRetWS)));
  FGNRERetorno.LerXml;

  Fcodigo            := FGNRERetorno.codigo;
  Fdescricao         := FGNRERetorno.descricao;
  Fnumero            := FGNRERetorno.numero;
  FdataHoraRecibo    := FGNRERetorno.dataHoraRecibo;
  FtempoEstimadoProc := FGNRERetorno.tempoEstimadoProc;
  FPMsg              := FGNRERetorno.descricao;

  Result := (FGNRERetorno.codigo = 100); //Lote recebido com Sucesso
end;

{ TGNRERetRecepcao }

constructor TGNRERetRecepcao.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FIncluirPDFGuias := False;

  FGuias := AGuias;
end;

destructor TGNRERetRecepcao.Destroy;
begin
  FGNRERetorno.Free;

  inherited Destroy;
end;

procedure TGNRERetRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stGNRERetRecepcao;
  FPLayout := LayGNRERetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  Fcodigo := 0;
  Fresultado := '';
  Fdescricao := '';
  Fprotocolo := '';

  if Assigned(FPConfiguracoesGNRE) then
  begin
    FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRE.WebServices.UFCodigo;
  end;

  if Assigned(FGNRERetorno) then
    FGNRERetorno.Free;
    
  FGNRERetorno := TTResultLote_GNRE.Create;
end;

procedure TGNRERetRecepcao.DefinirDadosMsg;
var
  ConsResLoteGNRE: TConsResLoteGNRE;
begin
  ConsResLoteGNRE := TConsResLoteGNRE.Create;
	try
		ConsResLoteGNRE.Ambiente     := FAmbiente;
		ConsResLoteGNRE.numeroRecibo := FnumeroRecibo;
		ConsResLoteGNRE.IncluirPDFGuias := FIncluirPDFGuias;
		ConsResLoteGNRE.GerarXML;

		FPDadosMsg := ConsResLoteGNRE.Gerador.ArquivoFormatoXML;
	finally
		ConsResLoteGNRE.Free;
	end;
end;

procedure TGNRERetRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreResultadoLote';

  FPSoapAction := FPServico;
end;

procedure TGNRERetRecepcao.DefinirURL;
var
  VerServ: Double;
begin
  (*
  if FGuias.Count > 0 then    // Tem GNRE ? Se SIM, use as informações do XML
  begin
    VerServ := 1.00;

//    if FPConfiguracoesGNRE.WebServices.Ambiente <> FGuias.Items[0].GNRE.Ide.Ambiente then
//      raise EACBrGNREException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRE, use as configurações do componente
    VerServ := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  end;
  *)

  VerServ   := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';
  FPLayout := LayGNRERetRecepcao;

  TACBrGNRE(FPDFeOwner).LerServicoDeParams(
    'GNRE',
    'PE',
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNRERetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: Integer;
begin
  Result := False;

  TACBrGNRE(FPDFeOwner).SetStatus(stGNRERetRecepcao);
  try
    if FPConfiguracoesGNRE.WebServices.AguardarConsultaRet < FtempoEstimadoProc then
      Sleep(FtempoEstimadoProc)
    else
      Sleep(FPConfiguracoesGNRE.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesGNRE.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesGNRE.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrGNRE(FPDFeOwner).SetStatus(stGNREIdle);
  end;

  if FGNRERetorno.codigo = 402 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TGNRERetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stGNREIdle... não ainda...;
end;

function TGNRERetRecepcao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak),
                   [TpAmbToStr(FGNRERetorno.Ambiente),
                    IntToStr(FGNRERetorno.codigo),
                    FGNRERetorno.descricao]);
  {*)}
end;

function TGNRERetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := numeroRecibo;
end;
{
function TGNRERetRecepcao.GetRecibo: String;
begin
  Result := Trim(numeroRecibo);
end;
}
function TGNRERetRecepcao.TratarResposta: Boolean;
var
  SL: TStringList;
  I: Integer;
  xData: TDateTime;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  //A função UTF8ToNativeString deve ser removida quando refatorado para usar ACBrXmlDocument
  FGNRERetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(AnsiString(FPRetWS)));
  FGNRERetorno.LerXML;

  FAmbiente  := FGNRERetorno.Ambiente;
  Fcodigo    := FGNRERetorno.codigo;
  Fdescricao := FGNRERetorno.descricao;
  Fresultado := FGNRERetorno.resultado;

  // Para aparecer as exceções que ocorreram / caso haja alguma
  SL := TStringList.Create;
  SL.Clear;
  for I := 0 to GNRERetorno.resRejeicaoGuia.Count - 1 do
   SL.Add(Trim(GNRERetorno.resRejeicaoGuia.Items[I].DescMotivoRejeicao)+#13);
  FPMsg      := FGNRERetorno.descricao + #13 + Trim(SL.Text);
  SL.Free;
  //
  Result := (FGNRERetorno.codigo = 401) or// Lote em Processamento
            (FGNRERetorno.codigo = 0); // Para retornos do SEFAZ onde tem apenas o numero do recibo
end;

function TGNRERetRecepcao.TratarRespostaFinal: Boolean;
var
  I: Integer;
  xData: TDateTime;
  PDFJaSalvo: Boolean;
begin
  Result := False;
  PDFJaSalvo := False;

  // Verificando se existe alguma guia confirmada
  for I := 0 to FGuias.Count - 1 do
  begin
    if FGuias.Items[I].Confirmada then
    begin
      Result := True;

      if FGNRERetorno.resGuia.Items[I].Versao = ve100 then
        Self.SalvarTXT(FGNRERetorno.resGuia.Items[I].TXT, I)
      else
      begin
        with FGNRERetorno.resGuia.Items[I] do
        begin
          if FPConfiguracoesGNRE.Arquivos.EmissaoPathGNRE then
            xData := StrToDateDef(DataVencimento, Now)
          else
            xData := Now;

          Self.SalvarXML(XML, NumeroControle, xData, DocEmitente, '', I);

          if not PDFJaSalvo then
          begin
            // Verifica se existe alguma guia no formato PDF
            if FGNRERetorno.pdfGuias <> '' then
            begin
              Self.SalvarPDF(FGNRERetorno.pdfGuias, NumeroControle, xData, DocEmitente, '');
              PDFJaSalvo := True;
            end;
          end;
        end;
      end;
    end;
  end;

  // Verificando se existe alguma guia nao confirmada
  for I := 0 to FGuias.Count - 1 do
  begin
    if not FGuias.Items[I].Confirmada then
    begin
      FPMsg := ACBrStr('Guia(s) não confirmadas:') + LineBreak;
      break;
    end;
  end;

  // Montando a mensagem de retorno para as guias nao confirmadas
  for I := 0 to FGuias.Count - 1 do
  begin
    if not FGuias.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FGuias.Items[I].GNRE.c02_receita) +
        '->' + FGuias.Items[I].Msg + LineBreak;
  end;
end;

function TGNRERetRecepcao.SalvarTXT(AResultado: String; Item: Integer): Boolean;
var
  SLAux: TStringList;
  RepresentacaoNumerica, SituacaoGuia, PathNome: String;
begin
  SLAux := TStringList.Create;
  try
    if SameText(Copy(AResultado, 1, 1), '1') then
    begin
      SituacaoGuia := Trim(Copy(AResultado, 6, 1));
      if SameText(SituacaoGuia, '0') then
      begin
        SLAux.Add(AResultado);
        RepresentacaoNumerica := Copy(AResultado, 979, 48);

        if FPConfiguracoesGNRE.Arquivos.SalvarTXT then
        begin
          if not DirectoryExists(FPConfiguracoesGNRE.Arquivos.PathArqTXT) then
            ForceDirectories(FPConfiguracoesGNRE.Arquivos.PathArqTXT);

          PathNome := PathWithDelim(FPConfiguracoesGNRE.Arquivos.PathArqTXT) +
                     RepresentacaoNumerica+'-guia.txt';

          FGNRERetorno.resGuia.Items[Item].NomeArq := PathNome;

          SLAux.SaveToFile(PathNome);
        end;
      end;
    end;

    SLAux.Clear;
    SituacaoGuia := '';
    RepresentacaoNumerica := '';

  finally
    FreeAndNil(SLAux);
    Result := True;
  end;
end;

function TGNRERetRecepcao.SalvarXML(AGuia, ANumero: String; aData: TDateTime;
  aCNPJ, aIE: string; Item: Integer): Boolean;
var
  PathNome: string;
begin
  Result := True;

  if FPConfiguracoesGNRE.Arquivos.Salvar then
  begin
    PathNome := FPConfiguracoesGNRE.Arquivos.GetPathGNRE(aData, aCNPJ, aIE);

    PathNome := PathWithDelim(PathNome) + ANumero + '-guia.xml';

    FGNRERetorno.resGuia.Items[Item].NomeArq := PathNome;

    Result := FPDFeOwner.Gravar(PathNome, AGuia);
  end;
end;

function TGNRERetRecepcao.SalvarPDF(AGuia, ANumero: String; aData: TDateTime;
  aCNPJ, aIE: string): Boolean;
var
  aPath, aNomeArq, aArq: string;
begin
  Result := True;

  aArq := DecodeBase64(AGuia);

  if FPConfiguracoesGNRE.Arquivos.Salvar then
  begin
    aPath := FPConfiguracoesGNRE.Arquivos.GetPathGNRE(aData, aCNPJ, aIE);

    aNomeArq := PathWithDelim(aPath) + ANumero + '-guias.pdf';

    WriteToTXT(aNomeArq, aArq, False, False);
  end;
end;

{ TGNRERecibo }

constructor TGNRERecibo.Create(AOwner: TACBrDFe; AGuias: TGuias);
begin
  inherited Create(AOwner);

  FGuias := AGuias;
end;

destructor TGNRERecibo.Destroy;
begin
  FGNRERetorno.Free;

  inherited Destroy;
end;

procedure TGNRERecibo.Clear;
begin
  inherited Clear;

  FPStatus := stGNRERetRecepcao;
  FPLayout := LayGNRERetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  Fcodigo := 0;
  Fresultado := '';
  Fdescricao := '';
  Fprotocolo := '';

  if Assigned(FPConfiguracoesGNRE) then
  begin
    FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRE.WebServices.UFCodigo;
  end;

  if Assigned(FGNRERetorno) then
    FGNRERetorno.Free;

  FGNRERetorno := TTResultLote_GNRE.Create;
end;

procedure TGNRERecibo.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreResultadoLote';
  FPSoapAction := FPServico;
end;

procedure TGNRERecibo.DefinirURL;
var
  VerServ: Double;
begin
  (*
  if FGuias.Count > 0 then    // Tem GNRE ? Se SIM, use as informações do XML
  begin
    VerServ := 1.00;;

//    if FPConfiguracoesGNRE.WebServices.Ambiente <> FGuias.Items[0].GNRE.Ide.Ambiente then
//      raise EACBrGNREException.Create( CErroAmbienteDiferente );
  end
  else
  begin                   // Se não tem GNRE, use as configurações do componente
    VerServ := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  end;
  *)

  VerServ   := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  FPLayout := LayGNRERetRecepcao;

  TACBrGNRE(FPDFeOwner).LerServicoDeParams(
    'GNRE',
    'PE',
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TGNRERecibo.DefinirDadosMsg;
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

function TGNRERecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  //A função UTF8ToNativeString deve ser removida quando refatorado para usar ACBrXmlDocument
  FGNRERetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(AnsiString(FPRetWS)));
  FGNRERetorno.LerXML;

  FAmbiente  := FGNRERetorno.Ambiente;
  Fcodigo    := FGNRERetorno.codigo;
  Fdescricao := FGNRERetorno.descricao;
  Fresultado := FGNRERetorno.resultado;
  FPMsg      := FGNRERetorno.descricao;

  Result := (FGNRERetorno.codigo = 402); // 402 = Lote processado com sucesso.
end;

function TGNRERecibo.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak),
                   [TpAmbToStr(FGNRERetorno.Ambiente),
                    IntToStr(FGNRERetorno.codigo),
                    FGNRERetorno.descricao]);
  {*)}
end;

function TGNRERecibo.GerarPrefixoArquivo: String;
begin
  Result := numeroRecibo;
end;

{ TGNREConsultaUF }

constructor TGNREConsultaUF.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TGNREConsultaUF.Destroy;
begin
  FGNRERetorno.Free;

  inherited Destroy;
end;

procedure TGNREConsultaUF.Clear;
begin
  inherited Clear;

  FPStatus := stGNRERetRecepcao;
  FPLayout := LayGNRERetRecepcao;
  FPArqEnv := 'ped-cfg';
  FPArqResp := 'cfg';
  Fcodigo := 0;
  Fdescricao := '';
  FexigeReceita := '';
  FexigeUfFavorecida := '';
  FexigeDetalhamentoReceita := '';
  FexigeContribuinteEmitente := '';
  FexigeDataVencimento := '';
  FexigeDocumentoOrigem := '';
  FexigePeriodoApuracao := '';
  FexigeDataPagamento := '';
  FexigePeriodoReferencia := '';
  FexigeContribuinteDestinatario := '';
  FexigeParcela := '';
  FexigeProduto := '';
  FexigeConvenio := '';

  if Assigned(FPConfiguracoesGNRE) then
  begin
    FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRE.WebServices.UFCodigo;
  end;

  if Assigned(FGNRERetorno) then
    FGNRERetorno.Free;

  FGNRERetorno := TTConfigUf.Create;
end;

procedure TGNREConsultaUF.DefinirDadosMsg;
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

procedure TGNREConsultaUF.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'GnreConfigUF';
  FPSoapAction := FPServico;
end;

procedure TGNREConsultaUF.DefinirURL;
var
  VerServ: Double;
begin
  VerServ   := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  FPLayout := LayGNREConsultaConfigUF;

  TACBrGNRE(FPDFeOwner).LerServicoDeParams(
    'GNRE',
    'PE',
    FAmbiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

function TGNREConsultaUF.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  //A função UTF8ToNativeString deve ser removida quando refatorado para usar ACBrXmlDocument
  FGNRERetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(AnsiString(FPRetWS)));
  FGNRERetorno.LerXML;

  FAmbiente                      := FGNRERetorno.Ambiente;
  Fcodigo                        := FGNRERetorno.codigo;
  Fdescricao                     := UTF8Decode(FGNRERetorno.descricao);
  FexigeReceita                  := FGNRERetorno.exigeReceita;
  FexigeDetalhamentoReceita      := FGNRERetorno.exigeDetalhamentoReceita;
  FexigeContribuinteEmitente     := FGNRERetorno.exigeContribuinteEmitente;
  FexigeDataVencimento           := FGNRERetorno.exigeDataVencimento;
  FexigeDocumentoOrigem          := FGNRERetorno.exigeDocumentoOrigem;
  FexigePeriodoApuracao          := FGNRERetorno.exigePeriodoApuracao;
  FexigeDataPagamento            := FGNRERetorno.exigeDataPagamento;
  FexigePeriodoReferencia        := FGNRERetorno.exigePeriodoReferencia;
  FexigeContribuinteDestinatario := FGNRERetorno.exigeContribuinteDestinatario;
  FexigeParcela                  := FGNRERetorno.exigeParcela;
  FexigeProduto                  := FGNRERetorno.exigeProduto;
  FexigeConvenio                 := FGNRERetorno.exigeConvenio;
  FexigeUfFavorecida             := FGNRERetorno.exigeUfFavorecida;
  FUf                            := FGNRERetorno.Uf;
  FPMsg                          := UTF8Decode(FGNRERetorno.descricao);

  Result := (FGNRERetorno.codigo = 450); // 450 = Consulta da configuração da UF realizada com sucesso.
end;

function TGNREConsultaUF.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak),
                   [TpAmbToStr(FGNRERetorno.Ambiente),
                    IntToStr(FGNRERetorno.codigo),
                    FGNRERetorno.descricao]);
  {*)}
end;

{ TGNREEnvioWebService }

constructor TGNREEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stEnvioWebService;
end;

destructor TGNREEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TGNREEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TGNREEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TGNREEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TGNREEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TGNREEnvioWebService.DefinirDadosMsg;
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

function TGNREEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TGNREEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: ' + FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TGNREEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrGNRE := TACBrGNRE(AOwner);

  FEnviar     := TGNRERecepcao.Create(FACBrGNRE, TACBrGNRE(FACBrGNRE).Guias);
  FRetorno    := TGNRERetRecepcao.Create(FACBrGNRE, TACBrGNRE(FACBrGNRE).Guias);
  FRecibo     := TGNRERecibo.Create(FACBrGNRE, TACBrGNRE(FACBrGNRE).Guias);
  FConsultaUF := TGNREConsultaUF.Create(FACBrGNRE);
end;

destructor TWebServices.Destroy;
begin
  FEnviar.Free;
  FRetorno.Free;
  FRecibo.Free;
  FConsultaUF.Free;

  inherited Destroy;
end;

function TWebServices.Envia: Boolean;
begin
  if not FEnviar.Executar then
    FEnviar.GerarException( FEnviar.Msg );

  FRetorno.numeroRecibo := FEnviar.numero;

  if not FRetorno.Executar then
    FRetorno.GerarException(FRetorno.Msg);

  Result := True;
end;

function TWebServices.ConsultaResultadoLote(ANumRecibo: String): Boolean;
begin
  FRecibo.numeroRecibo := ANumRecibo;

  if not FRecibo.Executar then
    FRecibo.GerarException( FRecibo.Msg );

  Result := true;
end;

end.
