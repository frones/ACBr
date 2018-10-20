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

  { TGNREWebService }

  TGNREWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrGNRE;
    FPLayout: TLayOutGNRE;
    FPConfiguracoesGNRE: TConfiguracoesGNRE;

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
  end;

  { TGNRERecepcao }

  TGNRERecepcao = class(TGNREWebService)
  private
    Fnumero: String;
    FtempoEstimadoProc: Integer;
    Fcodigo: Integer;
    Fdescricao: String;
    FdataHoraRecibo: TDateTime;
    FAmbiente: TpcnTipoAmbiente;
    FGuias: TGuias;

    FGNRERetorno: TTretLote_GNRE;
    FcUF: Integer;

//    function GetLote: String;
    function GetRecibo: String;
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
    property tempoEstimadoProc: Integer read FtempoEstimadoProc write FtempoEstimadoProc;
    property cUF: Integer read FcUF;
  end;

  { TGNRERetRecepcao }

  TGNRERetRecepcao = class(TGNREWebService)
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
    function SalvarTXT(AResultado: String): Boolean;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property numeroRecibo: String read FnumeroRecibo write FnumeroRecibo;
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
  StrUtils, Math,
  ACBrUtil, ACBrGNRE2,
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
  Result := FPDFeOwner.GetNameSpaceURI + '/webservice/';
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
  vGuias: String;
begin
  vGuias := '';
  for i := 0 to FGuias.Count - 1 do
    vGuias := vGuias + FGuias.Items[i].XML;

  FPDadosMsg := '<TLote_GNRE xmlns="http://www.gnre.pe.gov.br">' +
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
  xUF: String;
  ok: Boolean;
  VerServ: Double;
begin
  if FGuias.Count > 0 then    // Tem GNRE ? Se SIM, use as informações do XML
  begin
    VerServ := 1.00; //FGuias.Items[0].GNRE.infGNRE.Versao;

  end
  else
  begin                   // Se não tem GNRE, use as configurações do componente
    VerServ := VersaoGNREToDbl(FPConfiguracoesGNRE.Geral.VersaoDF);
  end;

  FAmbiente  := FPConfiguracoesGNRE.WebServices.Ambiente;
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
}
function TGNRERecepcao.GetRecibo: String;
begin
  Result := Trim(numero);
end;

function TGNRERecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'processarResponse');

  FGNRERetorno.Leitor.Arquivo := ParseText(FPRetWS);
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

  FGuias := AGuias;
end;

destructor TGNRERetRecepcao.Destroy;
begin
  FGNRERetorno.Free;

  inherited Destroy;
end;

procedure TGNRERetRecepcao.Clear;
var
  i, j: Integer;
begin
  inherited Clear;

  FPStatus := stGNRERetRecepcao;
  FPLayout := LayGNRERetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

//  FnumeroRecibo := '';
  Fcodigo := 0;
  Fresultado := '';
  Fdescricao := '';
  Fprotocolo := '';

  if Assigned(FPConfiguracoesGNRE) then
  begin
    FAmbiente := FPConfiguracoesGNRE.WebServices.Ambiente;
    FcUF := FPConfiguracoesGNRE.WebServices.UFCodigo;
  end;

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
  ok: Boolean;
begin
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

function TGNRERetRecepcao.GetRecibo: String;
begin
  Result := Trim(numeroRecibo);
end;

function TGNRERetRecepcao.TratarResposta: Boolean;
var
  SL: TStringList;
  I: Integer;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'gnreRespostaMsg');

  FGNRERetorno.Leitor.Arquivo := ParseText(FPRetWS);
  FGNRERetorno.LerXML;

  FAmbiente  := FGNRERetorno.Ambiente;
  Fcodigo    := FGNRERetorno.codigo;
  Fdescricao := FGNRERetorno.descricao;
  Fresultado := FGNRERetorno.resultado;
  // Para aparecer as exceções que ocorreram / caso haja alguma
  SL := TStringList.Create;
  SL.Clear;
  for I := 0 to GNRERetorno.resRejeicaGuia.Count - 1 do
   SL.Add(Trim(GNRERetorno.resRejeicaGuia.Items[I].DescMotivoRejeicao)+#13);
  FPMsg      := FGNRERetorno.descricao + #13 + Trim(SL.Text);
  SL.Free;
  //
  Result := (FGNRERetorno.codigo = 401); // Lote em Processamento
end;

function TGNRERetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: Integer;
  SalvarXML: Boolean;
begin
  Result := False;
  //Verificando se existe alguma guia confirmada
  for I := 0 to FGuias.Count - 1 do
  begin
    if FGuias.Items[I].Confirmada then
    begin
      Result := True;
      Self.SalvarTXT(FGNRERetorno.resultado);
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
      FPMsg := FPMsg + IntToStr(FGuias.Items[I].GNRE.c02_receita) +
        '->' + FGuias.Items[I].Msg + LineBreak;
  end;

end;

function TGNRERetRecepcao.SalvarTXT(AResultado: String): Boolean;
var
  SL, SLAux: TStringList;
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
          if FPConfiguracoesGNRE.Arquivos.SalvarTXT then
          begin
            if not DirectoryExists(FPConfiguracoesGNRE.Arquivos.PathArqTXT) then
              ForceDirectories(FPConfiguracoesGNRE.Arquivos.PathArqTXT);

            SLAux.SaveToFile(PathWithDelim(FPConfiguracoesGNRE.Arquivos.PathArqTXT)+RepresentacaoNumerica+'-gnre.txt');
          end;
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

  FnumeroRecibo := '';
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
  ok: Boolean;
begin
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

  FGNRERetorno.Leitor.Arquivo := ParseText(FPRetWS);
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
  ok: Boolean;
begin
  VerServ := 1.00;
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

  FGNRERetorno.Leitor.Arquivo := ParseText(FPRetWS);
  FGNRERetorno.LerXML;

  FAmbiente          := FGNRERetorno.Ambiente;
  Fcodigo            := FGNRERetorno.codigo;
  Fdescricao         := UTF8Decode(FGNRERetorno.descricao);
  FexigeReceita      := FGNRERetorno.exigeReceita;
  FexigeUfFavorecida := FGNRERetorno.exigeUfFavorecida;
  FUf                := FGNRERetorno.Uf;
  FPMsg              := UTF8Decode(FGNRERetorno.descricao);

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
