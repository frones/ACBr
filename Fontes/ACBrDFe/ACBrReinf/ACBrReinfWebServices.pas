{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

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
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

{$I ACBr.inc}

unit ACBrReinfWebServices;

interface

uses
  Classes, SysUtils,
  ACBrUtil, ACBrDFe, ACBrDFeWebService,
  pcnLeitor,
  ACBrReinfLoteEventos, ACBrReinfConfiguracoes,
  pcnConversaoReinf, pcnCommonReinf, pcnReinfRetEventos, pcnReinfConsulta,
  pcnReinfRetConsulta;

type
  { TReinfWebService }

  TReinfWebService = class(TDFeWebService)
  private
    FPStatus: TStatusReinf;
    FPLayout: TLayOutReinf;
    FPConfiguracoesReinf: TConfiguracoesReinf;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusReinf read FPStatus;
    property Layout: TLayOutReinf read FPLayout;
  end;

  { TEnvioLote }

  TEnvioLote = class(TReinfWebService)
  private
    FVersao: String;
    FLote: TLoteEventos;
    FRetEnvioLote: TRetEnvioLote;
    FVersaoDF: TVersaoReinf;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;
    procedure InicializarServico; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

    procedure Clear; override;
    procedure BeforeDestruction; override;

    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
  end;

  { TConsultar }

  TConsultar = class(TReinfWebService)
  private
    FProtocolo: string;
    FRetConsulta: TRetConsulta;
    FVersaoDF: TVersaoReinf;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;
    procedure InicializarServico; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

    procedure Clear; override;
    procedure BeforeDestruction; override;

    property Protocolo: string read FProtocolo write FProtocolo;
    property RetConsulta: TRetConsulta read FRetConsulta;
  end;

  { TConsultarReciboEvento }

  TConsultarReciboEvento = class(TReinfWebService)
  private
    FtipoEvento: TTipoEvento;
    FtpEventoStr: String;
    FperApur: String;
    FnrInscEstab: string;
    FcnpjPrestador: string;
    FnrInscTomador: string;
    FdtApur: TDateTime;

    FRetConsulta: TRetConsulta;
    FVersaoDF: TVersaoReinf;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirEnvelopeSoap; override;
    procedure InicializarServico; override;

    function TratarResposta: Boolean; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;
    procedure BeforeDestruction; override;

    property tipoEvento: TTipoEvento read FtipoEvento    write FtipoEvento;
    property tpEventoStr: String     read FtpEventoStr   write FtpEventoStr;
    property perApur: string         read FperApur       write FperApur;
    property nrInscEstab: string     read FnrInscEstab   write FnrInscEstab;
    property cnpjPrestador: string   read FcnpjPrestador write FcnpjPrestador;
    property nrInscTomador: string   read FnrInscTomador write FnrInscTomador;
    property dtApur: TDateTime       read FdtApur        write FdtApur;

    property RetConsulta: TRetConsulta read FRetConsulta;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrReinf: TACBrDFe;
    FEnvioLote: TEnvioLote;
    FConsultar: TConsultar;
    FConsultarReciboEvento: TConsultarReciboEvento;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia: Boolean;
    function Consulta(const AProtocolo: string): Boolean;
    function ConsultaReciboEvento(const APerApur: String;
                                  ATipoEvento: TTipoEvento;
                                  const AnrInscEstab: String = '';
                                  const AcnpjPrestador: string = '';
                                  const AnrInscTomador: string = '';
                                  AdtApur: TDateTime = 0): Boolean;

    property ACBrReinf: TACBrDFe read FACBrReinf write FACBrReinf;
    property EnvioLote: TEnvioLote read FEnvioLote write FEnvioLote;
    property Consultar: TConsultar read FConsultar write FConsultar;
    property ConsultarReciboEvento: TConsultarReciboEvento read FConsultarReciboEvento write FConsultarReciboEvento;
  end;

implementation

uses
  blcksock, DateUtils,
  pcnConversao, pcnGerador,
  ACBrReinf;

{ TReinfWebService }

constructor TReinfWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesReinf := TConfiguracoesReinf(FPConfiguracoes);
  FPStatus := stIdle;

  FPSoapVersion   := 'soap';
  FPHeaderElement := 'Header';
  FPBodyElement   := 'Body';

  FPSoapEnvelopeAtributtes :=
    ' xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ' +
    ' xmlns:v1="http://sped.fazenda.gov.br/"';

  FPMimeType := 'text/xml'; // Vazio, usará por default: 'application/soap+xml'
end;

procedure TReinfWebService.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TReinfWebService.GerarPrefixoArquivo: String;
begin
  Result := 'Reinf';
end;

function TReinfWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := 'v1.03.0';
   TACBrReinf(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := ''; // '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TReinfWebService.Clear;
begin
  inherited Clear;

  FPStatus := stIdle;
  if Assigned(FPDFeOwner) and Assigned(FPDFeOwner.SSL) then
    FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TReinfWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrReinf(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TReinfWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrReinf(FPDFeOwner).SetStatus(stIdle);
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
  Versao := VersaoReinfToDbl(FVersaoDF);
  FPVersaoServico := '';
  FPURL := '';

  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TEnvioLote.DefinirServicoEAction;
begin
  FPServico := FPDFeOwner.GetNameSpaceURI + '/ReceberLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TEnvioLote.DefinirDadosMsg;
begin
  FLote.GerarXML;

  FPDadosMsg := FLote.Xml;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, erEnvioLote);
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
  Texto := Texto + '<' + 'v1:ReceberLoteEventos>';
  Texto := Texto + '<' + 'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' + '/v1:loteEventos>';
  Texto := Texto + '<' + '/v1:ReceberLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

function TEnvioLote.TratarResposta: Boolean;
var
  i: Integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ReceberLoteEventosResult');

  FRetEnvioLote.Leitor.Arquivo := ParseText(FPRetWS);
  FRetEnvioLote.LerXml;

  for i := 0 to FRetEnvioLote.evento.Count - 1 do
  begin
    AXML := FRetEnvioLote.evento.Items[i].ArquivoReinf;

    if AXML <> '' then
    begin
      NomeArq := FRetEnvioLote.evento.Items[i].Id + '-R5001.xml';

      if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML, '',False);
    end;
  end;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPRetWS, erRetornoLote);

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
//                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoReinfToStr(FVersaoDF),
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente),
//                  FRetEnvioLote.dadosRecLote.versaoAplicRecepcao,
                  IntToStr(FRetEnvioLote.Status.cdStatus),
                  FRetEnvioLote.Status.descRetorno]);

//    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
//       [IfThen(FRetEnvioLote.dadosRecLote.dhRecepcao = 0, '',
//               FormatDateTimeBr(FRetEnvioLote.dadosRecLote.dhRecepcao))]);

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

procedure TEnvioLote.InicializarServico;
begin
  FVersaoDF := FPConfiguracoesReinf.Geral.VersaoDF;

  inherited InicializarServico;
end;

{ TConsultar }

constructor TConsultar.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
end;

procedure TConsultar.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaLoteEventos;
  FPStatus := stConsultaLote;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  if Assigned(FRetConsulta) then
    FRetConsulta.Free;

  FRetConsulta := TRetConsulta.Create;
end;

procedure TConsultar.BeforeDestruction;
begin
  inherited;

  FRetConsulta.Free;
end;

procedure TConsultar.DefinirDadosMsg;
var
  tpInsc, nrInsc: String;
begin
  nrInsc := TACBrReinf(FPDFeOwner).Configuracoes.Geral.IdContribuinte;

  if Length(nrInsc) = 14 then
  begin
    nrInsc := Copy( nrInsc, 1, 8 );
    tpInsc := '1';
  end
  else
    tpInsc := '2';

  FPDadosMsg :=
            '<consultar' + FPSoapEnvelopeAtributtes + '>' +
            '<v1:tipoInscricaoContribuinte>' + tpInsc + '</v1:tipoInscricaoContribuinte>' +
            '<v1:numeroInscricaoContribuinte>' + nrInsc + '</v1:numeroInscricaoContribuinte>' +
            '<v1:numeroProtocoloFechamento>' + FProtocolo + '</v1:numeroProtocoloFechamento>' +
            '</consultar>';

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, erEnvioConsulta);
end;

procedure TConsultar.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:ConsultaInformacoesConsolidadas>';
  Texto := Texto + SeparaDados(DadosMsg, 'consultar');
  Texto := Texto + '<' +  '/v1:ConsultaInformacoesConsolidadas>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TConsultar.DefinirServicoEAction;
begin
  FPServico := ACBRREINF_NAMESPACE_CON +
               '/ConsultaInformacoesConsolidadas';
  FPSoapAction := Trim(FPServico);
end;

procedure TConsultar.DefinirURL;
var
  Versao: Double;
begin
  Versao := VersaoReinfToDbl(FVersaoDF);
  FPVersaoServico := '';
  FPURL := '';

  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TConsultar.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak
//                         'Versão Aplicativo: %s ' + LineBreak +
//                         'Status Código: %s ' + LineBreak +
//                         'Status Descrição: %s ' + LineBreak
                        ),
                 [VersaoReinfToStr(FVersaoDF),
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente)
//                  FRetEnvioLote.dadosRecLote.versaoAplicRecepcao,
//                  IntToStr(FRetConsulta.Status.cdStatus),
//                  FRetConsulta.Status.descRetorno
                  ]);

  Result := aMsg;
end;

procedure TConsultar.InicializarServico;
begin
  FVersaoDF := FPConfiguracoesReinf.Geral.VersaoDF;

  inherited InicializarServico;
end;

function TConsultar.TratarResposta: Boolean;
var
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultaInformacoesConsolidadasResult');

  FRetConsulta.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsulta.LerXml;

  AXML := FRetConsulta.XML;

  if AXML <> '' then
  begin
    NomeArq := FRetConsulta.evtTotalContrib.Id + '-R5011.xml';

    if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
      FPDFeOwner.Gravar(NomeArq, AXML, '',False);
  end;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPRetWS, erRetornoConsulta);

  Result := True;
end;

{ TConsultarReciboEvento }

procedure TConsultarReciboEvento.BeforeDestruction;
begin
  inherited;

  FRetConsulta.Free;
end;

procedure TConsultarReciboEvento.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaLoteEventos;
  FPStatus := stConsultaLote;
  FPArqEnv := 'ped-con';
  FPArqResp := 'con';

  if Assigned(FRetConsulta) then
    FRetConsulta.Free;

  FRetConsulta := TRetConsulta.Create;
end;

constructor TConsultarReciboEvento.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);
  FtipoEvento := teR1000;
  tpEventoStr := '1000';
  FperApur := '';
  FnrInscEstab := '';
  FcnpjPrestador := '';
  FnrInscTomador := '';
  FdtApur := 0;
end;

procedure TConsultarReciboEvento.DefinirDadosMsg;
var
  Consulta: TReinfConsulta;
begin
  Consulta := TReinfConsulta.Create;
  try
    Consulta.SoapEnvelope  := FPSoapEnvelopeAtributtes;
    Consulta.TipoEvento    := TipoEvento;
    Consulta.nrInscContrib := TACBrReinf(FPDFeOwner).Configuracoes.Geral.IdContribuinte;
    Consulta.nrInscEstab   := nrInscEstab;
    Consulta.perApur       := perApur;
    Consulta.cnpjPrestador := cnpjPrestador;
    Consulta.nrInscTomador := nrInscTomador;
    Consulta.dtApur        := dtApur;

    AjustarOpcoes( Consulta.Gerador.Opcoes );
    Consulta.GerarXML;

    // Atribuindo o XML para propriedade interna //
    FPDadosMsg := Consulta.Gerador.ArquivoFormatoXML;
  finally
    Consulta.Free;
  end;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, erEnvioConsulta);
end;

procedure TConsultarReciboEvento.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}
  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:ConsultaReciboEvento' + tpEventoStr + '>';
  Texto := Texto + SeparaDados(DadosMsg, 'consultar');
  Texto := Texto + '<' +  '/v1:ConsultaReciboEvento' + tpEventoStr + '>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TConsultarReciboEvento.DefinirServicoEAction;
begin
  FPServico := ACBRREINF_NAMESPACE_CON +
                 '/ConsultaReciboEvento' + tpEventoStr;
  FPSoapAction := Trim(FPServico);
end;

procedure TConsultarReciboEvento.DefinirURL;
var
  Versao: Double;
begin
  Versao := VersaoReinfToDbl(FVersaoDF);
  FPVersaoServico := '';
  FPURL := '';

  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TConsultarReciboEvento.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak
//                         'Versão Aplicativo: %s ' + LineBreak +
//                         'Status Código: %s ' + LineBreak +
//                         'Status Descrição: %s ' + LineBreak
                        ),
                 [VersaoReinfToStr(FVersaoDF),
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente)
//                  FRetEnvioLote.dadosRecLote.versaoAplicRecepcao,
//                  IntToStr(FRetConsulta.Status.cdStatus),
//                  FRetConsulta.Status.descRetorno
                  ]);

  Result := aMsg;
end;

function TConsultarReciboEvento.GerarPrefixoArquivo: String;
begin
  Result := tpEventoStr;
end;

procedure TConsultarReciboEvento.InicializarServico;
begin
  FVersaoDF := FPConfiguracoesReinf.Geral.VersaoDF;

  inherited InicializarServico;
end;

function TConsultarReciboEvento.TratarResposta: Boolean;
var
  AXML: String;
begin
  //aqui nao esta pronto
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultaReciboEvento' + tpEventoStr + 'Result');

  FRetConsulta.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsulta.LerXml;

  AXML := FRetConsulta.XML;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPRetWS, erRetornoConsulta);

  Result := True;
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrReinf := TACBrDFe(AOwner);

  FEnvioLote    := TEnvioLote.Create(FACBrReinf);
  FConsultar := TConsultar.Create(FACBrReinf);
  FConsultarReciboEvento := TConsultarReciboEvento.Create(FACBrReinf);
end;

destructor TWebServices.Destroy;
begin
  FEnvioLote.Free;
  FConsultar.Free;
  FConsultarReciboEvento.Free;

  inherited Destroy;
end;

function TWebServices.Envia: Boolean;
begin
  FEnvioLote.Clear;

{$IFDEF FPC}
  Result := False;
{$ENDIF}

  if not EnvioLote.Executar then
    EnvioLote.GerarException(EnvioLote.Msg);

  Result := True;
end;

function TWebServices.Consulta(const AProtocolo: string): Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultar.FProtocolo := AProtocolo;

  if not FConsultar.Executar then
    FConsultar.GerarException(FConsultar.Msg);

  Result := True;
end;

function TWebServices.ConsultaReciboEvento(const APerApur: String;
                                  ATipoEvento: TTipoEvento;
                                  const AnrInscEstab: String;
                                  const AcnpjPrestador: string;
                                  const AnrInscTomador: string;
                                  AdtApur: TDateTime): Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}

  FConsultarReciboEvento.FperApur      := APerApur;
  FConsultarReciboEvento.FtipoEvento   := ATipoEvento;
  FConsultarReciboEvento.tpEventoStr   := Copy(TipoEventoToStr(ATipoEvento), 3, 4);
  FConsultarReciboEvento.nrInscEstab   := AnrInscEstab;
  FConsultarReciboEvento.cnpjPrestador := AcnpjPrestador;
  FConsultarReciboEvento.nrInscTomador := AnrInscTomador;
  FConsultarReciboEvento.dtApur        := AdtApur;

  if not FConsultarReciboEvento.Executar then
    FConsultarReciboEvento.GerarException(FConsultarReciboEvento.Msg);

  Result := True;
end;

end.
