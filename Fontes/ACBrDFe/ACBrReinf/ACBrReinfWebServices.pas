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
  ACBrUtil, ACBrDFe, ACBrDFeWebService, ACBrReinfConfiguracoes, pcnLeitor,
  pcnGerador, pcnConversaoReinf, pcnReinfRetEventos, pcnReinfRetConsulta;

type
  { TReinfWebService }

  TReinfWebService = class(TDFeWebService)
  private
    FPStatus: TStatusReinf;
    FPLayout: TLayOutReinf;
    FXMLEnvio: string;

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
    property XMLEnvio: string read FXMLEnvio write FXMLEnvio;
  end;

  { TEnvioLote }

  TEnvioLote = class(TReinfWebService)
  private
    FVersao: String;
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

    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
  end;

  { TConsultar }

  TConsultar = class(TReinfWebService)
  private
    FVersao: String;
    FProtocolo: String;
    FRetConsulta: TRetConsulta;
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

    property Protocolo: String read FProtocolo;
    property RetConsulta: TRetConsulta read FRetConsulta;
  end;

  TWebServices = class
  private
    FACBrReinf: TACBrDFe;
    FEnvioLote : TEnvioLote;
    FConsultar: TConsultar;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Enviar(const AXML: string): Boolean;
    function Consulta(const AProtocolo: string): Boolean;

    property ACBrReinf: TACBrDFe read FACBrReinf write FACBrReinf;
    property EnvioLote: TEnvioLote read FEnvioLote write FEnvioLote;
    property Consultar: TConsultar read FConsultar write FConsultar;
  end;

implementation

uses
  StrUtils, blcksock, DateUtils,
  pcnConversao, pcnReinfClasses,
  ACBrReinf;

{ TReinfWebService }

constructor TReinfWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesReinf := TConfiguracoesReinf(FPConfiguracoes);
  FPStatus := stIdle;

  FPSoapVersion   := 'soap';
  FPHeaderElement := '';
  FPBodyElement   := 'Body';

  FPSoapEnvelopeAtributtes :='xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v1="http://sped.fazenda.gov.br/"';

  FPMimeType := 'text/xml';
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
    FPVersaoServico := 'v1_03_00';
  // TACBreSocial(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := ''; // '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TreinfWebService.Clear;
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

  TACBrreinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TEnvioLote.DefinirServicoEAction;
begin
  FPServico := FPDFeOwner.GetNameSpaceURI + '/ReceberLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TEnvioLote.DefinirDadosMsg;
begin
  FPDadosMsg := FXMLEnvio;
  (*
  with FLote.IdeEmpregador do
  begin
    if Length(TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdEmpregador) = 14 then
      TpInsc := tiCNPJ
    else
      TpInsc := tiCPF;

    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdEmpregador;
  end;

  with FLote.IdeTransmissor do
  begin
    if Length(TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdTransmissor) = 14 then
      TpInsc := tiCNPJ
    else
      TpInsc := tiCPF;

    NrInsc := TACBreSocial(FPDFeOwner).Configuracoes.Geral.IdTransmissor;
  end;

  FLote.GerarXML(AGrupo);

  FPDadosMsg := FLote.Xml;

  if Assigned(TACBreSocial(FPDFeOwner).OnTransmissaoEventos) then
    TACBreSocial(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, eseEnvioLote);
  *)
end;

procedure TEnvioLote.DefinirEnvelopeSoap;
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
  Texto := Texto + '<' + 'v1:ReceberLoteEventos>';
  Texto := Texto + '<' +  'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' +  '/v1:loteEventos>';
  Texto := Texto + '<' +  '/v1:ReceberLoteEventos>';
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

  if Assigned(TACBrReinf(FPDFeOwner).OnAfterEnviar) then
    TACBrReinf(FPDFeOwner).OnAfterEnviar(FPRetWS);

  for i := 0 to FRetEnvioLote.evento.Count - 1 do
  begin
    AXML := FRetEnvioLote.evento.Items[i].ArquivoReinf;

    if AXML <> '' then
    begin
      NomeArq := FRetEnvioLote.evento.Items[i].Id + '-' +
                 FRetEnvioLote.evento.Items[i].Tipo + '.xml';

      if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML, '',False);
    end;
  end;

  Result := True;
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
                 ['2.4.01',
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
  Result := '';
end;

{ TConsultar }

constructor TConsultar.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

end;

procedure TConsultar.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaLoteEventos;
  FPStatus := stConsultaLote;
  FPArqEnv := 'sit-lot';
  FPArqResp := 'sit';
  FVersao := '';

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
            '<v1:tipoInscricaoContribuinte>' + tpInsc + '</v1:tipoInscricaoContribuinte>' +
            '<v1:numeroInscricaoContribuinte>' + nrInsc + '</v1:numeroInscricaoContribuinte>' +
            '<v1:numeroReciboFechamento>' + FProtocolo + '</v1:numeroReciboFechamento>';

//  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
//    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, eseEnvioLote);
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
  Texto := Texto + DadosMsg;
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
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TConsultar.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: ' + FPServico + #13#10 +
    '- Inativo ou Inoperante tente novamente.');
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
                 ['2.4.01',
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente)
//                  FRetEnvioLote.dadosRecLote.versaoAplicRecepcao,
//                  IntToStr(FRetConsulta.Status.cdStatus),
//                  FRetConsulta.Status.descRetorno
                  ]);

  Result := aMsg;
end;

function TConsultar.GerarPrefixoArquivo: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TConsultar.GerarVersaoDadosSoap: String;
begin
  Result := '';
end;

function TConsultar.TratarResposta: Boolean;
var
  i: Integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ConsultaInformacoesConsolidadasResult');

  FRetConsulta.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsulta.LerXml;

  if Assigned(TACBrReinf(FPDFeOwner).OnAfterEnviar) then
    TACBrReinf(FPDFeOwner).OnAfterEnviar(FPRetWS);

  for i := 0 to FRetConsulta.RetEventos.Count - 1 do
  begin
    AXML := FRetConsulta.RetEventos.Items[i].ArquivoReinf;

    if AXML <> '' then
    begin
      NomeArq := FRetConsulta.RetEventos.Items[i].Id + '-' +
                 FRetConsulta.RetEventos.Items[i].Tipo + '.xml';

      if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML, '',False);
    end;
  end;

  Result := True;
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrReinf := TACBrDFe(AOwner);

  FEnvioLote := TEnvioLote.Create(FACBrReinf);
  FConsultar := TConsultar.Create(FACBrReinf);
end;

destructor TWebServices.Destroy;
begin
  FEnvioLote.Free;
  FConsultar.Free;

  inherited Destroy;
end;

function TWebServices.Enviar(const AXML: string): Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}

  EnvioLote.XMLEnvio := Axml;

  if not EnvioLote.Executar then
    EnvioLote.GerarException(EnvioLote.Msg);

  Result := True;
end;

function TWebServices.Consulta(const AProtocolo: string): Boolean;
begin
  Consultar.FProtocolo := AProtocolo;

  if not Consultar.Executar then
    Consultar.GerarException(Consultar.Msg);

  Result := True;
end;

end.
