{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit ACBrReinfWebServices;

interface

uses
  Classes, SysUtils,
  ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.Base,
  ACBrDFe, ACBrDFeWebService,
  pcnLeitor, pcnConsts,
  ACBrReinfLoteEventos, ACBrReinfConfiguracoes,
  pcnConversaoReinf, pcnCommonReinf, pcnReinfRetEventos, pcnReinfConsulta,
  pcnReinfRetConsulta, pcnReinfRetConsulta_R9011, pcnReinfRetConsulta_R9015;

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
    FRetConsulta_R5011: TRetConsulta_R5011;
    FRetConsulta_R9011: TRetConsulta_R9011;
    FRetConsulta_R9015: TRetConsulta_R9015;
    FVersaoDF: TVersaoReinf;
    FRetEnvioLote: TRetEnvioLote;

    function GetRetConsulta: TRetConsulta; // Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
    function GetRetConsulta_R5011: TRetConsulta_R5011;
    function GetRetConsulta_R9011: TRetConsulta_R9011;
    function GetRetConsulta_R9015: TRetConsulta_R9015;
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

    property Protocolo: string read FProtocolo write FProtocolo;
    property RetConsulta: TRetConsulta read GetRetConsulta; // Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
    property RetConsulta_R5011: TRetConsulta_R5011 read GetRetConsulta_R5011;
    property RetConsulta_R9011: TRetConsulta_R9011 read GetRetConsulta_R9011;
    property RetConsulta_R9015: TRetConsulta_R9015 read GetRetConsulta_R9015;
    // Versão 2.1.1
    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
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
    FcpfCnpjBenef: String;
    FcnpjFonte: String;

    FRetConsulta_R5011: TRetConsulta_R5011;
    FRetConsulta_R9011: TRetConsulta_R9011;
    FRetConsulta_R9015: TRetConsulta_R9015;
    FVersaoDF: TVersaoReinf;
    FConsulta: TReinfConsulta;
    FRetEnvioLote: TRetEnvioLote;

    function GetRetConsulta: TRetConsulta; // Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
    function GetRetConsulta_R5011: TRetConsulta_R5011;
    function GetRetConsulta_R9011: TRetConsulta_R9011;
    function GetRetConsulta_R9015: TRetConsulta_R9015;
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
    property cpfCnpjBenef: String    read FcpfCnpjBenef  write FcpfCnpjBenef;
    property cnpjFonte: String       read FcnpjFonte     write FcnpjFonte;

    property RetConsulta: TRetConsulta read GetRetConsulta; // Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
    property RetConsulta_R5011: TRetConsulta_R5011 read GetRetConsulta_R5011;
    property RetConsulta_R9011: TRetConsulta_R9011 read GetRetConsulta_R9011;
    property RetConsulta_R9015: TRetConsulta_R9015 read GetRetConsulta_R9015;
    // Versão 2.1.1
    property Consulta: TReinfConsulta read FConsulta write FConsulta;
    property RetEnvioLote: TRetEnvioLote read FRetEnvioLote;
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
                                  AdtApur: TDateTime = 0;
                                  const AcpfCnpjBenef: string = '';
                                  const AcnpjFonte: string = ''): Boolean;

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
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TReinfWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := 'v1.03.0';
   TACBrReinf(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '';
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

  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
  begin
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
  end
  else
  begin
    FPMimeType := 'application/xml';
    FPValidateReturnCode := False;
    FPEnvelopeSoap := InserirDeclaracaoXMLSeNecessario(DadosMsg);
  end;
end;

function TEnvioLote.TratarResposta: Boolean;
var
  i: Integer;
  AXML, NomeArq: String;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
    FPRetWS := SeparaDados(FPRetornoWS, 'ReceberLoteEventosResult')
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'Reinf');

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
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [VersaoReinfToStr(FVersaoDF),
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente),
                  IntToStr(FRetEnvioLote.Status.cdStatus),
                  FRetEnvioLote.Status.descRetorno]);

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

  if Assigned(FRetEnvioLote) then
    FRetEnvioLote.Free;

  FRetEnvioLote := TRetEnvioLote.Create;

  if Assigned(FRetConsulta_R5011) then
    FRetConsulta_R5011.Free;

  if Assigned(FRetConsulta_R9011) then
    FRetConsulta_R9011.Free;

  if Assigned(FRetConsulta_R9015) then
    FRetConsulta_R9015.Free;

  FRetConsulta_R5011 := TRetConsulta_R5011.Create;
  FRetConsulta_R9011 := TRetConsulta_R9011.Create;
  FRetConsulta_R9015 := TRetConsulta_R9015.Create;
end;

procedure TConsultar.BeforeDestruction;
begin
  inherited;

  FRetEnvioLote.Free;

  FRetConsulta_R5011.Free;
  FRetConsulta_R9011.Free;
  FRetConsulta_R9015.Free;
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

  if FPConfiguracoesReinf.Geral.VersaoDF >= v1_05_01 then
    FPDadosMsg :=
            '<consultar' + FPSoapEnvelopeAtributtes + '>' +
            '<v1:tpInsc>' + tpInsc + '</v1:tpInsc>' +
            '<v1:nrInsc>' + nrInsc + '</v1:nrInsc>' +
            '<v1:numeroProtocoloFechamento>' + FProtocolo + '</v1:numeroProtocoloFechamento>' +
            '</consultar>'
  else
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
  xTag: string;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
  begin
    {$IFDEF FPC}
     Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
    {$ELSE}
     Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
    {$ENDIF}

    if FPConfiguracoesReinf.Geral.VersaoDF >= v1_05_01 then
      xTag := 'ConsultaResultadoFechamento2099'
    else
      xTag := 'ConsultaInformacoesConsolidadas';

    Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
    Texto := Texto + '<' + FPSoapVersion + ':Body>';
    Texto := Texto + '<' + 'v1:' + xTag + '>';
    Texto := Texto + SeparaDados(DadosMsg, 'consultar');
    Texto := Texto + '<' + '/v1:' + xTag + '>';
    Texto := Texto + '</' + FPSoapVersion + ':Body>';
    Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

    FPEnvelopeSoap := Texto;
  end
  else
  begin
    FPMimeType := 'application/xml';
    FPValidateReturnCode := False;
    FPURL := FPURL + '/' + FProtocolo;
    FPEnvelopeSoap := '';
  end;
end;

procedure TConsultar.DefinirServicoEAction;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF >= v1_05_01 then
    FPServico := ACBRREINF_NAMESPACE_CON +
                 '/ConsultaResultadoFechamento2099'
  else
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

  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    FPURL := FPURL + '/lotes';
end;

function TConsultar.GerarMsgLog: String;
var
  aMsg: String;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak
//                         'Status Código: %s ' + LineBreak +
//                         'Status Descrição: %s ' + LineBreak
                        ),
                 [VersaoReinfToStr(FVersaoDF),
                  TpAmbToStr(TACBrReinf(FPDFeOwner).Configuracoes.WebServices.Ambiente)
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
  i: Integer;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
  begin
    FPRetWS := SeparaDadosArray(['ConsultaInformacoesConsolidadasResult',
                                 'ConsultaResultadoFechamento2099Response'],
                                 FPRetornoWS);

    FRetConsulta_R5011.Leitor.Arquivo := ParseText(FPRetWS);
    FRetConsulta_R5011.LerXml;

    AXML := FRetConsulta_R5011.XML;

    if AXML <> '' then
      NomeArq := FRetConsulta_R5011.evtTotalContrib.Id + '-R5011.xml';

    if AXML <> '' then
    begin
      if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML, '',False);
    end;
  end
  else
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'retornoLoteEventosAssincrono');

    if FPRetWS <> '' then
      FPRetWS := '<retornoLoteEventosAssincrono>' + FPRetWS + '</retornoLoteEventosAssincrono>';

    FRetEnvioLote.Leitor.Arquivo := ParseText(FPRetWS);
    FRetEnvioLote.LerXml;

    for i := 0 to FRetEnvioLote.evento.Count - 1 do
    begin
      AXML := FRetEnvioLote.evento.Items[i].ArquivoReinf;

      if AXML <> '' then
      begin
        if Pos('</evtRetCons>', AXML) > 0 then
        begin
          FRetConsulta_R9015.Leitor.Arquivo := ParseText(AXML);
          FRetConsulta_R9015.LerXml;

          NomeArq := FRetEnvioLote.evento.Items[i].Id + '-R9015.xml';
        end
        else if Pos('</evtTotalContrib>', AXML) > 0 then
        begin
          FRetConsulta_R9011.Leitor.Arquivo := ParseText(AXML);
          FRetConsulta_R9011.LerXml;

          NomeArq := FRetEnvioLote.evento.Items[i].Id + '-R9011.xml';
        end
        else if Pos('</evtRet>', AXML) > 0 then
          NomeArq := FRetEnvioLote.evento.Items[i].Id + '-R9005.xml'
        else
          NomeArq := FRetEnvioLote.evento.Items[i].Id + '-R9001.xml';

        if (FPConfiguracoesReinf.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
          FPDFeOwner.Gravar(NomeArq, AXML, '',False);
      end;
    end;
  end;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPRetWS, erRetornoConsulta);

  // Controle para exibir mensagem clara ao usuário caso tente utilizar o leiaute ainda não implementado ou descontinuado pela Receita
  if (Pos('<codResp>MS0030</codResp>',FPRetornoWS) > 0) and
     (Pos('em desconformidade com o esquema XSD. O namespace',FPRetornoWS) > 0) and
     (Pos('informado no documento XML',FPRetornoWS) > 0) and
     (Pos('um namespace reconhecido',FPRetornoWS) > 0) then
  begin
    if TACBrReinf(FPDFeOwner).Configuracoes.Geral.ExibirErroSchema then
      FazerLog('Atenção: Os schemas da versão ' + VersaoReinfToStr(FPConfiguracoesReinf.Geral.VersaoDF) + ' ainda não estão diponíveis ou foram descontinuados do ambiente utilizado',True)
    else
      GerarException('Atenção: Os schemas da versão ' + VersaoReinfToStr(FPConfiguracoesReinf.Geral.VersaoDF) + ' ainda não estão diponíveis ou foram descontinuados do ambiente utilizado');
  end;

  Result := True;
end;

// Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
function TConsultar.GetRetConsulta: TRetConsulta;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    GerarException('Propriedade RetConsulta disponível apenas até a versão 1_05_01, utilize a RetConsulta_R9011 para versões posteriores');

  Result := TRetConsulta(FRetConsulta_R5011);
end;

function TConsultar.GetRetConsulta_R5011: TRetConsulta_R5011;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    GerarException('Propriedade RetConsulta_R5011 disponível apenas até a versão 1_05_01, utilize a RetConsulta_R9011 para versões posteriores');

  Result := FRetConsulta_R5011;
end;

function TConsultar.GetRetConsulta_R9011: TRetConsulta_R9011;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
    GerarException('Propriedade RetConsulta_R9011 disponível a partir da versão 2_01_01, utilize a RetConsulta_R5011 para versões anteriores');

  Result := FRetConsulta_R9011;
end;

function TConsultar.GetRetConsulta_R9015: TRetConsulta_R9015;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
    GerarException('Propriedade RetConsulta_R9015 disponível a partir da versão 2_01_01, registro indexistente em versões anteriores');

  Result := FRetConsulta_R9015;
end;

function TConsultar.GerarPrefixoArquivo: String;
begin
  Result := FProtocolo;
end;

{ TConsultarReciboEvento }

procedure TConsultarReciboEvento.BeforeDestruction;
begin
  inherited;
  
  FRetConsulta_R5011.Free;
  FRetConsulta_R9011.Free;
  FRetConsulta_R9015.Free;
  FRetEnvioLote.Free;
  FConsulta.Free;
end;

procedure TConsultarReciboEvento.Clear;
begin
  inherited Clear;

  FPLayout := LayConsultaLoteEventos;
  FPStatus := stConsultaLote;
  FPArqEnv := 'ped-con';
  FPArqResp := 'con';

  if Assigned(FRetConsulta_R5011) then
    FRetConsulta_R5011.Free;

  if Assigned(FRetConsulta_R9011) then
    FRetConsulta_R9011.Free;

  if Assigned(FRetConsulta_R9015) then
    FRetConsulta_R9015.Free;

  FRetConsulta_R5011 := TRetConsulta_R5011.Create;
  FRetConsulta_R9011 := TRetConsulta_R9011.Create;
  FRetConsulta_R9015 := TRetConsulta_R9015.Create;
end;

constructor TConsultarReciboEvento.Create(AOwner: TACBrDFe);
begin
  Inherited Create(AOwner);

  if Assigned(FRetEnvioLote) then
    FRetEnvioLote.Free;
  FRetEnvioLote := TRetEnvioLote.Create;

  if Assigned(FConsulta) then
    FConsulta.Free;
  FConsulta := TReinfConsulta.Create;

  FtipoEvento := teR1000;
  tpEventoStr := '1000';
  FperApur := '';
  FnrInscEstab := '';
  FcnpjPrestador := '';
  FnrInscTomador := '';
  FdtApur := 0;
end;

procedure TConsultarReciboEvento.DefinirDadosMsg;
begin
  Consulta.SoapEnvelope  := FPSoapEnvelopeAtributtes;
  Consulta.TipoEvento    := TipoEvento;
  Consulta.nrInscContrib := TACBrReinf(FPDFeOwner).Configuracoes.Geral.IdContribuinte;
  Consulta.nrInscEstab   := nrInscEstab;
  Consulta.perApur       := perApur;
  Consulta.cnpjPrestador := cnpjPrestador;
  Consulta.nrInscTomador := nrInscTomador;
  Consulta.dtApur        := dtApur;
  Consulta.cpfCnpjBenef  := cpfCnpjBenef;
  Consulta.cnpjFonte     := cnpjFonte;

  AjustarOpcoes( Consulta.Gerador.Opcoes );
  Consulta.GerarXML;

  // Atribuindo o XML para propriedade interna //
  FPDadosMsg := Consulta.Gerador.ArquivoFormatoXML;

  if Assigned(TACBrReinf(FPDFeOwner).OnTransmissaoEventos) then
    TACBrReinf(FPDFeOwner).OnTransmissaoEventos(FPDadosMsg, erEnvioConsulta);
end;

procedure TConsultarReciboEvento.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
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
  end
  else
  begin
    FPMimeType := 'application/xml';
    FPValidateReturnCode := False;
    Consulta.DefinirParametros(FPURL);
    FPEnvelopeSoap := '';
  end;
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

  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    FPURL := FPURL + '/reciboevento';
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
  Result := FperApur + '-R' + tpEventoStr;
end;

// Remover após entrar em vigor a versão 2_01_01 ou colocar exceção alertanto para usar a RetConsulta_R5011
function TConsultarReciboEvento.GetRetConsulta: TRetConsulta;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    GerarException('Propriedade RetConsulta disponível apenas até a versão 1_05_01, utilize a RetConsulta_R9011 para versões posteriores');

  Result := TRetConsulta(FRetConsulta_R5011);
end;

function TConsultarReciboEvento.GetRetConsulta_R5011: TRetConsulta_R5011;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF >= v2_01_01 then
    GerarException('Propriedade RetConsulta_R5011 disponível apenas até a versão 1_05_01, utilize a RetConsulta_R9011 para versões posteriores');

  Result := FRetConsulta_R5011;
end;

function TConsultarReciboEvento.GetRetConsulta_R9011: TRetConsulta_R9011;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
    GerarException('Propriedade RetConsulta_R9011 disponível a partir da versão 2_01_01, utilize a RetConsulta_R5011 para versões anteriores');

  Result := FRetConsulta_R9011;
end;

function TConsultarReciboEvento.GetRetConsulta_R9015: TRetConsulta_R9015;
begin
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
    GerarException('Propriedade RetConsulta_R9015 disponível a partir da versão 2_01_01, registro inexistente em versões anteriores');

  Result := FRetConsulta_R9015;
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
  if FPConfiguracoesReinf.Geral.VersaoDF < v2_01_01 then
  begin
    FPRetWS := SeparaDadosArray(['ConsultaReciboEvento' + tpEventoStr + 'Result',
                                 'ConsultaReciboEvento' + tpEventoStr + 'Response'],
                                 FPRetornoWS);

    FRetConsulta_R5011.Leitor.Arquivo := ParseText(FPRetWS);
    FRetConsulta_R5011.LerXml;

    AXML := FRetConsulta_R5011.XML;
  end
  else
  begin
    FPRetWS := FPRetornoWS;

    FRetConsulta_R9011.Leitor.Arquivo := ParseText(FPRetWS);
    FRetConsulta_R9011.LerXml;

    AXML := FRetConsulta_R9011.XML;
  end;

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
                                  AdtApur: TDateTime;
                                  const AcpfCnpjBenef: string;
                                  const AcnpjFonte: string): Boolean;
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
  FConsultarReciboEvento.cpfCnpjBenef  := AcpfCnpjBenef;
  FConsultarReciboEvento.cnpjFonte     := AcnpjFonte;

  if not FConsultarReciboEvento.Executar then
    FConsultarReciboEvento.GerarException(FConsultarReciboEvento.Msg);

  Result := True;
end;

end.
