{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
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
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBlocoX_WebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, pcnRetEnvBlocoX, ACBrDFeWebService,
  ACBrUtil, pcnConversao, ACBrBlocoX_Comum;

const
  ACBRBLOCOX_VERSAO = '1.1.0a';

  CPServico_Tempuri    = 'http://tempuri.org/';
  CPSoapAction_Tempuri = 'http://tempuri.org/';
  CPServico_WS         = 'http://webservices.sef.sc.gov.br/wsDfeSiv/';
  CPSoapAction_WS      = 'http://webservices.sef.sc.gov.br/wsDfeSiv/';

  type

  { TWebServiceBlocoX }

  TWebServiceBlocoX = class(TDFeWebService)
  private
    FURLWebService: TACBrBlocoX_URLWebService;
    FUsarTempuri: Boolean;
    FUsarCData: Boolean;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    property UsarCData: Boolean read FUsarCData write FUsarCData;
  end;

  { TEnviarBlocoX }

  TEnviarBlocoX = class(TWebServiceBlocoX)
  private
    fSituacaoProcCod: Integer;
    fSituacaoProcStr: AnsiString;
    fRecibo: AnsiString;
    fMensagem: AnsiString;
    fTipo: AnsiString;
    fVersao: AnsiString;
    FXML : AnsiString;
    FBlocoXRetorno: TRetEnvBlocoX;
    FXMLZipado: AnsiString;

    procedure SetXML(const AValue: AnsiString);
    function GetXMLZipado: AnsiString;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    procedure Clear; override;
    property XML: AnsiString read FXML write SetXML;
    property XMLZipado: AnsiString read GetXMLZipado write FXMLZipado;

    property BlocoXRetorno  : TRetEnvBlocoX read FBlocoXRetorno;
    property SituacaoProcCod: Integer       read fSituacaoProcCod;
    property SituacaoProcStr: AnsiString    read fSituacaoProcStr;
    property Recibo         : AnsiString    read fRecibo;
    property Tipo           : AnsiString    read fTipo;
    property Versao         : AnsiString    read fVersao;
    property Mensagem       : AnsiString    read fMensagem;
  end;

  { TConsultarBlocoX }

  TConsultarBlocoX = class(TWebServiceBlocoX)
  private
    FRecibo: String;
    FSituacaoProcCod: integer;
    FRetornoConsulta: TRetEnvBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Recibo: String read FRecibo write FRecibo;
    property SituacaoProcCod: integer read FSituacaoProcCod write FSituacaoProcCod;
    property BlocoXRetorno: TRetEnvBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;


  { TValidarBlocoX }

  TValidarBlocoX = class(TWebServiceBlocoX)
  private
    FXML : AnsiString;
    FValidarPafEcfEEcf: Boolean;
    FValidarAssinaturaDigital: Boolean;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    property XML: AnsiString read FXML write FXML;
    property ValidarPafEcfEEcf: Boolean read FValidarPafEcfEEcf write FValidarPafEcfEEcf;
    property ValidarAssinaturaDigital: Boolean read FValidarAssinaturaDigital write FValidarAssinaturaDigital;
  end;

  { TTransmitirArquivoBlocoX }

  TTransmitirArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FRecibo: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
    FMensagem: AnsiString;
    FXML: AnsiString;
    FBlocoXRetorno: TRetTransmitirBlocoX;
    FXMLCompactado: AnsiString;

    procedure SetXML(const AValue: AnsiString);
    function GetXMLCompactado: AnsiString;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    procedure Clear; override;
    property XML: AnsiString read FXML write SetXML;
    property XMLCompactado: AnsiString read GetXMLCompactado write FXMLCompactado;

    property Recibo:          AnsiString read FRecibo;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
    property Mensagem:        AnsiString read FMensagem;

    property BlocoXRetorno: TRetTransmitirBlocoX read FBlocoXRetorno;
  end;

  { TConsultarProcessamentoArquivoBlocoX }

  TConsultarProcessamentoArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FRetornoConsulta: TRetConsultaArquivoBlocoX;
    FXML: AnsiString;
    FRecibo: AnsiString;
    FSituacaoProcStr: AnsiString;
    FSituacaoProcCod: Integer;
    FMensagem: AnsiString;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property Recibo:          AnsiString read FRecibo;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
    property Mensagem:        AnsiString read FMensagem;

    property BlocoXRetorno: TRetConsultaArquivoBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;

  { TReprocessarArquivoBlocoX }

  TReprocessarArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FRetornoReprocessar: TRetReprocessarBlocoX;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
    FRecibo: AnsiString;
    FXML: AnsiString;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property Recibo:          AnsiString read FRecibo;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;

    property BlocoXRetorno: TRetReprocessarBlocoX read FRetornoReprocessar write FRetornoReprocessar;
  end;

  { TDownloadArquivoBlocoX }

  TDownloadArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FArquivo: AnsiString;
    FXML: AnsiString;
    FRetornoDownload: TRetDownloadBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property Arquivo:         AnsiString read FArquivo;

    property BlocoXRetorno: TRetDownloadBlocoX read FRetornoDownload write FRetornoDownload;
  end;

  { TCancelarArquivoBlocoX }

  TCancelarArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FRecibo: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
    FXML: AnsiString;
    FRetornoCancelar: TRetCancelarBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property Recibo:          AnsiString read FRecibo;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;

    property BlocoXRetorno: TRetCancelarBlocoX read FRetornoCancelar write FRetornoCancelar;
  end;

  { TConsultarHistoricoArquivoBlocoX }

  TConsultarHistoricoArquivoBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FXML: AnsiString;
    FRetornoConsulta: TRetConsHistBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;

    property BlocoXRetorno: TRetConsHistBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;

  { TListarArquivosBlocoX }

  TListarArquivosBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FIE: AnsiString;
    FXML: AnsiString;
    FRetornoConsulta: TRetListarBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property IE:              AnsiString read FIE; 
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;

    property BlocoXRetorno: TRetListarBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;

  { TConsultarPendenciasContribuinteBlocoX }

  TConsultarPendenciasContribuinteBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FIE: AnsiString;
    FXML: AnsiString;
    FRetornoConsulta: TRetConsPendContribuinteBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property IE:              AnsiString read FIE;
    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;

    property BlocoXRetorno: TRetConsPendContribuinteBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;

  { TConsultarPendenciasDesenvolvedorPafEcfBlocoX }

  TConsultarPendenciasDesenvolvedorPafEcfBlocoX = class(TWebServiceBlocoX)
  private
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FXML: AnsiString;
    FRetornoConsulta: TRetConsPendDesenvolvedorPafEcfBlocoX;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property XML:             AnsiString read FXML write FXML;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;

    property BlocoXRetorno: TRetConsPendDesenvolvedorPafEcfBlocoX read FRetornoConsulta write FRetornoConsulta;
  end;

  { TACBrBlocoX_WebServices }

  TACBrBlocoX_WebServices = class
  private
    FEnviarBlocoX: TEnviarBlocoX;
    FConsultarBlocoX: TConsultarBlocoX;
    FValidarBlocoX: TValidarBlocoX;

    FTransmitirArquivoBlocoX: TTransmitirArquivoBlocoX;
    FConsultarProcessamentoArquivoBlocoX: TConsultarProcessamentoArquivoBlocoX;
    FConsultarHistoricoArquivoBlocoX: TConsultarHistoricoArquivoBlocoX;
    FListarArquivosBlocoX: TListarArquivosBlocoX;
    FDownloadArquivoBlocoX: TDownloadArquivoBlocoX;
    FCancelarArquivoBlocoX: TCancelarArquivoBlocoX;
    FReprocessarArquivoBlocoX: TReprocessarArquivoBlocoX;
    FConsultarPendenciasContribuinteBlocoX: TConsultarPendenciasContribuinteBlocoX;
    FConsultarPendenciasDesenvolvedorPafEcfBlocoX: TConsultarPendenciasDesenvolvedorPafEcfBlocoX;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    property EnviarBlocoX:    TEnviarBlocoX    read FEnviarBlocoX    write FEnviarBlocoX;
    property ConsultarBlocoX: TConsultarBlocoX read FConsultarBlocoX write FConsultarBlocoX;
    property ValidarBlocoX:   TValidarBlocoX   read FValidarBlocoX   write FValidarBlocoX;

    property TransmitirArquivoBlocoX: TTransmitirArquivoBlocoX read FTransmitirArquivoBlocoX
      write FTransmitirArquivoBlocoX;

    property ConsultarProcessamentoArquivoBlocoX: TConsultarProcessamentoArquivoBlocoX
      read FConsultarProcessamentoArquivoBlocoX write FConsultarProcessamentoArquivoBlocoX;

    property ConsultarHistoricoArquivoBlocoX: TConsultarHistoricoArquivoBlocoX
      read FConsultarHistoricoArquivoBlocoX write FConsultarHistoricoArquivoBlocoX;

    property ListarArquivosBlocoX: TListarArquivosBlocoX read FListarArquivosBlocoX
      write FListarArquivosBlocoX;

    property DownloadArquivoBlocoX: TDownloadArquivoBlocoX read FDownloadArquivoBlocoX
      write FDownloadArquivoBlocoX;

    property CancelarArquivoBlocoX: TCancelarArquivoBlocoX read FCancelarArquivoBlocoX
      write FCancelarArquivoBlocoX;

    property ReprocessarArquivoBlocoX: TReprocessarArquivoBlocoX read FReprocessarArquivoBlocoX
      write FReprocessarArquivoBlocoX;

    property ConsultarPendenciasContribuinteBlocoX: TConsultarPendenciasContribuinteBlocoX
      read FConsultarPendenciasContribuinteBlocoX write FConsultarPendenciasContribuinteBlocoX;

    property ConsultarPendenciasDesenvolvedorPafEcfBlocoX: TConsultarPendenciasDesenvolvedorPafEcfBlocoX
      read FConsultarPendenciasDesenvolvedorPafEcfBlocoX write FConsultarPendenciasDesenvolvedorPafEcfBlocoX;
  end;

implementation

uses
  StrUtils, synacode;

{ TWebServiceBlocoX }

constructor TWebServiceBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FURLWebService  := wsRecepcao;
  FUsarTempuri    := False;
  FUsarCData      := False;
  FPHeaderElement := '';
  FPBodyElement   := '';
end;

procedure TWebServiceBlocoX.DefinirURL;
begin
  { Será descontinuada em  01/01/2020 segun reunião ACATS }
  if (FURLWebService = wsBlocoX) then
  begin
    if (FPConfiguracoes.WebServices.Ambiente = taProducao) then
      FPURL := 'http://webservices.sef.sc.gov.br/wsDfeSiv/BlocoX.asmx'
    else
      FPURL := 'http://webservices.sathomologa.sef.sc.gov.br/wsDfeSiv/BlocoX.asmx';
  end
  else
  begin
    if (FPConfiguracoes.WebServices.Ambiente = taProducao) then
      FPURL := 'http://webservices.sef.sc.gov.br/wsDfeSiv/Recepcao.asmx'
    else
      FPURL := 'http://webservices.sathomologa.sef.sc.gov.br/wsDfeSiv/Recepcao.asmx';
  end;
end;

procedure TWebServiceBlocoX.DefinirServicoEAction;
begin
  if (FUsarTempuri) then
  begin
    FPServico := CPServico_Tempuri;
    FPSoapAction := CPSoapAction_Tempuri;
  end
  else
  begin
    FPServico := CPServico_WS;
    FPSoapAction := CPSoapAction_WS;
  end;
end;

function TWebServiceBlocoX.GerarVersaoDadosSoap: String;
begin
  Result := '';
end;

{ TEnviarBlocoX }

destructor TEnviarBlocoX.Destroy;
begin
  FreeAndNil( FBlocoXRetorno );
  inherited Destroy;
end;

procedure TEnviarBlocoX.Clear;
begin
  inherited Clear;

  fSituacaoProcCod := 0;
  fSituacaoProcStr := '';
  fRecibo          := '';
  fTipo            := '';
  fVersao          := '';
  fMensagem        := '';
  if Assigned(FBlocoXRetorno) then
    FreeAndNil(FBlocoXRetorno);
  FBlocoXRetorno := TRetEnvBlocoX.Create;
end;

procedure TEnviarBlocoX.SetXML(const AValue: AnsiString);
begin
  if FXML = AValue then Exit;
  FXML := AValue;
  FXMLZipado := '';
end;

function TEnviarBlocoX.GetXMLZipado: AnsiString;
var
  AZip: AnsiString;
begin
  if FXMLZipado = '' then
  begin
    if FXML <> '' then
    begin
      AZip := ZipFile(FXML, FPBodyElement+'.xml');
      if AZip = '' then
        raise Exception.Create('O seu compilador não tem suporte nativo a ZipFile.'+sLineBreak+
                               'Informe o XML já Zipado + Base64 em "XMLZipado"');

      FXMLZipado := EncodeBase64(AZip);
    end;
  end;

  Result := FXMLZipado;
end;

procedure TEnviarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pXmlZipado>'+XMLZipado+'</pXmlZipado>';
end;

procedure TEnviarBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := True;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'Enviar';
end;

procedure TEnviarBlocoX.DefinirURL;
begin
  FURLWebService := wsRecepcao;
  inherited DefinirURL;
  FPBodyElement := 'Enviar';
end;

function TEnviarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'EnviarResponse')));

  FBlocoXRetorno.Leitor.Arquivo := FPRetWS;
  FBlocoXRetorno.LerXml;

  fSituacaoProcCod := FBlocoXRetorno.SituacaoProcCod;
  fSituacaoProcStr := FBlocoXRetorno.SituacaoProcStr;
  fRecibo          := FBlocoXRetorno.Recibo;
  fTipo            := FBlocoXRetorno.Tipo;
  fVersao          := FBlocoXRetorno.Versao;
  fMensagem        := FBlocoXRetorno.Mensagem;

  Result := (FPRetWS <> '');
end;

constructor TEnviarBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

{ TConsultarBlocoX }

destructor TConsultarBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TConsultarBlocoX.Clear;
begin
  inherited Clear;
  SituacaoProcCod := 0;

  if Assigned(FRetornoConsulta) then
    FRetornoConsulta.Free;

  FRetornoConsulta := TRetEnvBlocoX.Create;
end;

procedure TConsultarBlocoX.DefinirURL;
begin
  FURLWebService := wsRecepcao;
  inherited DefinirURL;
  FPURL := FPURL+'?op=Consultar';
  FPBodyElement := 'Consultar';
end;

procedure TConsultarBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := True;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'Consultar';
end;

procedure TConsultarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pRecibo>'+Recibo+'</pRecibo>';
end;

function TConsultarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ConsultarResponse')));

  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  Recibo := FRetornoConsulta.Recibo;
  SituacaoProcCod := FRetornoConsulta.SituacaoProcCod;

  Result := (FPRetWS <> '');
end;

{ TValidarBlocoX }

procedure TValidarBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := True;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'Validar';
end;

procedure TValidarBlocoX.DefinirURL;
begin
  FURLWebService := wsRecepcao;
  inherited DefinirURL;
  FPURL := FPURL+'?op=Validar';
  FPBodyElement := 'Validar';
end;

function TValidarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ValidarResponse')));
  Result  := (FPRetWS <> '');
end;

procedure TValidarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pXml>'+ParseText(XML,False)+'</pXml>'+
                '<pValidarPafEcfEEcf>'+IfThen(FValidarPafEcfEEcf, 'true', 'false')+'</pValidarPafEcfEEcf>'+
                '<pValidarAssinaturaDigital>'+IfThen(FValidarAssinaturaDigital, 'true', 'false')+'</pValidarAssinaturaDigital>';
end;

{ TTransmitirArquivoBlocoX }

destructor TTransmitirArquivoBlocoX.Destroy;
begin
  FreeAndNil(FBlocoXRetorno);
  inherited Destroy;
end;

procedure TTransmitirArquivoBlocoX.Clear;
begin
  inherited Clear;
  FRecibo          := '';
  FSituacaoProcCod := 0;
  FSituacaoProcStr := '';
  FMensagem        := '';
  if Assigned(FBlocoXRetorno) then
    FreeAndNil(FBlocoXRetorno);
  FBlocoXRetorno := TRetTransmitirBlocoX.Create;
end;

procedure TTransmitirArquivoBlocoX.SetXML(const AValue: AnsiString);
begin
  if FXML = AValue then Exit;
  FXML := AValue;
  FXMLCompactado := '';
end;

function TTransmitirArquivoBlocoX.GetXMLCompactado: AnsiString;
var
  AZip: AnsiString;
begin
  if FXMLCompactado = '' then
  begin
    if FXML <> '' then
    begin
      AZip := ZipFile(FXML, FPBodyElement+'.xml');
      if AZip = '' then
        raise Exception.Create('O seu compilador não tem suporte nativo a ZipFile.'+sLineBreak+
                               'Informe o XML já Zipado + Base64 em "XMLZipado"');

      FXMLCompactado := EncodeBase64(AZip);
    end;
  end;

  Result := FXMLCompactado;
end;

procedure TTransmitirArquivoBlocoX.DefinirDadosMsg;
begin
  //Não utiliza CDATA
  FPDadosMsg := '<pXmlCompactado>'+XMLCompactado+'</pXmlCompactado>';
end;

procedure TTransmitirArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'TransmitirArquivo';
end;

procedure TTransmitirArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=TransmitirArquivo';
  FPBodyElement := 'TransmitirArquivo';
end;

function TTransmitirArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'TransmitirArquivoResponse')));

  FBlocoXRetorno.Leitor.Arquivo := FPRetWS;
  FBlocoXRetorno.LerXml;

  FRecibo          := FBlocoXRetorno.Recibo;
  FSituacaoProcCod := FBlocoXRetorno.SituacaoProcCod;
  FSituacaoProcStr := FBlocoXRetorno.SituacaoProcStr;
  FMensagem        := FBlocoXRetorno.Mensagem;

  Result := (FPRetWS <> '');
end;

constructor TTransmitirArquivoBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

{ TConsultarProcessamentoArquivoBlocoX }

destructor TConsultarProcessamentoArquivoBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TConsultarProcessamentoArquivoBlocoX.Clear;
begin
  inherited Clear;
  FSituacaoProcCod := 0;
  FSituacaoProcStr := EmptyStr;
  FRecibo          := EmptyStr;
  FMensagem        := EmptyStr;

  if Assigned(FRetornoConsulta) then
    FreeAndNil(FRetornoConsulta);
  FRetornoConsulta := TRetConsultaArquivoBlocoX.Create;
end;

procedure TConsultarProcessamentoArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ConsultarProcessamentoArquivo';
  FPBodyElement := 'ConsultarProcessamentoArquivo';
end;

procedure TConsultarProcessamentoArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ConsultarProcessamentoArquivo';
end;

procedure TConsultarProcessamentoArquivoBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + FXML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + FXML + '</pXml>';
end;

function TConsultarProcessamentoArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ConsultarProcessamentoArquivoResult')));
  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  FRecibo          := FRetornoConsulta.Recibo;
  FSituacaoProcCod := FRetornoConsulta.SituacaoProcCod;
  FSituacaoProcStr := FRetornoConsulta.SituacaoProcStr;
  FMensagem        := FRetornoConsulta.Mensagem;

  Result := (FPRetWS <> '');
end;

{ TConsultarHistoricoArquivoBlocoX }

destructor TConsultarHistoricoArquivoBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TConsultarHistoricoArquivoBlocoX.Clear;
begin
  inherited Clear;

  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';

  if Assigned(FRetornoConsulta) then
    FRetornoConsulta.Free;

  FRetornoConsulta := TRetConsHistBlocoX.Create;
end;

procedure TConsultarHistoricoArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ConsultarHistoricoArquivo';
  FPBodyElement := 'ConsultarHistoricoArquivo';
end;

procedure TConsultarHistoricoArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ConsultarHistoricoArquivo';
end;

procedure TConsultarHistoricoArquivoBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TConsultarHistoricoArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ConsultarHistoricoArquivoResponse')));

  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  FSituacaoOperCod := FRetornoConsulta.SituacaoOperCod;
  FSituacaoOperStr := FRetornoConsulta.SituacaoOperStr;

  Result := (FPRetWS <> '');
end;

{ TListarArquivosBlocoX }

destructor TListarArquivosBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TListarArquivosBlocoX.Clear;
begin
  inherited Clear;

  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FIE              := '';

  if Assigned(FRetornoConsulta) then
    FRetornoConsulta.Free;

  FRetornoConsulta := TRetListarBlocoX.Create;
end;

procedure TListarArquivosBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ListarArquivos';
  FPBodyElement := 'ListarArquivos';
end;

procedure TListarArquivosBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ListarArquivos';
end;

procedure TListarArquivosBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TListarArquivosBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ListarArquivosResponse')));

  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  FSituacaoOperCod := FRetornoConsulta.SituacaoOperCod;
  FSituacaoOperStr := FRetornoConsulta.SituacaoOperStr;
  FIE              := FRetornoConsulta.IE;

  Result := (FPRetWS <> '');
end;

{ TDownloadArquivoBlocoX }

destructor TDownloadArquivoBlocoX.Destroy;
begin
  FRetornoDownload.Free;
  inherited Destroy;
end;

procedure TDownloadArquivoBlocoX.Clear;
begin
  inherited Clear;
  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FArquivo         := '';
  if Assigned(FRetornoDownload) then
    FreeAndNil(FRetornoDownload);
  FRetornoDownload := TRetDownloadBlocoX.Create;
end;

procedure TDownloadArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=DownloadArquivo';
  FPBodyElement := 'DownloadArquivo';
end;

procedure TDownloadArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'DownloadArquivo';
end;

procedure TDownloadArquivoBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TDownloadArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'DownloadArquivoResponse')));

  FRetornoDownload.Leitor.Arquivo := FPRetWS;
  FRetornoDownload.LerXml;

  FSituacaoOperCod := FRetornoDownload.SituacaoOperCod;
  FSituacaoOperStr := FRetornoDownload.SituacaoOperStr;
  FArquivo         := FRetornoDownload.Arquivo;

  Result := (FPRetWS <> '');
end;

{ TCancelarArquivoBlocoX }

destructor TCancelarArquivoBlocoX.Destroy;
begin
  FRetornoCancelar.Free;
  inherited Destroy;
end;

procedure TCancelarArquivoBlocoX.Clear;
begin
  inherited Clear;

  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FRecibo          := '';
  FSituacaoProcCod := 0;
  FSituacaoProcStr := '';

  if Assigned(FRetornoCancelar) then
    FRetornoCancelar.Free;

  FRetornoCancelar := TRetCancelarBlocoX.Create;
end;

procedure TCancelarArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=CancelarArquivo';
  FPBodyElement := 'CancelarArquivo';
end;

procedure TCancelarArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'CancelarArquivo';
end;

procedure TCancelarArquivoBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TCancelarArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'CancelarArquivoResponse')));
  FRetornoCancelar.Leitor.Arquivo := FPRetWS;
  FRetornoCancelar.LerXml;
  FSituacaoOperCod := FRetornoCancelar.SituacaoOperCod;
  FSituacaoOperStr := FRetornoCancelar.SituacaoOperStr;
  FRecibo          := FRetornoCancelar.Recibo;
  FSituacaoProcCod := FRetornoCancelar.SituacaoProcCod;
  FSituacaoProcStr := FRetornoCancelar.SituacaoProcStr;
  Result := (FPRetWS <> '');
end;

{ TReprocessarArquivoBlocoX }

destructor TReprocessarArquivoBlocoX.Destroy;
begin
  FRetornoReprocessar.Free;
  inherited Destroy;
end;

procedure TReprocessarArquivoBlocoX.Clear;
begin
  inherited Clear;
  FRecibo := '';
  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FSituacaoProcCod := 0;
  FSituacaoProcStr := '';
  if Assigned(FRetornoReprocessar) then
    FreeAndNil(FRetornoReprocessar);
  FRetornoReprocessar := TRetReprocessarBlocoX.Create;
end;

procedure TReprocessarArquivoBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ReprocessarArquivo';
  FPBodyElement := 'ReprocessarArquivo';
end;

procedure TReprocessarArquivoBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ReprocessarArquivo';
end;

procedure TReprocessarArquivoBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TReprocessarArquivoBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ReprocessarArquivoResponse')));
  FRetornoReprocessar.Leitor.Arquivo := FPRetWS;
  FRetornoReprocessar.LerXml;
  FRecibo := FRetornoReprocessar.Recibo;
  FSituacaoOperCod := FRetornoReprocessar.SituacaoOperCod;
  FSituacaoOperStr := FRetornoReprocessar.SituacaoOperStr;
  FSituacaoProcCod := FRetornoReprocessar.SituacaoProcCod;
  FSituacaoProcStr := FRetornoReprocessar.SituacaoProcStr;
  Result := (FPRetWS <> '');
end;

{ TConsultarPendenciasContribuinteBlocoX }

destructor TConsultarPendenciasContribuinteBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TConsultarPendenciasContribuinteBlocoX.Clear;
begin
  inherited Clear;

  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FIE              := '';

  if Assigned(FRetornoConsulta) then
    FRetornoConsulta.Free;

  FRetornoConsulta := TRetConsPendContribuinteBlocoX.Create;
end;

procedure TConsultarPendenciasContribuinteBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ConsultarPendenciasContribuinte';
  FPBodyElement := 'ConsultarPendenciasContribuinte';
end;

procedure TConsultarPendenciasContribuinteBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ConsultarPendenciasContribuinte';
end;

procedure TConsultarPendenciasContribuinteBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TConsultarPendenciasContribuinteBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ConsultarPendenciasContribuinteResponse')));

  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  FSituacaoOperCod := FRetornoConsulta.SituacaoOperCod;
  FSituacaoOperStr := FRetornoConsulta.SituacaoOperStr;
  FIE              := FRetornoConsulta.IE;

  Result := (FPRetWS <> '');
end;

{ TConsultarPendenciasDesenvolvedorPafEcfBlocoX }

destructor TConsultarPendenciasDesenvolvedorPafEcfBlocoX.Destroy;
begin
  FRetornoConsulta.Free;
  inherited Destroy;
end;

procedure TConsultarPendenciasDesenvolvedorPafEcfBlocoX.Clear;
begin
  inherited Clear;

  FSituacaoOperCod := 0;
  FSituacaoOperStr := '';
  FPArqEnv := 'blx-cdv';

  if Assigned(FRetornoConsulta) then
    FRetornoConsulta.Free;

  FRetornoConsulta := TRetConsPendDesenvolvedorPafEcfBlocoX.Create;
end;

procedure TConsultarPendenciasDesenvolvedorPafEcfBlocoX.DefinirURL;
begin
  FURLWebService := wsBlocoX;
  inherited DefinirURL;
  FPURL := FPURL+'?op=ConsultarPendenciasDesenvolvedorPafEcf';
  FPBodyElement := 'ConsultarPendenciasDesenvolvedorPafEcf';
end;

procedure TConsultarPendenciasDesenvolvedorPafEcfBlocoX.DefinirServicoEAction;
begin
  FUsarTempuri := False;
  inherited DefinirServicoEAction;
  FPSoapAction := FPSoapAction + 'ConsultarPendenciasDesenvolvedorPafEcf';
end;

procedure TConsultarPendenciasDesenvolvedorPafEcfBlocoX.DefinirDadosMsg;
begin
  if FUsarCData then
    FPDadosMsg := '<pXml>' + '<![CDATA[' + XML + ']]>' + '</pXml>'
  else
    FPDadosMsg := '<pXml>' + XML + '</pXml>';
end;

function TConsultarPendenciasDesenvolvedorPafEcfBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ConsultarPendenciasDesenvolvedorPafEcfResponse')));

  FRetornoConsulta.Leitor.Arquivo := FPRetWS;
  FRetornoConsulta.LerXml;

  FSituacaoOperCod := FRetornoConsulta.SituacaoOperCod;
  FSituacaoOperStr := FRetornoConsulta.SituacaoOperStr;

  Result := (FPRetWS <> '');
end;

{ TACBrBlocoX_WebServices }

constructor TACBrBlocoX_WebServices.Create(AOwner: TACBrDFe);
begin
  FEnviarBlocoX    := TEnviarBlocoX.Create(AOwner);
  FConsultarBlocoX := TConsultarBlocoX.Create(AOwner);
  FValidarBlocoX   := TValidarBlocoX.Create(AOwner);
  FTransmitirArquivoBlocoX                      := TTransmitirArquivoBlocoX.Create(AOwner);
  FConsultarProcessamentoArquivoBlocoX          := TConsultarProcessamentoArquivoBlocoX.Create(AOwner);
  FConsultarHistoricoArquivoBlocoX              := TConsultarHistoricoArquivoBlocoX.Create(AOwner);
  FListarArquivosBlocoX                         := TListarArquivosBlocoX.Create(AOwner);
  FDownloadArquivoBlocoX                        := TDownloadArquivoBlocoX.Create(AOwner);
  FCancelarArquivoBlocoX                        := TCancelarArquivoBlocoX.Create(AOwner);
  FReprocessarArquivoBlocoX                     := TReprocessarArquivoBlocoX.Create(AOwner);
  FConsultarPendenciasContribuinteBlocoX        := TConsultarPendenciasContribuinteBlocoX.Create(AOwner);
  FConsultarPendenciasDesenvolvedorPafEcfBlocoX := TConsultarPendenciasDesenvolvedorPafEcfBlocoX.Create(AOwner);
end;

destructor TACBrBlocoX_WebServices.Destroy;
begin
  FEnviarBlocoX.Free;
  FConsultarBlocoX.Free;
  FValidarBlocoX.Free;
  FTransmitirArquivoBlocoX.Free;
  FConsultarProcessamentoArquivoBlocoX.Free;
  FConsultarHistoricoArquivoBlocoX.Free;
  FListarArquivosBlocoX.Free;
  FDownloadArquivoBlocoX.Free;
  FCancelarArquivoBlocoX.Free;
  FReprocessarArquivoBlocoX.Free;
  FConsultarPendenciasContribuinteBlocoX.Free;
  FConsultarPendenciasDesenvolvedorPafEcfBlocoX.Free;
  inherited Destroy;
end;

end.
