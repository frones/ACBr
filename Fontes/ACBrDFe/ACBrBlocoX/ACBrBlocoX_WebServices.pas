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
  ACBrUtil, pcnConversao;

const
  ACBRBLOCOX_VERSAO = '1.1.0a';

  type

  { TWebServiceBlocoX }

  TWebServiceBlocoX = class(TDFeWebService)
  private
  protected
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
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
    fBlocoXRetorno: TRetEnvBlocoX;
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

    property BlocoXRetorno  : TRetEnvBlocoX read fBlocoXRetorno;
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

  { TACBrBlocoX_WebServices }

  TACBrBlocoX_WebServices = class
  private
    FEnviarBlocoX: TEnviarBlocoX;
    FConsultarBlocoX: TConsultarBlocoX;
    FValidarBlocoX: TValidarBlocoX;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    property EnviarBlocoX: TEnviarBlocoX read FEnviarBlocoX write FEnviarBlocoX;
    property ConsultarBlocoX: TConsultarBlocoX read FConsultarBlocoX write FConsultarBlocoX;
    property ValidarBlocoX: TValidarBlocoX read FValidarBlocoX write FValidarBlocoX;
  end;

function ZipFile(const DadosXML: AnsiString; const NomeArquivo: String): AnsiString;

implementation

uses
  StrUtils,
  synacode, ACBrCompress;

{ TWebServiceBlocoX }

constructor TWebServiceBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPHeaderElement := '';
  FPBodyElement := '';
end;

procedure TWebServiceBlocoX.DefinirURL;
begin
  if (FPConfiguracoes.WebServices.Ambiente = taProducao) then
    FPURL := 'http://webservices.sef.sc.gov.br/wsDfeSiv/Recepcao.asmx'
  else
    FPURL := 'http://webservices.sathomologa.sef.sc.gov.br/wsDfeSiv/Recepcao.asmx';
end;

function TWebServiceBlocoX.GerarVersaoDadosSoap: String;
begin
  Result:='';
end;


{ TEnviarBlocoX }

destructor TEnviarBlocoX.Destroy;
begin
  FreeAndNil( fBlocoXRetorno );
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

  if Assigned(fBlocoXRetorno) then
    FreeAndNil( fBlocoXRetorno );

  fBlocoXRetorno := TRetEnvBlocoX.Create;
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
  FPServico := 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Enviar';
end;

procedure TEnviarBlocoX.DefinirURL;
begin
  inherited DefinirURL;
  FPBodyElement := 'Enviar';
end;

function TEnviarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'EnviarResponse')));

  fBlocoXRetorno.Leitor.Arquivo := FPRetWS;
  fBlocoXRetorno.LerXml;

  fSituacaoProcCod := fBlocoXRetorno.SituacaoProcCod;
  fSituacaoProcStr := fBlocoXRetorno.SituacaoProcStr;
  fRecibo          := fBlocoXRetorno.Recibo;
  fTipo            := fBlocoXRetorno.Tipo;
  fVersao          := fBlocoXRetorno.Versao;
  fMensagem        := fBlocoXRetorno.Mensagem;

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
  inherited DefinirURL;
  FPURL := FPURL+'?op=Consultar';
  FPBodyElement := 'Consultar';
end;

procedure TConsultarBlocoX.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Consultar';
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
  FPServico := 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Validar';
end;

procedure TValidarBlocoX.DefinirURL;
begin
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

{ TACBrBlocoX_WebServices }

constructor TACBrBlocoX_WebServices.Create(AOwner: TACBrDFe);
begin
  FEnviarBlocoX    := TEnviarBlocoX.Create(AOwner);
  FConsultarBlocoX := TConsultarBlocoX.Create(AOwner);
  FValidarBlocoX   := TValidarBlocoX.Create(AOwner);
end;

destructor TACBrBlocoX_WebServices.Destroy;
begin
  FEnviarBlocoX.Free;
  FConsultarBlocoX.Free;
  FValidarBlocoX.Free;
  inherited Destroy;
end;

function ZipFile(const DadosXML: AnsiString; const NomeArquivo: String): AnsiString;
begin
  Result := ACBrCompress.ZipFileCompress(DadosXML, NomeArquivo);
end;

end.
