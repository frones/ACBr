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
    fEstadoProcCod: Integer;
    fEstadoProcStr: AnsiString;
    fRecibo: AnsiString;
    fTipo: AnsiString;
    fVersao: AnsiString;
    FXML : AnsiString;

    fBlocoXRetorno: TRetEnvBlocoX;
    FXMLZipado: AnsiString;
  private
    procedure SetXML(AValue: AnsiString);
    function GetXMLZipado: AnsiString;

  protected
    procedure DefinirServicoEAction; override;
    function TratarResposta: Boolean; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    procedure Clear; override;
    property XML: AnsiString read FXML write SetXML;
    property XMLZipado: AnsiString read GetXMLZipado write FXMLZipado;

    property BlocoXRetorno: TRetEnvBlocoX read fBlocoXRetorno;
    property EstadoProcCod: Integer       read fEstadoProcCod;
    property EstadoProcStr: AnsiString    read fEstadoProcStr;
    property Recibo       : AnsiString    read fRecibo;
    property Tipo         : AnsiString    read fTipo;
    property Versao       : AnsiString    read fVersao;
  end;

  { TEnviarReducaoZ }

  TEnviarReducaoZ = class(TEnviarBlocoX)
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  end;

  TEnviarEstoque = class(TEnviarBlocoX)
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  end;

  { TConsultarBlocoX }

  TConsultarBlocoX = class(TWebServiceBlocoX)
    FRecibo : String;
    FEstadoProcessamentoCodigo: integer;
    FRetornoConsulta: TRetEnvBlocoX;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Recibo: String read FRecibo write FRecibo;
    property EstadoProcessamentoCodigo: integer read FEstadoProcessamentoCodigo write FEstadoProcessamentoCodigo;
  end;


  { TValidarBlocoX }

  TValidarBlocoX = class(TWebServiceBlocoX)
    FXML : AnsiString;
    FValidarPafEcf : Boolean;
    FValidarEcf: Boolean;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
  public
    property XML: AnsiString read FXML write FXML;
    property ValidarPafEcf: Boolean read FValidarPafEcf write FValidarPafEcf;
    property ValidarEcf: Boolean read FValidarEcf write FValidarEcf;
  end;

  { TValidarReducaoZ }

  TValidarReducaoZ = class(TValidarBlocoX)
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    function TratarResposta: Boolean; override;
  end;

  { TValidarEstoque }

  TValidarEstoque = class(TValidarBlocoX)
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    function TratarResposta: Boolean; override;
  end;

  { TACBrBlocoX_WebServices }

  TACBrBlocoX_WebServices = class
  private
    FEnviarReducaoZ: TEnviarReducaoZ;
    FEnviarEstoque: TEnviarEstoque;
    FConsultarBlocoX: TConsultarBlocoX;
    FValidarEstoque: TValidarEstoque;
    FValidarReducaoZ: TValidarReducaoZ;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    property EnviarReducaoZ: TEnviarReducaoZ read FEnviarReducaoZ write FEnviarReducaoZ;
    property EnviarEstoque: TEnviarEstoque read FEnviarEstoque write FEnviarEstoque;
    property ConsultarBlocoX: TConsultarBlocoX read FConsultarBlocoX write FConsultarBlocoX;
    property ValidarReducaoZ: TValidarReducaoZ read FValidarReducaoZ write fValidarReducaoZ;
    property ValidarEstoque: TValidarEstoque read FValidarEstoque write fValidarEstoque;
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

  fEstadoProcCod := 0;
  fEstadoProcStr := '';
  fRecibo        := '';
  fTipo          := '';
  fVersao        := '';

  if Assigned(fBlocoXRetorno) then
    FreeAndNil( fBlocoXRetorno );

  fBlocoXRetorno := TRetEnvBlocoX.Create;
end;

procedure TEnviarBlocoX.SetXML(AValue: AnsiString);
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

procedure TEnviarBlocoX.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
end;

function TEnviarBlocoX.TratarResposta: Boolean;
begin
  fBlocoXRetorno.Leitor.Arquivo := FPRetWS;
  fBlocoXRetorno.LerXml;

  fEstadoProcCod := fBlocoXRetorno.EstadoProcCod;
  fEstadoProcStr := fBlocoXRetorno.EstadoProcStr;
  fRecibo        := fBlocoXRetorno.Recibo;
  fTipo          := fBlocoXRetorno.Tipo;
  fVersao        := fBlocoXRetorno.Versao;

  Result := (FPRetWS <> '');
end;

constructor TEnviarBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

{ TEnviarReducaoZ }

procedure TEnviarReducaoZ.DefinirDadosMsg;
var
  wCNPJ, wCredECF, wDataRef: String;
  PI, PF: integer;
begin
  wCNPJ     := LerTagXML(XML, 'Cnpj');
  wDataRef  := LerTagXML(XML, 'DataReferencia');
  //
  PI := Pos('<NumeroCredenciamento>', XML);
  PI := PosEx('<NumeroCredenciamento>', XML, PI + Length('NumeroCredenciamento'));
  PI := PI + Length('NumeroCredenciamento') + 2;
  PF := PosEx('</NumeroCredenciamento>', XML, PI);
  if PF = 0 then
     PF := Length(XML);
  wCredECF := copy(XML, PI, PF-PI);
  //
  FPDadosMsg := '<pXmlZipado>'+XMLZipado+'</pXmlZipado>';
end;

procedure TEnviarReducaoZ.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Enviar';
end;

procedure TEnviarReducaoZ.DefinirURL;
begin
  inherited DefinirURL;
  FPBodyElement := 'Enviar';
end;

function TEnviarReducaoZ.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'EnviarResult')));
  Result := inherited TratarResposta;
end;

{ TEnviarEstoque }

procedure TEnviarEstoque.DefinirDadosMsg;
var
  wCNPJ, wDataRef: String;
begin
  wCNPJ     := LerTagXML(XML, 'Cnpj');
  wDataRef := LerTagXML(XML, 'DataReferencia');
  
  FPDadosMsg := '<pXmlZipado>'+XMLZipado+'</pXmlZipado>';
end;

procedure TEnviarEstoque.DefinirServicoEAction;
begin
  inherited;
  FPSoapAction := 'http://tempuri.org/Enviar';
end;

procedure TEnviarEstoque.DefinirURL;
begin
  inherited;
  FPBodyElement := 'Enviar';
end;

function TEnviarEstoque.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'EnviarResult')));
  Result := inherited TratarResposta;
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
  EstadoProcessamentoCodigo := 0;

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
  EstadoProcessamentoCodigo := FRetornoConsulta.EstadoProcCod;

  Result := (FPRetWS <> '');
end;

{ TValidarBlocoX }

procedure TValidarBlocoX.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
end;

procedure TValidarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pXml>'+ParseText(XML,False)+'</pXml>'+
                '<pValidarPafEcf>'+IfThen(FValidarPafEcf, 'true', 'false')+'</pValidarPafEcf>'+
                '<pValidarEcf>'+IfThen(FValidarEcf, 'true', 'false')+'</pValidarEcf>';
end;

{ TValidarReducaoZ }

procedure TValidarReducaoZ.DefinirURL;
begin
  inherited DefinirURL;
  FPURL := FPURL+'?op=ValidarReducaoZ';
  FPBodyElement := 'ValidarReducaoZ';
end;

procedure TValidarReducaoZ.DefinirServicoEAction;
begin
  inherited DefinirServicoEAction;
  FPSoapAction := 'http://tempuri.org/ValidarReducaoZ';
end;

function TValidarReducaoZ.TratarResposta: Boolean;
begin
  //WriteToTXT('_RespValidar.xml', FPRetornoWS);
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ValidarReducaoZResponse')));
  Result  := (FPRetWS <> '');
end;

{ TValidarEstoque }

procedure TValidarEstoque.DefinirURL;
begin
  inherited DefinirURL;
  FPURL := FPURL+'?op=ValidarEstoque';
  FPBodyElement := 'ValidarEstoque';
end;

procedure TValidarEstoque.DefinirServicoEAction;
begin
  inherited DefinirServicoEAction;
  FPSoapAction := 'http://tempuri.org/ValidarEstoque';
end;

function TValidarEstoque.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ValidarEstoqueResponse')));
  Result  := (FPRetWS <> '');
end;

{ TACBrBlocoX_WebServices }

constructor TACBrBlocoX_WebServices.Create(AOwner: TACBrDFe);
begin
  FEnviarReducaoZ  := TEnviarReducaoZ.Create(AOwner);
  FEnviarEstoque   := TEnviarEstoque.Create(AOwner);
  FConsultarBlocoX := TConsultarBlocoX.Create(AOwner);
  FValidarReducaoZ := TValidarReducaoZ.Create(AOwner);
  FValidarEstoque  := TValidarEstoque.Create(AOwner);
end;

destructor TACBrBlocoX_WebServices.Destroy;
begin
  FEnviarReducaoZ.Free;
  FEnviarEstoque.Free;
  FConsultarBlocoX.Free;
  FValidarReducaoZ.Free;
  FValidarEstoque.Free;
  inherited Destroy;
end;

function ZipFile(const DadosXML: AnsiString; const NomeArquivo: String): AnsiString;
begin
  Result := ACBrCompress.ZipFileCompress(DadosXML, NomeArquivo);
end;

end.
