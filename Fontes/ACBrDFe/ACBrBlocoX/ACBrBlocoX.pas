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

unit ACBrBlocoX;

interface

uses
  Classes, SysUtils, ACBrDFe, ACBrDFeException, ACBrDFeConfiguracoes,
  ACBrBlocoX_ReducaoZ, ACBrBlocoX_Estoque, pcnRetEnvBlocoX, ACBrDFeWebService,
  ACBrUtil;

const
  ACBRBLOCOX_VERSAO = '1.1.0a';

  type

  { TWebServiceBlocoX }

  TWebServiceBlocoX = class(TDFeWebService)
  private

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

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    procedure Clear; override;

    property XML: AnsiString read FXML write FXML;

    property BlocoXRetorno: TRetEnvBlocoX read fBlocoXRetorno;
    property EstadoProcCod: Integer       read fEstadoProcCod;
    property EstadoProcStr: AnsiString    read fEstadoProcStr;
    property Recibo       : AnsiString    read fRecibo;
    property Tipo         : AnsiString    read fTipo;
    property Versao       : AnsiString    read fVersao;
  end;

  { TConsultarBlocoX }

  TConsultarBlocoX = class(TWebServiceBlocoX)
    FRecibo : String;

    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    property Recibo: String read FRecibo write FRecibo;
  end;


  { TValidarBlocoX }

  TValidarBlocoX = class(TWebServiceBlocoX)
    FXML : AnsiString;
    FValidarPafEcf : Boolean;
    FValidarEcf: Boolean;

    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    property XML: AnsiString read FXML write FXML;
    property ValidarPafEcf: Boolean read FValidarPafEcf write FValidarPafEcf;
    property ValidarEcf: Boolean read FValidarEcf write FValidarEcf;
  end;


  TConfiguracoesBlocoX = class(TConfiguracoes)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesBlocoX: TConfiguracoesBlocoX); overload;
  published
    property Geral;
    property WebServices;
    property Certificados;
  end;

  TACBrBlocoX_Estabelecimento = class(TPersistent)
  private
    FCnpj: String;
    FIe: String;
    FNomeEmpresarial: String;
  published
    property Ie: String read FIe write FIe;
    property Cnpj: String read FCnpj write FCnpj;
    property NomeEmpresarial: String read FNomeEmpresarial write FNomeEmpresarial;
  end;

  TACBrBlocoX_PafECF = class(TPersistent)
  private
    FVersao: String;
    FNumeroCredenciamento: String;
    FNomeComercial: String;
    FNomeEmpresarialDesenvolvedor: String;
    FCnpjDesenvolvedor: String;
  published
    property NumeroCredenciamento: String read FNumeroCredenciamento write FNumeroCredenciamento;
    property NomeComercial: String read FNomeComercial write FNomeComercial;
    property Versao: String read FVersao write FVersao;
    property CnpjDesenvolvedor: String read FCnpjDesenvolvedor write FCnpjDesenvolvedor;
    property NomeEmpresarialDesenvolvedor: String read FNomeEmpresarialDesenvolvedor write FNomeEmpresarialDesenvolvedor;
  end;

  TACBrBlocoX_ECF = class(TPersistent)
  private
    FVersao: String;
    FNumeroFabricacao: String;
    FModelo: String;
    FMarca: String;
    FCaixa: String;
    FTipo: String;
  published
    property NumeroFabricacao: String read FNumeroFabricacao write FNumeroFabricacao;
    property Tipo: String read FTipo write FTipo;
    property Marca: String read FMarca write FMarca;
    property Modelo: String read FModelo write FModelo;
    property Versao: String read FVersao write FVersao;
    property Caixa: String read FCaixa write FCaixa;
  end;

  { TWebServices }

  TWebServices = class
  private
    FEnviarBlocoX : TEnviarBlocoX;
    FConsultarBlocoX: TConsultarBlocoX;
    FValidarBlocoX: TValidarBlocoX;

    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;
  public
    property EnviarBlocoX: TEnviarBlocoX read FEnviarBlocoX write FEnviarBlocoX;
    property ConsultarBlocoX: TConsultarBlocoX read FConsultarBlocoX write FConsultarBlocoX;
    property ValidarBlocoX: TValidarBlocoX read FValidarBlocoX write FValidarBlocoX;
  end;


  TACBrBlocoX = class(TACBrDFe)
  private
    FPafECF: TACBrBlocoX_PafECF;
    FEstabelecimento: TACBrBlocoX_Estabelecimento;
    FEstoque: TACBrBlocoX_Estoque;
    FReducoesZ: TACBrBlocoX_ReducaoZ;
    FECF: TACBrBlocoX_ECF;
    FWebServices: TWebServices;
    function GetConfiguracoes: TConfiguracoesBlocoX;
    procedure SetConfiguracoes(const Value: TConfiguracoesBlocoX);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
    function GetAbout: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Estoque: TACBrBlocoX_Estoque read FEstoque write FEstoque;
    property ReducoesZ: TACBrBlocoX_ReducaoZ read FReducoesZ write FReducoesZ;
    property WebServices: TWebServices read FWebServices write FWebServices;
  published
    property Estabelecimento: TACBrBlocoX_Estabelecimento read FEstabelecimento write FEstabelecimento;
    property PafECF: TACBrBlocoX_PafECF read FPafECF write FPafECF;
    property ECF: TACBrBlocoX_ECF read FECF write FECF;
    property Configuracoes: TConfiguracoesBlocoX read GetConfiguracoes Write SetConfiguracoes;
  end;


implementation

uses
  ACBrBlocoX_Comum, StrUtils;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FEnviarBlocoX := TEnviarBlocoX.Create(AOwner);
  FConsultarBlocoX := TConsultarBlocoX.Create(AOwner);
  FValidarBlocoX := TValidarBlocoX.Create(AOwner);
end;

destructor TWebServices.Destroy;
begin
  FEnviarBlocoX.Free;
  FConsultarBlocoX.Free;
  FValidarBlocoX.Free;
  inherited Destroy;
end;

{ TConsultarBlocoX }

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

  Result := (FPRetWS <> '');
end;

{ TEnviarBlocoX }

procedure TEnviarBlocoX.DefinirURL;
begin
  inherited DefinirURL;
  FPURL := FPURL+'?op=Enviar';
  FPBodyElement := 'Enviar';
end;

procedure TEnviarBlocoX.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Enviar';
end;

procedure TEnviarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pXml>'+ParseText(XML,False)+'</pXml>';
end;

function TEnviarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'EnviarResponse')));

  Clear;
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
  fBlocoXRetorno := TRetEnvBlocoX.Create;
end;

destructor TEnviarBlocoX.Destroy;
begin
  fBlocoXRetorno.Free;
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
    fBlocoXRetorno.Free;
  fBlocoXRetorno := TRetEnvBlocoX.Create;
end;

{ TWebServiceBlocoX }

procedure TWebServiceBlocoX.DefinirURL;
begin
  FPURL := 'http://webservices.sathomologa.sef.sc.gov.br/wsDfeSiv/Recepcao.asmx';
end;

function TWebServiceBlocoX.GerarVersaoDadosSoap: String;
begin
  Result:='';
end;

constructor TWebServiceBlocoX.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPHeaderElement := '';
  FPBodyElement := '';
end;

{ TValidarBlocoX }

procedure TValidarBlocoX.DefinirURL;
begin
  inherited DefinirURL;
  FPURL := FPURL+'?op=Validar';
  FPBodyElement := 'Validar';
end;

procedure TValidarBlocoX.DefinirServicoEAction;
begin
  FPServico:= 'http://tempuri.org/';
  FPSoapAction := 'http://tempuri.org/Validar';
end;

procedure TValidarBlocoX.DefinirDadosMsg;
begin
  FPDadosMsg := '<pXml>'+ParseText(XML,False)+'</pXml>'+
                '<pValidarPafEcf>'+IfThen(FValidarPafEcf, 'true', 'false')+'</pValidarPafEcf>'+
                '<pValidarEcf>'+IfThen(FValidarEcf, 'true', 'false')+'</pValidarEcf>';
end;

function TValidarBlocoX.TratarResposta: Boolean;
begin
  FPRetWS := Trim(ParseText(SeparaDados(FPRetornoWS, 'ValidarResponse')));

  Result := (FPRetWS <> '');
end;

{ TACBrBlocoX }

constructor TACBrBlocoX.Create(AOwner: TComponent);
begin
  inherited;
  FEstoque   := TACBrBlocoX_Estoque.Create(Self);
  FReducoesZ := TACBrBlocoX_ReducaoZ.Create(Self);

  FPafECF := TACBrBlocoX_PafECF.Create;
  FEstabelecimento := TACBrBlocoX_Estabelecimento.Create;
  FECF := TACBrBlocoX_ECF.Create;
  FWebServices := TWebServices.Create(Self);
end;

destructor TACBrBlocoX.Destroy;
begin
  FEstoque.Free;
  FReducoesZ.Free;
  FPafECF.Free;
  FEstabelecimento.Free;
  FECF.Free;
  FWebServices.Free;

  inherited;
end;

function TACBrBlocoX.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesBlocoX.Create(Self);
end;

function TACBrBlocoX.GetAbout: String;
begin
  Result := 'ACBrBlocoX Ver: ' + ACBRBLOCOX_VERSAO;
end;

function TACBrBlocoX.GetConfiguracoes: TConfiguracoesBlocoX;
begin
  Result := TConfiguracoesBlocoX(FPConfiguracoes);
end;

procedure TACBrBlocoX.SetConfiguracoes(const Value: TConfiguracoesBlocoX);
begin
  FPConfiguracoes := Value;
end;

{ TConfiguracoesBlocoX }

procedure TConfiguracoesBlocoX.Assign(
  DeConfiguracoesBlocoX: TConfiguracoesBlocoX);
begin
  WebServices.Assign(DeConfiguracoesBlocoX.WebServices);
  Certificados.Assign(DeConfiguracoesBlocoX.Certificados);
end;

constructor TConfiguracoesBlocoX.Create(AOwner: TComponent);
begin
  inherited;
  //
end;

end.
