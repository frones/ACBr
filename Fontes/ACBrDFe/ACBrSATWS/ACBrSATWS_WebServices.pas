{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Andre Ferreira de Moraes                        }
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

unit ACBrSATWS_WebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, pcnSATConsulta, pcnSATConsultaRet, ACBrDFeWebService,
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  pcnConversao;

const
  ACBRSATWS_VERSAO = '0.1';

  type

  { TWebServiceSATWS }

  TWebServiceSATWS = class(TDFeWebService)
  private
  protected
    procedure DefinirURL; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
  end;

  { TConsultarSATWS }

  TConsultarSATWS = class(TWebServiceSATWS)
    FtpAmb          : TpcnTipoAmbiente;
    FcUF            : Integer;
    FversaoDados    : String;
    FnserieSAT      : Integer;
    FdhInicial      : TDateTime;
    FdhFinal        : TDateTime;
    FchaveSeguranca : String;
    FConsultaRet : TSATConsultaRet;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  protected
    function GerarVersaoDadosSoap: String; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property tpAmb          : TpcnTipoAmbiente read FtpAmb          write FtpAmb;
    property cUF            : Integer          read FcUF            write FcUF;
    property versaoDados    : String           read FversaoDados    write FversaoDados;
    property nserieSAT      : Integer          read FnserieSAT      write FnserieSAT;
    property dhInicial      : TDateTime        read FdhInicial      write FdhInicial;
    property dhFinal        : TDateTime        read FdhFinal        write FdhFinal;
    property chaveSeguranca : String           read FchaveSeguranca write FchaveSeguranca;
    property ConsultaRet: TSATConsultaRet read FConsultaRet write FConsultaRet;
  end;

  { TACBrSATWS_WebServices }

  TACBrSATWS_WebServices = class
  private
    FConsultarSATWS: TConsultarSATWS;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    property ConsultarSATWS: TConsultarSATWS read FConsultarSATWS write FConsultarSATWS;
  end;

implementation

uses
  ACBrSATWS;

{ TWebServiceSATWS }

constructor TWebServiceSATWS.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPHeaderElement := 'cfeCabecMsg';
  FPBodyElement := 'CfeConsultarLotes';

  FPDFeOwner.SSL.UseCertificateHTTP := False;
end;

procedure TWebServiceSATWS.DefinirURL;
begin
  FPURL := 'https://wssatsp.fazenda.sp.gov.br/CfeConsultarLotes/CfeConsultarLotes.asmx';
end;

{ TConsultarBlocoX }

destructor TConsultarSATWS.Destroy;
begin
  FConsultaRet.Free;
  inherited Destroy;
end;

procedure TConsultarSATWS.Clear;
begin
  inherited Clear;

  if Assigned(FConsultaRet) then
    FConsultaRet.Free;

  FConsultaRet := TSATConsultaRet.Create;

  FPArqEnv := 'ped-con-sat';
  FPArqResp := 'resp-con-sat'
end;

procedure TConsultarSATWS.DefinirURL;
begin
  inherited DefinirURL;
  FPBodyElement := 'CfeConsultarLotes';
end;

procedure TConsultarSATWS.DefinirServicoEAction;
begin
  FPServico := 'http://www.fazenda.sp.gov.br/sat/wsdl/CfeConsultaLotes';
  FPSoapAction := 'http://www.fazenda.sp.gov.br/sat/wsdl/CfeConsultar';
end;

procedure TConsultarSATWS.DefinirDadosMsg;
var
  ConsultaSAT: TSATConsulta;
begin
  ConsultaSAT := TSATConsulta.Create;
  try
    ConsultaSAT.tpAmb       := tpAmb;
    ConsultaSAT.versaoDados := versaoDados;
    ConsultaSAT.nserieSAT   := nserieSAT;
    ConsultaSAT.dhInicial   := dhInicial;
    ConsultaSAT.dhFinal     := dhFinal;
    ConsultaSAT.chaveSeguranca := chaveSeguranca;
    ConsultaSAT.GerarXML;

    FPDadosMsg := '<cfeDadosMsg>'+'<![CDATA['+ConsultaSAT.Gerador.ArquivoFormatoXML+ ']]>'+'</cfeDadosMsg>';
  finally
    ConsultaSAT.Free;
  end;
end;

function TConsultarSATWS.TratarResposta: Boolean;
begin
  //A função UTF8ToNativeString deve ser removida quando refatorado para usar ACBrXMlDocument
  FPRetWS := Trim(UTF8ToNativeString(ParseText(SeparaDados(FPRetornoWS, 'CfeConsultarLotesResult'))));

  FConsultaRet.Leitor.Arquivo := FPRetWS;
  FConsultaRet.LerXml;

  Result := (FPRetWS <> '');
end;

function TConsultarSATWS.GerarVersaoDadosSoap: String;
begin
  Result:='<versaoDados>'+versaoDados+'</versaoDados>';
end;


{ TACBrSATWS_WebServices }

constructor TACBrSATWS_WebServices.Create(AOwner: TACBrDFe);
begin
  FConsultarSATWS := TConsultarSATWS.Create(AOwner);
end;

destructor TACBrSATWS_WebServices.Destroy;
begin
  FConsultarSATWS.Free;
  inherited Destroy;
end;


end.
