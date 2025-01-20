{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - José Junior                                                                }
{ - Antônio Júnior                                                             }
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

unit ACBrPagamentosAPI;

interface

uses
  Classes, SysUtils, ACBrSocket, ACBrBase,
  ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrSchemasPagamentosAPI;

type

  TACBrPagamentosAPIAmbiente = (eamHomologacao, eamProducao);

  TACBrPagamentosAPIBancoConsulta = (
    bccNenhum,
    bccBancoDoBrasil
  );

  TACBrPagamentosAPI = class;
  TACBrPagamentosAPIBancoClass = class;
  EACBrPagamentosAPIException = class(EACBrException);

  { TACBrPagamentosAPIClass }

  TACBrPagamentosAPIClass = class
  private
    fLoteBoletosCriado: TACBrLoteBoletosResposta;
    fLoteBoletosConsultado: TACBrLoteBoletosResposta;
    fLoteBoletosSolicitado: TACBrLoteBoletosRequisicao;
    fLoteDARFConsultado: TACBrLoteDARFResposta;
    fLoteDARFCriado: TACBrLoteDARFResposta;
    fLoteDARFSolicitado: TACBrLoteDARFRequisicao;
    fLoteGPSConsultado: TACBrLoteGPSResposta;
    fLoteGPSCriado: TACBrLoteGPSResposta;
    fLoteGPSSolicitado: TACBrLoteGPSRequisicao;
    fLoteGRUConsultado: TACBrLoteGRURespostaConsulta;
    fLoteGRUCriado: TACBrLoteGRUResposta;
    fLoteGRUSolicitado: TACBrLoteGRURequisicao;
    fLoteGuiasCodigoBarrasConsultado: TACBrLoteGuiaCodBarrasRespostaConsulta;
    fLoteGuiasCodigoBarrasCriado: TACBrLoteGuiaCodBarrasResposta;
    fLoteGuiasCodigoBarrasSolicitado: TACBrLoteGuiasCodigoBarrasRequisicao;
    fPagamentoBoletoConsultado: TACBrPagamentoEspecificoBoletoResposta;
    fPagamentoGPSConsultado: TACBrPagamentoEspecificoGRUResposta;
    fPagamentoGRUConsultado: TACBrPagamentoEspecificoGRUResposta;
    fPagamentoGuiaCodigoBarrasConsultado: TACBrPagamentoEspecificoGuiaCodBarrasResposta;
    fRespostaErros: TACBrPagamentosAPIErros;
    function GetLoteBoletosConsultado: TACBrLoteBoletosResposta;
    function GetLoteBoletosCriado: TACBrLoteBoletosResposta;
    function GetLoteBoletosSolicitado: TACBrLoteBoletosRequisicao;
    function GetLoteDARFConsultado: TACBrLoteDARFResposta;
    function GetLoteDARFCriado: TACBrLoteDARFResposta;
    function GetLoteDARFSolicitado: TACBrLoteDARFRequisicao;
    function GetLoteGPSConsultado: TACBrLoteGPSResposta;
    function GetLoteGPSCriado: TACBrLoteGPSResposta;
    function GetLoteGPSSolicitado: TACBrLoteGPSRequisicao;
    function GetLoteGRUConsultado: TACBrLoteGRURespostaConsulta;
    function GetLoteGRUCriado: TACBrLoteGRUResposta;
    function GetLoteGRUSolicitado: TACBrLoteGRURequisicao;
    function GetLoteGuiasCodigoBarrasConsultado: TACBrLoteGuiaCodBarrasRespostaConsulta;
    function GetLoteGuiasCodigoBarrasCriado: TACBrLoteGuiaCodBarrasResposta;
    function GetLoteGuiasCodigoBarrasSolicitado: TACBrLoteGuiasCodigoBarrasRequisicao;
    function GetPagamentoBoletoConsultado: TACBrPagamentoEspecificoBoletoResposta;
    function GetPagamentoGPSConsultado: TACBrPagamentoEspecificoGRUResposta;
    function GetPagamentoGRUConsultado: TACBrPagamentoEspecificoGRUResposta;
    function GetPagamentoGuiaCodigoBarrasConsultado: TACBrPagamentoEspecificoGuiaCodBarrasResposta;
    function GetRespostaErros: TACBrPagamentosAPIErros;
  protected
    fpBanco: TACBrPagamentosAPIBancoClass;

    function Token: AnsiString;
    procedure RegistrarLog(const aStr: String; NivelMin: Integer = 1);
  public
    constructor Create(aBanco: TACBrPagamentosAPIBancoClass);
    destructor Destroy; override;
    procedure Clear;

    function BoletoSolicitarLotePagamentos: Boolean; virtual;
    function BoletoConsultarLotePagamentos(const aId: String): Boolean; virtual;
    function BoletoConsultarPagamentoEspecifico(const aId: String): Boolean; virtual;

    function GuiaCodigoBarrasSolicitarLotePagamentos: Boolean; virtual;
    function GuiaCodigoBarrasConsultarLotePagamentos(const aId: String): Boolean; virtual;
    function GuiaCodigoBarrasConsultarPagamentoEspecifico(const aId: String): Boolean; virtual;

    function GRUSolicitarPagamentos: Boolean; virtual;
    function GRUConsultarLotePagamentos(const aId: String): Boolean; virtual;
    function GRUConsultarPagamentoEspecifico(const aId: String): Boolean; virtual;

    function DARFSolicitarPagamentos: Boolean; virtual;
    function DARFConsultarLotePagamentos(const aId: String): Boolean; virtual;
    function DARFConsultarPagamentoEspecifico(const aId: String): Boolean; virtual;

    function GPSSolicitarPagamentos: Boolean; virtual;
    function GPSConsultarLotePagamentos(const aId: String): Boolean; virtual;
    function GPSConsultarPagamentoEspecifico(const aId: String): Boolean; virtual;

    property LoteBoletosCriado: TACBrLoteBoletosResposta read GetLoteBoletosCriado write fLoteBoletosCriado;
    property LoteBoletosConsultado: TACBrLoteBoletosResposta read GetLoteBoletosConsultado write fLoteBoletosConsultado;
    property LoteBoletosSolicitado: TACBrLoteBoletosRequisicao read GetLoteBoletosSolicitado write fLoteBoletosSolicitado;
    property PagamentoBoletoConsultado: TACBrPagamentoEspecificoBoletoResposta read GetPagamentoBoletoConsultado write fPagamentoBoletoConsultado;

    property LoteGuiasCodigoBarrasCriado: TACBrLoteGuiaCodBarrasResposta read GetLoteGuiasCodigoBarrasCriado write fLoteGuiasCodigoBarrasCriado;
    property LoteGuiasCodigoBarrasConsultado: TACBrLoteGuiaCodBarrasRespostaConsulta read GetLoteGuiasCodigoBarrasConsultado write fLoteGuiasCodigoBarrasConsultado;
    property LoteGuiasCodigoBarrasSolicitado: TACBrLoteGuiasCodigoBarrasRequisicao read GetLoteGuiasCodigoBarrasSolicitado write fLoteGuiasCodigoBarrasSolicitado;
    property PagamentoGuiaCodigoBarrasConsultado: TACBrPagamentoEspecificoGuiaCodBarrasResposta read GetPagamentoGuiaCodigoBarrasConsultado write fPagamentoGuiaCodigoBarrasConsultado;

    property LoteGRUCriado: TACBrLoteGRUResposta read GetLoteGRUCriado write fLoteGRUCriado;
    property LoteGRUConsultado: TACBrLoteGRURespostaConsulta read GetLoteGRUConsultado write fLoteGRUConsultado;
    property LoteGRUSolicitado: TACBrLoteGRURequisicao read GetLoteGRUSolicitado write fLoteGRUSolicitado;
    property PagamentoGRUConsultado: TACBrPagamentoEspecificoGRUResposta read GetPagamentoGRUConsultado write fPagamentoGRUConsultado;

    property LoteDARFCriado: TACBrLoteDARFResposta read GetLoteDARFCriado write fLoteDARFCriado;
    property LoteDARFConsultado: TACBrLoteDARFResposta read GetLoteDARFConsultado write fLoteDARFConsultado;
    property LoteDARFSolicitado: TACBrLoteDARFRequisicao read GetLoteDARFSolicitado write fLoteDARFSolicitado;
    //property PagamentoDARFConsultado: TACBrPagamentoEspecificoGRUResposta read GetPagamentoDARFConsultado write fPagamentoDARFConsultado;

    property LoteGPSCriado: TACBrLoteGPSResposta read GetLoteGPSCriado write fLoteGPSCriado;
    property LoteGPSConsultado: TACBrLoteGPSResposta read GetLoteGPSConsultado write fLoteGPSConsultado;
    property LoteGPSSolicitado: TACBrLoteGPSRequisicao read GetLoteGPSSolicitado write fLoteGPSSolicitado;
    property PagamentoGPSConsultado: TACBrPagamentoEspecificoGRUResposta read GetPagamentoGPSConsultado write fPagamentoGPSConsultado;

    property RespostaErros: TACBrPagamentosAPIErros read GetRespostaErros write fRespostaErros;
  end;

  { TACBrPagamentosAPIBancoClass }

  TACBrPagamentosAPIBancoClass = class(TACBrHTTP)
  private
    fClientID: String;
    fClientSecret: String;
    fDeveloperApplicationKey: String;
    procedure SetACBrPagamentosAPI(aValue: TACBrPagamentosAPI);
    procedure VerificarAutenticacao;
    procedure VerificarValidadeToken;
  protected
    fpToken: String;
    fpAutenticado: Boolean;
    fpValidadeToken: TDateTime;
    fpPagamentosAPI: TACBrPagamentosAPI;
    fpPagamentos: TACBrPagamentosAPIClass;

    function GetPagamentos: TACBrPagamentosAPIClass; virtual;

    procedure Autenticar; virtual;
    procedure RenovarToken; virtual;
    procedure DispararExcecao(E: Exception);

    function CalcularURL: String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
                  
    procedure PrepararHTTP; virtual;
    procedure RegistrarLog(const aStr: String; NivelMin: Integer = 1);

    property Pagamentos: TACBrPagamentosAPIClass read GetPagamentos;
  published
    property ClientID: String read fClientID write fClientID;
    property ClientSecret: String read fClientSecret write fClientSecret;

    property PagamentosAPI: TACBrPagamentosAPI read fpPagamentosAPI write SetACBrPagamentosAPI;
  end;

  { TACBrPagamentosAPI }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPagamentosAPI = class(TACBrComponent)
  private
    fAmbiente: TACBrPagamentosAPIAmbiente;
    fArqLOG: String;
    fBanco: TACBrPagamentosAPIBancoClass;
    fNivelLog: Byte;
    fTimeOut: Integer;
    procedure SetBanco(aValue: TACBrPagamentosAPIBancoClass);
  public
    procedure RegistrarLog(const aStr: String; NivelMin: Integer = 1);
  published                                                                      
    property Banco: TACBrPagamentosAPIBancoClass read fBanco write SetBanco;
    property Ambiente: TACBrPagamentosAPIAmbiente read fAmbiente write fAmbiente;
    property TimeOut: Integer read fTimeOut write fTimeOut default ChttpTimeOutDef;

    property ArqLOG: String read fArqLOG write fArqLOG;
    property NivelLog: Byte read fNivelLog write fNivelLog default 1;
  end;

implementation

uses
  ACBrAPIBase;

{ TACBrPagamentosAPIClass }

function TACBrPagamentosAPIClass.GetLoteBoletosSolicitado: TACBrLoteBoletosRequisicao;
begin
  if (not Assigned(fLoteBoletosSolicitado)) then
    fLoteBoletosSolicitado := TACBrLoteBoletosRequisicao.Create;
  Result := fLoteBoletosSolicitado;
end;

function TACBrPagamentosAPIClass.GetLoteDARFConsultado: TACBrLoteDARFResposta;
begin
  if (not Assigned(fLoteDARFConsultado)) then
    fLoteDARFConsultado := TACBrLoteDARFResposta.Create;
  Result := fLoteDARFConsultado;
end;

function TACBrPagamentosAPIClass.GetLoteDARFCriado: TACBrLoteDARFResposta;
begin
  if (not Assigned(fLoteDARFCriado)) then
    fLoteDARFCriado := TACBrLoteDARFResposta.Create;
  Result := fLoteDARFCriado;
end;

function TACBrPagamentosAPIClass.GetLoteDARFSolicitado: TACBrLoteDARFRequisicao;
begin
  if (not Assigned(fLoteDARFSolicitado)) then
    fLoteDARFSolicitado := TACBrLoteDARFRequisicao.Create;
  Result := fLoteDARFSolicitado;
end;

function TACBrPagamentosAPIClass.GetLoteGPSConsultado: TACBrLoteGPSResposta;
begin
  if (not Assigned(fLoteGPSConsultado)) then
    fLoteGPSConsultado := TACBrLoteGPSResposta.Create;
  Result := fLoteGPSConsultado;
end;

function TACBrPagamentosAPIClass.GetLoteGPSCriado: TACBrLoteGPSResposta;
begin
  if (not Assigned(fLoteGPSCriado)) then
    fLoteGPSCriado := TACBrLoteGPSResposta.Create;
  Result := fLoteGPSCriado;
end;

function TACBrPagamentosAPIClass.GetLoteGPSSolicitado: TACBrLoteGPSRequisicao;
begin
  if (not Assigned(fLoteGPSSolicitado)) then
    fLoteGPSSolicitado := TACBrLoteGPSRequisicao.Create;
  Result := fLoteGPSSolicitado;
end;

function TACBrPagamentosAPIClass.GetLoteGRUConsultado: TACBrLoteGRURespostaConsulta;
begin
  if (not Assigned(fLoteGRUConsultado)) then
    fLoteGRUConsultado := TACBrLoteGRURespostaConsulta.Create;
  Result := fLoteGRUConsultado;
end;

function TACBrPagamentosAPIClass.GetLoteGRUCriado: TACBrLoteGRUResposta;
begin
  if (not Assigned(fLoteGRUCriado)) then
    fLoteGRUCriado := TACBrLoteGRUResposta.Create;
  Result := fLoteGRUCriado;
end;

function TACBrPagamentosAPIClass.GetLoteGRUSolicitado: TACBrLoteGRURequisicao;
begin
  if (not Assigned(fLoteGRUSolicitado)) then
    fLoteGRUSolicitado := TACBrLoteGRURequisicao.Create;
  Result := fLoteGRUSolicitado;
end;

function TACBrPagamentosAPIClass.GetLoteGuiasCodigoBarrasConsultado: TACBrLoteGuiaCodBarrasRespostaConsulta;
begin
  if (not Assigned(fLoteGuiasCodigoBarrasConsultado)) then
    fLoteGuiasCodigoBarrasConsultado := TACBrLoteGuiaCodBarrasRespostaConsulta.Create;
  Result := fLoteGuiasCodigoBarrasConsultado;
end;

function TACBrPagamentosAPIClass.GetLoteGuiasCodigoBarrasCriado: TACBrLoteGuiaCodBarrasResposta;
begin
  if (not Assigned(fLoteGuiasCodigoBarrasCriado)) then
    fLoteGuiasCodigoBarrasCriado := TACBrLoteGuiaCodBarrasResposta.Create;
  Result := fLoteGuiasCodigoBarrasCriado;
end;

function TACBrPagamentosAPIClass.GetLoteGuiasCodigoBarrasSolicitado: TACBrLoteGuiasCodigoBarrasRequisicao;
begin
  if (not Assigned(fLoteGuiasCodigoBarrasSolicitado)) then
    fLoteGuiasCodigoBarrasSolicitado := TACBrLoteGuiasCodigoBarrasRequisicao.Create;
  Result := fLoteGuiasCodigoBarrasSolicitado;
end;

function TACBrPagamentosAPIClass.GetPagamentoBoletoConsultado: TACBrPagamentoEspecificoBoletoResposta;
begin
  if (not Assigned(fPagamentoBoletoConsultado)) then
    fPagamentoBoletoConsultado := TACBrPagamentoEspecificoBoletoResposta.Create;
  Result := fPagamentoBoletoConsultado;
end;

function TACBrPagamentosAPIClass.GetPagamentoGPSConsultado: TACBrPagamentoEspecificoGRUResposta;
begin
  if (not Assigned(fPagamentoGPSConsultado)) then
    fPagamentoGPSConsultado := TACBrPagamentoEspecificoGRUResposta.Create;
  Result := fPagamentoGPSConsultado;
end;

function TACBrPagamentosAPIClass.GetPagamentoGRUConsultado: TACBrPagamentoEspecificoGRUResposta;
begin
  if (not Assigned(fPagamentoGRUConsultado)) then
    fPagamentoGRUConsultado := TACBrPagamentoEspecificoGRUResposta.Create;
  Result := fPagamentoGRUConsultado;
end;

function TACBrPagamentosAPIClass.GetPagamentoGuiaCodigoBarrasConsultado: TACBrPagamentoEspecificoGuiaCodBarrasResposta;
begin
  if (not Assigned(fPagamentoGuiaCodigoBarrasConsultado)) then
    fPagamentoGuiaCodigoBarrasConsultado := TACBrPagamentoEspecificoGuiaCodBarrasResposta.Create;
  Result := fPagamentoGuiaCodigoBarrasConsultado;
end;

function TACBrPagamentosAPIClass.GetRespostaErros: TACBrPagamentosAPIErros;
begin
  if (not Assigned(fRespostaErros)) then
    fRespostaErros := TACBrPagamentosAPIErros.Create('erros');
  Result := fRespostaErros;
end;

function TACBrPagamentosAPIClass.Token: AnsiString;
begin
  Result := fpBanco.fpToken;
end;

procedure TACBrPagamentosAPIClass.RegistrarLog(const aStr: String; NivelMin: Integer);
begin
  fpBanco.RegistrarLog(aStr, NivelMin);
end;

constructor TACBrPagamentosAPIClass.Create(aBanco: TACBrPagamentosAPIBancoClass);
begin
  fpBanco := aBanco;
end;

function TACBrPagamentosAPIClass.GetLoteBoletosCriado: TACBrLoteBoletosResposta;
begin
  if (not Assigned(fLoteBoletosCriado)) then
    fLoteBoletosCriado := TACBrLoteBoletosResposta.Create;
  Result := fLoteBoletosCriado;
end;

function TACBrPagamentosAPIClass.GetLoteBoletosConsultado: TACBrLoteBoletosResposta;
begin
  if (not Assigned(fLoteBoletosConsultado)) then
    fLoteBoletosConsultado := TACBrLoteBoletosResposta.Create;
  Result := fLoteBoletosConsultado;
end;

destructor TACBrPagamentosAPIClass.Destroy;
begin
  if Assigned(fLoteBoletosCriado) then
    fLoteBoletosCriado.Free;
  if Assigned(fLoteBoletosConsultado) then
    fLoteBoletosConsultado.Free;
  if Assigned(fLoteBoletosSolicitado) then
    fLoteBoletosSolicitado.Free;
  if Assigned(fRespostaErros) then
    fRespostaErros.Free;
  if Assigned(fLoteGuiasCodigoBarrasCriado) then
    fLoteGuiasCodigoBarrasCriado.Free;
  if Assigned(fLoteGuiasCodigoBarrasConsultado) then
    fLoteGuiasCodigoBarrasConsultado.Free;
  if Assigned(fLoteGuiasCodigoBarrasSolicitado) then
    fLoteGuiasCodigoBarrasSolicitado.Free;
  if Assigned(fLoteGRUCriado) then
    fLoteGRUCriado.Free;
  if Assigned(fLoteGRUConsultado) then
    fLoteGRUConsultado.Free;
  if Assigned(fLoteGRUSolicitado) then
    fLoteGRUSolicitado.Free;
  if Assigned(fLoteDARFSolicitado) then
    fLoteDARFSolicitado.Free;
  if Assigned(fLoteDARFConsultado) then
    fLoteDARFConsultado.Free;
  if Assigned(fLoteDARFCriado) then
    fLoteDARFCriado.Free;
  if Assigned(fLoteGPSCriado) then
    fLoteGPSCriado.Free;
  if Assigned(fLoteGPSConsultado) then
    fLoteGPSConsultado.Free;
  if Assigned(fLoteGPSSolicitado) then
    fLoteGPSSolicitado.Free;
  if Assigned(fPagamentoGRUConsultado) then
    fPagamentoGRUConsultado.Free;
  if Assigned(fPagamentoGPSConsultado) then
    fPagamentoGPSConsultado.Free;
  if Assigned(fPagamentoBoletoConsultado) then
    fPagamentoBoletoConsultado.Free;
  if Assigned(fPagamentoGuiaCodigoBarrasConsultado) then
    fPagamentoGuiaCodigoBarrasConsultado.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosAPIClass.Clear;
begin
  if Assigned(fLoteBoletosCriado) then
    fLoteBoletosCriado.Clear;
  if Assigned(fLoteBoletosConsultado) then
    fLoteBoletosConsultado.Clear;
  if Assigned(fLoteBoletosSolicitado) then
    fLoteBoletosSolicitado.Clear;
  if Assigned(fRespostaErros) then
    fRespostaErros.Clear;
  if Assigned(fLoteGuiasCodigoBarrasCriado) then
    fLoteGuiasCodigoBarrasCriado.Clear;
  if Assigned(fLoteGuiasCodigoBarrasConsultado) then
    fLoteGuiasCodigoBarrasConsultado.Clear;
  if Assigned(fLoteGuiasCodigoBarrasSolicitado) then
    fLoteGuiasCodigoBarrasSolicitado.Clear;
  if Assigned(fLoteGRUCriado) then
    fLoteGRUCriado.Clear;
  if Assigned(fLoteGRUConsultado) then
    fLoteGRUConsultado.Clear;
  if Assigned(fLoteGRUSolicitado) then
    fLoteGRUSolicitado.Clear;
  if Assigned(fLoteDARFCriado) then
    fLoteDARFCriado.Clear;
  if Assigned(fLoteDARFConsultado) then
    fLoteDARFConsultado.Clear;
  if Assigned(fLoteDARFSolicitado) then
    fLoteDARFSolicitado.Clear;
  if Assigned(fLoteGPSCriado) then
    fLoteGPSCriado.Clear;
  if Assigned(fLoteGPSConsultado) then
    fLoteGPSConsultado.Clear;
  if Assigned(fLoteGPSSolicitado) then
    fLoteGPSSolicitado.Clear;
  if Assigned(fPagamentoGRUConsultado) then
    fPagamentoGRUConsultado.Clear;
  if Assigned(fPagamentoGPSConsultado) then
    fPagamentoGPSConsultado.Clear;
  if Assigned(fPagamentoBoletoConsultado) then
    fPagamentoBoletoConsultado.Clear;
  if Assigned(fPagamentoGuiaCodigoBarrasConsultado) then
    fPagamentoGuiaCodigoBarrasConsultado.Clear;
end;

function TACBrPagamentosAPIClass.BoletoSolicitarLotePagamentos: Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.BoletoConsultarLotePagamentos(const aId: String): Boolean;
begin
  { abstract }
end;

function TACBrPagamentosAPIClass.BoletoConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  { abstract }
end;

function TACBrPagamentosAPIClass.GuiaCodigoBarrasSolicitarLotePagamentos: Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GuiaCodigoBarrasConsultarLotePagamentos(
  const aId: String): Boolean;
begin 
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GuiaCodigoBarrasConsultarPagamentoEspecifico(
  const aId: String): Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GRUSolicitarPagamentos: Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GRUConsultarLotePagamentos(const aId: String
  ): Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GRUConsultarPagamentoEspecifico(
  const aId: String): Boolean;
begin 
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.DARFSolicitarPagamentos: Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.DARFConsultarLotePagamentos(const aId: String): Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.DARFConsultarPagamentoEspecifico(const aId: String): Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GPSSolicitarPagamentos: Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GPSConsultarLotePagamentos(const aId: String
  ): Boolean;
begin
  Result := False;
  { abstract }
end;

function TACBrPagamentosAPIClass.GPSConsultarPagamentoEspecifico(
  const aId: String): Boolean;
begin
  Result := False;
  { abstract }
end;

{ TACBrPagamentosAPIBancoClass }

procedure TACBrPagamentosAPIBancoClass.VerificarAutenticacao;
begin
  if (not fpAutenticado) then
  begin
    RegistrarLog('Autenticar', 3);
    Autenticar;
  end;

  VerificarValidadeToken;
end;

procedure TACBrPagamentosAPIBancoClass.SetACBrPagamentosAPI(aValue: TACBrPagamentosAPI);
var
  va: TACBrPagamentosAPI;
begin
  if (aValue = fpPagamentosAPI) then
    Exit;

  if Assigned(fpPagamentosAPI) then
    fpPagamentosAPI.RemoveFreeNotification(Self);

  // Usa outra variavel para evitar Loop Infinito na remoção da associação dos componentes
  va := fpPagamentosAPI;
  fpPagamentosAPI := aValue;

  if Assigned(va) and Assigned(va.Banco) then
    va.Banco := Nil;

  if Assigned(aValue) then
  begin
    aValue.FreeNotification(Self);
    aValue.Banco := Self;
  end;
end;

function TACBrPagamentosAPIBancoClass.GetPagamentos: TACBrPagamentosAPIClass;
begin
  if (not Assigned(fpPagamentos)) then
    fpPagamentos := TACBrPagamentosAPIClass.Create(Self);
  Result := fpPagamentos;
end;

procedure TACBrPagamentosAPIBancoClass.VerificarValidadeToken;
begin
  if (fpValidadeToken <> 0) and (fpValidadeToken < Now) then
  begin
    RegistrarLog('RenovarToken', 3);
    RenovarToken;
  end;
end;

procedure TACBrPagamentosAPIBancoClass.Autenticar;
begin
  fpAutenticado := True;
end;

procedure TACBrPagamentosAPIBancoClass.RenovarToken;
begin
  Autenticar;
end;

procedure TACBrPagamentosAPIBancoClass.PrepararHTTP;
begin
  RegistrarLog('PrepararHTTP', 3);
  VerificarAutenticacao;
  LimparHTTP;
end;

procedure TACBrPagamentosAPIBancoClass.DispararExcecao(E: Exception);
begin
  if (not Assigned(E)) then
    Exit;

  RegistrarLog(E.ClassName + ': ' + E.Message);
  raise E;
end;

function TACBrPagamentosAPIBancoClass.CalcularURL: String;
begin
  { Sobreescrever na Classe do Banco }
  Result := EmptyStr;
end;

constructor TACBrPagamentosAPIBancoClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpToken := EmptyStr;
  fpAutenticado := False;
  fClientID := EmptyStr;
  fClientSecret := EmptyStr;
end;

destructor TACBrPagamentosAPIBancoClass.Destroy;
begin
  if Assigned(fpPagamentos) then
    fpPagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosAPIBancoClass.Clear;
begin
  if Assigned(fpPagamentos) then
    fpPagamentos.Clear;
end;

procedure TACBrPagamentosAPIBancoClass.RegistrarLog(const aStr: String; NivelMin: Integer);
begin
  if Assigned(fpPagamentosAPI) then
    fpPagamentosAPI.RegistrarLog(aStr, NivelMin);
end;

{ TACBrPagamentosAPI }

procedure TACBrPagamentosAPI.SetBanco(aValue: TACBrPagamentosAPIBancoClass);
var
  banco: TACBrPagamentosAPIBancoClass;
begin
  if (aValue = fBanco) then
    Exit;

  if Assigned(aValue) then
    RegistrarLog('Atribuindo Banco: ' + aValue.ClassName + ', Nome: ' + aValue.Name)
  else
    RegistrarLog('Atribuindo Banco: Nil');

  if Assigned(fBanco) then
    fBanco.RemoveFreeNotification(Self);

  // // Usa outra variavel para evitar Loop Infinito na remoção da associação dos componentes
  banco := fBanco;
  fBanco := aValue;
  if Assigned(banco) and Assigned(banco.PagamentosAPI) then
    banco.PagamentosAPI := Nil;

  if Assigned(aValue) then
  begin
    aValue.FreeNotification(Self);
    aValue.PagamentosAPI := Self;

    if EstaVazio(aValue.ArqLOG) and NaoEstaVazio(fArqLOG) then
    begin
      aValue.ArqLOG := fArqLOG;
      aValue.NivelLog := fNivelLog;
    end;
  end;
end;

procedure TACBrPagamentosAPI.RegistrarLog(const aStr: String; NivelMin: Integer);
begin
  if (NivelLog < NivelMin) or EstaVazio(fArqLOG) then
    Exit;

  WriteLog(fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) + ' - ' + aStr);
end;

end.
