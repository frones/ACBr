{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrTEFDPayGoWeb;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrTEFDClass, ACBrTEFPayGoWebComum;

type

  { TACBrTEFDRespPayGoWeb }

  TACBrTEFDRespPayGoWeb = class( TACBrTEFDResp )
  public
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFDPayGoWeb }

  TACBrTEFDPayGoWeb = class( TACBrTEFDClass )
  private
    fsPGWebAPI: TACBrTEFPGWebAPI;

    function GetDiretorioTrabalho: String;
    function GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    function GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    function GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    function GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    function GetPathDLL: string;
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
    procedure SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
    procedure SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
    procedure SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
    procedure SetPathDLL(AValue: string);

    procedure GravaLogAPI(const ALogLine: String; var Tratado: Boolean);
  protected
  public
    property PathDLL: string read GetPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read GetDiretorioTrabalho write SetDiretorioTrabalho;

    constructor Create( AOwner : TComponent ) ; override;
    destructor Destroy ; override;

    procedure Inicializar ; override;
    procedure DesInicializar ; override;

    procedure AtivarGP ; override;
    procedure VerificaAtivo ; override;

    Procedure ATV ; override;
    Function ADM : Boolean ; override;
    Function CRT( Valor : Double; IndiceFPG_ECF : String;
       DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
    Function CHQ( Valor : Double; IndiceFPG_ECF : String;
       DocumentoVinculado : String = ''; CMC7 : String = '';
       TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
       DataCheque : TDateTime = 0; Banco   : String = '';
       Agencia    : String = ''; AgenciaDC : String = '';
       Conta      : String = ''; ContaDC   : String = '';
       Cheque     : String = ''; ChequeDC  : String = '';
       Compensacao: String = '' ) : Boolean ; override;
    Procedure NCN(Rede, NSU, Finalizacao : String;
       Valor : Double = 0; DocumentoVinculado : String = ''); override;
    Procedure CNF(Rede, NSU, Finalizacao : String;
       DocumentoVinculado : String = ''); override;
    Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
       Valor : Double) : Boolean; overload; override;
    Function PRE(Valor : Double; DocumentoVinculado : String = '';
       Moeda : Integer = 0) : Boolean; override;
    function CDP(const EntidadeCliente: string; out Resposta: string): Boolean; override;
  published
    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read GetOnExibeMenu write SetOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read GetOnObtemCampo write SetOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read GetOnExibeMensagem write SetOnExibeMensagem;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read GetOnAguardaPinPad write SetOnAguardaPinPad;
  end;

implementation

uses
  ACBrUtil;

{ TACBrTEFDRespPayGoWeb }

procedure TACBrTEFDRespPayGoWeb.ConteudoToProperty;
begin
  inherited ConteudoToProperty;
end;

{ TACBrTEFDPayGoWeb }

constructor TACBrTEFDPayGoWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fsPGWebAPI := TACBrTEFPGWebAPI.Create;
  fsPGWebAPI.OnGravarLog := GravaLogAPI;

  ArqReq := '';
  ArqResp := '';
  ArqSTS := '';
  ArqTemp := '';
  GPExeName := '';
  fpTipo := gpCliSiTef;
  Name := 'PayGoWeb';

  if Assigned(fpResp) then
    fpResp.Free ;

  fpResp := TACBrTEFDRespPayGoWeb.Create;
  fpResp.TipoGP := fpTipo;
end;

destructor TACBrTEFDPayGoWeb.Destroy;
begin
  fsPGWebAPI.Free;
  inherited Destroy;
end;

procedure TACBrTEFDPayGoWeb.Inicializar;
begin
  if Inicializado then
    Exit;

  fsPGWebAPI.Inicializada := True;
  GravaLog( Name +' Inicializado '+Name );

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDPayGoWeb.DesInicializar;
begin
  fsPGWebAPI.Inicializada := False; ;
  inherited DesInicializar;
end;

procedure TACBrTEFDPayGoWeb.AtivarGP;
begin
  raise EACBrTEFDErro.Create( ACBrStr( 'AtivarGP não se aplica a '+Name )) ;
end;

procedure TACBrTEFDPayGoWeb.VerificaAtivo;
begin
  inherited VerificaAtivo;
end;

function TACBrTEFDPayGoWeb.GetPathDLL: string;
begin
  Result := fsPGWebAPI.PathDLL;
end;

function TACBrTEFDPayGoWeb.GetDiretorioTrabalho: String;
begin
  Result := fsPGWebAPI.DiretorioTrabalho;
end;

function TACBrTEFDPayGoWeb.GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
begin
  Result := fsPGWebAPI.OnAguardaPinPad;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
begin
  Result := fsPGWebAPI.OnExibeMensagem;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
begin
  Result := fsPGWebAPI.OnExibeMenu;
end;

function TACBrTEFDPayGoWeb.GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
begin
  Result := fsPGWebAPI.OnObtemCampo;
end;

procedure TACBrTEFDPayGoWeb.SetDiretorioTrabalho(AValue: String);
begin
  fsPGWebAPI.DiretorioTrabalho := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
begin
  fsPGWebAPI.OnAguardaPinPad := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
begin
  fsPGWebAPI.OnExibeMensagem := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
begin
  fsPGWebAPI.OnExibeMenu := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
begin
  fsPGWebAPI.OnObtemCampo := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPathDLL(AValue: string);
begin
  fsPGWebAPI.PathDLL := AValue;
end;

procedure TACBrTEFDPayGoWeb.GravaLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  GravaLog(ALogLine);
end;

procedure TACBrTEFDPayGoWeb.ATV;
begin
  inherited ATV;
end;

function TACBrTEFDPayGoWeb.ADM: Boolean;
begin
  fsPGWebAPI.IniciarTransacao(PWOPER_ADMIN);
  Result := fsPGWebAPI.ExcutarTransacao;
  if Result then
  begin
    fsPGWebAPI.FinalizarTrancao(PWCNF_CNF_AUTO);
    ProcessarResposta;
  end;
end;

function TACBrTEFDPayGoWeb.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
begin
  Result := inherited CRT(Valor, IndiceFPG_ECF, DocumentoVinculado, Moeda);
end;

function TACBrTEFDPayGoWeb.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
begin
  Result := inherited CHQ(Valor, IndiceFPG_ECF, DocumentoVinculado, CMC7,
    TipoPessoa, DocumentoPessoa, DataCheque, Banco, Agencia, AgenciaDC, Conta,
    ContaDC, Cheque, ChequeDC, Compensacao);
end;

procedure TACBrTEFDPayGoWeb.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
begin
  inherited NCN(Rede, NSU, Finalizacao, Valor, DocumentoVinculado);
end;

procedure TACBrTEFDPayGoWeb.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  inherited CNF(Rede, NSU, Finalizacao, DocumentoVinculado);
end;

function TACBrTEFDPayGoWeb.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double): Boolean;
begin
  Result := inherited CNC(Rede, NSU, DataHoraTransacao, Valor);
end;

function TACBrTEFDPayGoWeb.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
begin
  Result := inherited PRE(Valor, DocumentoVinculado, Moeda);
end;

function TACBrTEFDPayGoWeb.CDP(const EntidadeCliente: string; out
  Resposta: string): Boolean;
begin
  Result := inherited CDP(EntidadeCliente, Resposta);
end;

end.

