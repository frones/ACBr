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
    fOperacaoADM: Word;
    fOperacaoATV: Word;
    fOperacaoCHQ: Word;
    fOperacaoCNC: Word;
    fOperacaoCRT: Word;
    fOperacaoPRE: Word;
    fsPGWebAPI: TACBrTEFPGWebAPI;
    procedure GravaLogAPI(const ALogLine: String; var Tratado: Boolean);

    function GetCNPJEstabelecimento: String;
    function GetDiretorioTrabalho: String;
    function GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    function GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    function GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    function GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    function GetPathDLL: string;
    function GetPontoCaptura: String;
    function GetSuportaViasDiferenciadas: Boolean;
    function GetUtilizaSaldoTotalVoucher: Boolean;
    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
    procedure SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
    procedure SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
    procedure SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
    procedure SetPathDLL(AValue: string);
    procedure SetPontoCaptura(AValue: String);
    procedure SetSuportaViasDiferenciadas(AValue: Boolean);
    procedure SetUtilizaSaldoTotalVoucher(AValue: Boolean);
  protected
    procedure FazerRequisicao(PWOPER: Word; AHeader: String = '';
        Valor: Double = 0; Documento: String = '');
    function ContinuarRequisicao: Boolean;
    procedure ObterDadosTransacao;
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
    property CNPJEstabelecimento: String read GetCNPJEstabelecimento write SetCNPJEstabelecimento;
    property PontoCaptura: String read GetPontoCaptura write SetPontoCaptura;
    property SuportaViasDiferenciadas: Boolean read GetSuportaViasDiferenciadas
      write SetSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read GetUtilizaSaldoTotalVoucher
      write SetUtilizaSaldoTotalVoucher;

    property OperacaoATV: Word read fOperacaoATV write fOperacaoATV default PWOPER_INITIALIZ;
    property OperacaoADM: Word read fOperacaoADM write fOperacaoADM default PWOPER_ADMIN;
    property OperacaoCRT: Word read fOperacaoCRT write fOperacaoCRT default PWOPER_SALE;
    property OperacaoCHQ: Word read fOperacaoCHQ write fOperacaoCHQ default PWOPER_CHECKINQ;
    property OperacaoCNC: Word read fOperacaoCNC write fOperacaoCNC default PWOPER_SALEVOID;
    property OperacaoPRE: Word read fOperacaoPRE write fOperacaoPRE default PWOPER_PREPAID;

    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read GetOnExibeMenu write SetOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read GetOnObtemCampo write SetOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read GetOnExibeMensagem write SetOnExibeMensagem;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read GetOnAguardaPinPad write SetOnAguardaPinPad;
  end;

implementation

uses
  strutils, math,
  ACBrTEFD, ACBrUtil;

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
  fpTipo := gpPayGoWeb;
  Name := 'PayGoWeb';

  fOperacaoATV := PWOPER_INITIALIZ;
  fOperacaoADM := PWOPER_ADMIN;
  fOperacaoCRT := PWOPER_SALE;
  fOperacaoCHQ := PWOPER_CHECKINQ;
  fOperacaoCNC := PWOPER_SALEVOID;
  fOperacaoPRE := PWOPER_PREPAID;

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

  fsPGWebAPI.SoftwareHouse := TACBrTEFD(Owner).Identificacao.SoftwareHouse;
  fsPGWebAPI.NomeAplicacao := TACBrTEFD(Owner).Identificacao.NomeAplicacao;
  fsPGWebAPI.VersaoAplicacao := TACBrTEFD(Owner).Identificacao.VersaoAplicacao;
  fsPGWebAPI.SuportaDesconto := TACBrTEFD(Owner).SuportaDesconto;
  fsPGWebAPI.SuportaSaque := TACBrTEFD(Owner).SuportaSaque;
  fsPGWebAPI.ImprimirViaClienteReduzida := TACBrTEFD(Owner).ImprimirViaClienteReduzida;

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

procedure TACBrTEFDPayGoWeb.ATV;
begin
  FazerRequisicao(fOperacaoADM, 'ATV');
  if ContinuarRequisicao then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.ADM: Boolean;
begin
  FazerRequisicao(fOperacaoADM, 'ADM');
  if ContinuarRequisicao then
    ProcessarResposta;
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

function TACBrTEFDPayGoWeb.GetPathDLL: string;
begin
  Result := fsPGWebAPI.PathDLL;
end;

function TACBrTEFDPayGoWeb.GetPontoCaptura: String;
begin
  Result := fsPGWebAPI.PontoCaptura;
end;

function TACBrTEFDPayGoWeb.GetSuportaViasDiferenciadas: Boolean;
begin
  Result := fsPGWebAPI.SuportaViasDiferenciadas;
end;

function TACBrTEFDPayGoWeb.GetUtilizaSaldoTotalVoucher: Boolean;
begin
  Result := fsPGWebAPI.UtilizaSaldoTotalVoucher;
end;

procedure TACBrTEFDPayGoWeb.SetCNPJEstabelecimento(AValue: String);
begin
  fsPGWebAPI.CNPJEstabelecimento := AValue;
end;

function TACBrTEFDPayGoWeb.GetDiretorioTrabalho: String;
begin
  Result := fsPGWebAPI.DiretorioTrabalho;
end;

function TACBrTEFDPayGoWeb.GetCNPJEstabelecimento: String;
begin
  Result := fsPGWebAPI.CNPJEstabelecimento;
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

procedure TACBrTEFDPayGoWeb.SetSuportaViasDiferenciadas(AValue: Boolean);
begin
  fsPGWebAPI.SuportaViasDiferenciadas := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetUtilizaSaldoTotalVoucher(AValue: Boolean);
begin
  fsPGWebAPI.UtilizaSaldoTotalVoucher := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPontoCaptura(AValue: String);
begin
  fsPGWebAPI.PontoCaptura := AValue;
end;

procedure TACBrTEFDPayGoWeb.FazerRequisicao(PWOPER: Word; AHeader: String;
  Valor: Double; Documento: String);
begin
  GravaLog('FazerRequisicao: Oper:'+PWINFOToString(PWOPER)+', Header'+AHeader+
           ', Valor:'+FloatToStr(Valor)+', Documento:'+Documento);

  fsPGWebAPI.IniciarTransacao(PWOPER);

  { Adiciona Campos já conhecidos em Resp, para processa-los em
    métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
  Resp.Clear;
  with TACBrTEFDRespPayGoWeb( Resp ) do
  begin
    fpIDSeq := fpIDSeq + 1 ;
    if Documento = '' then
      Documento := IntToStr(fpIDSeq) ;

    Conteudo.GravaInformacao(899,100, AHeader ) ;
    Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
    Conteudo.GravaInformacao(899,102, Documento ) ;
    Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );

    Resp.TipoGP := fpTipo;
  end;
end;

function TACBrTEFDPayGoWeb.ContinuarRequisicao: Boolean;
begin
  Result := fsPGWebAPI.ExecutarTransacao;
  if Result then
    ObterDadosTransacao;
end;

procedure TACBrTEFDPayGoWeb.ObterDadosTransacao;
var
  i, P1,p2, Ainfo: Integer;
  Lin, AValue: String;
begin
  fsPGWebAPI.ObterDadosDaTransacao;

  with TACBrTEFDRespPayGoWeb(Resp) do
  begin
    for i := 0 to fsPGWebAPI.DadosDaTransacao.Count-1 do
    begin
      Lin := fsPGWebAPI.DadosDaTransacao[i];
      p1 := pos('=', Lin);
      if (p1 > 0) then
      begin
        AValue := copy(Lin, P1+1, Length(Lin));
        p1 := pos('(', Lin);
        p2 := PosEx(')', Lin, p1);
        Ainfo := StrToInt(copy(Lin, p1+1, p2-p1-1));
        Conteudo.GravaInformacao(Ainfo, 0, AValue);
      end;
    end;

    ConteudoToProperty;
  end;
end;

end.

