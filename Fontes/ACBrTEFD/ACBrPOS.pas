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

unit ACBrPOS;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrTEFPayGoComum, ACBrPOSPGWebAPI, ACBrTEFComum;

type

  TACBrPOSAposFinalizarTransacao = procedure(const TerminalId: String;
    Transacao: TACBrTEFResp; Status: TACBrPOSPGWebStatusTransacao) of object ;

  { TACBrPOSResp }

  TACBrPOSResp = class(TACBrTEFResp)
  private
    fTerminalId: String;
  public
    constructor Create(const TerminalId: String);
    property TerminalId: String read fTerminalId;
  end;


  { TACBrPOSRespList }

  TACBrPOSRespList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  private
    function GetRespPorTerminal(const TerminalId: String): TACBrPOSResp;
  protected
    procedure SetObject(Index: Integer; Item: TACBrPOSResp);
    function GetObject(Index: Integer): TACBrPOSResp;
  public
    function Add(Obj: TACBrPOSResp): Integer;
    procedure Insert(Index: Integer; Obj: TACBrPOSResp);
    property Objects[Index: Integer]: TACBrPOSResp read GetObject write SetObject; default;
    property Terminal[const TerminalId: String]: TACBrPOSResp read GetRespPorTerminal;
  end;

  { TACBrPOS }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllDesktopPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPOS = class( TACBrComponent )
  private
    fPOSPGWeb: TACBrPOSPGWebAPI;
    fArqLOG: String;
    fTEFRespList: TACBrPOSRespList;
    fOnGravarLog: TACBrGravarLog;
    fOnAposFinalizarTransacao: TACBrPOSAposFinalizarTransacao;
    function GetConfirmarAntesImpressao: Boolean;
    function GetConfirmarTransacoesPendentes: Boolean;
    function GetDadosTransacao(const TerminalId: String): TACBrTEFParametros;
    function GetDiretorioTrabalho: String;
    function GetImprimirViaClienteReduzida: Boolean;
    function GetInicializada: Boolean;
    function GetMaximoTerminaisConectados: Word;
    function GetMensagemBoasVindas: String;
    function GetNomeAplicacao: String;

    function GetOnAvaliarTransacaoPendente: TACBrPOSPGWebAvaliarTransacaoPendente;
    function GetOnMudaEstadoTerminal: TACBrPOSPGWebNovoEstadoTerminal;
    function GetOnNovaConexao: TACBrPOSPGWebNovaConexao;
    function GetParametrosAdicionais(const TerminalId: String): TACBrPOSPGWebAPIParametros;
    function GetPathDLL: String;
    function GetPortaTCP: Word;
    function GetSoftwareHouse: String;
    function GetSuportaDesconto: Boolean;
    function GetSuportaSaque: Boolean;
    function GetSuportaViasDiferenciadas: Boolean;
    function GetTEFResp(const TerminalId: String): TACBrPOSResp;
    function GetTempoDesconexaoAutomatica: Word;
    function GetUtilizaSaldoTotalVoucher: Boolean;
    function GetVersaoAplicacao: String;
    procedure SetConfirmarAntesImpressao(AValue: Boolean);
    procedure SetConfirmarTransacoesPendentes(AValue: Boolean);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetMensagemBoasVindas(AValue: String);
    procedure SetImprimirViaClienteReduzida(AValue: Boolean);
    procedure SetInicializada(AValue: Boolean);
    procedure SetMaximoTerminaisConectados(AValue: Word);
    procedure SetNomeAplicacao(AValue: String);
    procedure SetOnAvaliarTransacaoPendente(AValue: TACBrPOSPGWebAvaliarTransacaoPendente);
    procedure SetOnMudaEstadoTerminal(AValue: TACBrPOSPGWebNovoEstadoTerminal);
    procedure SetOnNovaConexao(AValue: TACBrPOSPGWebNovaConexao);
    procedure SetPathDLL(AValue: String);
    procedure SetPortaTCP(AValue: Word);
    procedure SetSoftwareHouse(AValue: String);
    procedure SetSuportaDesconto(AValue: Boolean);
    procedure SetSuportaSaque(AValue: Boolean);
    procedure SetSuportaViasDiferenciadas(AValue: Boolean);
    procedure SetTempoDesconexaoAutomatica(AValue: Word);
    procedure SetUtilizaSaldoTotalVoucher(AValue: Boolean);
    procedure SetVersaoAplicacao(AValue: String);

    procedure DadosDaTransacaoParaTEFResp(const TerminalId: String);
    procedure GravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure AposFinalizarTransacaoAPI(const TerminalId: String;
      Status: TACBrPOSPGWebStatusTransacao);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(aString: AnsiString; Traduz: Boolean = False);
    procedure ExibirMensagem(const TerminalId: String; const AMensagem: String;
      TempoEspera: Word = 0);
    procedure LimparTeclado(const TerminalId: String);
    function AguardarTecla(const TerminalId: String; Espera: Word = 0): Integer;
    function ObterDado(const TerminalId: String; const Titulo: String;
      const Mascara: String = ''; uiLenMin: Word = 1; uiLenMax: Word = 20;
      AlinhaAEsqueda: Boolean = False; PermiteAlfa: Boolean = False;
      OcultarDigitacao: Boolean = False; IntervaloMaxTeclas: Word = 0;
      const ValorInicial: String = ''; LinhaCaptura: Word = 2): String;
    function ExecutarMenu(const TerminalId: String; Opcoes: TStrings;
      const Titulo: String = ''; IntervaloMaxTeclas: Word = 0;
      OpcaoInicial: SmallInt = 0): SmallInt; overload;
    function ExecutarMenu(const TerminalId: String; const OpcoesPipe: String;
      const Titulo: String = ''; IntervaloMaxTeclas: Word = 0;
      OpcaoInicial: SmallInt = 0): SmallInt; overload;
    procedure Beep(const TerminalId: String; TipoBeep: TACBrPOSPGWebBeep = beepOK);
    procedure ImprimirTexto(const TerminalId: String; const ATexto: String);
    procedure AvancarPapel(const TerminalId: String);
    procedure ImprimirCodBarras(const TerminalId: String; const Codigo: String;
      Tipo: TACBrPOSPGWebCodBarras);
    procedure ImprimirQRCode(const TerminalId: String; const Conteudo: String);
    procedure ImprimirComprovantesTEF(const TerminalId: String;
      Tipo: TACBrPOSPGWebComprovantes = prnAmbas;
      IgnoraErroSemComprovante: Boolean = True);
    procedure ObterEstado(const TerminalId: String;
      out EstadoAtual: TACBrPOSPGWebEstadoTerminal;
      out Modelo: String; out MAC: String; out Serial: String); overload;
    function ObterEstado(const TerminalId: String): TACBrPOSPGWebEstadoTerminal; overload;

    procedure ExecutarTransacaoTEF(const TerminalId: String;
     Operacao: TACBrPOSPGWebOperacao;
     Comprovantes: TACBrPOSPGWebComprovantes = prnAmbas;
     ParametrosAdicionaisTransacao: TStrings = nil);
    procedure ExecutarTransacaoPagamento(const TerminalId: String;
     ValorPagto: Double; Comprovantes: TACBrPOSPGWebComprovantes = prnAmbas);

    procedure IniciarTransacao(const TerminalId: String;
      Operacao: TACBrPOSPGWebOperacao;
      ParametrosAdicionaisTransacao: TStrings = nil);
    procedure AdicionarParametro(const TerminalId: String; iINFO: Word;
      const AValor: AnsiString); overload;
    procedure AdicionarParametro(const TerminalId: String; AKeyValueStr: String);
      overload;
    function ExecutarTransacao(const TerminalId: String): SmallInt;
    function ObterInfo(const TerminalId: String; iINFO: Word): String;
    function ObterUltimoRetorno(const TerminalId: String): String;
    procedure ObterDadosDaTransacao(const TerminalId: String);
    procedure FinalizarTrancao(const TerminalId: String; Status: TACBrPOSPGWebStatusTransacao);

    function Desconectar(const TerminalId: String; Segundos: Word = 0): Boolean;
    procedure TerminarConexao(const TerminalId: String);
    function TerminarTodasConexoes: Integer;

    property Inicializada: Boolean read GetInicializada write SetInicializada;
    function EmTransacao(const TerminalId: String): Boolean;

    property ParametrosAdicionais[const TerminalId: String]: TACBrPOSPGWebAPIParametros read GetParametrosAdicionais;
    property DadosDaTransacao[const TerminalId: String]: TACBrTEFParametros read GetDadosTransacao;
    property TEFResp[const TerminalId: String]: TACBrPOSResp read GetTEFResp;

  published
    property PathDLL: String read GetPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read GetDiretorioTrabalho write SetDiretorioTrabalho;
    property ArqLOG: String read fArqLOG write fArqLOG;

    property SoftwareHouse: String read GetSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read GetNomeAplicacao write SetNomeAplicacao;
    property VersaoAplicacao: String read GetVersaoAplicacao write SetVersaoAplicacao;

    property PortaTCP: Word read GetPortaTCP write SetPortaTCP default CACBrPOSPGWebPortaTCP;
    property MaximoTerminaisConectados: Word read GetMaximoTerminaisConectados write SetMaximoTerminaisConectados default CACBrPOSPGWebMaxTerm;
    property TempoDesconexaoAutomatica: Word read GetTempoDesconexaoAutomatica write SetTempoDesconexaoAutomatica default CACBrPOSPGWebTempoDesconexao;
    property MensagemBoasVindas: String read GetMensagemBoasVindas write SetMensagemBoasVindas;

    Property SuportaSaque: Boolean read GetSuportaSaque write SetSuportaSaque default False;
    Property SuportaDesconto: Boolean read GetSuportaDesconto write SetSuportaDesconto default True;
    property ImprimirViaClienteReduzida : Boolean read GetImprimirViaClienteReduzida write SetImprimirViaClienteReduzida default True;
    property SuportaViasDiferenciadas: Boolean read GetSuportaViasDiferenciadas write SetSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read GetUtilizaSaldoTotalVoucher write SetUtilizaSaldoTotalVoucher default False;
    property ConfirmarTransacoesPendentes: Boolean read GetConfirmarTransacoesPendentes write SetConfirmarTransacoesPendentes default True;
    property ConfirmarAntesImpressao: Boolean read GetConfirmarAntesImpressao write SetConfirmarAntesImpressao default True;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnNovaConexao: TACBrPOSPGWebNovaConexao read GetOnNovaConexao write SetOnNovaConexao;
    property OnMudaEstadoTerminal: TACBrPOSPGWebNovoEstadoTerminal read GetOnMudaEstadoTerminal
      write SetOnMudaEstadoTerminal;
    property OnAposFinalizarTransacao: TACBrPOSAposFinalizarTransacao
      read fOnAposFinalizarTransacao write fOnAposFinalizarTransacao;
    property OnAvaliarTransacaoPendente: TACBrPOSPGWebAvaliarTransacaoPendente
      read GetOnAvaliarTransacaoPendente write SetOnAvaliarTransacaoPendente;

  end;

implementation

uses
  math, StrUtils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrConsts;

{ TACBrPOSResp }

constructor TACBrPOSResp.Create(const TerminalId: String);
begin
  inherited Create;
  fTerminalId := TerminalId;
end;

{ TACBrPOSRespList }

function TACBrPOSRespList.GetRespPorTerminal(const TerminalId: String
  ): TACBrPOSResp;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to Count-1 do
  begin
    if Objects[i].TerminalId = TerminalId then
    begin
      Result := Objects[i];
      Break;
    end;
  end;

  if Result = Nil then // Ainda não existe uma Resposta para esse Terminal, vamos criá-la
  begin
    i := Add( TACBrPOSResp.Create(TerminalId) );
    Result := Objects[i];
  end;
end;

procedure TACBrPOSRespList.SetObject(Index: Integer; Item: TACBrPOSResp);
begin
  inherited Items[Index] := Item;
end;

function TACBrPOSRespList.GetObject(Index: Integer): TACBrPOSResp;
begin
  Result := TACBrPOSResp(inherited Items[Index]);
end;

function TACBrPOSRespList.Add(Obj: TACBrPOSResp): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrPOSRespList.Insert(Index: Integer; Obj: TACBrPOSResp);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrPOS }

constructor TACBrPOS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fArqLOG := '';
  fOnGravarLog := Nil;
  fOnAposFinalizarTransacao := Nil;

  fTEFRespList := TACBrPOSRespList.Create(True); // FreeObjects;
  fPOSPGWeb := TACBrPOSPGWebAPI.Create;
  fPOSPGWeb.OnGravarLog := GravarLogAPI;
  fPOSPGWeb.OnAposFinalizarTransacao := AposFinalizarTransacaoAPI;
end;

destructor TACBrPOS.Destroy;
begin
  fTEFRespList.Free;
  fPOSPGWeb.Free;
  inherited Destroy;
end;

procedure TACBrPOS.GravarLog(aString: AnsiString; Traduz: Boolean);
var
  Tratado: Boolean;
begin
  Tratado := False;

  if Traduz then
    aString := TranslateUnprintable(aString);

  if Assigned(fOnGravarLog) then
    fOnGravarLog(aString, Tratado);

  if (not Tratado) and (fArqLOG <> '') then
    WriteLog(fArqLOG, ' -- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', Now) +
                      ' -- ' + aString);
end;

procedure TACBrPOS.GravarLogAPI(const ALogLine: String; var Tratado: Boolean);
begin
  GravarLog(ALogLine);
end;

function TACBrPOS.GetDadosTransacao(const TerminalId: String): TACBrTEFParametros;
begin
  Result := fPOSPGWeb.DadosDaTransacao[TerminalId];
end;

function TACBrPOS.GetConfirmarTransacoesPendentes: Boolean;
begin
  Result := fPOSPGWeb.ConfirmarTransacoesPendentes;
end;

function TACBrPOS.GetConfirmarAntesImpressao: Boolean;
begin
  Result := fPOSPGWeb.ConfirmarAntesImpressao;
end;

function TACBrPOS.GetDiretorioTrabalho: String;
begin
  Result := fPOSPGWeb.DiretorioTrabalho;
end;

function TACBrPOS.GetImprimirViaClienteReduzida: Boolean;
begin
  Result := fPOSPGWeb.ImprimirViaClienteReduzida;
end;

function TACBrPOS.GetInicializada: Boolean;
begin
  Result := fPOSPGWeb.Inicializada;
end;

function TACBrPOS.GetMaximoTerminaisConectados: Word;
begin
  Result := fPOSPGWeb.MaximoTerminaisConectados;
end;

function TACBrPOS.GetMensagemBoasVindas: String;
begin
  Result := fPOSPGWeb.MensagemBoasVindas;
end;

function TACBrPOS.GetNomeAplicacao: String;
begin
  Result := fPOSPGWeb.NomeAplicacao;
end;

function TACBrPOS.GetOnAvaliarTransacaoPendente: TACBrPOSPGWebAvaliarTransacaoPendente;
begin
  Result := fPOSPGWeb.OnAvaliarTransacaoPendente;
end;

function TACBrPOS.GetOnMudaEstadoTerminal: TACBrPOSPGWebNovoEstadoTerminal;
begin
  Result := fPOSPGWeb.OnMudaEstadoTerminal;
end;

function TACBrPOS.GetOnNovaConexao: TACBrPOSPGWebNovaConexao;
begin
  Result := fPOSPGWeb.OnNovaConexao;
end;

function TACBrPOS.GetParametrosAdicionais(const TerminalId: String
  ): TACBrPOSPGWebAPIParametros;
begin
  Result := fPOSPGWeb.ParametrosAdicionais[TerminalId];
end;

function TACBrPOS.GetPathDLL: String;
begin
  Result := fPOSPGWeb.PathDLL;
end;

function TACBrPOS.GetPortaTCP: Word;
begin
  Result := fPOSPGWeb.PortaTCP;
end;

function TACBrPOS.GetSoftwareHouse: String;
begin
  Result := fPOSPGWeb.SoftwareHouse;
end;

function TACBrPOS.GetSuportaDesconto: Boolean;
begin
  Result := fPOSPGWeb.SuportaDesconto;
end;

function TACBrPOS.GetSuportaSaque: Boolean;
begin
  Result := fPOSPGWeb.SuportaSaque;
end;

function TACBrPOS.GetSuportaViasDiferenciadas: Boolean;
begin
  Result := fPOSPGWeb.SuportaViasDiferenciadas;
end;

function TACBrPOS.GetTEFResp(const TerminalId: String): TACBrPOSResp;
begin
  Result := fTEFRespList.Terminal[TerminalId];
end;

function TACBrPOS.GetTempoDesconexaoAutomatica: Word;
begin
  Result := fPOSPGWeb.TempoDesconexaoAutomatica;
end;

function TACBrPOS.GetUtilizaSaldoTotalVoucher: Boolean;
begin
  Result := fPOSPGWeb.UtilizaSaldoTotalVoucher;
end;

function TACBrPOS.GetVersaoAplicacao: String;
begin
  Result := fPOSPGWeb.VersaoAplicacao;
end;

procedure TACBrPOS.SetConfirmarAntesImpressao(AValue: Boolean);
begin
  fPOSPGWeb.ConfirmarAntesImpressao := AValue;
end;

procedure TACBrPOS.SetConfirmarTransacoesPendentes(AValue: Boolean);
begin
  fPOSPGWeb.ConfirmarTransacoesPendentes := AValue;
end;

procedure TACBrPOS.SetDiretorioTrabalho(AValue: String);
begin
  fPOSPGWeb.DiretorioTrabalho := AValue;
end;

procedure TACBrPOS.SetMensagemBoasVindas(AValue: String);
begin
  fPOSPGWeb.MensagemBoasVindas := AValue;
end;

procedure TACBrPOS.SetImprimirViaClienteReduzida(AValue: Boolean);
begin
  fPOSPGWeb.ImprimirViaClienteReduzida := AValue;
end;

procedure TACBrPOS.SetInicializada(AValue: Boolean);
begin
  fPOSPGWeb.Inicializada := AValue;
end;

procedure TACBrPOS.SetMaximoTerminaisConectados(AValue: Word);
begin
  fPOSPGWeb.MaximoTerminaisConectados := AValue;
end;

procedure TACBrPOS.SetNomeAplicacao(AValue: String);
begin
  fPOSPGWeb.NomeAplicacao := AValue;
end;

procedure TACBrPOS.SetOnAvaliarTransacaoPendente(AValue: TACBrPOSPGWebAvaliarTransacaoPendente);
begin
  fPOSPGWeb.OnAvaliarTransacaoPendente := AValue;
end;

procedure TACBrPOS.SetOnMudaEstadoTerminal(AValue: TACBrPOSPGWebNovoEstadoTerminal);
begin
  fPOSPGWeb.OnMudaEstadoTerminal := AValue;
end;

procedure TACBrPOS.SetOnNovaConexao(AValue: TACBrPOSPGWebNovaConexao);
begin
  fPOSPGWeb.OnNovaConexao := AValue;
end;

procedure TACBrPOS.SetPathDLL(AValue: String);
begin
  fPOSPGWeb.PathDLL := AValue;
end;

procedure TACBrPOS.SetPortaTCP(AValue: Word);
begin
  fPOSPGWeb.PortaTCP := AValue;
end;

procedure TACBrPOS.SetSoftwareHouse(AValue: String);
begin
  fPOSPGWeb.SoftwareHouse := AValue;
end;

procedure TACBrPOS.SetSuportaDesconto(AValue: Boolean);
begin
  fPOSPGWeb.SuportaDesconto := AValue;
end;

procedure TACBrPOS.SetSuportaSaque(AValue: Boolean);
begin
  fPOSPGWeb.SuportaSaque := AValue;
end;

procedure TACBrPOS.SetSuportaViasDiferenciadas(AValue: Boolean);
begin
  fPOSPGWeb.SuportaViasDiferenciadas := AValue;
end;

procedure TACBrPOS.SetTempoDesconexaoAutomatica(AValue: Word);
begin
  fPOSPGWeb.TempoDesconexaoAutomatica := AValue;
end;

procedure TACBrPOS.SetUtilizaSaldoTotalVoucher(AValue: Boolean);
begin
  fPOSPGWeb.UtilizaSaldoTotalVoucher := AValue;
end;

procedure TACBrPOS.SetVersaoAplicacao(AValue: String);
begin
  fPOSPGWeb.VersaoAplicacao := AValue;
end;

procedure TACBrPOS.DadosDaTransacaoParaTEFResp(const TerminalId: String);
var
  i, p, AInfo: Integer;
  Lin, AValue: String;
  DadosDaTransacaoTerminal: TACBrTEFParametros;
  TEFRespTerminal: TACBrPOSResp;
begin
  DadosDaTransacaoTerminal := DadosDaTransacao[TerminalId];
  TEFRespTerminal := TEFResp[TerminalId];
  TEFRespTerminal.Clear;

  for i := 0 to DadosDaTransacaoTerminal.Count-1 do
  begin
    Lin := DadosDaTransacaoTerminal[i];
    p := pos('=', Lin);
    if (p > 0) then
    begin
      AInfo := StrToIntDef(copy(Lin, 1, p-1), -1);
      if (AInfo >= 0) then
      begin
        AValue := copy(Lin, P+1, Length(Lin));
        TEFRespTerminal.Conteudo.GravaInformacao(Ainfo, 0, AValue);
      end;
    end;
  end;

  ConteudoToPropertyPayGoWeb( TEFRespTerminal );
end;

procedure TACBrPOS.AposFinalizarTransacaoAPI(const TerminalId: String;
  Status: TACBrPOSPGWebStatusTransacao);
begin
  DadosDaTransacaoParaTEFResp(TerminalId);
  if Assigned(fOnAposFinalizarTransacao) then
    fOnAposFinalizarTransacao( TerminalId, TEFResp[TerminalId], Status);
end;

procedure TACBrPOS.Inicializar;
begin
  fPOSPGWeb.Inicializar;
end;

procedure TACBrPOS.DesInicializar;
begin
  fPOSPGWeb.DesInicializar;
end;

procedure TACBrPOS.ExibirMensagem(const TerminalId: String;
  const AMensagem: String; TempoEspera: Word);
begin
  fPOSPGWeb.ExibirMensagem(TerminalId, AMensagem, TempoEspera);
end;

procedure TACBrPOS.LimparTeclado(const TerminalId: String);
begin
  fPOSPGWeb.LimparTeclado(TerminalId);
end;

function TACBrPOS.AguardarTecla(const TerminalId: String; Espera: Word
  ): Integer;
begin
  Result := fPOSPGWeb.AguardarTecla(TerminalId, Espera);
end;

function TACBrPOS.ObterDado(const TerminalId: String; const Titulo: String;
  const Mascara: String; uiLenMin: Word; uiLenMax: Word;
  AlinhaAEsqueda: Boolean; PermiteAlfa: Boolean; OcultarDigitacao: Boolean;
  IntervaloMaxTeclas: Word; const ValorInicial: String; LinhaCaptura: Word
  ): String;
begin
   Result := fPOSPGWeb.ObterDado( TerminalId, Titulo, Mascara, uiLenMin, uiLenMax,
                                  AlinhaAEsqueda, PermiteAlfa, OcultarDigitacao,
                                  IntervaloMaxTeclas, ValorInicial, LinhaCaptura);
end;

function TACBrPOS.ExecutarMenu(const TerminalId: String; Opcoes: TStrings;
  const Titulo: String; IntervaloMaxTeclas: Word; OpcaoInicial: SmallInt
  ): SmallInt;
begin
  Result := fPOSPGWeb.ExecutarMenu(TerminalId, Opcoes, Titulo, IntervaloMaxTeclas, OpcaoInicial);
end;

function TACBrPOS.ExecutarMenu(const TerminalId: String;
  const OpcoesPipe: String; const Titulo: String; IntervaloMaxTeclas: Word;
  OpcaoInicial: SmallInt): SmallInt;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(OpcoesPipe, '|', sLineBreak, [rfReplaceAll] );
    Result := ExecutarMenu(TerminalId, SL, Titulo, IntervaloMaxTeclas, OpcaoInicial);
  finally
    SL.Free;
  end;
end;

procedure TACBrPOS.Beep(const TerminalId: String; TipoBeep: TACBrPOSPGWebBeep);
begin
  fPOSPGWeb.Beep(TerminalId, TipoBeep);
end;

procedure TACBrPOS.ImprimirTexto(const TerminalId: String; const ATexto: String);
begin
  fPOSPGWeb.ImprimirTexto(TerminalId, ATexto);
end;

procedure TACBrPOS.AvancarPapel(const TerminalId: String);
begin
  fPOSPGWeb.AvancarPapel(TerminalId);
end;

procedure TACBrPOS.ImprimirCodBarras(const TerminalId: String;
  const Codigo: String; Tipo: TACBrPOSPGWebCodBarras);
begin
  fPOSPGWeb.ImprimirCodBarras(TerminalId, Codigo, Tipo);
end;

procedure TACBrPOS.ImprimirQRCode(const TerminalId: String;
  const Conteudo: String);
begin
  fPOSPGWeb.ImprimirCodBarras(TerminalId, Conteudo, codeQRCODE);
end;

procedure TACBrPOS.ImprimirComprovantesTEF(const TerminalId: String;
  Tipo: TACBrPOSPGWebComprovantes; IgnoraErroSemComprovante: Boolean);
begin
  fPOSPGWeb.ImprimirComprovantesTEF(TerminalId, Tipo, IgnoraErroSemComprovante);
end;

procedure TACBrPOS.ObterEstado(const TerminalId: String; out
  EstadoAtual: TACBrPOSPGWebEstadoTerminal; out Modelo: String; out
  MAC: String; out Serial: String);
begin
   fPOSPGWeb.ObterEstado(TerminalId, EstadoAtual, Modelo, MAC, Serial);
end;

function TACBrPOS.ObterEstado(const TerminalId: String
  ): TACBrPOSPGWebEstadoTerminal;
begin
  Result := fPOSPGWeb.ObterEstado(TerminalId);
end;

procedure TACBrPOS.ExecutarTransacaoTEF(const TerminalId: String;
  Operacao: TACBrPOSPGWebOperacao; Comprovantes: TACBrPOSPGWebComprovantes;
  ParametrosAdicionaisTransacao: TStrings);
begin
  fPOSPGWeb.ExecutarTransacaoTEF(TerminalId, Operacao, Comprovantes, ParametrosAdicionaisTransacao);
end;

procedure TACBrPOS.ExecutarTransacaoPagamento(const TerminalId: String;
  ValorPagto: Double; Comprovantes: TACBrPOSPGWebComprovantes);
var
  PA: TACBrTEFParametros;
begin
  PA := TACBrTEFParametros.Create;
  try
    PA.ValueInfo[PWINFO_CURREXP] := '2'; // centavos
    PA.ValueInfo[PWINFO_TOTAMNT] := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
    PA.ValueInfo[PWINFO_CURRENCY] := '986';

    ExecutarTransacaoTEF( TerminalId, operVenda, Comprovantes, PA);
  finally
    PA.Free;
  end;
end;

procedure TACBrPOS.IniciarTransacao(const TerminalId: String;
  Operacao: TACBrPOSPGWebOperacao; ParametrosAdicionaisTransacao: TStrings);
begin
  fPOSPGWeb.IniciarTransacao(TerminalId, Operacao, ParametrosAdicionaisTransacao);
end;

procedure TACBrPOS.AdicionarParametro(const TerminalId: String; iINFO: Word;
  const AValor: AnsiString);
begin
  fPOSPGWeb.AdicionarParametro(TerminalId, AValor);
end;

procedure TACBrPOS.AdicionarParametro(const TerminalId: String;
  AKeyValueStr: String);
begin
  fPOSPGWeb.AdicionarParametro(TerminalId, AKeyValueStr);
end;

function TACBrPOS.ExecutarTransacao(const TerminalId: String): SmallInt;
begin
  Result := fPOSPGWeb.ExecutarTransacao(TerminalId);
end;

function TACBrPOS.ObterInfo(const TerminalId: String; iINFO: Word): String;
begin
  Result := fPOSPGWeb.ObterInfo(TerminalId, iINFO);
end;

function TACBrPOS.ObterUltimoRetorno(const TerminalId: String): String;
begin
  Result := fPOSPGWeb.ObterUltimoRetorno(TerminalId);
end;

procedure TACBrPOS.ObterDadosDaTransacao(const TerminalId: String);
begin
  fPOSPGWeb.ObterDadosDaTransacao(TerminalId);
  DadosDaTransacaoParaTEFResp(TerminalId);
end;

procedure TACBrPOS.FinalizarTrancao(const TerminalId: String;
  Status: TACBrPOSPGWebStatusTransacao);
begin
  fPOSPGWeb.FinalizarTrancao(TerminalId, Status);
end;

function TACBrPOS.Desconectar(const TerminalId: String; Segundos: Word): Boolean;
begin
  Result := fPOSPGWeb.Desconectar(TerminalId, Segundos);
end;

procedure TACBrPOS.TerminarConexao(const TerminalId: String);
begin
  fPOSPGWeb.TerminarConexao(TerminalId);
end;

function TACBrPOS.TerminarTodasConexoes: Integer;
begin
  Result := fPOSPGWeb.TerminarTodasConexoes;
end;

function TACBrPOS.EmTransacao(const TerminalId: String): Boolean;
begin
  Result := (fPOSPGWeb.ObterEstado(TerminalId) = statTEF);
end;

end.

