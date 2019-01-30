{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Elias César Vieira                     }
{                                                                              }
{ Colaboradores nesse arquivo: Daniel Simões de Almeida                        }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 11/05/2016: Elias César Vieira
|*  - Primeira Versao ACBrMTer
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTer;

interface

uses
  Classes, SysUtils, contnrs, Controls, ExtCtrls, ACBrBase, ACBrUtil,
  ACBrConsts, ACBrSocket, ACBrMTerClass, ACBrBAL, blcksock;

type

  TACBrMTerModelo = (mtrNenhum, mtrVT100, mtrStxEtx, mtrPMTG, mtrSB100);
  TACBrMTerEchoMode = (mdeNormal, mdeNone, mdePassword);

  { Evento disparado quando Conecta }
  TACBrMTerConecta = procedure(const IP: String) of object;

  { Evento disparado quando Desconecta }
  TACBrMTerDesconecta = procedure(const IP: String; Erro: Integer;
    ErroDesc: String) of object;

  { Evento disparado quando Recebe Dados }
  TACBrMTerRecebeDados = procedure(const IP: String; var
    Recebido: AnsiString; var EchoMode: TACBrMTerEchoMode) of object;

  { Evento disparado quando ACBrMTer Recebe Peso }
  TACBrMTerRecebePeso = procedure(const IP: String;
    const PesoRecebido: Double) of object;

  { TACBrMTer }
  TACBrMTer = class;

  { TACBrMTerConexoes }
  TACBrMTerConexoes = class;

  { TACBrMTerConexao }

  TACBrMTerConexao = class
  private
    fBuffer: AnsiString;
    fTimerLerPeso: TACBrThreadTimer;
    fBalanca: TACBrBAL;
    fConectado: Boolean;
    fIP: String;
    fSerialBalanca: Integer;
    fConexoes: TACBrMTerConexoes;
    fUltimoDadoRecebido: AnsiString;
    fUltimoPesoLido: Double;
    function GetBALCon: TACBrBAL;
    function GetLendoPeso: Boolean;
    function GetTimerLerPeso: TACBrThreadTimer;

    procedure DoHookEnviaStringSerial(const aCmd: AnsiString);
    procedure OnTimerLerPeso(Sender: TObject);
  public
    constructor Create(aOwner: TACBrMTerConexoes; const AIP: String);
    destructor Destroy; override;
    procedure Clear;

    function AdicionarBuffer(ADados: AnsiString): Boolean;  // Retorna True, se Tem uma reposta completa
    procedure SolicitarPeso(aSerial: Integer);

    property Conectado: Boolean read fConectado write fConectado;
    property IP: String read fIP;
    property BALConexao: TACBrBAL read GetBALCon;
    property TimerLerPeso: TACBrThreadTimer read GetTimerLerPeso;
    property LendoPeso: Boolean read GetLendoPeso;

    property Buffer: AnsiString read fBuffer;
    property UltimoDadoRecebido: AnsiString read fUltimoDadoRecebido;
    property UltimoPesoLido: Double read fUltimoPesoLido;
  end;

  { TACBrMTerConexoes }

  TACBrMTerConexoes = class(TObjectList)
  private
    fACBrMTer: TACBrMTer;
    function GetConexao(aIP: String): TACBrMTerConexao;
    function GetObject(aIndex: Integer): TACBrMTerConexao;
    procedure SetObject(aIndex: Integer; aItem: TACBrMTerConexao);

  public
    constructor Create(FreeObjects: Boolean; aOwner: TACBrMTer);

    function Add(aObj: TACBrMTerConexao): Integer;
    procedure Insert(aIndex: Integer; aObj: TACBrMTerConexao);

    property ACBrMTer: TACBrMTer read fACBrMTer;

    property Conexao[aIP: String]: TACBrMTerConexao read GetConexao;
    property Objects[aIndex: Integer]: TACBrMTerConexao read GetObject
      write SetObject; default;
  end;

  { TACBrMTer }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrMTer = class(TACBrComponent)
  private
    fArqLog: String;
    fBalanca: TACBrBAL;
    fCmdEnviado: AnsiString;
    fConexoes: TACBrMTerConexoes;
    fDisplayColunas: Integer;
    fDisplayLinhas: Integer;
    fEchoMode: TACBrMTerEchoMode;
    fTimeOutBalanca: Integer;
    fMTer: TACBrMTerClass;
    fModelo: TACBrMTerModelo;
    fOnConecta: TACBrMTerConecta;
    fOnDesconecta: TACBrMTerDesconecta;
    fOnGravarLog: TACBrGravarLog;
    fOnRecebeDados: TACBrMTerRecebeDados;
    fOnRecebePeso: TACBrMTerRecebePeso;
    fPassWordChar: Char;
    fTCPServer: TACBrTCPServer;
    fTerminador: AnsiString;
    fTerminadorAsc: AnsiString;
    fTerminadorBalanca: AnsiString;
    fTerminadorBalancaAsc: AnsiString;
    function GetAtivo: Boolean;
    function GetIP: String;
    function GetModeloStr: String;
    function GetPort: String;
    function GetTimeOut: Integer;
    procedure SetAtivo(AValue: Boolean);
    procedure SetBalanca(AValue: TACBrBAL);
    procedure SetEchoMode(AValue: TACBrMTerEchoMode);
    procedure SetIP(const AValue: String);
    procedure SetModelo(AValue: TACBrMTerModelo);
    procedure SetPasswordChar(AValue: Char);
    procedure SetPort(const AValue: String);
    procedure SetTerminador(AValue: AnsiString);
    procedure SetTerminadorBalanca(AValue: AnsiString);
    procedure SetTimeOut(AValue: Integer);

    procedure DoConecta(const TCPBlockSocket: TTCPBlockSocket;
      var Enviar: AnsiString);
    procedure DoDesconecta(const TCPBlockSocket: TTCPBlockSocket;
      Erro: Integer; ErroDesc: String);
    procedure DoRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
      const Recebido: AnsiString; var Enviar: AnsiString);

    procedure EnviarComando(ASocket: TTCPBlockSocket; const ACmd: AnsiString); overload;
    function LerResposta(ASocket: TTCPBlockSocket; const aTimeOut: Integer;
      NumBytes: Integer = 0; const Terminador: AnsiString = ''): AnsiString; overload;

    function BuscarPorIP(const aIP: String): TTCPBlockSocket;
    function EncontrarConexao(aIP: String = ''): TTCPBlockSocket;

    procedure AdicionarConexao(const aIP: String);
    procedure DesconectarConexao(const aIP: String);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoRecebePeso(const aIP: String; const PesoRecebido: Double);

    property TerminadorAsc        : AnsiString  read fTerminadorAsc;
    property TerminadorBalancaAsc : AnsiString  read fTerminadorBalancaAsc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Ativar;
    procedure Desativar;
    procedure VerificarAtivo;

    procedure EnviarComando(const aIP: String; const ACmd: AnsiString); overload;
    function LerResposta(const aIP: String; const aTimeOut: Integer;
      NumBytes: Integer = 0; const Terminador: AnsiString = ''): AnsiString; overload;

    procedure GravaLog(aString: AnsiString; Traduz: Boolean = False);

    procedure BackSpace(const aIP: String);
    procedure Beep(const aIP: String);
    procedure DeslocarCursor(const aIP: String; aValue: Integer);
    procedure DeslocarLinha(const aIP: String; aValue: Integer);
    procedure EnviarParaParalela(const aIp: String; aDados: AnsiString);
    procedure EnviarParaSerial(const aIP: String; aDados: AnsiString; aSerial: Integer);
    procedure EnviarTexto(const aIP: String; aTexto: AnsiString);
    procedure LimparDisplay(const aIP: String);
    procedure LimparLinha(const aIP: String; aLinha: Integer);
    procedure PosicionarCursor(const aIP: String; aLinha, aColuna: Integer);
    procedure SolicitarPeso(const aIP: String; aSerial: Integer);
    function Online(const aIP: String): Boolean;

    property Ativo     : Boolean           read GetAtivo    write SetAtivo;
    property CmdEnviado: AnsiString        read fCmdEnviado;
    property Conexoes  : TACBrMTerConexoes read fConexoes;
    property MTer      : TACBrMTerClass    read fMTer;
    property ModeloStr : String            read GetModeloStr;
    property TCPServer : TACBrTCPServer    read fTCPServer;
  published
    property ArqLog        : String            read fArqLog         write fArqLog;
    property Balanca       : TACBrBAL          read fBalanca        write SetBalanca;
    property EchoMode      : TACBrMTerEchoMode read fEchoMode       write SetEchoMode;
    property IP            : String            read GetIP           write SetIP;
    property PasswordChar  : Char              read fPasswordChar   write SetPasswordChar;
    property Port          : String            read GetPort         write SetPort;
    property Terminador       : AnsiString     read fTerminador        write SetTerminador;
    property TerminadorBalanca: AnsiString     read fTerminadorBalanca write SetTerminadorBalanca;
    property TimeOut       : Integer           read GetTimeOut      write SetTimeOut default 5000;
    property TimeOutBalanca: Integer           read fTimeOutBalanca write fTimeOutBalanca default 1000;
    property Modelo        : TACBrMTerModelo   read fModelo         write SetModelo default mtrNenhum;
    property DisplayLinhas : Integer           read fDisplayLinhas  write fDisplayLinhas default 4;
    property DisplayColunas: Integer           read fDisplayColunas write fDisplayColunas default 20;

    property OnConecta     : TACBrMTerConecta     read fOnConecta     write fOnConecta;
    property OnDesconecta  : TACBrMTerDesconecta  read fOnDesconecta  write fOnDesconecta;
    property OnRecebeDados : TACBrMTerRecebeDados read fOnRecebeDados write fOnRecebeDados;
    property OnRecebePeso  : TACBrMTerRecebePeso  read fOnRecebePeso  write fOnRecebePeso;
    property OnGravarLog   : TACBrGravarLog       read fOnGravarLog   write fOnGravarLog;
  end;


implementation

uses
  strutils,
  ACBrMTerVT100, ACBrMTerPMTG, ACBrMTerStxEtx, ACBrMTerSB100;

{ TACBrMTerConexoes }

function TACBrMTerConexoes.GetConexao(aIP: String): TACBrMTerConexao;
var
  I: Integer;
begin
  Result := Nil;

  if (aIP = EmptyStr) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    if (Objects[I].IP = aIP) then
    begin
      Result := Objects[I];
      Exit;
    end;
  end;
end;

function TACBrMTerConexoes.GetObject(aIndex: Integer): TACBrMTerConexao;
begin
  Result := inherited GetItem(aIndex) as TACBrMTerConexao;
end;

procedure TACBrMTerConexoes.SetObject(aIndex: Integer; aItem: TACBrMTerConexao);
begin
  inherited SetItem(aIndex, aItem);
end;

constructor TACBrMTerConexoes.Create(FreeObjects: Boolean; aOwner: TACBrMTer);
begin
  inherited Create(FreeObjects);

  fACBrMTer := aOwner;
end;

function TACBrMTerConexoes.Add(aObj: TACBrMTerConexao): Integer;
begin
  Result := inherited Add(aObj);
end;

procedure TACBrMTerConexoes.Insert(aIndex: Integer; aObj: TACBrMTerConexao);
begin
  inherited Insert(aIndex, aObj);
end;

{ TACBrMTerConexao }

constructor TACBrMTerConexao.Create(aOwner: TACBrMTerConexoes; const AIP: String
  );
begin
  fConexoes := aOwner;
  fIP := AIP;

  fConectado := True;
  fBalanca := Nil;
  fTimerLerPeso := Nil;
  Clear;

  fConexoes.Add(Self);
end;

destructor TACBrMTerConexao.Destroy;
begin
  if Assigned(fBalanca) then
    fBalanca.Free;

  if Assigned(fTimerLerPeso) then
    fTimerLerPeso.Free;

  inherited Destroy;
end;

procedure TACBrMTerConexao.Clear;
begin
  fUltimoDadoRecebido := '';
  fUltimoPesoLido := 0;
end;

function TACBrMTerConexao.AdicionarBuffer(ADados: AnsiString): Boolean;
var
  Terminador: AnsiString;
  P, LenTer: Integer;
begin
  Result := False;
  fBuffer := fBuffer + ADados;

  if LendoPeso then
    Terminador := fConexoes.ACBrMTer.TerminadorBalancaAsc
  else
    Terminador := fConexoes.ACBrMTer.TerminadorAsc;

  LenTer := Length(Terminador);

  if (LenTer > 0) then
  begin
    P := pos(Terminador, fBuffer);
    if (P > 0) then
    begin
      fUltimoDadoRecebido := copy(fBuffer, 1, P+(LenTer-1));
      fBuffer := copy(fBuffer, P+LenTer, Length(fBuffer));
      Result := True;
    end;
  end
  else
  begin
    fUltimoDadoRecebido := fBuffer;
    fBuffer := '';
    Result := True;
  end;
end;

function TACBrMTerConexao.GetBALCon: TACBrBAL;
begin
  if (fBalanca = Nil) then
  begin
    fBalanca := TACBrBAL.Create(fConexoes.ACBrMTer);

    fBalanca.Device.Porta           := 'USB';
    fBalanca.Device.HookEnviaString := DoHookEnviaStringSerial;
  end;

  Result := fBalanca;
end;

function TACBrMTerConexao.GetLendoPeso: Boolean;
begin
  Result := TimerLerPeso.Enabled;
end;


function TACBrMTerConexao.GetTimerLerPeso: TACBrThreadTimer;
begin
  if (fTimerLerPeso = Nil) then
  begin
    fTimerLerPeso := TACBrThreadTimer.Create;
    fTimerLerPeso.Enabled := False;
    fTimerLerPeso.Interval := fConexoes.ACBrMTer.TimeOutBalanca;
    fTimerLerPeso.OnTimer := OnTimerLerPeso;
  end;

  Result := fTimerLerPeso;
end;

procedure TACBrMTerConexao.SolicitarPeso(aSerial: Integer);
begin
  fSerialBalanca := aSerial;
  Clear;
  BALConexao.Modelo := fConexoes.ACBrMTer.Balanca.Modelo;
  BALConexao.SolicitarPeso;
  TimerLerPeso.Enabled := True;
end;

procedure TACBrMTerConexao.DoHookEnviaStringSerial(const aCmd: AnsiString);
begin
  fConexoes.ACBrMTer.EnviarParaSerial(fIP, aCmd, fSerialBalanca);
end;

procedure TACBrMTerConexao.OnTimerLerPeso(Sender: TObject);
begin
  TimerLerPeso.Enabled := False;

  if (Length(fUltimoDadoRecebido) < 1) then
    fUltimoPesoLido := -9      // -9 = TimeOut em ACBrBAL
  else
    fUltimoPesoLido := BALConexao.InterpretarRepostaPeso(Trim(fUltimoDadoRecebido));

  fConexoes.ACBrMTer.DoRecebePeso(fIP, fUltimoPesoLido);
end;

{ TACBrMTer }

procedure TACBrMTer.SetPort(const AValue: String);
begin
  if (fTCPServer.Port = AValue) then
    Exit;

  VerificarAtivo;
  fTCPServer.Port := AValue;
end;

procedure TACBrMTer.SetTerminador(AValue: AnsiString);
begin
  if fTerminador = AValue then
    Exit;

  fTerminador  := AValue;
  fTerminadorAsc := TraduzComando( fTerminador ) ;

  if (fTerminadorAsc = '') and (AValue <> '') then  // não usou notação '#13,#10'
  begin
    fTerminadorAsc := AValue;
    fTerminador := StringToAsc(AValue);
  end;
end;

procedure TACBrMTer.SetTerminadorBalanca(AValue: AnsiString);
begin
  if fTerminadorBalanca = AValue then
    Exit;

  fTerminadorBalanca := AValue;
  fTerminadorBalancaAsc := TraduzComando( fTerminadorBalanca ) ;

  if (fTerminadorBalancaAsc = '') and (AValue <> '') then  // não usou notação '#13,#10'
  begin
    fTerminadorBalancaAsc := AValue;
    fTerminadorBalanca := StringToAsc(AValue);
  end;
end;

procedure TACBrMTer.SetTimeOut(AValue: Integer);
begin
  if (fTCPServer.TimeOut = AValue) then
    Exit;

  VerificarAtivo;
  fTCPServer.TimeOut := AValue;
end;

procedure TACBrMTer.DoConecta(const TCPBlockSocket: TTCPBlockSocket;
  var Enviar: AnsiString);
var
  wIP: String;
begin
  wIP := TCPBlockSocket.GetRemoteSinIP;
  AdicionarConexao(wIP);

  GravaLog('Terminal: ' + wIP + ' - Conectou');

  TCPBlockSocket.SendString(fMTer.ComandoBoasVindas);

  if Assigned(fOnConecta) then
    OnConecta(wIP);
end;

procedure TACBrMTer.DoDesconecta(const TCPBlockSocket: TTCPBlockSocket;
  Erro: Integer; ErroDesc: String);
var
  wIP, ErroMsg: String;
begin
  ErroMsg := IntToStr(Erro)+'-'+ErroDesc;

  if Assigned(TCPBlockSocket) then
  begin
    wIP := TCPBlockSocket.GetRemoteSinIP;
    GravaLog('Terminal: ' + wIP + ' - Desconectou - '+ErroMsg);

    DesconectarConexao(wIP);
  end
  else
  begin
    wIP := '';
    GravaLog(ErroMsg);
  end;

  if Assigned(fOnDesconecta) then
    OnDesconecta(wIP, Erro, ErroDesc);
end;

procedure TACBrMTer.DoRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
  const Recebido: AnsiString; var Enviar: AnsiString);
var
  wIP: String;
  wRecebido: AnsiString;
  wConexao: TACBrMTerConexao;
  wEchoMode: TACBrMTerEchoMode;
  wLendoPeso, RespostaCompleta: Boolean;
begin
  wIP       := TCPBlockSocket.GetRemoteSinIP;
  wConexao  := fConexoes.Conexao[wIP];
  if (Length(Recebido) < 1) or (not Assigned(wConexao)) then
    Exit;

  wLendoPeso := wConexao.LendoPeso;
  GravaLog( 'Terminal: ' + wIP + ' - ' +
            IfThen(wLendoPeso, 'LendoPeso: ', 'RecebeDados: ') +
            Recebido, True);

  RespostaCompleta := wConexao.AdicionarBuffer(Recebido);

  if RespostaCompleta then
  begin
    if wLendoPeso then
      wConexao.TimerLerPeso.OnTimer(Self)
    else
    begin
      wRecebido := fMTer.InterpretarResposta(wConexao.UltimoDadoRecebido);
      wEchoMode := EchoMode;
      if Assigned(fOnRecebeDados) then
        OnRecebeDados(wIP, wRecebido, wEchoMode);

      case wEchoMode of
        mdeNormal  :
          Enviar := fMTer.ComandoEco(wRecebido);
        mdePassword:
          Enviar := fMTer.ComandoEco(StringOfChar(PasswordChar, Length(wRecebido)));
      else
        Enviar := '';
      end;
    end;
  end;
end;

procedure TACBrMTer.DoRecebePeso(const aIP: String; const PesoRecebido: Double);
begin
  GravaLog('Terminal: ' + aIP + ' - RecebePeso: ' + FormatFloatBr(PesoRecebido, FloatMask(4)));
  if Assigned(fOnRecebePeso) then
    fOnRecebePeso(aIP, PesoRecebido);
end;

procedure TACBrMTer.EnviarComando(const aIP: String; const ACmd: AnsiString);
begin
  EnviarComando(EncontrarConexao(aIP), ACmd);
end;

procedure TACBrMTer.EnviarComando(ASocket: TTCPBlockSocket;
  const ACmd: AnsiString);
begin
  if (Length(ACmd) < 1) then
    Exit;

  if (not Ativo) then
    raise Exception.Create(ACBrStr('Componente ACBrMTer não está ATIVO'));

  fCmdEnviado := ACmd;
  GravaLog('Terminal: ' + ASocket.GetRemoteSinIP + ' - EnviarComando: ' + ACmd, True);
  ASocket.SendString(ACmd);
end;

function TACBrMTer.LerResposta(const aIP: String; const aTimeOut: Integer;
  NumBytes: Integer; const Terminador: AnsiString): AnsiString;
begin
  Result := LerResposta( EncontrarConexao(aIP), aTimeOut, NumBytes, Terminador );
end;

function TACBrMTer.LerResposta(ASocket: TTCPBlockSocket; const aTimeOut: Integer;
  NumBytes: Integer; const Terminador: AnsiString): AnsiString;
begin
  if NumBytes > 0 then
     Result := ASocket.RecvBufferStr( NumBytes, aTimeOut)
  else if (Terminador <> EmptyStr) then
     Result := ASocket.RecvTerminated( aTimeOut, Terminador)
  else
     Result := ASocket.RecvPacket( aTimeOut );
end;

function TACBrMTer.BuscarPorIP(const aIP: String): TTCPBlockSocket;
var
  wIP: String;
  I: Integer;
begin
  // Procura IP nas conexões ativas.
  Result := Nil;
  wIP    := EmptyStr;

  with fTCPServer.ThreadList.LockList do
  try
    for I := 0 to (Count - 1) do
    begin
      with TACBrTCPServerThread(Items[I]) do
      begin
        wIP := TCPBlockSocket.GetRemoteSinIP;

        if (aIP = wIP) or (aIP = OnlyNumber(wIP)) then
        begin
          Result := TCPBlockSocket;
          Break;
        end;
      end;
    end;
  finally
    fTCPServer.ThreadList.UnlockList;
  end;
end;

function TACBrMTer.EncontrarConexao(aIP: String): TTCPBlockSocket;
begin
  Result := Nil;
  aIP    := Trim(aIP);

  if (aIP = EmptyStr) then
    Exit;

  Result := BuscarPorIP(aIP);

  if not Assigned(Result) then
    raise Exception.Create(ACBrStr('Terminal '+ QuotedStr(aIP) +' não encontrado'));
end;

procedure TACBrMTer.AdicionarConexao(const aIP: String);
var
  wConexao: TACBrMTerConexao;
begin
  wConexao := fConexoes.Conexao[aIP];

  if Assigned(wConexao) then
    wConexao.Conectado := True
  else
    TACBrMTerConexao.Create(fConexoes, aIP);
end;

procedure TACBrMTer.DesconectarConexao(const aIP: String);
var
  wConexao: TACBrMTerConexao;
begin
  wConexao := fConexoes.Conexao[aIP];

  if Assigned(wConexao) then
    wConexao.Conectado := False;
end;

procedure TACBrMTer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation <> opRemove) then
    Exit;

  if (AComponent is TACBrBAL) and (fBalanca <> Nil) then
    fBalanca := Nil;
end;

procedure TACBrMTer.SetAtivo(AValue: Boolean);
begin
  if AValue then
    Ativar
  else
    Desativar;
end;

procedure TACBrMTer.SetBalanca(AValue: TACBrBAL);
begin
  if (fBalanca = AValue) then
    Exit;

  if Assigned(fBalanca) then
    fBalanca.RemoveFreeNotification(Self);

  fBalanca := AValue;

  if (fBalanca <> Nil) then
  begin
    fBalanca.FreeNotification(Self);

    // Utilizar sempre porta USB/DLL para Micro Terminal
    fBalanca.Porta := 'USB';
  end;
end;

procedure TACBrMTer.SetEchoMode(AValue: TACBrMTerEchoMode);
begin
  if (fEchoMode = AValue) then
    Exit;

  fEchoMode := AValue;

  case fEchoMode of
    mdeNone  : PasswordChar := ' ';
    mdeNormal: PasswordChar :=  #0;
  else
    if (PasswordChar = #0) or (PasswordChar = ' ') then
      PasswordChar := '*';
  end;
end;

procedure TACBrMTer.SetIP(const AValue: String);
begin
  if (fTCPServer.IP = AValue) then
    Exit;

  VerificarAtivo;
  fTCPServer.IP := AValue;
end;

procedure TACBrMTer.SetModelo(AValue: TACBrMTerModelo);
begin
  if (fModelo = AValue) then
    Exit;

  VerificarAtivo;
  FreeAndNil(fMTer);

  case AValue of
    mtrPMTG  : fMTer := TACBrMTerPMTG.Create(Self);
    mtrVT100 : fMTer := TACBrMTerVT100.Create(Self);
    mtrStxEtx: fMTer := TACBrMTerStxEtx.Create(Self);
    mtrSB100 : fMTer := TACBrMTerSB100.Create(Self);
  else
    fMTer := TACBrMTerClass.Create(Self);
  end;

  fModelo := AValue;
end;

procedure TACBrMTer.SetPasswordChar(AValue: Char);
begin
  if (fPasswordChar = AValue) then
    Exit;

  fPasswordChar    := AValue;
  case fPassWordChar of
    #0 : EchoMode := mdeNormal;
    ' ': EchoMode := mdeNone;
  else
    EchoMode := mdePassword;
  end;
end;

function TACBrMTer.GetAtivo: Boolean;
begin
  Result := fTCPServer.Ativo;
end;

function TACBrMTer.GetIP: String;
begin
  Result := fTCPServer.IP;
end;

function TACBrMTer.GetModeloStr: String;
begin
  Result := fMTer.ModeloStr;
end;

function TACBrMTer.GetPort: String;
begin
  Result := fTCPServer.Port;
end;

function TACBrMTer.GetTimeOut: Integer;
begin
  Result := fTCPServer.TimeOut;
end;

constructor TACBrMTer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fDisplayLinhas := 4;
  fDisplayColunas := 20;
  fTimeOutBalanca := 1000;
  fTerminador := '';
  fTerminadorAsc := '';
  fTerminadorBalanca := '#3';
  fTerminadorBalancaAsc := #3;

  fConexoes := TACBrMTerConexoes.Create(True, Self);

  { Instanciando TACBrTCPServer }
  fTCPServer := TACBrTCPServer.Create(Self);
  fTCPServer.OnConecta     := DoConecta;
  fTCPServer.OnDesConecta  := DoDesconecta;
  fTCPServer.OnRecebeDados := DoRecebeDados;

  { Instanciando fMTer com modelo genérico }
  fMTer := TACBrMTerClass.Create(Self);
  fModelo := mtrNenhum;
end;

destructor TACBrMTer.Destroy;
begin
  Desativar;

  if Assigned(fConexoes) then
    fConexoes.Free;

  if Assigned(fTCPServer) then
    FreeAndNil(fTCPServer);

  if Assigned(fMTer) then
    FreeAndNil(fMTer);

  inherited Destroy;
end;

procedure TACBrMTer.Ativar;
begin
  if Ativo then
    Exit;

  if (Modelo = mtrNenhum) then
    raise Exception.Create(ACBrStr('Modelo ainda não foi definido'));

  GravaLog(sLineBreak + StringOfChar('-', 80) + sLineBreak +
           'ATIVAR - ' + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) +
           ' - Modelo: ' + ModeloStr + ' - Porta: ' + fTCPServer.Port +
           ' - Terminador: ' + fTCPServer.Terminador +
           ' - Timeout: ' + IntToStr(fTCPServer.TimeOut) +
           sLineBreak + StringOfChar('-', 80) + sLineBreak);

  fTCPServer.Ativar;
end;

procedure TACBrMTer.Desativar;
begin
  if (not Ativo) then
    Exit;

  fTCPServer.Desativar;
end;

procedure TACBrMTer.VerificarAtivo;
begin
  if Ativo then
    raise Exception.Create(ACBrStr('Não é possível modificar as propriedades ' +
                                   'com ACBrMTer Ativo'));
end;

procedure TACBrMTer.GravaLog(aString: AnsiString; Traduz: Boolean);
var
  Tratado: Boolean;
begin
  Tratado := False;

  if Traduz then
    aString := TranslateUnprintable(aString);

  if Assigned(fOnGravarLog) then
    fOnGravarLog(aString, Tratado);

  if (not Tratado) then
    WriteLog(fArqLog, ' -- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', Now) +
                      ' -- ' + aString);
end;

procedure TACBrMTer.BackSpace(const aIP: String);
begin
  EnviarComando(aIP, fMTer.ComandoBackSpace);
end;

procedure TACBrMTer.Beep(const aIP: String);
begin
  EnviarComando(aIP, fMTer.ComandoBeep);
end;

procedure TACBrMTer.DeslocarCursor(const aIP: String; aValue: Integer);
begin
  // Desloca Cursor a partir da posição atual (Permite valores negativos)
  EnviarComando(aIP, fMTer.ComandoDeslocarCursor(aValue));
end;

procedure TACBrMTer.DeslocarLinha(const aIP: String; aValue: Integer);
begin
  // Desloca Linha a partir da posição atual(Valores: 1 ou -1)
  EnviarComando(aIP, fMTer.ComandoDeslocarLinha(aValue));
end;

procedure TACBrMTer.EnviarParaParalela(const aIp: String; aDados: AnsiString);
begin
  // Envia String para Porta Paralela
  EnviarComando(aIP, fMTer.ComandoEnviarParaParalela(aDados));
end;

procedure TACBrMTer.EnviarParaSerial(const aIP: String; aDados: AnsiString;
  aSerial: Integer);
begin
  // Envia String para Porta Serial
  EnviarComando(aIP, fMTer.ComandoEnviarParaSerial(aDados, aSerial));
end;

procedure TACBrMTer.EnviarTexto(const aIP: String; aTexto: AnsiString);
begin
  // Envia String para o Display
  EnviarComando(aIP, fMTer.ComandoEnviarTexto(aTexto));
end;

procedure TACBrMTer.LimparDisplay(const aIP: String);
begin
  // Limpa Display e posiciona cursor em 0,0
  EnviarComando(aIP, fMTer.ComandoLimparDisplay);
end;

procedure TACBrMTer.LimparLinha(const aIP: String; aLinha: Integer);
begin
  // Apaga Linha, mantendo cursor na posição atual
  EnviarComando(aIP, fMTer.ComandoLimparLinha(aLinha));
end;

procedure TACBrMTer.PosicionarCursor(const aIP: String; aLinha, aColuna: Integer
  );
begin
  // Posiciona cursor na posição informada
  EnviarComando(aIP, fMTer.ComandoPosicionarCursor(aLinha, aColuna));
end;

procedure TACBrMTer.SolicitarPeso(const aIP: String; aSerial: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  if not Assigned(fBalanca) then
    raise Exception.Create(ACBrStr('Componente ACBrMTer, não foi associado a um Componente ACBrbal'));

  wConexao := fConexoes.Conexao[aIP];
  if Assigned(wConexao) then
    wConexao.SolicitarPeso(aSerial);
end;

function TACBrMTer.Online(const aIP: String): Boolean;
var
  aSocket: TTCPBlockSocket;
  CmdOnLine, Resp: AnsiString;
begin
  Result := True;
  CmdOnLine := fMTer.ComandoOnline;

  if CmdOnLine = '' then   // protocolo não suporta comando OnLine
    Exit;

  aSocket := BuscarPorIP(aIP);
  // Desliga a Thread desta conexão, para ler a resposta manualmente
  if aSocket.Owner is TACBrTCPServerThread then
    TACBrTCPServerThread(aSocket.Owner).Enabled := False;

  try
    EnviarComando(aSocket, CmdOnLine);
    Resp := LerResposta(aSocket, TimeOut, 0, TCPServer.Terminador);
  finally
    if aSocket.Owner is TACBrTCPServerThread then
      TACBrTCPServerThread(aSocket.Owner).Enabled := True;
  end;

  Result := (Resp <> '');
end;

end.
