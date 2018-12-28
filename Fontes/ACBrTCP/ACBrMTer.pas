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

  TACBrMTerModelo = (mtrNenhum, mtrVT100, mtrStxEtx, mtrPMTG);
  TACBrMTerEchoMode = (mdeNormal, mdeNone, mdePassword);

  { Evento disparado quando Conecta }
  TACBrMTerConecta = procedure(const IP: AnsiString) of object;

  { Evento disparado quando Desconecta }
  TACBrMTerDesconecta = procedure(const IP: AnsiString; Erro: Integer;
    ErroDesc: AnsiString) of object;

  { Evento disparado quando Recebe Dados }
  TACBrMTerRecebeDados = procedure(const IP: AnsiString; var
    Recebido: AnsiString; var EchoMode: TACBrMTerEchoMode) of object;

  { Evento disparado quando ACBrMTer Recebe Peso }
  TACBrMTerRecebePeso = procedure(const IP: AnsiString;
    const PesoRecebido: Double) of object;

  { TACBrMTer }
  TACBrMTer = class;

  { TACBrMTerConexoes }
  TACBrMTerConexoes = class;

  { TACBrMTerConexao }

  TACBrMTerConexao = class
  private
    fBalanca: TACBrBAL;
    fConectado: Boolean;
    fIP: AnsiString;
    fLendoPeso: Boolean;
    fBalPorta: Integer;
    fOnRecebePeso: TACBrMTerRecebePeso;
    fOwner: TACBrMTerConexoes;
    fUltimoDadoRecebido: AnsiString;
    fUltimoPesoLido: Double;
    function GetBalanca: TACBrBAL;

    procedure DoHookEnviaString(const aCmd: AnsiString);
    //procedure DoHookLeString(const aNumBytes, aTimeOut: Integer;
    //  var aRetorno: AnsiString);
    procedure SetUltimoDadoRecebido(const AValue: AnsiString);
  public
    constructor Create(aOwner: TACBrMTerConexoes);
    destructor Destroy; override;

    procedure SolicitarPeso(aSerial: Integer);
    //function LePeso(aSerial: Integer): Double;

    property Conectado: Boolean    read fConectado write fConectado;
    property IP:        AnsiString read fIP        write fIP;
    property Balanca:   TACBrBAL   read GetBalanca;
    property LendoPeso: Boolean    read fLendoPeso;

    property UltimoPesoLido: Double read fUltimoPesoLido;
    property UltimoDadoRecebido: AnsiString read fUltimoDadoRecebido
      write SetUltimoDadoRecebido;

    property OnRecebePeso: TACBrMTerRecebePeso
      read fOnRecebePeso write fOnRecebePeso;
  end;

  { TACBrMTerConexoes }

  TACBrMTerConexoes = class(TObjectList)
  private
    fACBrMTer: TACBrMTer;
    function GetConexao(aIP: AnsiString): TACBrMTerConexao;
    function GetObject(aIndex: Integer): TACBrMTerConexao;
    procedure SetObject(aIndex: Integer; aItem: TACBrMTerConexao);

  public
    constructor Create(FreeObjects: Boolean; aOwner: TACBrMTer);

    function Add(aObj: TACBrMTerConexao): Integer;
    procedure Insert(aIndex: Integer; aObj: TACBrMTerConexao);

    property ACBrMTer: TACBrMTer read fACBrMTer;

    property Conexao[aIP: AnsiString]: TACBrMTerConexao read GetConexao;
    property Objects[aIndex: Integer]: TACBrMTerConexao read GetObject
      write SetObject; default;
  end;

  { TACBrMTer }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrMTer = class(TACBrComponent)
  private
    fArqLog: AnsiString;
    fBalanca: TACBrBAL;
    fCmdEnviado: AnsiString;
    fConexoes: TACBrMTerConexoes;
    fEchoMode: TACBrMTerEchoMode;
    fModeloStr: AnsiString;
    fMTer: TACBrMTerClass;
    fModelo: TACBrMTerModelo;
    fOnConecta: TACBrMTerConecta;
    fOnDesconecta: TACBrMTerDesconecta;
    fOnGravarLog: TACBrGravarLog;
    fOnRecebeDados: TACBrMTerRecebeDados;
    fOnRecebePeso: TACBrMTerRecebePeso;
    fPassWordChar: Char;
    fTCPServer: TACBrTCPServer;
    function GetAtivo: Boolean;
    function GetIP: AnsiString;
    function GetPort: AnsiString;
    function GetTerminador: AnsiString;
    function GetTimeOut: Integer;
    procedure SetAtivo(AValue: Boolean);
    procedure SetBalanca(AValue: TACBrBAL);
    procedure SetEchoMode(AValue: TACBrMTerEchoMode);
    procedure SetIP(const AValue: AnsiString);
    procedure SetModelo(AValue: TACBrMTerModelo);
    procedure SetPasswordChar(AValue: Char);
    procedure SetPort(const AValue: AnsiString);
    procedure SetTerminador(const AValue: AnsiString);
    procedure SetTimeOut(AValue: Integer);

    procedure DoConecta(const TCPBlockSocket: TTCPBlockSocket;
      var Enviar: AnsiString);
    procedure DoDesconecta(const TCPBlockSocket: TTCPBlockSocket;
      Erro: Integer; ErroDesc: String);
    procedure DoRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
      const Recebido: AnsiString; var Enviar: AnsiString);
    procedure DoConexaoRecebePeso(const aIP: AnsiString; const PesoRecebido: Double);

    procedure EnviarComando(ASocket: TTCPBlockSocket; const ACmd: AnsiString); overload;
    function LerResposta(ASocket: TTCPBlockSocket; const aTimeOut: Integer;
      NumBytes: Integer = 0; const Terminador: AnsiString = ''): AnsiString; overload;

    function BuscarPorIP(const aIP: AnsiString): TTCPBlockSocket;
    function EncontrarConexao(aIP: AnsiString = ''): TTCPBlockSocket;

    procedure AdicionarConexao(const aIP: AnsiString);
    procedure DesconectarConexao(const aIP: AnsiString);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Ativar;
    procedure Desativar;
    procedure VerificarAtivo;

    procedure EnviarComando(const aIP: AnsiString; const ACmd: AnsiString); overload;
    function LerResposta(const aIP: AnsiString; const aTimeOut: Integer;
      NumBytes: Integer = 0; const Terminador: AnsiString = ''): AnsiString; overload;

    procedure GravaLog(aString: AnsiString; Traduz: Boolean = False);

    procedure BackSpace(const aIP: AnsiString);
    procedure Beep(const aIP: AnsiString);
    procedure DeslocarCursor(const aIP: AnsiString; aValue: Integer);
    procedure DeslocarLinha(const aIP: AnsiString; aValue: Integer);
    procedure EnviarParaParalela(const aIp, aDados: AnsiString);
    procedure EnviarParaSerial(const aIP, aDados: AnsiString; aSerial: Integer);
    procedure EnviarTexto(const aIP, aTexto: AnsiString);
    procedure LimparDisplay(const aIP: AnsiString);
    procedure LimparLinha(const aIP: AnsiString; aLinha: Integer);
    procedure PosicionarCursor(const aIP: AnsiString; aLinha, aColuna: Integer);
    procedure SolicitarPeso(const aIP: AnsiString; aSerial: Integer);
    function Online(const aIP: AnsiString): Boolean;

    //function LePeso(aIP: AnsiString; aSerial: Integer): Double;

    property Ativo     : Boolean           read GetAtivo    write SetAtivo;
    property CmdEnviado: AnsiString        read fCmdEnviado;
    property Conexoes  : TACBrMTerConexoes read fConexoes;
    property MTer      : TACBrMTerClass    read fMTer;
    property ModeloStr : AnsiString        read fModeloStr;
    property TCPServer : TACBrTCPServer    read fTCPServer;

  published
    property ArqLog      : AnsiString        read fArqLog       write fArqLog;
    property Balanca     : TACBrBAL          read fBalanca      write SetBalanca;
    property EchoMode    : TACBrMTerEchoMode read fEchoMode     write SetEchoMode;
    property IP          : AnsiString        read GetIP         write SetIP;
    property PasswordChar: Char              read fPasswordChar write SetPasswordChar;
    property Port        : AnsiString        read GetPort       write SetPort;
    property Terminador  : AnsiString        read GetTerminador write SetTerminador;
    property TimeOut     : Integer           read GetTimeOut    write SetTimeOut;
    property Modelo      : TACBrMTerModelo   read fModelo       write SetModelo default mtrNenhum;

    property OnConecta    : TACBrMTerConecta     read fOnConecta     write fOnConecta;
    property OnDesconecta : TACBrMTerDesconecta  read fOnDesconecta  write fOnDesconecta;
    property OnRecebeDados: TACBrMTerRecebeDados read fOnRecebeDados write fOnRecebeDados;
    property OnRecebePeso : TACBrMTerRecebePeso  read fOnRecebePeso  write fOnRecebePeso;
    property OnGravarLog  : TACBrGravarLog       read fOnGravarLog   write fOnGravarLog;
  end;


implementation

uses ACBrMTerVT100, ACBrMTerPMTG, ACBrMTerStxEtx;

{ TACBrMTerConexoes }

function TACBrMTerConexoes.GetConexao(aIP: AnsiString): TACBrMTerConexao;
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

function TACBrMTerConexao.GetBalanca: TACBrBAL;
begin
  if (fBalanca = Nil) then
  begin
    fBalanca := TACBrBAL.Create(fOwner.ACBrMTer);

    fBalanca.Device.Porta           := 'USB';
    //fBalanca.Device.HookLeString    := DoHookLeString;
    fBalanca.Device.HookEnviaString := DoHookEnviaString;
  end;

  Result := fBalanca;
end;

procedure TACBrMTerConexao.DoHookEnviaString(const aCmd: AnsiString);
begin
  if fLendoPeso then
    fOwner.ACBrMTer.EnviarParaSerial(fIP, aCmd, fBalPorta);
end;

{procedure TACBrMTerConexao.DoHookLeString(const aNumBytes, aTimeOut: Integer;
  var aRetorno: AnsiString);
begin
  if fUltimoPesoLido;
  if (fBalResp = EmptyStr) then
    aRetorno := '-1'
  else
    aRetorno := fBalResp;
end;   }

procedure TACBrMTerConexao.SetUltimoDadoRecebido(const AValue: AnsiString);
begin
  if (AValue = EmptyStr) then
    Exit;

  fUltimoDadoRecebido := AValue;

  if fLendoPeso then
  begin
    try
      fUltimoPesoLido := fBalanca.InterpretarRepostaPeso(fUltimoDadoRecebido);

      if Assigned(fOnRecebePeso) then
        fOnRecebePeso(fIP, fUltimoPesoLido);
    finally
      fBalPorta      := -1;
      fLendoPeso     := False;
    end;
  end;
end;

constructor TACBrMTerConexao.Create(aOwner: TACBrMTerConexoes);
begin
  fOwner              := aOwner;
  fLendoPeso          := False;
  fConectado          := True;
  fBalanca            := Nil;
  fIP                 := '';
  fUltimoDadoRecebido := '';
  fUltimoPesoLido     := 0;
end;

destructor TACBrMTerConexao.Destroy;
begin
  if Assigned(fBalanca) then
    fBalanca.Free;

  inherited Destroy;
end;

procedure TACBrMTerConexao.SolicitarPeso(aSerial: Integer);
begin
  fLendoPeso := True;
  fBalPorta  := aSerial;

  fBalanca.SolicitarPeso;
end;

{function TACBrMTerConexao.LePeso(aSerial: Integer): Double;
var
  wUsaSynchOld: Boolean;
begin
  Result := 0;

  if (fBalanca = Nil) or (aSerial < 0) then
    Exit;

  fLendoPeso   := True;
  fBalPorta    := aSerial;
  wUsaSynchOld := fOwner.ACBrMTer.TCPServer.UsaSynchronize;
  fOwner.TCPServer.UsaSynchronize := False;

  try
    fBalanca.SolicitarPeso;

    while (fBalResp = '') and (Result <> -9) do
      Sleep(200);

    Result := fBalanca.InterpretarRepostaPeso(fBalResp);
  finally
    fBalPorta  := -1;
    fBalResp   := '';
    fLendoPeso := False;
    fOwner.TCPServer.UsaSynchronize := wUsaSynchOld;
  end;
end;}

{ TACBrMTer }

procedure TACBrMTer.SetPort(const AValue: AnsiString);
begin
  if (fTCPServer.Port = AValue) then
    Exit;

  VerificarAtivo;
  fTCPServer.Port := AValue;
end;

procedure TACBrMTer.SetTerminador(const AValue: AnsiString);
begin
  if (fTCPServer.Terminador = AValue) then
    Exit;

  VerificarAtivo;
  fTCPServer.Terminador := AValue;
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
  wIP: AnsiString;
begin
  wIP := TCPBlockSocket.GetRemoteSinIP;
  AdicionarConexao(wIP);

  GravaLog('Terminal: ' + wIP + ' Conectou');

  TCPBlockSocket.SendString(fMTer.ComandoBoasVindas);

  if Assigned(fOnConecta) then
    OnConecta(wIP);
end;

procedure TACBrMTer.DoDesconecta(const TCPBlockSocket: TTCPBlockSocket;
  Erro: Integer; ErroDesc: String);
var
  wIP, ErroMsg: AnsiString;
begin
  ErroMsg := IntToStr(Erro)+'-'+ErroDesc;

  if Assigned(TCPBlockSocket) then
  begin
    wIP := TCPBlockSocket.GetRemoteSinIP;
    GravaLog('Terminal: ' + wIP + ' Desconectou - '+ErroMsg);

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
  wIP, wRecebido: AnsiString;
  wConexao: TACBrMTerConexao;
  wEchoMode: TACBrMTerEchoMode;
  wLendoPeso: Boolean;
begin
  wIP        := TCPBlockSocket.GetRemoteSinIP;
  wConexao   := fConexoes.Conexao[wIP];
  wEchoMode  := EchoMode;
  wRecebido  := fMTer.InterpretarResposta(Recebido);

  if (wRecebido = EmptyStr) or (not Assigned(wConexao)) then
    Exit;

  wLendoPeso := wConexao.LendoPeso;

  GravaLog('Terminal: ' + wIP + ' - LendoPeso: ' + BoolToStr(wLendoPeso, True) +
           ' - RecebeDados: ' + Recebido);

  wConexao.UltimoDadoRecebido := MTer.LimparConteudoParaEnviar(wRecebido);

  if wLendoPeso then
    Exit;

  if Assigned(fOnRecebeDados) then
    OnRecebeDados(wIP, wRecebido, wEchoMode);

  case wEchoMode of
    mdeNormal  : Enviar := fMTer.ComandoEco(wRecebido);
    mdePassword: Enviar := fMTer.ComandoEco(PadCenter('', Length(wRecebido), PasswordChar));
  end;
end;

procedure TACBrMTer.DoConexaoRecebePeso(const aIP: AnsiString;
  const PesoRecebido: Double);
var
  wPesoStr: AnsiString;
begin
  wPesoStr  := FormatFloat('##0.000', PesoRecebido);

  GravaLog('Terminal: ' + aIP + ' - RecebePeso: ' + wPesoStr);

  if Assigned(fOnRecebePeso) then
    fOnRecebePeso(aIP, PesoRecebido);
end;

procedure TACBrMTer.EnviarComando(const aIP: AnsiString; const ACmd: AnsiString);
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
  GravaLog('Terminal: ' + ASocket.GetRemoteSinIP + ' - Comando enviado: ' + ACmd);
  ASocket.SendString(ACmd);
end;

function TACBrMTer.LerResposta(const aIP: AnsiString; const aTimeOut: Integer;
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

function TACBrMTer.BuscarPorIP(const aIP: AnsiString): TTCPBlockSocket;
var
  wIP: AnsiString;
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

function TACBrMTer.EncontrarConexao(aIP: AnsiString): TTCPBlockSocket;
begin
  Result := Nil;
  aIP    := Trim(aIP);

  if (aIP = EmptyStr) then
    Exit;

  Result := BuscarPorIP(aIP);

  if not Assigned(Result) then
    raise Exception.Create(ACBrStr('Terminal '+ QuotedStr(aIP) +' não encontrado'));
end;

procedure TACBrMTer.AdicionarConexao(const aIP: AnsiString);
var
  wConexao: TACBrMTerConexao;
begin
  wConexao := fConexoes.Conexao[aIP];

  if Assigned(wConexao) then
    wConexao.Conectado := True
  else
  begin
    wConexao := TACBrMTerConexao.Create(fConexoes);
    wConexao.IP := aIP;

    fConexoes.Add(wConexao);
  end;

  wConexao.OnRecebePeso := DoConexaoRecebePeso;
end;

procedure TACBrMTer.DesconectarConexao(const aIP: AnsiString);
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

procedure TACBrMTer.SetIP(const AValue: AnsiString);
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

function TACBrMTer.GetIP: AnsiString;
begin
  Result := fTCPServer.IP;
end;

function TACBrMTer.GetPort: AnsiString;
begin
  Result := fTCPServer.Port;
end;

function TACBrMTer.GetTerminador: AnsiString;
begin
  Result := fTCPServer.Terminador;
end;

function TACBrMTer.GetTimeOut: Integer;
begin
  Result := fTCPServer.TimeOut;
end;

constructor TACBrMTer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Modelo    := mtrNenhum;
  fConexoes := TACBrMTerConexoes.Create(True, Self);

  { Instanciando TACBrTCPServer }
  fTCPServer := TACBrTCPServer.Create(Self);
  fTCPServer.OnConecta     := DoConecta;
  fTCPServer.OnDesConecta  := DoDesconecta;
  fTCPServer.OnRecebeDados := DoRecebeDados;

  { Instanciando fMTer com modelo genérico }
  fMTer := TACBrMTerClass.Create(Self);
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

procedure TACBrMTer.BackSpace(const aIP: AnsiString);
begin
  EnviarComando(aIP, fMTer.ComandoBackSpace);
end;

procedure TACBrMTer.Beep(const aIP: AnsiString);
begin
  EnviarComando(aIP, fMTer.ComandoBeep);
end;

procedure TACBrMTer.DeslocarCursor(const aIP: AnsiString; aValue: Integer);
begin
  // Desloca Cursor a partir da posição atual (Permite valores negativos)
  EnviarComando(aIP, fMTer.ComandoDeslocarCursor(aValue));
end;

procedure TACBrMTer.DeslocarLinha(const aIP: AnsiString; aValue: Integer);
begin
  // Desloca Linha a partir da posição atual(Valores: 1 ou -1)
  EnviarComando(aIP, fMTer.ComandoDeslocarLinha(aValue));
end;

procedure TACBrMTer.EnviarParaParalela(const aIp, aDados: AnsiString);
begin
  // Envia String para Porta Paralela
  EnviarComando(aIP, fMTer.ComandoEnviarParaParalela(aDados));
end;

procedure TACBrMTer.EnviarParaSerial(const aIP, aDados: AnsiString; aSerial: Integer);
begin
  // Envia String para Porta Serial
  EnviarComando(aIP, fMTer.ComandoEnviarParaSerial(aDados, aSerial));
end;

procedure TACBrMTer.EnviarTexto(const aIP, aTexto: AnsiString);
begin
  // Envia String para o Display
  EnviarComando(aIP, fMTer.ComandoEnviarTexto(aTexto));
end;

procedure TACBrMTer.LimparDisplay(const aIP: AnsiString);
begin
  // Limpa Display e posiciona cursor em 0,0
  EnviarComando(aIP, fMTer.ComandoLimparDisplay);
end;

procedure TACBrMTer.LimparLinha(const aIP: AnsiString; aLinha: Integer);
begin
  // Apaga Linha, mantendo cursor na posição atual
  EnviarComando(aIP, fMTer.ComandoLimparLinha(aLinha));
end;

procedure TACBrMTer.PosicionarCursor(const aIP: AnsiString; aLinha, aColuna: Integer);
begin
  // Posiciona cursor na posição informada
  EnviarComando(aIP, fMTer.ComandoPosicionarCursor(aLinha, aColuna));
end;

procedure TACBrMTer.SolicitarPeso(const aIP: AnsiString; aSerial: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  wConexao := fConexoes.Conexao[aIP];

  if (not Assigned(fBalanca)) then
    Exit;

  if Assigned(wConexao) then
  begin
    wConexao.Balanca.Modelo := fBalanca.Modelo;
    wConexao.SolicitarPeso(aSerial);
  end;
end;

{function TACBrMTer.LePeso(aIP: AnsiString; aSerial: Integer): Double;
var
  wConexao: TACBrMTerConexao;
begin
  wConexao := fConexoes.Conexao[aIP];

  if (not Assigned(fBalanca)) then
    Exit;

  if Assigned(wConexao) then
  begin
    wConexao.Balanca.Modelo := fBalanca.Modelo;
    Result := wConexao.LePeso(aSerial);
  end;
end;}

function TACBrMTer.Online(const aIP: AnsiString): Boolean;
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


