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
  Classes, SysUtils, contnrs, Controls, ExtCtrls,
  ACBrBase, ACBrSocket, ACBrMTerClass, ACBrBAL, blcksock;

type

  TACBrMTerModelo = (mtrNenhum, mtrVT100, mtrStxEtx, mtrPMTG, mtrSB100);
  TACBrMTerEchoMode = (mdeNormal, mdeNone, mdePassword);

  { Evento disparado quando Conecta }
  TACBrMTerConecta = procedure(const IP: String) of object;

  { Evento disparado quando Desconecta }
  TACBrMTerDesconecta = procedure(const IP: String; Erro: Integer;
    ErroDesc: String) of object;

  { Evento disparado quando Recebe Dados }
  TACBrMTerRecebeDados = procedure(const IP: String; const
    Recebido: AnsiString; var EchoMode: TACBrMTerEchoMode) of object;

  { Evento disparado quando ACBrMTer Recebe Peso }
  TACBrMTerRecebePeso = procedure(const IP: String; const PesoRecebido: Double) of object;

  { Evento disparado após a Chamada de ACBrMTer.Online(aIP)  }
  TACBrMTerRecebeOnLine = procedure(const IP: String; const Conectado: Boolean;
    const RepostaOnLine: AnsiString) of object;

  TACBrMTer = class;
  TACBrMTerConexao = class;
  TACBrMTerConexoes = class;

  { TACBrMTerComandoEnviado }

  TACBrMTerComandoEnviado = class
  private
    FComando: AnsiString;
    FEnviadoEm: TDateTime;
    FRespostaTratada: AnsiString;
    FTimeOut: Integer;
    FTag: Integer;
    FPesoLido: Double;
    FResposta: AnsiString;
    FRespostaEm: TDateTime;
  public
    constructor Create;
    procedure Clear;

    property Comando: AnsiString read FComando write FComando;
    property EnviadoEm: TDateTime read FEnviadoEm write FEnviadoEm;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property Resposta: AnsiString read FResposta write FResposta;
    property RespostaEm: TDateTime read FRespostaEm write FRespostaEm;
    property RespostaTratada: AnsiString read FRespostaTratada write FRespostaTratada;
    property Tag: Integer read FTag write FTag;
    property PesoLido: Double read FPesoLido write FPesoLido;
  end;

  { TACBrMTerConexao }

  TACBrMTerConexao = class
  private
    fComandos: TACBrMTerComandos;
    fUltimoComando: TACBrMTerComandoEnviado;
    fBuffer: AnsiString;
    fTimerFilaComandos: TACBrThreadTimer;
    fTimerTimeOut: TACBrThreadTimer;
    fTimerWaitBuffer: TACBrThreadTimer;
    fACBrBAL: TACBrBAL;
    fConectado: Boolean;
    fIP: String;
    fSerialBalanca: Integer;
    fConexoes: TACBrMTerConexoes;
    function GetACBrBALConexao: TACBrBAL;

    procedure DoHookEnviaStringSerial(const aCmd: AnsiString);
    function GetEsperandoResposta: Boolean;
    procedure OnTimeOutResposta(Sender: TObject);
    procedure OnProcessarComandoDaFila(Sender: TObject);
    procedure OnBufferWaitDone(Sender: TObject);

    procedure LigarEsperaDeResposta;
  protected
    property BAL: TACBrBAL read GetACBrBALConexao;

  public
    constructor Create(aOwner: TACBrMTerConexoes; const AIP: String);
    destructor Destroy; override;
    procedure Clear;

    procedure LigarFilaDeComandos;
    procedure AdicionarBufferResposta(ABuffer: AnsiString);
    procedure SolicitarPeso(aSerial: Integer);
    procedure VerificarOnLine;

    property Conectado: Boolean read fConectado write fConectado;
    property IP: String read fIP;
    property Comandos: TACBrMTerComandos read fComandos;
    property EsperandoResposta: Boolean read GetEsperandoResposta;

    property Buffer: AnsiString read fBuffer;
    property UltimoComando: TACBrMTerComandoEnviado read fUltimoComando;
  end;

  { TACBrMTerConexoes }

  TACBrMTerConexoes = class(TObjectList)
  private
    fACBrMTer: TACBrMTer;
    function GetConexao(aIP: String): TACBrMTerConexao;
    function GetObject(aIndex: Integer): TACBrMTerConexao;
    procedure SetObject(aIndex: Integer; aItem: TACBrMTerConexao);
  public
    constructor Create(aOwner: TACBrMTer);

    function New(const AIP: String): TACBrMTerConexao;
    procedure Remove(const AIP: String);

    function Add(aObj: TACBrMTerConexao): Integer;
    procedure Insert(aIndex: Integer; aObj: TACBrMTerConexao);

    property ACBrMTer: TACBrMTer read fACBrMTer;
    property Conexao[aIP: String]: TACBrMTerConexao read GetConexao;
    property Objects[aIndex: Integer]: TACBrMTerConexao read GetObject write SetObject; default;
  end;

  { TACBrMTer }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrMTer = class(TACBrComponent)
  private
    fArqLog: String;
    fACBrBAL: TACBrBAL;
    fCmdEnviado: AnsiString;
    fConexoes: TACBrMTerConexoes;
    fDisplayColunas: Integer;
    fDisplayLinhas: Integer;
    fEchoMode: TACBrMTerEchoMode;
    fMTer: TACBrMTerClass;
    fModelo: TACBrMTerModelo;
    fOnConecta: TACBrMTerConecta;
    fOnDesconecta: TACBrMTerDesconecta;
    fOnGravarLog: TACBrGravarLog;
    fOnRecebeDados: TACBrMTerRecebeDados;
    fOnRecebePeso: TACBrMTerRecebePeso;
    fOnRecebeOnLine: TACBrMTerRecebeOnLine;
    fPassWordChar: Char;
    fTCPServer: TACBrTCPServer;
    fTerminador: AnsiString;
    fTerminadorAsc: AnsiString;
    fTerminadorBalanca: AnsiString;
    fTerminadorBalancaAsc: AnsiString;
    fWaitInterval: Integer;
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
    procedure SetWaitInterval(AValue: Integer);

    procedure MTErOnConecta(const TCPBlockSocket: TTCPBlockSocket;
      var Enviar: AnsiString);
    procedure MTErOnDesconecta(const TCPBlockSocket: TTCPBlockSocket;
      Erro: Integer; ErroDesc: String);
    procedure MTErOnRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
      const Recebido: AnsiString; var Enviar: AnsiString);

    function BuscarSocketPorIP(const aIP: String): TTCPBlockSocket;
    function EncontrarSocket(const aIP: String = ''): TTCPBlockSocket;
    function EncontrarConexao(const aIP: String = ''): TACBrMTerConexao;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoRecebePeso(const aIP: String; const PesoRecebido: Double);
    procedure DoRecebeOnLine(const aIP: String; const Conectado: Boolean;
      const RespostaOnLine: AnsiString);
    procedure DoRecebeDados(const aIP: String; const DadosRecebidos: AnsiString);
    procedure DoEnviarComando(const aIP: String; const ACmd: AnsiString);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Ativar;
    procedure Desativar;
    procedure VerificarAtivo;

    procedure EnviarComando(const aIP: String; const ACmd: AnsiString);

    procedure GravaLog(aString: AnsiString; Traduz: Boolean = False);

    procedure BackSpace(const aIP: String);
    procedure Beep(const aIP: String; const aTempo: Integer = 0);
    procedure DeslocarCursor(const aIP: String; aValue: Integer);
    procedure DeslocarLinha(const aIP: String; aValue: Integer);
    procedure EnviarParaParalela(const aIp: String; aDados: AnsiString);
    procedure EnviarParaSerial(const aIP: String; aDados: AnsiString; aSerial: Integer);
    procedure EnviarTexto(const aIP: String; aTexto: AnsiString);
    procedure LimparDisplay(const aIP: String);
    procedure LimparLinha(const aIP: String; aLinha: Integer);
    procedure PosicionarCursor(const aIP: String; aLinha, aColuna: Integer);
    procedure SolicitarPeso(const aIP: String; aSerial: Integer);
    procedure VerificarOnline(const aIP: String);

    property Ativo     : Boolean           read GetAtivo    write SetAtivo;
    property CmdEnviado: AnsiString        read fCmdEnviado;
    property Conexoes  : TACBrMTerConexoes read fConexoes;
    property MTer      : TACBrMTerClass    read fMTer;
    property ModeloStr : String            read GetModeloStr;
    property TCPServer : TACBrTCPServer    read fTCPServer;

    property TerminadorAsc        : AnsiString  read fTerminadorAsc;
    property TerminadorBalancaAsc : AnsiString  read fTerminadorBalancaAsc;
  published
    property ArqLog        : String            read fArqLog         write fArqLog;
    property Balanca       : TACBrBAL          read fACBrBAL        write SetBalanca;
    property EchoMode      : TACBrMTerEchoMode read fEchoMode       write SetEchoMode;
    property IP            : String            read GetIP           write SetIP;
    property PasswordChar  : Char              read fPasswordChar   write SetPasswordChar;
    property Port          : String            read GetPort         write SetPort;
    property Terminador       : AnsiString     read fTerminador        write SetTerminador;
    property TerminadorBalanca: AnsiString     read fTerminadorBalanca write SetTerminadorBalanca;
    property TimeOut       : Integer           read GetTimeOut      write SetTimeOut default 1000;
    property WaitInterval  : Integer           read fWaitInterval   write SetWaitInterval default 200;
    property Modelo        : TACBrMTerModelo   read fModelo         write SetModelo default mtrNenhum;
    property DisplayLinhas : Integer           read fDisplayLinhas  write fDisplayLinhas default 4;
    property DisplayColunas: Integer           read fDisplayColunas write fDisplayColunas default 20;

    property OnConecta     : TACBrMTerConecta      read fOnConecta      write fOnConecta;
    property OnDesconecta  : TACBrMTerDesconecta   read fOnDesconecta   write fOnDesconecta;
    property OnRecebeDados : TACBrMTerRecebeDados  read fOnRecebeDados  write fOnRecebeDados;
    property OnRecebePeso  : TACBrMTerRecebePeso   read fOnRecebePeso   write fOnRecebePeso;
    property OnRecebeOnLine: TACBrMTerRecebeOnLine read fOnRecebeOnLine write fOnRecebeOnLine;
    property OnGravarLog   : TACBrGravarLog        read fOnGravarLog    write fOnGravarLog;
  end;


implementation

uses
  strutils, dateutils, math,
  ACBrMTerVT100, ACBrMTerPMTG, ACBrMTerStxEtx, ACBrMTerSB100,
  ACBrConsts, ACBrUtil;

{ TACBrMTerComandoEnviado }

constructor TACBrMTerComandoEnviado.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrMTerComandoEnviado.Clear;
begin
  FComando := '';
  FEnviadoEm := 0;
  FTimeOut := 0;
  FTag := 0;
  FPesoLido := 0;
  FResposta := '';
  FRespostaEm := 0;
  FRespostaTratada := '';
end;

{ TACBrMTerConexao }

constructor TACBrMTerConexao.Create(aOwner: TACBrMTerConexoes; const AIP: String
  );
begin
  fConexoes := aOwner;
  fIP := AIP;

  fComandos := TACBrMTerComandos.Create(True);
  fUltimoComando := TACBrMTerComandoEnviado.Create;
  fConectado := True;
  fACBrBAL := Nil;

  fTimerTimeOut := TACBrThreadTimer.Create;
  fTimerTimeOut.Enabled := False;
  fTimerTimeOut.Interval := fConexoes.ACBrMTer.TimeOut;
  fTimerTimeOut.OnTimer := OnTimeOutResposta;

  fTimerFilaComandos := TACBrThreadTimer.Create;
  fTimerFilaComandos.Enabled := False;
  fTimerFilaComandos.Interval := 10;
  fTimerFilaComandos.OnTimer := OnProcessarComandoDaFila;

  fTimerWaitBuffer := TACBrThreadTimer.Create;
  fTimerWaitBuffer.Enabled := False;
  fTimerWaitBuffer.OnTimer := OnBufferWaitDone;

  Clear;
end;

destructor TACBrMTerConexao.Destroy;
begin
  fConectado := False;
  fTimerTimeOut.Enabled := False;
  fTimerWaitBuffer.Enabled := False;
  fTimerFilaComandos.Enabled := False;

  fComandos.Free;
  fUltimoComando.Free;
  fTimerTimeOut.Free;
  fTimerFilaComandos.Free;
  fTimerWaitBuffer.Free;

  if Assigned(fACBrBAL) then
    fACBrBAL.Free;

  inherited Destroy;
end;

procedure TACBrMTerConexao.Clear;
begin
  fUltimoComando.Clear;
  fSerialBalanca := 0;
  fComandos.Clear;
end;

function TACBrMTerConexao.GetACBrBALConexao: TACBrBAL;
begin
  if (fACBrBAL = Nil) then
  begin
    fACBrBAL := TACBrBAL.Create(Nil);

    fACBrBAL.Modelo := fConexoes.ACBrMTer.Balanca.Modelo;
    fACBrBAL.Device.Porta := 'USB';
    fACBrBAL.Device.HookEnviaString := DoHookEnviaStringSerial;
  end;

  Result := fACBrBAL;
end;

procedure TACBrMTerConexao.SolicitarPeso(aSerial: Integer);
begin
  fSerialBalanca := aSerial;
  BAL.Modelo := fConexoes.ACBrMTer.Balanca.Modelo;
  BAL.SolicitarPeso;
  LigarFilaDeComandos;
end;

procedure TACBrMTerConexao.DoHookEnviaStringSerial(const aCmd: AnsiString);
var
  LenFila: Integer;
begin
  LenFila := fComandos.Count;
  fConexoes.ACBrMTer.MTer.ComandoEnviarParaSerial(fComandos, aCmd, fSerialBalanca);

  if (fComandos.Count > LenFila) then   // Presume que o último comando, é o de Leitura de Peso
  begin
    with fComandos[fComandos.Count-1] do
    begin
      Tag := 1;  // Inserindo Flag, para sinalizar com Leitura de Peso
      TimeOut := fConexoes.ACBrMTer.TimeOut;
    end;
  end;
end;

procedure TACBrMTerConexao.VerificarOnLine;
var
  LenFila: Integer;
begin
  LenFila := fComandos.Count;
  fConectado := False;
  fConexoes.ACBrMTer.MTer.ComandoOnline(fComandos);

  if (fComandos.Count > LenFila) then
  begin
    with fComandos[fComandos.Count-1] do
    begin
      Tag := 2;    // Inserindo Flag, para sinalizar com Leitura de OnLine
      TimeOut := fConexoes.ACBrMTer.TimeOut;
    end;
  end
  else
  begin
    fConectado := True;
    with fConexoes.ACBrMTer do
      DoRecebeOnLine(fIP, True, ACBrStr('Modelo: '+ModeloStr+' não suporta verificação Status OnLine'));

    Exit;
  end;

  LigarFilaDeComandos;
end;

procedure TACBrMTerConexao.LigarFilaDeComandos;
begin
  fTimerFilaComandos.Enabled := (not fTimerTimeOut.Enabled) and
                                (fComandos.Count > 0);
end;

procedure TACBrMTerConexao.OnProcessarComandoDaFila(Sender: TObject);
var
  ACmd: TACBrMTerComando;
begin
  fTimerFilaComandos.Enabled := False;

  if (fComandos.Count < 1) then
    Exit;

  repeat
    ACmd := fComandos[0];     // Lê comando da Fila

    with fUltimoComando do
    begin
      Clear;
      Comando := ACmd.Comando;
      TimeOut := ACmd.TimeOut;
      EnviadoEm := Now;
      Tag := ACmd.Tag;
    end;

    fComandos.Delete(0);  // Remove comando da Fila

    fConexoes.ACBrMTer.DoEnviarComando(IP, fUltimoComando.Comando);
    LigarEsperaDeResposta;
  until (fComandos.Count < 1) or (not fConectado) or EsperandoResposta;
end;

procedure TACBrMTerConexao.AdicionarBufferResposta(ABuffer: AnsiString);
var
  LigarTimer: Boolean;
begin
  LigarTimer := (fBuffer = '');
  fBuffer := fBuffer + ABuffer;

  if LigarTimer then;
  begin
    if (fConexoes.ACBrMTer.WaitInterval > 0) then
    begin
      fTimerWaitBuffer.Interval := fConexoes.ACBrMTer.WaitInterval;
      fTimerWaitBuffer.Enabled := True;
    end
    else
      OnBufferWaitDone(Nil);
  end;
end;

procedure TACBrMTerConexao.OnBufferWaitDone(Sender: TObject);
var
  AResposta, TermBal: AnsiString;
  P, LenResp: Integer;
  TempoFinal: TDateTime;

  procedure ExtrairResposta;
  begin
    // Se extrair uma Resposta de Buffer, remove a String dele, até o mesmo ficar vazio
    AResposta := fConexoes.ACBrMTer.MTer.ExtrairResposta(fBuffer);
    LenResp := Length(AResposta);
  end;

begin
  fTimerWaitBuffer.Enabled := False;

  ExtrairResposta;
  while (LenResp > 0) do
  begin
    with fUltimoComando do
    begin
      Resposta := AResposta;
      RespostaEm := Now;
      RespostaTratada := fConexoes.ACBrMTer.MTer.InterpretarResposta(AResposta);

      if (RespostaTratada <> EmptyStr) then
      begin
        fTimerTimeout.Enabled := False;

        if (Tag = 0) then
        begin
          TermBal := fConexoes.ACBrMTer.TerminadorBalancaAsc;
          if (Length(TermBal) > 0) then
            if ( RightStr(RespostaTratada, Length(TermBal)) = TermBal ) then
              Tag := 1;
        end;

        if (Tag = 1) then  // Lendo Peso
        begin
          Tag := 0;
          PesoLido := BAL.InterpretarRepostaPeso(Trim(RespostaTratada));
          fConexoes.ACBrMTer.DoRecebePeso(IP, PesoLido);
        end

        else if (Tag = 2) then   // Verificando Conexão
        begin
          Tag := 0;
          fConectado := True;
          fConexoes.ACBrMTer.DoRecebeOnLine(IP, fConectado, Trim(RespostaTratada));
        end

        else
          fConexoes.ACBrMTer.DoRecebeDados(IP, RespostaTratada);
      end
      else
        fTimerTimeout.Enabled := (Tag > 0);

      if (TimeOut < 0) then  // Timeout em modo Pausa, aguardando o termino
      begin
        TempoFinal := IncMilliSecond(EnviadoEm, abs(TimeOut));
        if TempoFinal > Now then
          Sleep( MilliSecondsBetween(Now, TempoFinal) );
      end;
    end;

    ExtrairResposta;
  end;

  LigarFilaDeComandos;
end;


procedure TACBrMTerConexao.LigarEsperaDeResposta;
begin
  fTimerTimeOut.Enabled := (fUltimoComando.TimeOut <> 0);
  if fTimerTimeOut.Enabled then
    fTimerTimeOut.Interval := abs(fUltimoComando.TimeOut);
end;

function TACBrMTerConexao.GetEsperandoResposta: Boolean;
begin
  Result := fTimerTimeOut.Enabled
end;

procedure TACBrMTerConexao.OnTimeOutResposta(Sender: TObject);
begin
  fTimerTimeout.Enabled := False;

  with fUltimoComando do
  begin
    Resposta := fBuffer;
    RespostaEm := Now;

    if (Tag = 1) then    // Lendo Peso
    begin
      if (Length(fBuffer) < 1) then
        fUltimoComando.PesoLido := -9      // -9 = TimeOut em ACBrBAL
      else
        fUltimoComando.PesoLido := BAL.InterpretarRepostaPeso(Trim(fBuffer));

      fConexoes.ACBrMTer.DoRecebePeso(fIP, fUltimoComando.PesoLido);
    end

    else if (Tag = 2) then   // Verificando Conexão
    begin
      fConectado := False;
      fConexoes.ACBrMTer.DoRecebeOnLine(fIP, fConectado, Trim(fBuffer));
    end;
  end;

  fBuffer := '';
  LigarFilaDeComandos;
end;

{ TACBrMTerConexoes }

constructor TACBrMTerConexoes.Create(aOwner: TACBrMTer);
begin
  inherited Create(True);

  fACBrMTer := aOwner;
end;

function TACBrMTerConexoes.New(const AIP: String): TACBrMTerConexao;
begin
  if GetConexao(AIP) <> Nil then
    raise Exception.Create(ACBrStr('Conexão com: '+AIP+' já existe'));

  Result := TACBrMTerConexao.Create(Self, AIP);
  Add( Result );
end;

procedure TACBrMTerConexoes.Remove(const AIP: String);
var
  AConexao: TACBrMTerConexao;
begin
  AConexao := GetConexao(AIP);
  if Assigned(AConexao) then
    inherited Remove(AConexao);
end;

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
      Break;
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

function TACBrMTerConexoes.Add(aObj: TACBrMTerConexao): Integer;
begin
  Result := inherited Add(aObj);
end;

procedure TACBrMTerConexoes.Insert(aIndex: Integer; aObj: TACBrMTerConexao);
begin
  inherited Insert(aIndex, aObj);
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

procedure TACBrMTer.SetWaitInterval(AValue: Integer);
begin
  if AValue = fWaitInterval then
    Exit;

  fWaitInterval := min(max(AValue,0),5000);
end;

procedure TACBrMTer.MTErOnConecta(const TCPBlockSocket: TTCPBlockSocket;
  var Enviar: AnsiString);
var
  wIP: String;
  wConexao: TACBrMTerConexao;
begin
  wIP := TCPBlockSocket.GetRemoteSinIP;
  wConexao := fConexoes.Conexao[wIP];
  if not Assigned(wConexao) then
    wConexao := fConexoes.New(wIP);

  GravaLog('Terminal: ' + wIP + ' - Conectou');

  fMTer.ComandoBoasVindas( wConexao.Comandos ) ;

  if Assigned(fOnConecta) then
    OnConecta(wIP);

  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.MTErOnDesconecta(const TCPBlockSocket: TTCPBlockSocket;
  Erro: Integer; ErroDesc: String);
var
  wIP, ErroMsg: String;
begin
  ErroMsg := IntToStr(Erro)+'-'+ErroDesc;

  if Assigned(TCPBlockSocket) then
  begin
    wIP := TCPBlockSocket.GetRemoteSinIP;
    GravaLog('Terminal: ' + wIP + ' - Desconectou - '+ErroMsg);
    fConexoes.Remove(wIP);
  end
  else
  begin
    wIP := '';
    GravaLog(ErroMsg);
  end;

  if Assigned(fOnDesconecta) then
    OnDesconecta(wIP, Erro, ErroDesc);
end;

procedure TACBrMTer.MTErOnRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
  const Recebido: AnsiString; var Enviar: AnsiString);
var
  wIP: String;
  wConexao: TACBrMTerConexao;
begin
  wIP := TCPBlockSocket.GetRemoteSinIP;
  wConexao := fConexoes.Conexao[wIP];
  if (Length(Recebido) < 1) or (not Assigned(wConexao)) then
    Exit;

  GravaLog( 'Terminal: ' + wIP + ' - RX <- ' +IntToStr(Length(Recebido)) +' bytes -> '+ Recebido, True);

  wConexao.AdicionarBufferResposta(Recebido);
  Enviar := '';
end;

procedure TACBrMTer.DoRecebePeso(const aIP: String; const PesoRecebido: Double);
begin
  GravaLog('Terminal: ' + aIP + ' - RecebePeso: ' + FormatFloatBr(PesoRecebido, FloatMask(4))+
           IfThen(Assigned(fACBrBAL), ', Balança: ' + fACBrBAL.ModeloStr, '') );
  if Assigned(fOnRecebePeso) then
    fOnRecebePeso(aIP, PesoRecebido);
end;

procedure TACBrMTer.DoRecebeOnLine(const aIP: String; const Conectado: Boolean;
  const RespostaOnLine: AnsiString);
begin
  GravaLog('Terminal: ' + aIP + ' - RecebeOnLine: ' + IfThen(Conectado, 'SIM', 'NÃO')+ ' - ' + RespostaOnLine, True);
  if Assigned(fOnRecebeOnLine) then
    fOnRecebeOnLine(aIP, Conectado, RespostaOnLine);
end;

procedure TACBrMTer.DoRecebeDados(const aIP: String;
  const DadosRecebidos: AnsiString);
var
  wEchoMode: TACBrMTerEchoMode;
  wConexao: TACBrMTerConexao;
begin
  if (Length(DadosRecebidos) < 1) then
    Exit;

  wConexao := fConexoes.Conexao[aIP];
  if not Assigned(wConexao) then
    Exit;

  GravaLog( 'Terminal: ' + aIP + ' - RecebeResposta: ' +IntToStr(Length(DadosRecebidos)) +
              ' bytes -> '+ DadosRecebidos, True);

  wEchoMode := EchoMode;
  if Assigned(fOnRecebeDados) then
    OnRecebeDados(aIP, DadosRecebidos, wEchoMode);

  case wEchoMode of
    mdeNormal  :
      fMTer.ComandoEco(wConexao.Comandos, DadosRecebidos);
    mdePassword:
      fMTer.ComandoEco(wConexao.Comandos, StringOfChar(PasswordChar, Length(DadosRecebidos)));
  end;
end;

procedure TACBrMTer.EnviarComando(const aIP: String; const ACmd: AnsiString);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('EnviarComando( ' + aIP + ', "' +ACmd+'" )', True);

  if (not Ativo) then
    raise Exception.Create(ACBrStr('Componente ACBrMTer não está ATIVO'));

  if (Length(ACmd) < 1) then
    Exit;

  wConexao := EncontrarConexao(aIP);
  wConexao.Comandos.New(ACmd);
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.DoEnviarComando(const aIP: String; const ACmd: AnsiString);
var
  ASocket: TTCPBlockSocket;
begin
  fCmdEnviado := ACmd;
  ASocket := EncontrarSocket(aIP);
  if (ASocket <> Nil) then
  begin
    GravaLog('Terminal: ' + aIP + ' - TX -> ' +IntToStr(Length(ACmd)) +' bytes -> '+ACmd, True);
    ASocket.SendString(ACmd);
  end
  else
    GravaLog('Terminal: ' + aIP + ' - EnviarComando: Conexão não encontrada');
end;


function TACBrMTer.BuscarSocketPorIP(const aIP: String): TTCPBlockSocket;
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

function TACBrMTer.EncontrarSocket(const aIP: String): TTCPBlockSocket;
var
  wIP: String;
begin
  Result := Nil;
  wIP := Trim(aIP);
  if (wIP = EmptyStr) then
    Exit;

  Result := BuscarSocketPorIP(wIP);
  if not Assigned(Result) then
    raise Exception.Create(ACBrStr('Socket ['+ QuotedStr(wIP) +'] não encontrado'));
end;

function TACBrMTer.EncontrarConexao(const aIP: String): TACBrMTerConexao;
var
  wIP: String;
begin
  Result := Nil;
  wIP := Trim(aIP);
  if (wIP = EmptyStr) then
    Exit;

  Result := fConexoes.Conexao[wIP];
  if not Assigned(Result) then
    raise Exception.Create(ACBrStr('Conexão ['+ QuotedStr(wIP) +'] não encontrado'));
end;

procedure TACBrMTer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation <> opRemove) then
    Exit;

  if (AComponent is TACBrBAL) and (fACBrBAL <> Nil) then
    fACBrBAL := Nil;
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
  if (fACBrBAL = AValue) then
    Exit;

  if Assigned(fACBrBAL) then
    fACBrBAL.RemoveFreeNotification(Self);

  fACBrBAL := AValue;

  if (fACBrBAL <> Nil) then
  begin
    fACBrBAL.FreeNotification(Self);

    // Utilizar sempre porta USB/DLL para Micro Terminal
    fACBrBAL.Porta := 'USB';
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
  fTerminador := '';
  fTerminadorAsc := '';
  fTerminadorBalanca := '#3';
  fTerminadorBalancaAsc := #3;
  fWaitInterval := 200;

  fConexoes := TACBrMTerConexoes.Create(Self);

  { Instanciando TACBrTCPServer }
  fTCPServer := TACBrTCPServer.Create(Self);
  fTCPServer.OnConecta     := MTErOnConecta;
  fTCPServer.OnDesConecta  := MTErOnDesconecta;
  fTCPServer.OnRecebeDados := MTErOnRecebeDados;
  fTCPServer.TimeOut := 1000;

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
           ' - Balança: ' + IfThen(Assigned(fACBrBAL), fACBrBAL.ModeloStr, 'Nenhuma')+
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
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('BackSpace( ' + aIP + ' )');
  // Envia BS para o Display
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoBackSpace( wConexao.Comandos );
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.Beep(const aIP: String; const aTempo: Integer);
var
  wConexao: TACBrMTerConexao;
  wTempo: Integer;
begin
  if (aTempo = 0) then
    wTempo := 500   // 0.5 segundo
  else
    wTempo := aTempo;

  GravaLog('Beep( ' + aIP + ', ' +IntToStr(wTempo)+' )');

  // Envia BS para o Display
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoBeep( wConexao.Comandos, wTempo );
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.DeslocarCursor(const aIP: String; aValue: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('DeslocarCursor( ' + aIP + ', ' +IntToStr(aValue)+' )');

  // Desloca Cursor a partir da posição atual (Permite valores negativos)
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoDeslocarCursor( wConexao.Comandos, aValue );
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.DeslocarLinha(const aIP: String; aValue: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('DeslocarLinha( ' + aIP + ', ' +IntToStr(aValue)+' )');

  // Desloca Linha a partir da posição atual(Valores: 1 ou -1)
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoDeslocarLinha( wConexao.Comandos, aValue );
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.EnviarParaParalela(const aIp: String; aDados: AnsiString);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('EnviarParaParalela( ' + aIP + ', "' +aDados+'" )', True);

  // Envia String para Porta Paralela
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoEnviarParaParalela( wConexao.Comandos, aDados );
  wConexao.LigarFilaDeComandos
end;

procedure TACBrMTer.EnviarParaSerial(const aIP: String; aDados: AnsiString;
  aSerial: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('EnviarParaSerial( ' + aIP + ', "' +aDados+'" )', True);

  // Envia String para Porta Serial
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoEnviarParaSerial( wConexao.Comandos, aDados, aSerial );
  wConexao.LigarFilaDeComandos
end;

procedure TACBrMTer.EnviarTexto(const aIP: String; aTexto: AnsiString);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('EnviarTexto( ' + aIP + ', "' +aTexto+'" )', True);

  // Envia String para o Display
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoEnviarTexto(wConexao.Comandos, aTexto);
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.LimparDisplay(const aIP: String);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('LimparDisplay( ' + aIP + ' )');

  // Limpa Display e posiciona cursor em 0,0
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoLimparDisplay(wConexao.Comandos);
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.LimparLinha(const aIP: String; aLinha: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('LimparLinha( ' + aIP + ', '+IntToStr(aLinha)+' )');

  // Apaga Linha, mantendo cursor na posição atual
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoLimparLinha(wConexao.Comandos, aLinha);
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.PosicionarCursor(const aIP: String; aLinha, aColuna: Integer
  );
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('PosicionarCursor( ' + aIP + ', '+IntToStr(aLinha)+', '+IntToStr(aColuna)+' )');

  // Posiciona cursor na posição informada
  wConexao := EncontrarConexao(aIP);
  fMTer.ComandoPosicionarCursor(wConexao.Comandos, aLinha, aColuna);
  wConexao.LigarFilaDeComandos;
end;

procedure TACBrMTer.SolicitarPeso(const aIP: String; aSerial: Integer);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('SolicitarPeso( ' + aIP + ', '+IntToStr(aSerial)+' )');

  if not Assigned(fACBrBAL) then
    raise Exception.Create(ACBrStr('Componente ACBrMTer, não foi associado a um Componente ACBrBAL'));

  if (aSerial < 1) then
    raise Exception.Create(ACBrStr('Porta Serial ['+IntToStr(aSerial)+'] inválida.'));

  wConexao := EncontrarConexao(aIP);
  // NOTA: Comando SolicitarPeso está dentro da Conexão porque precisa salvar em variável externa o número da Serial;
  wConexao.SolicitarPeso(aSerial);
end;

procedure TACBrMTer.VerificarOnline(const aIP: String);
var
  wConexao: TACBrMTerConexao;
begin
  GravaLog('VerificarOnline( ' + aIP + ' )');

  wConexao := fConexoes.Conexao[aIP];
  if not Assigned(wConexao) then
  begin
    DoRecebeOnLine(aIP, False, '');
    Exit;
  end;

  wConexao.UltimoComando.Clear;
  wConexao.VerificarOnLine;
end;

end.
