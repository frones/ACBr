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

unit ACBrDeviceSerial;

interface

uses
  Classes, SysUtils,
  synaser,
  ACBrDeviceClass, ACBrBase;

const
  CFailCount = 10;

type
  TACBrSerialParity = (pNone, pOdd, pEven, pMark, pSpace);
  TACBrSerialStop = (s1, s1eMeio, s2);
  TACBrHandShake = (hsNenhum, hsXON_XOFF, hsRTS_CTS, hsDTR_DSR);

  { TACBrDeviceSerial }

  TACBrDeviceSerial = class(TACBrDeviceClass)
  private
    fsSerial: TBlockSerial;
    fsHardFlow: Boolean;
    fsSoftFlow: Boolean;
    fsHandShake: TACBrHandShake;
    fsParity: char;
    fsData: Integer;
    fsBaud: Integer;
    fsStop: Integer;
    function GetOnStatus: THookSerialStatus;
    function GetParamsString: String;
    function GetParity: TACBrSerialParity;
    function GetStop: TACBrSerialStop;
    procedure SetBaud(AValue: Integer);
    procedure SetData(AValue: Integer);
    procedure SetHandShake(AValue: TACBrHandShake);
    procedure SetHardFlow(AValue: Boolean);
    procedure SetOnStatus(AValue: THookSerialStatus);
    procedure SetParamsString(const Value: String);
    procedure SetParity(AValue: TACBrSerialParity);
    procedure SetSoftFlow(AValue: Boolean);
    procedure SetStop(AValue: TACBrSerialStop);
  protected
    function GetMaxSendBandwidth: Integer; override;
    function GetTimeOutMilissegundos: Integer; override;
    procedure SetMaxSendBandwidth(AValue: Integer); override;
    procedure SetTimeOutMilissegundos(AValue: Integer); override;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure Desconectar(IgnorarErros: Boolean = True); override;

    function EmLinha(const ATimeOutMilissegundos: Integer): Boolean; override;
    function BytesParaLer: Integer; override;

    procedure ConfigurarSerial;
    procedure ValoresPadroes;
    function ParametrosSerial(ApenasExcecoes: Boolean = True): String;
    procedure AcharPortasSeriais(const AStringList: TStrings; UltimaPorta: Integer);

    procedure EnviaString(const AString: AnsiString); override;
    procedure EnviaByte(const AByte: byte); override;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; override;
    function LeByte(ATimeOutMilissegundos: Integer = 0): byte; override;
    procedure Limpar; override;

    property Baud: Integer read fsBaud write SetBaud default 9600;
    property Data: Integer read fsData write SetData default 8;
    property Parity: TACBrSerialParity read GetParity write SetParity default pNone;
    property Stop: TACBrSerialStop read GetStop write SetStop default s1;
    property HandShake: TACBrHandShake read fsHandShake write SetHandShake default hsNenhum;
    property SoftFlow: Boolean read fsSoftFlow write SetSoftFlow default False;
    property HardFlow: Boolean read fsHardFlow write SetHardFlow default False;

    property OnStatus: THookSerialStatus read GetOnStatus write SetOnStatus;

    property Serial: TBlockSerial read fsSerial;
    property ParamsString: String read GetParamsString write SetParamsString;
  end;

implementation

uses
  typinfo, dateutils, math,
  ACBrDevice, ACBrUtil, ACBrConsts;

{ TACBrDeviceSerial }

constructor TACBrDeviceSerial.Create(AOwner: TComponent);
begin
  inherited;
  fsSerial := TBlockSerial.Create;
  fsSerial.RaiseExcept := True;
end;

destructor TACBrDeviceSerial.Destroy;
begin
  fsSerial.Free;
  inherited Destroy;
end;

procedure TACBrDeviceSerial.Conectar(const APorta: String; const ATimeOutMilissegundos: Integer);
var
  ErrorMsg: String;
begin
  inherited;
  try
    Desconectar; { Fecha se ficou algo aberto }
    fsSerial.DeadlockTimeout := ATimeOutMilissegundos;
    fsSerial.Connect(APorta);
    ConfigurarSerial;
    Limpar;  { Limpa Buffer de envio e recepção }
  except
    on E: ESynaSerError do
    begin
      if (fsSerial.LastError = 2) then
        ErrorMsg := Format(ACBrStr(cACBrDeviceAtivarPortaNaoEncontrada), [APorta])
      else
        ErrorMsg := E.Message;

      Desconectar;
      DoException(Exception.Create(ErrorMsg));
    end;

    On E: Exception do
      DoException(Exception.Create(E.ClassName + ': ' + E.Message));
  end;
end;

procedure TACBrDeviceSerial.Desconectar(IgnorarErros: Boolean);
begin
  inherited;
  try
    fsSerial.RaiseExcept := not IgnorarErros;
    if fsSerial.InstanceActive then
      fsSerial.CloseSocket;
  finally
    fsSerial.RaiseExcept := True;
  end;
end;

function TACBrDeviceSerial.EmLinha(const ATimeOutMilissegundos: Integer): Boolean;
var
  TempoLimite: TDateTime;
begin
  Result := False;
  if fsSerial.InstanceActive then
  begin
    TempoLimite := IncMilliSecond(Now, ATimeOutMilissegundos);
    while (not Result) and (Now < TempoLimite) do
    begin
      case HandShake of
        hsRTS_CTS:
          Result := fsSerial.CTS;
        hsDTR_DSR:
          Result := fsSerial.DSR;
        else;
          Result := True;    { Nao há sinal de HandShake para verificar }
      end;

      if not Result then
        Sleep(10);
    end;
  end;
end;

function TACBrDeviceSerial.BytesParaLer: Integer;
begin
  Result := fsSerial.WaitingDataEx;
end;

procedure TACBrDeviceSerial.ConfigurarSerial;
begin
  if not fsSerial.InstanceActive then
    Exit;

  GravaLog('  ConfigurarSerial');
  fsSerial.Config(fsBaud, fsData, fsParity, fsStop, fsSoftFlow, fsHardFlow);

  if HandShake = hsRTS_CTS then
  begin
    GravaLog('  Serial.RTS');
    fsSerial.RTS := True;
  end
  else if HandShake = hsDTR_DSR then
  begin
    GravaLog('  Serial.DTR');
    fsSerial.DTR := True;
  end;
end;

function TACBrDeviceSerial.GetParamsString: String;
begin
  Result := ParametrosSerial(True);
end;

function TACBrDeviceSerial.ParametrosSerial(ApenasExcecoes: Boolean): String;
var
  sStop, sHandShake: String;
begin
  Result := '';

  if (not ApenasExcecoes) or (fsBaud <> 9600) then
    Result := Result + ' BAUD=' + IntToStr(fsBaud);

  if (not ApenasExcecoes) or (fsData <> 8) then
    Result := Result + ' DATA=' + IntToStr(fsData);

  if (not ApenasExcecoes) or (fsParity <> 'N') then
    Result := Result + ' PARITY=' + fsParity;

  if (not ApenasExcecoes) or (fsStop <> 0) then
  begin
    if fsStop = 2 then
      sStop := '2'
    else if fsStop = 1 then
      sStop := '1,5'
    else
      sStop := '1';

    Result := Result + ' STOP=' + sStop;
  end;

  if (not ApenasExcecoes) or (fsHandShake <> hsNenhum) then
  begin
    if fsHandShake = hsXON_XOFF then
      sHandShake := 'XON/XOFF'
    else if fsHandShake = hsDTR_DSR then
      sHandShake := 'DTR/DSR'
    else if fsHandShake = hsRTS_CTS then
      sHandShake := 'RTS/CTS';

    Result := Result + ' HANDSHAKE=' + sHandShake;
  end;

  if fsHardFlow then
    Result := Result + ' HARDFLOW';

  if fsSoftFlow then
    Result := Result + ' SOFTFLOW';

  with TACBrDevice(fpOwner) do
  begin
    if (not ApenasExcecoes) or (MaxBandwidth > 0) then
      Result := Result + ' MAXBANDWIDTH=' + IntToStr(MaxBandwidth);

    if (not ApenasExcecoes) or (SendBytesCount > 0) then
      Result := Result + ' SENDBYTESCOUNT=' + IntToStr(SendBytesCount);

    if (not ApenasExcecoes) or (SendBytesInterval > 0) then
      Result := Result + ' SENDBYTESINTERVAL=' + IntToStr(SendBytesInterval);
  end;

  Result := Trim(Result);
  GravaLog('  DeviceToString: ' + Result);
end;


procedure TACBrDeviceSerial.SetParamsString(const Value: String);
var
  S, Linha: String;

  function GetValue(LinhaParametros, Parametro: String): String;
  var
    P: Integer;
    Sub: String;
  begin
    Result := '';
    P := pos(Parametro, LinhaParametros);

    if P > 0 then
    begin
      Sub := Trim(copy(LinhaParametros, P + Length(Parametro), 200));
      if copy(Sub, 1, 1) = '=' then
        Sub := Trim(copy(Sub, 2, 200));

      P := pos(' ', Sub);
      if P = 0 then
        P := Length(Sub);

      Result := Trim(copy(Sub, 1, P));
    end;
  end;
begin
  GravaLog('SetParamsString(' + Value + ')');
  ValoresPadroes;

  Linha := Trim(UpperCase(Value));

  Baud := StrToIntDef(GetValue(Linha, 'BAUD'), Baud);

  S := GetValue(Linha, 'PARITY');
  if S <> '' then
    if CharInSet(S[1], ['O', 'E', 'M', 'S', 'N']) then
      fsParity := S[1];

  Data := StrToIntDef(GetValue(Linha, 'DATA'), Data);

  S := GetValue(Linha, 'STOP');
  if S = '1' then
    Stop := s1
  else if S = '1,5' then
    Stop := s1eMeio
  else if S = '2' then
    Stop := s2;

  HardFlow := (pos('HARDFLOW', Linha) > 0);
  SoftFlow := (pos('SOFTFLOW', Linha) > 0);

  S := GetValue(Linha, 'HANDSHAKE');
  if S = 'XON/XOFF' then
    HandShake := hsXON_XOFF
  else if S = 'DTR/DSR' then
    HandShake := hsDTR_DSR
  else if S = 'RTS/CTS' then
    HandShake := hsRTS_CTS;

  with TACBrDevice(fpOwner) do
  begin
    S := GetValue(Linha, 'MAXBANDWIDTH');
    MaxBandwidth := StrToIntDef(S, MaxBandwidth);

    S := GetValue(Linha, 'SENDBYTESCOUNT');
    SendBytesCount := StrToIntDef(S, SendBytesCount);

    S := GetValue(Linha, 'SENDBYTESINTERVAL');
    SendBytesInterval := StrToIntDef(S, SendBytesInterval);
  end;
end;

procedure TACBrDeviceSerial.AcharPortasSeriais(const AStringList : TStrings ;
   UltimaPorta : Integer) ;
var
   I: Integer ;
   BS: TBlockSerial ;
   UmaPorta : String ;
begin
   GravaLog('AcharPortasSeriais('+IntToStr(UltimaPorta)+')');
   BS := TBlockSerial.Create;
   try
      For I := 1 to UltimaPorta do
      begin
        try
           UmaPorta := 'COM'+IntToStr(I) ;

           BS.Connect( UmaPorta );
           if not (BS.LastError in [2,5,13]) then
              AStringList.Add(UmaPorta) ;

           BS.CloseSocket;
        except
        end ;
      end ;
   finally
      BS.Free ;
   end ;
end ;

procedure TACBrDeviceSerial.ValoresPadroes;
begin
  fsHardFlow := False;
  fsSoftFlow := False;
  fsHandShake := hsNenhum;
  fsParity := 'N';
  fsData := 8;
  fsBaud := 9600;
  fsStop := 0;
end;

function TACBrDeviceSerial.GetOnStatus: THookSerialStatus;
begin
  Result := fsSerial.OnStatus;
end;

function TACBrDeviceSerial.GetParity: TACBrSerialParity;
begin
  case fsParity of
    'O': Result := pOdd;
    'E': Result := pEven;
    'M': Result := pMark;
    'S': Result := pSpace;
    else
      Result := pNone;
  end;
end;

function TACBrDeviceSerial.GetStop: TACBrSerialStop;
begin
  case fsStop of
    1: Result := s1eMeio;
    2: Result := s2;
    else
      Result := s1;
  end;
end;

procedure TACBrDeviceSerial.SetBaud(AValue: Integer);
begin
  if fsBaud = AValue then
    exit;

  GravaLog('SetBaud(' + IntToStr(AValue) + ')');

  if (AValue < 50) or (AValue > 4000000) then
    DoException(Exception.Create(ACBrStr(cACBrDeviceSetBaudException)));

  fsBaud := AValue;
  ConfigurarSerial;
end;

procedure TACBrDeviceSerial.SetData(AValue: Integer);
begin
  if fsData = AValue then
    exit;

  GravaLog('SetData(' + IntToStr(AValue) + ')');

  if (AValue < 5) or (AValue > 8) then
    DoException(Exception.Create(ACBrStr(cACBrDeviceSetDataException)));

  fsData := AValue;
  ConfigurarSerial;
end;

procedure TACBrDeviceSerial.SetHandShake(AValue: TACBrHandShake);
begin
  GravaLog('SetHandShake('+GetEnumName(TypeInfo(TACBrHandShake), integer(AValue))+')');

  fsHardFlow  := (AValue = hsRTS_CTS);
  fsSoftFlow  := (AValue = hsXON_XOFF);

  fsHandShake := AValue;
  ConfigurarSerial ;
end;

procedure TACBrDeviceSerial.SetHardFlow(AValue: Boolean);
begin
  GravaLog('SetHardFlow(' + BoolToStr(AValue, True) + ')');

  if AValue then
    fsHandShake := hsRTS_CTS
  else
  if fsHandShake = hsRTS_CTS then
    fsHandShake := hsNenhum;
end;

procedure TACBrDeviceSerial.SetOnStatus(AValue: THookSerialStatus);
begin
  fsSerial.OnStatus := AValue;
end;

procedure TACBrDeviceSerial.SetParity(AValue: TACBrSerialParity);
begin
  if Parity = AValue then
    exit;

  GravaLog('SetParity(' + GetEnumName(TypeInfo(TACBrSerialParity), Integer(AValue)) + ')');

  case AValue of
    pOdd: fsParity := 'O';
    pEven: fsParity := 'E';
    pMark: fsParity := 'M';
    pSpace: fsParity := 'S';
    else
      fsParity := 'N';
  end;
  ConfigurarSerial;
end;

procedure TACBrDeviceSerial.SetSoftFlow(AValue: Boolean);
begin
  GravaLog('SetSoftFlow(' + BoolToStr(AValue, True) + ')');

  if AValue then
    fsHandShake := hsXON_XOFF
  else
  if fsHandShake = hsXON_XOFF then
    fsHandShake := hsNenhum;
end;

procedure TACBrDeviceSerial.SetStop(AValue: TACBrSerialStop);
begin
  if Stop = AValue then
    exit;

  GravaLog('SetStop(' + GetEnumName(TypeInfo(TACBrSerialStop), Integer(AValue)) + ')');

  case AValue of
    s1eMeio: fsStop := 1;
    s2: fsStop := 2;
    else
      fsStop := 0;
  end;
  ConfigurarSerial;
end;

function TACBrDeviceSerial.GetMaxSendBandwidth: Integer;
begin
  Result := fsSerial.MaxSendBandwidth;
end;

function TACBrDeviceSerial.GetTimeOutMilissegundos: Integer;
begin
  Result := fsSerial.DeadlockTimeout;
end;

procedure TACBrDeviceSerial.SetMaxSendBandwidth(AValue: Integer);
begin
  fsSerial.MaxSendBandwidth := AValue;
end;

procedure TACBrDeviceSerial.SetTimeOutMilissegundos(AValue: Integer);
begin
  fsSerial.DeadlockTimeout := AValue;
end;

procedure TACBrDeviceSerial.EnviaString(const AString: AnsiString);
Var
  I, Max, BytesToSend, BytesSent, FailCount : Integer ;
  Buffer: AnsiString;
begin
  GravaLog('  TACBrDeviceSerial.EnviaString');

  with TACBrDevice(fpOwner) do
  begin
    I := 1 ;
    Max := Length(AString) ;
    FailCount := 0;

    while (I <= Max) and (FailCount < CFailCount) do
    begin
      BytesToSend := SendBytesCount ;
      if BytesToSend = 0 then
        BytesToSend := Max ;

      GravaLog('  BytesToSend:'+IntToStr(BytesToSend));

      Buffer := copy(AString, I, BytesToSend );
      BytesToSend := min(Length(Buffer), BytesToSend);
      BytesSent := fsSerial.SendBuffer(Pointer(Buffer), BytesToSend);

      if BytesSent <= 0 then
      begin
        Inc( FailCount );
        GravaLog('  FailCount:'+IntToStr(FailCount));
      end
      else
        FailCount := 0;

      if SendBytesInterval > 0 then
      begin
        GravaLog('  Sleep('+IntToStr(SendBytesInterval)+')');
        Sleep( SendBytesInterval ) ;
      end;

      I := I + BytesSent;
    end;
  end;

  if FailCount >= CFailCount then
    DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceEnviaStrFailCount), [fpPorta]));
end;

procedure TACBrDeviceSerial.EnviaByte(const AByte: byte);
begin
  fsSerial.SendByte(AByte);
end;

function TACBrDeviceSerial.LeString(ATimeOutMilissegundos: Integer; NumBytes: Integer;
  const Terminador: AnsiString): AnsiString;
begin
  if (NumBytes > 0) then
    Result := fsSerial.RecvBufferStr(NumBytes, ATimeOutMilissegundos)
  else if (Terminador <> '') then
    Result := fsSerial.RecvTerminated(ATimeOutMilissegundos, Terminador)
  else
    Result := fsSerial.RecvPacket(ATimeOutMilissegundos);
end;

function TACBrDeviceSerial.LeByte(ATimeOutMilissegundos: Integer): byte;
begin
  Result := fsSerial.RecvByte(ATimeOutMilissegundos);
end;

procedure TACBrDeviceSerial.Limpar;
begin
  fsSerial.Purge;
end;

end.
