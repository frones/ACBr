{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{******************************************************************************
|* Historico
|*
|* 30/10/2008: Primeira Versao
|*    Daniel Simoes de Almeida
|*  - Migraçao do componente TACBrDevice de ACBrBase.pas para essa Unit para
|*    evitar que componentes, que usem TACBrComponente (de ACBrBase.pas) mas que
|*    nao usem TACBrDevice, carreguem codigo desnecessario como por exemplo
|*    toda a classe SynaSer
******************************************************************************}

{$I ACBr.inc}

//{$DEFINE ThreadEnviaLPT}  { Use // no inicio dessa linha para desabilitar a Thread}

{$IFDEF LINUX}
   { Thread TACBrThreadEnviaLPT nao funciona muito bem no Linux }
   { infelizmente, Strings grandes nao funcionam bem no LINUX usando a Thread}
  {$UNDEF ThreadEnviaLPT}
{$ENDIF}

{$IFDEF FPC}
 {$DEFINE Use_Stream}
{$ENDIF}

unit ACBrDevice ;

interface

uses synaser, {Unit da SynaSer (comunicação serial) }
     SysUtils, math,
     ACBrConsts, 
     {$IFDEF COMPILER6_UP}
       {$IFDEF MSWINDOWS}
         Windows,
       {$ENDIF}
       DateUtils, Types, StrUtils,
     {$ELSE}
        Windows, ACBrD5,
     {$ENDIF}
     {$IFNDEF NOGUI}
       {$IFDEF VisualCLX}
          QForms, QPrinters,
       {$ELSE}
          Forms, Printers,
       {$ENDIF}
     {$ENDIF}
     Classes ;

type

TACBrECFEstado = (estNaoInicializada, { Porta Serial ainda nao foi aberta }
                  estDesconhecido, {Porta aberta, mas estado ainda nao definido}
                  estLivre, { Impressora Livre, sem nenhum cupom aberto,
                              pronta para nova venda, Reducao Z e Leitura X ok,
                              pode ou nao já ter ocorrido 1ª venda no dia...}
                  estVenda, { Cupom de Venda Aberto com ou sem venda do 1º Item}
                  estPagamento, { Iniciado Fechamento de Cupom com Formas Pagto
                                  pode ou nao ter efetuado o 1º pagto. Nao pode
                                  mais vender itens, ou alterar Subtotal}
                  estRelatorio, { Imprimindo Cupom Fiscal Vinculado ou
                                  Relatorio Gerencial }
                  estBloqueada, { Reduçao Z já emitida, bloqueada até as 00:00 }
                  estRequerZ, {Reducao Z dia anterior nao emitida. Emita agora }
                  estRequerX,  {Esta impressora requer Leitura X todo inicio de
                               dia. Imprima uma Leitura X para poder vender}
                  estNaoFiscal  { Comprovante Nao Fiscal Aberto }
                  ) ;
TACBrECFEstadoSet = set of TACBrECFEstado ;

TACBrGAVAberturaAntecipada = ( aaIgnorar , aaException, aaAguardar ) ;

TACBrETQUnidade = (etqMilimetros, etqPolegadas, etqDots);

TACBrETQDPI = (dpi203, dpi300, dpi600) ;

TACBrETQOrientacao = (orNormal, or270, or180, or90);

TACBrETQBarraExibeCodigo = (becPadrao, becSIM, becNAO);

{Criando o tipo enumerado para tipos de código de barras }
TACBrECFTipoCodBarra =  ( barEAN13, barEAN8, barSTANDARD, barINTERLEAVED,
                         barCODE128, barCODE39, barCODE93, barUPCA,
                         barCODABAR, barMSI, barCODE11 );

{Criando tipo enumerado para a finalidade do arquivo MFD}
TACBrECFFinalizaArqMFD = (finMF, finMFD, finTDM, finRZ, finRFD, finNFP,
                          finNFPTDM, finSintegra, finSPED);

{ Criando tipo enumerado para o tipo do contador }
TACBrECFTipoContador = (tpcCOO, tpcCRZ);

{Criando o tipo enumerado para tipo de documentos em Leitura da MFD }
TACBrECFTipoDocumento = ( docRZ, docLX, docCF, docCFBP, docCupomAdicional,
                          docCFCancelamento, docCCD, docAdicionalCCD,
                          docSegViaCCD, docReimpressaoCCD, docEstornoCCD,
                          docCNF, docCNFCancelamento, docSangria, docSuprimento,
                          docEstornoPagto, docRG, docLMF, docTodos, docNenhum);
TACBrECFTipoDocumentoSet = set of TACBrECFTipoDocumento;

TACBrSerialParity = (pNone, pOdd, pEven, pMark, pSpace) ;
TACBrSerialStop   = (s1, s1eMeio, s2) ;
TACBrHandShake    = (hsNenhum, hsXON_XOFF, hsRTS_CTS, hsDTR_DSR) ;

TACBrAlinhamento = (alDireita, alEsquerda, alCentro);

TACBrECFCHQEstado = (chqIdle, chqPosicione, chqImprimindo, chqFimImpressao, chqRetire, chqAutenticacao);

TACBrDeviceHookEnviaString = procedure(const cmd: AnsiString) of object;
TACBrDeviceHookLeString = procedure(const NumBytes, ATimeOut: Integer; var Retorno: AnsiString) of object;

{ ACBrDevice é um componente apenas para usarmos o recurso de AutoExpand do
  ObjectInspector para SubComponentes, poderia ser uma Classe }

TACBrECFConfigBarras = class(TPersistent)
  private
    FMostrarCodigo: Boolean;
    FAltura: Integer;
    FLarguraLinha: Integer;
  published
    property MostrarCodigo: Boolean read FMostrarCodigo write FMostrarCodigo;
    property LarguraLinha: Integer read FLarguraLinha write FLarguraLinha;
    property Altura: Integer read FAltura write FAltura;
end;

{ TACBrDevice }

TACBrDevice = class( TComponent )
  private
    fsHardFlow: Boolean;
    fsHookEnviaString: TACBrDeviceHookEnviaString;
    fsHookLeString: TACBrDeviceHookLeString;
    fsSoftFlow: Boolean;
    fsParity: Char ;
    fsData : Integer;
    fsBaud: Integer;
    fsStop: Integer ;
    fsPorta: String;
    fsTimeOut: Integer ;
    fsAtivo: Boolean;

    fsHandShake: TACBrHandShake;
    fsSendBytesCount: Integer;
    fsSendBytesInterval: Integer;
    fProcessMessages: Boolean;

    procedure ConfiguraSerial ;
    procedure EnviaStringSerial(const AString : AnsiString) ;
    procedure EnviaStringArquivo(const AString : AnsiString) ;
    function GetParamsString: String;
    {$IFDEF ThreadEnviaLPT}
    procedure EnviaStrThread( AString : AnsiString ) ;
    {$ENDIF}

    procedure SetBaud(const Value: Integer);
    procedure SetData(const Value: Integer);
    procedure SetHardFlow(const Value: Boolean);
    function GetParity: TACBrSerialParity;
    procedure SetParity(const Value: TACBrSerialParity);
    procedure SetSoftFlow(const Value: Boolean);
    function GetStop: TACBrSerialStop;
    procedure SetStop(const Value: TACBrSerialStop);
    procedure SetPorta(const Value: String);
    procedure SetTimeOut(const Value: Integer);
    procedure SetOnStatus(const Value: THookSerialStatus);
    function GetOnStatus: THookSerialStatus;
    procedure SetAtivo(const Value: Boolean);
    procedure SetHandShake(const Value: TACBrHandShake);
    procedure SetParamsString(const Value: String);
    function GetMaxBandwidth: Integer;
    procedure SetMaxBandwidth(const Value: Integer);
  public
    Serial : TBlockSerial ;
    PosImp : TPoint;

    procedure Assign(Source: TPersistent); override;

    property Ativo : Boolean read fsAtivo write SetAtivo ;

    property Porta   : String  read fsPorta write SetPorta ;
    property TimeOut : Integer read fsTimeOut write SetTimeOut ;
    Function EmLinha( lTimeOut : Integer = 1) : Boolean  ;

    property ParamsString : String read GetParamsString write SetParamsString ;

    constructor Create(AOwner: TComponent); override ;
    destructor Destroy ; override ;

    procedure Ativar ;
    procedure Desativar ;
    Procedure EnviaString( const AString : AnsiString ) ;
    Function LeString( ATimeOut: Integer=0; NumBytes: Integer=0 ): String;
    Function LeByte( ATimeOut: Integer=0 ): Byte;

    Procedure ImprimePos(const Linha, Coluna : Integer; const AString: AnsiString) ;
    Procedure Eject ;

    Procedure SetDefaultValues ;

    Function IsSerialPort : Boolean ;
    Function IsTXTFilePort: Boolean ;
    Function IsDLLPort: Boolean ;

    procedure AcharPortasSeriais( const AStringList : TStrings;
       UltimaPorta : Integer = 64 ) ;
    function DeviceToString(OnlyException: Boolean): String;

  published
     property Baud     : Integer read fsBaud write SetBaud default 9600 ;
     property Data     : Integer read fsData write SetData default 8 ;
     property Parity   : TACBrSerialParity read GetParity write SetParity
                         default pNone ;
     property Stop     : TACBrSerialStop read GetStop write SetStop
                         default s1 ;
     property MaxBandwidth : Integer read  GetMaxBandwidth
                                     write SetMaxBandwidth default 0 ;
     property SendBytesCount : Integer read  fsSendBytesCount
                                       write fsSendBytesCount  default 0 ;
     property SendBytesInterval : Integer read  fsSendBytesInterval
                                       write fsSendBytesInterval  default 0 ;
     property HandShake : TACBrHandShake read fsHandShake write SetHandShake
                         default hsNenhum ;
     property SoftFlow : Boolean read fsSoftFlow write SetSoftFlow
                         default false ;
     property HardFlow : Boolean read fsHardFlow write SetHardFlow
                         default false ;

     { propriedade que ativa/desativa o processamento de mensagens do windows }
     property ProcessMessages : Boolean read fProcessMessages
        write fProcessMessages default True ;

     property OnStatus : THookSerialStatus read GetOnStatus write SetOnStatus ;
     property HookEnviaString : TACBrDeviceHookEnviaString read fsHookEnviaString
        write fsHookEnviaString;
     property HookLeString : TACBrDeviceHookLeString read fsHookLeString
        write fsHookLeString;
end ;

{ Essa classe é usada pela função EnviaStrThread para detectar se os Dados
  estao sendo "gravados" com sucesso na porta paralela ou arquivo. }
TACBrThreadEnviaLPT = class(TThread)
  private
    { Private declarations }
    fsTextoAEnviar : String ;
    fsBytesSent    : Integer;
    fsOwner        : TObject ;
  protected
    procedure Execute ; override;
  public
    constructor Create(AOwner : TObject; AString : String) ;
    property BytesSent : Integer read fsBytesSent ;
  end;


implementation
Uses ACBrUtil ;

{ TACBrDevice }
constructor TACBrDevice.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

  { Classe SynaSer para acesso direto a Serial }
  Serial := TBlockSerial.Create ;
  Serial.RaiseExcept := true ;

  { Variaveis Internas }
  fsPorta   := '' ;
  fsTimeOut := cTimeout ;

  fsSendBytesCount    := 0 ;
  fsSendBytesInterval := 0 ;

  fProcessMessages := True ;

  fsHookEnviaString := nil;
  fsHookLeString    := nil;

  SetDefaultValues ;
end;

destructor TACBrDevice.Destroy;
begin
  Serial.Free ;
  IOResult ;

  inherited Destroy ;
end;

procedure TACBrDevice.SetDefaultValues;
begin
  fsHardFlow := false ;
  fsSoftFlow := false ;
  fsHandShake:= hsNenhum ;
  fsParity   := 'N' ;
  fsData     := 8 ;
  fsBaud     := 9600 ;
  fsStop     := 0 ;

  PosImp.X   := 0 ;
  PosImp.Y   := 0 ;
end;


procedure TACBrDevice.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrDevice.Ativar;
begin
  if fsAtivo then exit ;

  if fsPorta = '' then
     raise Exception.Create(ACBrStr(cACBrDeviceAtivarPortaException));

  if IsSerialPort then
   begin
     try
        Serial.CloseSocket ;  { Fecha se ficou algo aberto }
        Serial.DeadlockTimeout := (TimeOut * 1000) ;
        Serial.Connect( fsPorta ) ;
        ConfiguraSerial ;

        Serial.Purge ;  { Limpa Buffer de envio e recepção }
     except
        try
           Serial.RaiseExcept := false ;
           Serial.CloseSocket ;
        finally
           Serial.RaiseExcept := true ;
        end ;
        raise ;
     end ;
   end
  else if not IsDLLPort then
   begin
      { Tenta Abrir Arquivo/Porta para ver se xiste e está disponivel}
      if IsTXTFilePort and FileExists(Porta) then
         SysUtils.DeleteFile(Porta);

      EnviaStringArquivo( '' );
   end ;

  fsAtivo := true ;
end;

procedure TACBrDevice.ConfiguraSerial ;
begin
  if not Serial.InstanceActive then exit ;

  Serial.Config( fsBaud, fsData, fsParity, fsStop, fsSoftFlow, fsHardFlow);

  if HandShake = hsRTS_CTS then
     Serial.RTS := true
  else if HandShake = hsDTR_DSR then
     Serial.DTR := true ;
end ;

procedure TACBrDevice.Desativar;
begin
  if not fsAtivo then exit ;

  if IsSerialPort then
  begin
     try
        Serial.RaiseExcept := false ;
        Serial.CloseSocket ;
     finally
        Serial.RaiseExcept := true ;
     end ;
  end ;

  fsAtivo := false ;
end;

function TACBrDevice.GetOnStatus: THookSerialStatus;
begin
  result := Serial.OnStatus ;
end;

procedure TACBrDevice.SetOnStatus(const Value: THookSerialStatus);
begin
  Serial.OnStatus := Value ;
end;

procedure TACBrDevice.SetBaud(const Value: Integer);
begin
  if fsBaud = Value then exit ;

  if (Value < 50) or (Value > 4000000) then
     raise Exception.Create( ACBrStr(cACBrDeviceSetBaudException) );

  fsBaud := Value ;
  ConfiguraSerial ;
end;

procedure TACBrDevice.SetData(const Value: Integer);
begin
  if fsData = Value then exit ;

  if (Value < 5) or (Value > 8) then
     raise Exception.Create( ACBrStr(cACBrDeviceSetDataException) );

   fsData := Value ;
   ConfiguraSerial ;
end;

function TACBrDevice.GetParity: TACBrSerialParity;
begin
  case fsParity of
     'O' : result := pOdd   ;
     'E' : result := pEven  ;
     'M' : result := pMark  ;
     'S' : result := pSpace ;
  else
     result := pNone ;
  end;
end;

procedure TACBrDevice.SetParity(const Value: TACBrSerialParity);
begin
  if Parity = Value then exit ;

  case Value of
     pOdd   : fsParity := 'O' ;
     pEven  : fsParity := 'E' ;
     pMark  : fsParity := 'M' ;
     pSpace : fsParity := 'S' ;
  else
     fsParity := 'N' ;
  end;
  ConfiguraSerial ;
end;

function TACBrDevice.GetStop: TACBrSerialStop;
begin
  case fsStop of
     1 : result := s1eMeio ;
     2 : result := s2      ;
  else
     result := s1 ;
  end;
end;

procedure TACBrDevice.SetStop(const Value: TACBrSerialStop );
begin
  if Stop = Value then exit ;

  case Value of
     s1eMeio : fsStop := 1 ;
     s2      : fsStop := 2 ;
  else
     fsStop  := 0 ;
  end;
  ConfiguraSerial ;
end;

function TACBrDevice.GetMaxBandwidth: Integer;
begin
  Result := Serial.MaxSendBandwidth ;
end;

procedure TACBrDevice.SetMaxBandwidth(const Value: Integer);
begin
  Serial.MaxLineLength := Value ;
end;

procedure TACBrDevice.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TACBrDevice then
  begin
     Baud              := TACBrDevice(Source).Baud ;
     Data              := TACBrDevice(Source).Data ;
     Parity            := TACBrDevice(Source).Parity ;
     Stop              := TACBrDevice(Source).Stop ;
     MaxBandwidth      := TACBrDevice(Source).MaxBandwidth ;
     SendBytesCount    := TACBrDevice(Source).SendBytesCount ;
     SendBytesInterval := TACBrDevice(Source).SendBytesInterval ;
     HandShake         := TACBrDevice(Source).HandShake ;
     SoftFlow          := TACBrDevice(Source).SoftFlow ;
     HardFlow          := TACBrDevice(Source).HardFlow ;
     ProcessMessages   := TACBrDevice(Source).ProcessMessages ;
     OnStatus          := TACBrDevice(Source).OnStatus ;
     HookEnviaString   := TACBrDevice(Source).HookEnviaString ;
     HookLeString      := TACBrDevice(Source).HookLeString ;
  end;
end;

procedure TACBrDevice.SetHardFlow(const Value: Boolean);
begin
  if Value then
     HandShake := hsRTS_CTS
  else
     if HandShake = hsRTS_CTS then
        HandShake := hsNenhum ;
end;

procedure TACBrDevice.SetSoftFlow(const Value: Boolean);
begin
  if Value then
     HandShake := hsXON_XOFF
  else
     if HandShake = hsXON_XOFF then
        HandShake := hsNenhum ;
end;

procedure TACBrDevice.SetHandShake(const Value: TACBrHandShake);
begin
  fsHardFlow  := (Value = hsRTS_CTS);
  fsSoftFlow  := (Value = hsXON_XOFF);

  fsHandShake := Value;
  ConfiguraSerial ;
end;

procedure TACBrDevice.SetPorta(const Value: String);
Var StrTemp : String ;
begin
  if fsPorta = Value then exit ;

  if Ativo then
     raise Exception.Create( ACBrStr(cACBrDeviceSetPortaException) );

  StrTemp := UpperCase( Value ) ;
  if (pos('LPT',StrTemp) = 1) or (pos('COM',StrTemp) = 1) then
     fsPorta := StrTemp
  else
     fsPorta := Value ;
end;

procedure TACBrDevice.SetTimeOut(const Value: Integer);
begin
  if Value = fsTimeOut then
     exit ;

  if Value < 1 then
     fsTimeOut := 1
  else
     fsTimeOut := Value ;

  if IsSerialPort then
     Serial.DeadlockTimeout := (TimeOut * 1000) ;
end;

function TACBrDevice.EmLinha( lTimeOut : Integer) : Boolean;
var TempoLimite : TDateTime ;
begin
  Result := True;
  if IsDLLPort then exit;

  if not IsSerialPort then
  begin
     try
        {$IFDEF ThreadEnviaLPT}
        EnviaStrThread(#0);  { Tenta escrever algo (#0) na Porta/Arquivo }
        {$ENDIF}
        Result := true ;
     except
        Result := false ;
     end ;

     exit ;
  end ;

  Result := false ;
  if lTimeout < 1 then
     lTimeOut := 1 ;

  if not Serial.InstanceActive then
     exit ;

  TempoLimite := IncSecond( now, lTimeOut)  ;
  while (not result) and (now < TempoLimite) do
  begin
     case HandShake of
       hsRTS_CTS :
          Result := Serial.CTS ;

       hsDTR_DSR :
          Result := Serial.DSR ;
     else ;
        Result := true ;    { Nao há sinal de HandShake para verificar }
     end;

     if not result then
     begin
        {$IFNDEF NOGUI}
          if fProcessMessages then
             Application.ProcessMessages ;  { para redesenhar a tela do programa }
        {$ENDIF}
        sleep(10) ;
     end ;
  end ;
end;

function TACBrDevice.IsSerialPort: Boolean;
begin
   Result := (pos('COM', fsPorta) =  1)
             {$IFDEF LINUX}
               or (pos('/dev/tty', fsPorta) =  1)
             {$ELSE}
               or (copy(fsPorta,1,4) = '\\.\')
             {$ENDIF}
end;

function TACBrDevice.IsTXTFilePort: Boolean;
begin
  Result := UpperCase(RightStr(fsPorta,4)) = '.TXT' ;
end;

function TACBrDevice.IsDLLPort : Boolean ;
Var
  AStr : String;
begin
  AStr   := UpperCase(copy(Porta,1,3));
  Result := (pos(AStr,'USB|DLL') > 0) ;
end ;

procedure TACBrDevice.AcharPortasSeriais(const AStringList : TStrings ;
   UltimaPorta : Integer) ;
var
   I     : Integer ;
   BS    : TBlockSerial ;
   Porta : String ;
begin
   AStringList.Clear;

   BS := TBlockSerial.Create;
   try
      For I := 1 to UltimaPorta do
      begin
        try
           Porta := 'COM'+IntToStr(I) ;

           BS.Connect( Porta );
           if BS.LastError <> 2 then
              AStringList.Add(Porta) ;

           BS.CloseSocket;
        except
        end ;
      end ;
   finally
      BS.Free ;
   end ;
end ;

function TACBrDevice.DeviceToString( OnlyException: Boolean): String;
Var
  sStop, sHandShake : String ;
begin
  Result := '' ;

  if (not OnlyException) or (fsBaud <> 9600)  then
     Result := Result + ' BAUD='+IntToStr(fsBaud) ;

  if (not OnlyException) or (fsData <> 8) then
     Result := Result + ' DATA='+IntToStr(fsData) ;

  if (not OnlyException) or (fsParity <> 'N') then
     Result := Result + ' PARITY='+fsParity ;

  if (not OnlyException) or (fsStop <> 0) then
  begin
     if fsStop = 2 then
        sStop := '2'
     else if fsStop = 1 then
        sStop := '1,5'
     else
        sStop := '1' ;

     Result := Result + ' STOP='+sStop ;
  end ;

  if (not OnlyException) or (fsHandShake <> hsNenhum) then
  begin
     if fsHandShake = hsXON_XOFF then
        sHandShake := 'XON/XOFF'
     else if fsHandShake = hsDTR_DSR then
        sHandShake := 'DTR/DSR'
     else if fsHandShake = hsRTS_CTS then
        sHandShake := 'RTS/CTS' ;

     Result := Result +  ' HANDSHAKE='+sHandShake ;
  end ;

  if fsHardFlow then
     Result := Result + ' HARDFLOW' ;

  if fsSoftFlow then
     Result := Result + ' SOFTFLOW' ;

  if (not OnlyException) or (MaxBandwidth > 0) then
     Result := Result + ' MAXBANDWIDTH='+IntToStr(MaxBandwidth) ;
     
  Result := Trim(Result) ;
end;

procedure TACBrDevice.SetParamsString(const Value: String);
Var S, Linha   : String ;
  Function GetValue( LinhaParametros, Parametro : String ) : String ;
    Var P   : Integer ;
        Sub : String ;
  begin
    Result := '' ;
    P := pos(Parametro,LinhaParametros) ;

    if P > 0 then
    begin
      Sub := Trim(copy(LinhaParametros, P + Length(Parametro) ,200)) ;
      if copy(Sub,1,1) = '=' then
         Sub := Trim(copy(Sub,2,200)) ;

      P := pos(' ',Sub) ;
      if P = 0 then
         P := Length(Sub) ;

      Result := Trim(copy(Sub,1,P)) ;
    end ;
  end ;
begin
  SetDefaultValues ;

  Linha := Trim(UpperCase(Value)) ;

  Baud := StrToIntDef(GetValue(Linha,'BAUD'),Baud) ;

  S := GetValue(Linha,'PARITY') ;
  if S <> '' then
    if S[1] in ['O','E','M','S','N'] then
      fsParity := S[1] ;

  Data := StrToIntDef(GetValue(Linha,'DATA'),Data) ;

  S := GetValue(Linha,'STOP') ;
  if S = '1' then
    Stop := s1
  else if S = '1,5' then
    Stop := s1eMeio
  else if S = '2' then
    Stop := s2 ;

  HardFlow := (pos('HARDFLOW',Linha) > 0) ;
  SoftFlow := (pos('SOFTFLOW',Linha) > 0) ;

  S := GetValue(Linha,'HANDSHAKE') ;
  if S = 'XON/XOFF' then
     HandShake := hsXON_XOFF
  else if S = 'DTR/DSR' then
     HandShake := hsDTR_DSR
  else if S = 'RTS/CTS' then
     HandShake := hsRTS_CTS ;

  S := GetValue(Linha,'MAXBANDWIDTH') ;
  MaxBandwidth := StrToIntDef(S,MaxBandwidth) ;
end;


procedure TACBrDevice.EnviaString( const AString: AnsiString);
begin
  if IsSerialPort then
     EnviaStringSerial( AString )

  else if IsDLLPort then
   begin
     if Assigned( HookEnviaString ) then
        HookEnviaString( AString );
   end

  else
   begin
     {$IFNDEF ThreadEnviaLPT}
       EnviaStringArquivo( AString )
     {$ELSE}
       EnviaStrThread( AString )
     {$ENDIF} ;
   end ;
end;

function TACBrDevice.LeString(ATimeOut: Integer; NumBytes: Integer): String;
var
   Buffer: AnsiString;
   Fim: TDateTime;
begin
  Result := '';

  if ATimeOut = 0 then
     ATimeOut := Self.TimeOut;

  if IsSerialPort then
   begin
     if NumBytes = 0 then
        Result := Serial.RecvPacket( ATimeOut )
     else
        Result := Serial.RecvBufferStr( NumBytes, ATimeOut) ;
   end

  else if IsDLLPort and Assigned( HookLeString ) then
   begin
     Fim := IncMilliSecond( Now, ATimeOut );
     repeat
        Buffer := '';
        HookLeString( max(NumBytes,1), ATimeOut, Buffer );
        Result := Result + Buffer;
     until (now > Fim) or ( (NumBytes > 0) and (Length(Result) >= NumBytes) );
   end;
end;

function TACBrDevice.LeByte(ATimeOut: Integer): Byte;
Var
  Buffer: AnsiString;
begin
  Result := 0;

  if ATimeOut = 0 then
     ATimeOut := Self.TimeOut;

  if IsSerialPort then
     Result := Serial.RecvByte( ATimeOut )

  else if IsDLLPort and Assigned( HookLeString ) then
   begin
     Buffer := '';
     HookLeString( 1, ATimeOut, Buffer );
     if Length(Buffer) > 0 then
        Result := Ord( Buffer[1] );
   end;
end;

procedure TACBrDevice.EnviaStringSerial(const AString : AnsiString) ;
Var
  I, Max, NBytes : Integer ;
begin
  I   := 1 ;
  Max := Length(AString) ;
  NBytes := fsSendBytesCount ;
  if NBytes = 0 then
     NBytes := Max ;

  while I <= Max do
  begin
     Serial.SendString( copy(AString, I, NBytes ) ) ;    { Envia para Porta Serial }
     if fsSendBytesInterval > 0 then
        Sleep( fsSendBytesInterval ) ;
     I := I + NBytes ;
  end ;
end ;

procedure TACBrDevice.EnviaStringArquivo( const AString: AnsiString);
Var
  I, Max, NBytes : Integer ;
  {$IFDEF Use_Stream}
    FS     : TFileStream ;
    Buffer : AnsiString ;
  {$ELSE}
    ArqPrn : TextFile ;
  {$ENDIF}
begin
  if IsSerialPort then exit;

  I   := 1 ;
  Max := Length(AString) ;
  NBytes := fsSendBytesCount ;
  if NBytes = 0 then
     NBytes := Max ;

  {$IFDEF Use_Stream}
    FS := TFileStream.Create( Porta, IfThen( IsTXTFilePort and FileExists(Porta),
       fmOpenReadWrite, fmCreate) or fmShareDenyWrite );
    try
       FS.Seek(0, soFromEnd);  // vai para EOF

       while I <= Max do
       begin
          Buffer := copy(AString, I, NBytes ) ;

          FS.Write(Pointer(Buffer)^,Length(Buffer));

          if fsSendBytesInterval > 0 then
             Sleep( fsSendBytesInterval ) ;
          I := I + NBytes ;
       end ;
    finally
       FS.Free ;
    end;
  {$ELSE}
    AssignFile( ArqPrn, Porta );
    try
       if IsTXTFilePort and FileExists(Porta) then
          Append( ArqPrn )
       else
          Rewrite( ArqPrn ) ;

       while I <= Max do
       begin
          Write( ArqPrn, copy(AString, I, NBytes ) ) ;
          if fsSendBytesInterval > 0 then
             Sleep( fsSendBytesInterval ) ;
          I := I + NBytes ;
       end ;

       Flush( ArqPrn ) ;
    finally
       {$I-}
       {$IFNDEF FPC}System.{$ENDIF}CloseFile( ArqPrn ) ;
       {$I+}
    end ;
  {$ENDIF}
end ;

function TACBrDevice.GetParamsString: String;
begin
   Result := DeviceToString( True );
end;

{$IFDEF ThreadEnviaLPT}
{ A ideia dessa Thread é testar se os dados estão sendo gravados com sucesso na
  Porta Paralela (ou arquivo). É criada uma Thread para "gravar" os dados em
  segundo plano, enquanto o programa monitora se as linhas estão sendo enviadas.
  Caso a Thread nao consiga enviar uma linha dentro do Timeout definido a Thread
  é cancelada e é gerado um TIMEOUT. Isso evita o "travamento" do programa
  quando a porta ou arquivo não estão prontos para a gravação com o comando
  Write() }
procedure TACBrDevice.EnviaStrThread(AString: AnsiString);
Var IsTimeOut  : Boolean ;
    TempoFinal : TDateTime ;
    UltimoBytesSent : Integer ;
    ThreadEnviaLPT  : TACBrThreadEnviaLPT ;
begin
  { Criando Thread para monitorar o envio de dados a Porta Paralela }
  IsTimeOut       := false ;
  UltimoBytesSent := -1 ;
  TempoFinal      := -1 ;
  ThreadEnviaLPT  := TACBrThreadEnviaLPT.Create( Self, AString ) ;
  try
     while not ThreadEnviaLPT.Terminated do
     begin
        if UltimoBytesSent <> ThreadEnviaLPT.BytesSent then
        begin
           TempoFinal := IncSecond(now,TimeOut) ;
           UltimoBytesSent := ThreadEnviaLPT.BytesSent ;
        end ;

        {$IFNDEF NOGUI}
          if fProcessMessages then
             Application.ProcessMessages ;
        {$ENDIF}
        IsTimeOut := (now > TempoFinal) ; {Verifica se estourou o tempo TIMEOUT}
        if IsTimeOut then
           Break ;

        sleep(200) ;
     end ;
  finally
     ThreadEnviaLPT.Terminate ;

     if IsTimeOut then
        raise Exception.Create( Format(ACBrStr(cACBrDeviceEnviaStrThreadException), [ Porta ] )) ;
  end ;
end;
{$ENDIF}

procedure TACBrDevice.ImprimePos(const Linha, Coluna : Integer;
  const AString: AnsiString);
Var Cmd : String ;
begin
  if (AString = '') or
     (Linha < 0)    or
     (Coluna < 0) then
     exit ;

  Cmd := '' ;

  if Linha < PosImp.X then
     Eject ;

  if Linha > PosImp.X then
  begin
     Cmd := StringOfChar( LF, (Linha - PosImp.X) ) ;
     PosImp.X := Linha ;
  end ;

  if Coluna < PosImp.Y then
  begin
     Cmd := Cmd + CR ;
     PosImp.Y := 0 ;
  end ;

  if Coluna > PosImp.Y then
  begin
     Cmd := Cmd + StringOfChar( ' ', (Coluna - PosImp.Y) ) ;
     PosImp.Y := Coluna ;
  end ;

  EnviaString( Cmd + AString ) ;
  PosImp.Y := PosImp.Y + Length( AString );

end;

procedure TACBrDevice.Eject;
begin
  EnviaString( FF );
  PosImp.X := 0 ;
end;

{---------------------------- TACBrThreadEnviaLPT -----------------------------}
constructor TACBrThreadEnviaLPT.Create(AOwner : TObject; AString: String ) ;
begin
  if not (AOwner is TACBrDevice) then
     raise Exception.Create(ACBrStr('Uso Inválido da TACBrThreadEnviaLPT'));

  inherited Create( false ) ; { Rodar Imediatemanete }
  FreeOnTerminate := true ;

  fsOwner        := AOwner  ;
  fsTextoAEnviar := AString ;
end;

procedure TACBrThreadEnviaLPT.Execute;
var
  I, MaxLen, BufferLen : Integer ;
begin
  if fsTextoAEnviar <> '' then
  begin
     fsBytesSent := 0 ;
     I           := 1 ;
     MaxLen      := Length( fsTextoAEnviar ) ;
     BufferLen   := 256 ;

     with TACBrDevice(fsOwner) do
     begin
        while (I <= MaxLen) and (not Terminated) do
        begin
           EnviaStringArquivo( copy( fsTextoAEnviar, I, BufferLen ) );
           fsBytesSent := fsBytesSent + BufferLen ;
           I := I + BufferLen ;
        end ;
     end ;
  end ;

  Terminate ;
end;



end.

