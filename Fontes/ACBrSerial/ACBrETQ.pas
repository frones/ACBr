{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrETQ;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrDevice, ACBrETQClass, synacode, synautil;

type

  TACBrETQModelo = (etqNenhum, etqPpla, etqPplb, etqZPLII, etqEpl2, etqEscLabel);

  { TACBrETQCmdList }

  TACBrETQCmdList = class(TStringList)
  public
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;


{ TACBrETQ }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrETQ = class(TACBrComponent)
  private
    fsMargemEsquerda: Integer;
    fsEtqFinalizada: Boolean;
    fsEtqInicializada: Boolean;
    fsCopias, fsAvancoEtq: Integer;
    fsOnGravarLog: TACBrGravarLog;
    fsModelo: TACBrETQModelo;
    fsListaCmd: TACBrETQCmdList;
    fsDevice: TACBrDevice;   { SubComponente ACBrDevice }
    fsETQ: TACBrETQClass;
    fsArqLOG: String;
    fsAtivo: Boolean;

    function GetLimparMemoria: Boolean;
    function GetModeloStr: String;
    function GetBackFeed: TACBrETQBackFeed;
    function GetDeteccaoEtiqueta: TACBrETQDeteccaoEtiqueta;
    function GetOrigem: TACBrETQOrigem;
    function GetPaginaDeCodigo: TACBrETQPaginaCodigo;
    function GetUnidade: TACBrETQUnidade;
    function GetTemperatura: Integer;
    function GetVelocidade: Integer;
    function GetPorta: String;
    function GetDPI : TACBrETQDPI;
    function GetAvanco: Integer;
    function GetGuilhotina: Boolean;

    procedure SetUnidade(const AValue: TACBrETQUnidade);
    procedure SetModelo(const Value: TACBrETQModelo);
    procedure SetBackFeed(AValue: TACBrETQBackFeed);
    procedure SetDeteccaoEtiqueta(AValue: TACBrETQDeteccaoEtiqueta);
    procedure SetLimparMemoria(const Value: Boolean);
    procedure SetTemperatura(const Value: Integer);
    procedure SetVelocidade(const Value: Integer);
    procedure SetDPI(const AValue: TACBrETQDPI);
    procedure SetPorta(const Value: String);
    procedure SetOrigem(AValue: TACBrETQOrigem);
    procedure SetAvanco(const AValue: Integer);
    procedure SetGuilhotina(AValue: Boolean);
    procedure SetAtivo(const Value: Boolean);
    procedure SetPaginaDeCodigo(AValue: TACBrETQPaginaCodigo);
    function GetNumeroPaginaDeCodigo(APagCod: TACBrETQPaginaCodigo): word;
    function CodificarPaginaDeCodigo(const ATexto: AnsiString): AnsiString;

    procedure AtivarSeNecessario;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Ativar;
    procedure Desativar;
    procedure IniciarEtiqueta;
    procedure FinalizarEtiqueta(Copias: Integer = 1; AvancoEtq: Integer = 0);

    procedure Imprimir(Copias: Integer = 1; AvancoEtq: Integer = 0);
    function  GerarStreamBase64(Copias: Integer = 1; AvancoEtq: Integer = 0 ) : AnsiString;

    procedure ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
      MultiplicadorV, Vertical, Horizontal: Integer; const Texto: String;
      SubFonte: Integer = 0; ImprimirReverso: Boolean = False); overload;
    procedure ImprimirTexto(Orientacao: TACBrETQOrientacao; const Fonte: String;
      MultiplicadorH, MultiplicadorV, Vertical, Horizontal: Integer; const Texto: String;
      SubFonte: Integer = 0; ImprimirReverso: Boolean = False); overload;

    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao;
      TipoBarras: TACBrTipoCodBarra;
      LarguraBarraLarga, LarguraBarraFina, Vertical, Horizontal: Integer;
      const Texto: String; AlturaCodBarras: Integer; ExibeCodigo:
      TACBrETQBarraExibeCodigo = becPadrao); overload;
    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao;
      const TipoBarras: String;
      LarguraBarraLarga, LarguraBarraFina, Vertical, Horizontal: Integer;
      const Texto: String; AlturaCodBarras: Integer; ExibeCodigo:
      TACBrETQBarraExibeCodigo = becPadrao); overload;
    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao; const TipoBarras,
          LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
          const Texto: String; AlturaCodBarras: Integer = 0;
          ExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao); overload;

    procedure ImprimirQRCode(Vertical, Horizontal: Integer; const Texto: String;
          LarguraModulo: Integer = 4; ErrorLevel: Integer = 0; Tipo: Integer = 2);

    procedure ImprimirLinha(Vertical, Horizontal, Largura, Altura: Integer); overload;
    procedure ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer; Canto: Integer = 0);

    procedure ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal: Integer;
      const NomeImagem: String);
    procedure CarregarImagem(aStream: TStream; var NomeImagem: String;
      Flipped: Boolean = True; const Tipo: String = ''); overload;
    procedure CarregarImagem(const ArquivoImagem: String; var NomeImagem: String;
      Flipped: Boolean = True); overload;
    procedure ApagarImagem(const NomeImagem: String = '*');

    procedure DefinirCor(FrenteCor: Cardinal; FrenteOpacidade: Byte;
      FundoCor: Cardinal; FundoOpacidade: Byte); overload;
    procedure DefinirCor(FrenteR, FrenteG, FrenteB, FrenteOpacidade: Byte;
      FundoR, FundoG, FundoB, FundoOpacidade: Byte); overload;
    procedure DefinirCorPadrao;
    procedure DefinirDimensoes(Largura, Altura: Integer;
      EspacoEntreEtiquetas: Integer = -1; EspacoEsquerda: Integer = -1);

    procedure ComandoGravaRFIDHexaDecimal(aValue:String);
    procedure ComandoGravaRFIDASCII( aValue:String );

    procedure GravarLog(aString: AnsiString; Traduz: Boolean = False);

    property ETQ:             TACBrETQClass    read fsETQ;
    property ListaCmd:        TACBrETQCmdList  read fsListaCmd;
    property ModeloStr:       String           read GetModeloStr;
    property EtqFinalizada:   Boolean          read fsEtqFinalizada;
    property EtqInicializada: Boolean          read fsEtqInicializada;

  published
    property PaginaDeCodigo:  TACBrETQPaginaCodigo read GetPaginaDeCodigo write SetPaginaDeCodigo default pceNone;
    property Unidade:         TACBrETQUnidade  read GetUnidade       write SetUnidade default etqDecimoDeMilimetros;
    property Modelo:          TACBrETQModelo   read fsModelo         write SetModelo default etqNenhum;
    property BackFeed:        TACBrETQBackFeed read GetBackFeed      write SetBackFeed default bfNone;
    property LimparMemoria:   Boolean          read GetLimparMemoria write SetLimparMemoria default True;
    property Temperatura:     Integer          read GetTemperatura   write SetTemperatura default 10;
    property Velocidade:      Integer          read GetVelocidade    write SetVelocidade default -1;
    property Origem:          TACBrETQOrigem   read GetOrigem        write SetOrigem default ogNone;
    property DPI:             TACBrETQDPI      read GetDPI           write SetDPI default dpi203;
    property Avanco:          Integer          read GetAvanco        write SetAvanco default 0;
    property Guilhotina:      Boolean          read GetGuilhotina    write SetGuilhotina default False;
    property MargemEsquerda:  Integer          read fsMargemEsquerda write fsMargemEsquerda default 0;
    property DeteccaoEtiqueta:TACBrETQDeteccaoEtiqueta read GetDeteccaoEtiqueta write SetDeteccaoEtiqueta default mdeGap;

    property OnGravarLog:     TACBrGravarLog   read fsOnGravarLog    write fsOnGravarLog;
    property ArqLOG:          String           read fsArqLOG         write fsArqLOG;
    property Porta:           String           read GetPorta         write SetPorta;
    property Ativo:           Boolean          read fsAtivo          write SetAtivo;

    { Instancia do Componente ACBrDevice, será passada para fsETQ.create }
    property Device: TACBrDevice read fsDevice;
  end;

function ConverterUnidade(UnidadeEntrada: TACBrETQUnidade; ValorEntrada: Double;
  UnidadeSaida: TACBrETQUnidade; DPI: TACBrETQDPI = dpi203): Double;

implementation

uses
  math, typinfo,
  {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5{$ENDIF},
  ACBrETQPpla, ACBrETQZplII, ACBrETQEpl2, ACBrETQEscLabel,
  {$IfDef MSWINDOWS}
  ACBrWinUSBDevice,
  {$EndIf}
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO;

function ConverterUnidade(UnidadeEntrada: TACBrETQUnidade;
  ValorEntrada: Double; UnidadeSaida: TACBrETQUnidade; DPI: TACBrETQDPI): Double;
var
  DotsMM, DotsPI: Double;
begin
  Result := ValorEntrada;
  if (UnidadeSaida = UnidadeEntrada) then
    Exit;

  case DPI of
    dpi300: DotsPI := 300;
    dpi600: DotsPI := 600;
  else
    DotsPI := 203;
  end;

  // 1 Inch = 2.54 cm = 25.4 mm
  DotsMM := DotsPI / CInchCM / 10;

  case UnidadeSaida of
    etqMilimetros:
    begin
      case UnidadeEntrada of
        etqPolegadas:          Result := (ValorEntrada*10) * CInchCM;
        etqDots:               Result := ValorEntrada / DotsMM;
        etqDecimoDeMilimetros: Result := ValorEntrada * 10;
      end;
    end;

    etqPolegadas:
    begin
      case UnidadeEntrada of
        etqMilimetros:         Result := ((ValorEntrada/10) / CInchCM);
        etqDots:               Result := ValorEntrada / DotsPI;
        etqDecimoDeMilimetros: Result := ((ValorEntrada/100) / CInchCM);
      end;
    end;

    etqDots:  // pixels
    begin
      case UnidadeEntrada of
        etqMilimetros:         Result := (ValorEntrada * DotsMM);
        etqPolegadas:          Result := (ValorEntrada * DotsPI);
        etqDecimoDeMilimetros: Result := ((ValorEntrada/10) * DotsMM);
      end;
    end;
  end;
end;

{ TACBrETQCmdList }

function TACBrETQCmdList.Add(const S: string): Integer;
begin
  if NaoEstaVazio(S) then
    Result := inherited Add(S)
  else
    Result := -1;
end;

procedure TACBrETQCmdList.Insert(Index: Integer; const S: string);
begin
  if NaoEstaVazio(S) then
    inherited Insert(Index, S);
end;

{ TACBrETQ }

constructor TACBrETQ.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fsAtivo    := False;
  fsModelo   := etqNenhum;
  fsListaCmd := TACBrETQCmdList.Create;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create(Self);  // O dono é o proprio componente
  fsDevice.Name := 'ACBrDevice';         // Apenas para aparecer no Object Inspector
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent(True);        // Para gravar no DFM/XFM
  {$ENDIF}
  fsDevice.Porta := 'LPT1';
  fsDevice.Serial.DeadlockTimeout := 1000;
  {$IfDef MSWINDOWS}
  fsDevice.WinUSB.HardwareType := htLabelPrinter;
  {$EndIf}

  { Instanciando fsETQ com modelo Generico (TACBrETQClass) }
  fsETQ := TACBrETQClass.create(Self);

  fsArqLOG          := '';
  fsOnGravarLog     := Nil;
  fsEtqFinalizada   := False;
  fsEtqInicializada := False;
  fsCopias          := 1;
  fsAvancoEtq       := 0;
  fsMargemEsquerda  := 0;
end;

destructor TACBrETQ.Destroy;
begin
  Desativar;

  if Assigned(fsETQ) then
    FreeAndNil(fsETQ);

  FreeAndNil(fsListaCmd);
  FreeAndNil(fsDevice);

  inherited Destroy;
end;

function TACBrETQ.GetModeloStr: String;
begin
  Result := ACBrStr(fsETQ.ModeloStr);
end;

function TACBrETQ.GetLimparMemoria: Boolean;
begin
  Result := fsETQ.LimparMemoria;
end;

function TACBrETQ.GetGuilhotina: Boolean;
begin
  Result := fsETQ.Guilhotina;
end;

function TACBrETQ.GetBackFeed: TACBrETQBackFeed;
begin
  Result := fsETQ.BackFeed;
end;

function TACBrETQ.GetDeteccaoEtiqueta: TACBrETQDeteccaoEtiqueta;
begin
  Result := fsETQ.DeteccaoEtiqueta;
end;

function TACBrETQ.GetOrigem: TACBrETQOrigem;
begin
  Result := fsETQ.Origem;
end;

function TACBrETQ.GetPaginaDeCodigo: TACBrETQPaginaCodigo;
begin
  Result := fsETQ.PaginaDeCodigo;
end;

function TACBrETQ.GetUnidade: TACBrETQUnidade;
begin
  Result := fsETQ.Unidade;
end;

function TACBrETQ.GetTemperatura: Integer;
begin
  Result := fsETQ.Temperatura;
end;

function TACBrETQ.GetVelocidade: Integer;
begin
  Result := fsETQ.Velocidade;
end;

function TACBrETQ.GetPorta: String;
begin
  Result := fsDevice.Porta;
end;

function TACBrETQ.GetDPI: TACBrETQDPI;
begin
   Result := fsETQ.DPI;
end;

function TACBrETQ.GetAvanco: Integer;
begin
  Result := fsETQ.Avanco;
end;

procedure TACBrETQ.SetPaginaDeCodigo(AValue: TACBrETQPaginaCodigo);
begin
  fsETQ.PaginaDeCodigo := AValue;
end;

procedure TACBrETQ.SetDeteccaoEtiqueta(AValue: TACBrETQDeteccaoEtiqueta);
begin
  fsETQ.DeteccaoEtiqueta := AValue;
end;

function TACBrETQ.GetNumeroPaginaDeCodigo(APagCod: TACBrETQPaginaCodigo): word;
begin
  case APagCod of
    pce437: Result := 437;
    pce850: Result := 850;
    pce852: Result := 852;
    pce860: Result := 860;
    pce1250: Result := 1250;
    pce1252: Result := 1252;
  else
    Result := 0;
  end;
end;

function TACBrETQ.CodificarPaginaDeCodigo(const ATexto: AnsiString): AnsiString;
var
  NumPagCod: word;
begin
  NumPagCod := GetNumeroPaginaDeCodigo(PaginaDeCodigo);
  //GravarLog('CodificarPaginaDeCodigo: '+IntToStr(NumPagCod) );

  if (NumPagCod > 0) then
  begin
    {$IfDef MSWINDOWS}
    Result := TranslateString(ACBrStrToAnsi(ATexto), NumPagCod)
    {$Else}
    Result := TranslateString(ATexto, NumPagCod)
    {$EndIf}
  end
  else
    Result := TiraAcentos(ATexto);
end;

procedure TACBrETQ.ComandoGravaRFIDASCII(aValue: String);
var
wCmd:AnsiString;

begin
  GravarLog('- GravarRFIDASCII : ' +aValue );
  wCmd := fsEtq.ComandoGravaRFIDASCII(aValue);
  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ComandoGravaRFIDHexaDecimal(aValue: String);
var
wCmd:AnsiString;
begin
  GravarLog('- GravarRFIDHEXADecimal : ' +aValue );
  wCmd := fsEtq.ComandoGravaRFIDHexaDecimal(aValue);
  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.SetUnidade(const AValue: TACBrETQUnidade);
begin
  fsETQ.Unidade := AValue;
end;

procedure TACBrETQ.SetModelo(const Value: TACBrETQModelo);
var
  wUnidade: TACBrETQUnidade;
  wDPI: TACBrETQDPI;
  wLimparMemoria: Boolean;
  wTemperatura, wVelocidade, wAvanco: Integer;
begin
  GravarLog('- SetModelo. '+GetEnumName(TypeInfo(TACBrETQModelo), Integer(Value)));

  if (fsModelo = Value) then
    Exit;

  wTemperatura   := Temperatura;
  wLimparMemoria := LimparMemoria;
  wAvanco        := Avanco;
  wUnidade       := Unidade;
  wDPI           := DPI;
  wVelocidade    := Velocidade;

  if fsAtivo then
    raise Exception.Create(ACBrStr('Não é possível mudar o Modelo com ACBrETQ Ativo'));

  FreeAndNil(fsETQ);

  { Instanciando uma nova classe de acordo com fsModelo }
  case Value of
    etqPpla:          fsETQ := TACBrETQPpla.Create(Self);
    etqPplb, etqEpl2: fsETQ := TACBrETQEpl2.Create(Self);  // EPL2 = PPLB
    etqZPLII:         fsETQ := TACBrETQZplII.Create(Self);
    etqEscLabel:      fsETQ := TACBrETQEscLabel.Create(Self);
  else
    fsETQ := TACBrETQClass.Create(Self);
  end;

  Temperatura  := wTemperatura;
  LimparMemoria:= wLimparMemoria;
  Avanco       := wAvanco;
  Unidade      := wUnidade;
  DPI          := wDPI;
  Velocidade   := wVelocidade;
  fsModelo     := Value;
end;

procedure TACBrETQ.SetBackFeed(AValue: TACBrETQBackFeed);
begin
  fsETQ.BackFeed := AValue;
end;

procedure TACBrETQ.SetTemperatura(const Value: Integer);
begin
  fsETQ.Temperatura := Value;
end;

procedure TACBrETQ.SetVelocidade(const Value: Integer);
begin
  fsETQ.Velocidade := Value;
end;

procedure TACBrETQ.SetDPI(const AValue: TACBrETQDPI);
begin
  fsETQ.DPI := AValue;
end;

procedure TACBrETQ.SetLimparMemoria(const Value: Boolean);
begin
  fsETQ.LimparMemoria := Value;
end;

procedure TACBrETQ.SetPorta(const Value: String);
begin
  fsDevice.Porta := Value;
end;

procedure TACBrETQ.SetOrigem(AValue: TACBrETQOrigem);
begin
  fsETQ.Origem := AValue;
end;

procedure TACBrETQ.SetAvanco(const AValue: Integer);
begin
  fsETQ.Avanco := AValue;
end;

procedure TACBrETQ.SetGuilhotina(AValue: Boolean);
begin
  fsETQ.Guilhotina := AValue;
end;

procedure TACBrETQ.SetAtivo(const Value: Boolean);
begin
  if Value then
    Ativar
  else
    Desativar;
end;

procedure TACBrETQ.AtivarSeNecessario;
begin
  if (not Ativo) then
    Ativar;
end;

procedure TACBrETQ.Ativar;
{$IfDef MSWINDOWS}
var
  TipoHardware: TACBrUSBHardwareType;
  ProtocoloACBr: Integer;
{$EndIf}
begin
  if fsAtivo then
    Exit;

  GravarLog(sLineBreak + StringOfChar('-', 80) + sLineBreak +
    'ATIVAR - ' + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) +
    ' - Modelo: ' + ModeloStr +
    ' - Porta: '  + fsDevice.Porta +
    ' - Device: ' + fsDevice.DeviceToString(False) +
    sLineBreak + StringOfChar('-', 80) + sLineBreak);

  if (fsDevice.Porta <> '') then
  begin
    {$IfDef MSWINDOWS}
     ProtocoloACBr := 0;
     TipoHardware := htLabelPrinter;
     fsDevice.DetectarTipoEProtocoloDispositivoUSB(TipoHardware, ProtocoloACBr);

     if not (TipoHardware in [htUnknown, htLabelPrinter]) then
       raise Exception.Create(ACBrStr('Porta: '+fsDevice.Porta+' não está conectada a uma Impressora de Etiquetas'));

     if (ProtocoloACBr > 0) then
       Modelo := TACBrETQModelo(ProtocoloACBr);
    {$EndIf}

    fsDevice.Ativar;
  end;

  fsAtivo           := True;
  fsEtqFinalizada   := False;
  fsEtqInicializada := False;
  fsCopias          := 1;
  fsAvancoEtq       := 0;
end;

procedure TACBrETQ.Desativar;
begin
  if (not fsAtivo) then
    Exit;

  GravarLog('DESATIVAR');

  if (fsDevice.Porta <> '') then
    fsDevice.Desativar;

  fsAtivo := False;
end;

procedure TACBrETQ.IniciarEtiqueta;
var
  wCmd: AnsiString;
begin
  GravarLog('- IniciarEtiqueta');

  AtivarSeNecessario;

  wCmd := fsETQ.ComandosIniciarEtiqueta;

  if (not (fsEtqInicializada or fsEtqFinalizada)) then
    fsListaCmd.Insert(0, wCmd)       //Se Etiqueta não foi iniciada, comandos incluídos no início
  else
  begin
    if fsEtqFinalizada then
      fsListaCmd.Add(fsETQ.ComandosFinalizarEtiqueta(fsCopias, fsAvancoEtq));

    fsListaCmd.Add(wCmd);    //Se Etiqueta foi iniciada, comandos são concatenados
  end;

  fsEtqInicializada := True;
  fsEtqFinalizada   := False;
  fsCopias          := 1;
  fsAvancoEtq       := 0;
end;

procedure TACBrETQ.FinalizarEtiqueta(Copias: Integer = 1; AvancoEtq: Integer = 0);
begin
  GravarLog('- FinalizarEtiqueta: Copias:'+IntToStr(Copias)+', AvancoEtq:'+IntToStr(AvancoEtq));

  if not fsEtqInicializada then
    IniciarEtiqueta;

  fsEtqInicializada := False;
  fsEtqFinalizada   := True;
  fsCopias          := Copias;
  fsAvancoEtq       := AvancoEtq;
end;

procedure TACBrETQ.GravarLog(aString: AnsiString; Traduz: Boolean);
var
  wTratado: Boolean;
begin
  wTratado := False;

  if Traduz then
    AString := TranslateUnprintable(AString);

  if Assigned(fsOnGravarLog) then
    fsOnGravarLog( AString, wTratado);

  if not wTratado then
    WriteLog(fsArqLOG, '-- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', Now) + ' ' + AString);
end;

procedure TACBrETQ.Imprimir(Copias: Integer; AvancoEtq: Integer);
var
  wCmd: AnsiString;

begin
  GravarLog('- Imprimir. Copias:'+IntToStr(Copias)+', AvancoEtq:'+IntToStr(AvancoEtq));

  AtivarSeNecessario;

  try
    // Verifica se é necessário IniciarEtiqueta. Só será utilizado quando
    //  o comando não foi enviado manualmente
    if (not (fsEtqInicializada or fsEtqFinalizada)) then
      IniciarEtiqueta;

    // Verifica se ficou um bloco de etiquetas sem ser Finalizado
    if (not fsEtqFinalizada) then
      FinalizarEtiqueta(Copias, AvancoEtq)
    else
    begin
      if Copias > 1 then
        fsCopias := Copias;

      if AvancoEtq > 0 then
        fsAvancoEtq := AvancoEtq;
    end;

    wCmd := fsETQ.ComandosFinalizarEtiqueta(fsCopias, fsAvancoEtq);
    fsListaCmd.Add(wCmd);

    if LimparMemoria then
    begin
      wCmd := fsETQ.ComandoLimparMemoria;
      fsListaCmd.Add(wCmd);
    end;

    wCmd := fsETQ.TratarComandoAntesDeEnviar(ListaCmd.Text);
    GravarLog(wCmd, True);


    fsDevice.EnviaString(wCmd);
  finally
    fsListaCmd.Clear;
    fsEtqInicializada := False;
    fsEtqFinalizada   := False;
    fsCopias          := 1;
    fsAvancoEtq       := 0;
  end;
end;

function TACBrETQ.GerarStreamBase64(Copias: Integer; AvancoEtq: Integer): AnsiString;
var
  wCmd: AnsiString;
  SLConteudoImpressao : TStringList;
  LStream : TMemoryStream;
begin
  GravarLog('- Gerar Stream. Copias:'+IntToStr(Copias)+', AvancoEtq:'+IntToStr(AvancoEtq));

  AtivarSeNecessario;
  LStream := TMemoryStream.Create;
  SLConteudoImpressao := TStringList.Create;

  try
    // Verifica se é necessário IniciarEtiqueta. Só será utilizado quando
    //  o comando não foi enviado manualmente
    if (not (fsEtqInicializada or fsEtqFinalizada)) then
      IniciarEtiqueta;

    // Verifica se ficou um bloco de etiquetas sem ser Finalizado
    if (not fsEtqFinalizada) then
      FinalizarEtiqueta(Copias, AvancoEtq)
    else
    begin
      if Copias > 1 then
        fsCopias := Copias;

      if AvancoEtq > 0 then
        fsAvancoEtq := AvancoEtq;
    end;

    wCmd := fsETQ.ComandosFinalizarEtiqueta(fsCopias, fsAvancoEtq);
    fsListaCmd.Add(wCmd);

    if LimparMemoria then
    begin
      wCmd := fsETQ.ComandoLimparMemoria;
      fsListaCmd.Add(wCmd);
    end;

    wCmd := fsETQ.TratarComandoAntesDeEnviar(ListaCmd.Text);
    GravarLog(wCmd, True);

    SLConteudoImpressao.Text:= wCmd;
    SLConteudoImpressao.SaveToStream( LStream );
    LStream.Position := 0;
    result := EncodeBase64(ReadStrFromStream(LStream, LStream.Size));
  finally
    fsListaCmd.Clear;
    fsEtqInicializada := False;
    fsEtqFinalizada   := False;
    fsCopias          := 1;
    fsAvancoEtq       := 0;
    LStream.free;
    SLConteudoImpressao.Free;
  end;

end;

procedure TACBrETQ.ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte,
  MultiplicadorH, MultiplicadorV, Vertical, Horizontal: Integer;
  const Texto: String; SubFonte: Integer; ImprimirReverso: Boolean);
var
  cFonte: Char;
begin
  if Fonte < 10 then
    cFonte := chr(48 + Fonte)  // '0'..'9'
  else
    cFonte := chr(55 + Fonte); // 'A'..'Z'

  ImprimirTexto(Orientacao, cFonte, MultiplicadorH, MultiplicadorV,
    Vertical, Horizontal, Texto, SubFonte, ImprimirReverso);
end;

procedure TACBrETQ.ImprimirTexto(Orientacao: TACBrETQOrientacao; const Fonte: String;
  MultiplicadorH, MultiplicadorV, Vertical, Horizontal: Integer; const Texto: String;
  SubFonte: Integer; ImprimirReverso: Boolean);
var
  wCmd: AnsiString;
begin
  GravarLog('- ImprimirTexto:'+
            '  Orientacao:'+GetEnumName(TypeInfo(TACBrETQOrientacao), Integer(Orientacao))+
            ', Fonte:'+Fonte+
            ', MultiplicadorH:'+IntToStr(MultiplicadorH)+
            ', MultiplicadorV:'+IntToStr(MultiplicadorV)+
            ', Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', Texto:'+Texto+
            ', SubFonte:'+IntToStr(SubFonte)+
            ', ImprimirReverso:'+BoolToStr(ImprimirReverso, True));

  wCmd := fsETQ.ComandoImprimirTexto( Orientacao,
                                      Fonte, MultiplicadorH, MultiplicadorV,
                                      Vertical, (Horizontal+MargemEsquerda),
                                      CodificarPaginaDeCodigo(Texto),
                                      SubFonte,
                                      ImprimirReverso);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ImprimirBarras(Orientacao: TACBrETQOrientacao;
  TipoBarras: TACBrTipoCodBarra; LarguraBarraLarga, LarguraBarraFina, Vertical,
  Horizontal: Integer; const Texto: String; AlturaCodBarras: Integer;
  ExibeCodigo: TACBrETQBarraExibeCodigo);
var
  TipoBarrasStr: String;
begin
  TipoBarrasStr := fsETQ.ConverterTipoBarras(TipoBarras);
  if (TipoBarrasStr = '') then
     raise Exception.Create(ACBrStr('Código '+
                            GetEnumName(TypeInfo(TACBrTipoCodBarra), Integer(TipoBarras))+
                            'não suportado por: '+fsETQ.ModeloStr));

  ImprimirBarras(Orientacao, TipoBarrasStr, LarguraBarraLarga,
    LarguraBarraFina, Vertical, Horizontal, Texto, AlturaCodBarras, ExibeCodigo);
end;

procedure TACBrETQ.ImprimirBarras(Orientacao: TACBrETQOrientacao; const TipoBarras,
  LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
  const Texto: String; AlturaCodBarras: Integer; ExibeCodigo: TACBrETQBarraExibeCodigo
  );

  function StrParamToInt(AParam: String): Integer;
  var
    cParam: Char;
  begin
    if StrIsNumber(AParam) then
      Result := StrToIntDef(AParam,0)
    else
    begin
      cParam := PadLeft(UpperCase(AParam),1,'A')[1];
      Result := ord(cParam)-55;  // Ex: 'A' = 65; 65-55 = 10
    end;
  end;

begin
  ImprimirBarras( Orientacao, TipoBarras,
                  StrParamToInt(LarguraBarraLarga),
                  StrParamToInt(LarguraBarraFina),
                  Vertical, Horizontal, Texto, AlturaCodBarras, ExibeCodigo);
end;

procedure TACBrETQ.ImprimirQRCode(Vertical, Horizontal: Integer;
  const Texto: String; LarguraModulo: Integer; ErrorLevel: Integer;
  Tipo: Integer);
var
  wCmd: AnsiString;
begin
  Tipo := Min(Max(Tipo,1),2);
  LarguraModulo := Max(1,LarguraModulo);

  GravarLog('- ImprimirQRCode:'+
            '  Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', Texto:'+Texto+
            ', LarguraModulo:'+IntToStr(LarguraModulo)+
            ', ErrorLevel:'+IntToStr(ErrorLevel)+
            ', Tipo:'+IntToStr(Tipo));

  wCmd := fsETQ.ComandoImprimirQRCode( Vertical, (Horizontal+MargemEsquerda),
                                       Texto, LarguraModulo, ErrorLevel, Tipo);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ImprimirBarras(Orientacao: TACBrETQOrientacao;
  const TipoBarras: String; LarguraBarraLarga, LarguraBarraFina, Vertical,
  Horizontal: Integer; const Texto: String; AlturaCodBarras: Integer;
  ExibeCodigo: TACBrETQBarraExibeCodigo);
var
  wCmd: AnsiString;
begin
  GravarLog('- ImprimirBarras:'+
            '  Orientacao:'+GetEnumName(TypeInfo(TACBrETQOrientacao), Integer(Orientacao))+
            ', TipoBarras:'+TipoBarras+
            ', LarguraBarraLarga:'+IntToStr(LarguraBarraLarga)+
            ', LarguraBarraFina:'+IntToStr(LarguraBarraFina)+
            ', Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', Texto:'+Texto+
            ', AlturaCodBarras:'+IntToStr(AlturaCodBarras)+
            ', ExibeCodigo:'+GetEnumName(TypeInfo(TACBrETQBarraExibeCodigo), Integer(ExibeCodigo)));

  wCmd := fsETQ.ComandoImprimirBarras(Orientacao, TipoBarras, LarguraBarraLarga,
    LarguraBarraFina, Vertical, (Horizontal+MargemEsquerda), Texto,
    AlturaCodBarras, ExibeCodigo);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ImprimirLinha(Vertical, Horizontal, Largura, Altura: Integer);
var
  wCmd: AnsiString;
begin
  GravarLog('- ImprimirLinha:'+
            '  Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', Largura:'+IntToStr(Largura)+
            ', Altura:'+IntToStr(Altura));

  wCmd := fsETQ.ComandoImprimirLinha(Vertical, (Horizontal+MargemEsquerda), Largura, Altura);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
  EspessuraVertical, EspessuraHorizontal: Integer; Canto: Integer);
var
  wCmd: AnsiString;
begin
  GravarLog('- ImprimirCaixa:'+
            '  Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', Largura:'+IntToStr(Largura)+
            ', Altura:'+IntToStr(Altura)+
            ', EspessuraVertical:'+IntToStr(EspessuraVertical)+
            ', EspessuraHorizontal:'+IntToStr(EspessuraHorizontal)+
            ', Canto:'+IntToStr(Canto));

  wCmd := fsETQ.ComandoImprimirCaixa(Vertical, (Horizontal+MargemEsquerda),
       Largura, Altura, EspessuraVertical, EspessuraHorizontal, Canto);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.ImprimirImagem(MultiplicadorImagem, Vertical,
  Horizontal: Integer; const NomeImagem: String);
var
  wCmd: AnsiString;
begin
  GravarLog('- ImprimirImagem:'+
            '  MultiplicadorImagem:'+IntToStr(MultiplicadorImagem)+
            ', Vertical:'+IntToStr(Vertical)+
            ', Horizontal:'+IntToStr(Horizontal)+
            ', NomeImagem:'+NomeImagem);

  wCmd := fsETQ.ComandoImprimirImagem(MultiplicadorImagem, Vertical,
     (Horizontal+MargemEsquerda), NomeImagem);

  fsListaCmd.Add(wCmd);
end;

procedure TACBrETQ.CarregarImagem(aStream: TStream; var NomeImagem: String;
  Flipped: Boolean; const Tipo: String);
var
  wCmd: AnsiString;
begin
  GravarLog('- CarregarImagem:'+
            '  AStream.Size: '+IntToStr(aStream.Size)+
            ', NomeImagem: '+NomeImagem+
            ', Flipped: '+BoolToStr(Flipped, True)+
            ', Tipo: '+Tipo);

  AtivarSeNecessario;

  wCmd := fsETQ.ComandoCarregarImagem(aStream, NomeImagem, Flipped, Tipo);
  GravarLog(wCmd, True);
  fsDevice.EnviaString(wCmd);
end;

procedure TACBrETQ.CarregarImagem(const ArquivoImagem: String;
  var NomeImagem: String; Flipped: Boolean);
var
  wMS: TMemoryStream;
  wTipo: AnsiString;
begin
  if (not FileExists(ArquivoImagem)) then
    raise Exception.Create(ACBrStr('Arquivo ' + ArquivoImagem + ' não encontrado'));

  wTipo := ExtractFileExt(ArquivoImagem);
  wMS   := TMemoryStream.Create;
  try
    wMS.LoadFromFile(ArquivoImagem);
    CarregarImagem(wMS, NomeImagem, Flipped, wTipo);
  finally
    wMS.Free;
  end;
end;

procedure TACBrETQ.ApagarImagem(const NomeImagem: String);
var
  wCmd: AnsiString;
begin
  GravarLog('- ApagarImagem:'+NomeImagem);
  AtivarSeNecessario;

  wCmd := fsETQ.ComandoApagarImagem(NomeImagem);
  GravarLog(wCmd, True);
  fsDevice.EnviaString(wCmd);
end;

procedure TACBrETQ.DefinirCor(FrenteCor: Cardinal; FrenteOpacidade: Byte;
  FundoCor: Cardinal; FundoOpacidade: Byte);
begin
  GravarLog('- DefinirCor:'+
            '  Frente:'+IntToHex(FrenteCor, 6)+
            ', FrenteOpacidade:'+IntToStr(FrenteOpacidade)+
            '  Fundo:'+IntToHex(FundoCor, 6)+
            ', FundoOpacidade:'+IntToStr(FundoOpacidade) );

  fsETQ.CorFrente.Color := FrenteCor;
  fsETQ.CorFrente.Opacidade := FrenteOpacidade;
  fsETQ.CorFundo.Color := FundoCor;
  fsETQ.CorFundo.Opacidade := FundoOpacidade;
end;

procedure TACBrETQ.DefinirCor(FrenteR, FrenteG, FrenteB, FrenteOpacidade: Byte;
  FundoR, FundoG, FundoB, FundoOpacidade: Byte);
begin
  GravarLog('- DefinirCor:'+
            '  FrenteR:'+IntToStr(FrenteR)+
            ', FrenteG:'+IntToStr(FrenteG)+
            ', FrenteB:'+IntToStr(FrenteB)+
            ', FrenteOpacidade:'+IntToStr(FrenteOpacidade)+
            ', FundoR:'+IntToStr(FundoR)+
            ', FundoG:'+IntToStr(FundoG)+
            ', FundoB:'+IntToStr(FundoB)+
            ', FundoOpacidade:'+IntToStr(FundoOpacidade) );

  fsETQ.CorFrente.R := FrenteR;
  fsETQ.CorFrente.G := FrenteG;
  fsETQ.CorFrente.B := FrenteB;
  fsETQ.CorFrente.Opacidade := FrenteOpacidade;
  fsETQ.CorFundo.R := FundoR;
  fsETQ.CorFundo.G := FundoG;
  fsETQ.CorFundo.B := FundoB;
  fsETQ.CorFundo.Opacidade := FundoOpacidade;
end;

procedure TACBrETQ.DefinirCorPadrao;
begin
  DefinirCor($000000, 255, $FFFFFF, 0);
end;

procedure TACBrETQ.DefinirDimensoes(Largura, Altura: Integer;
  EspacoEntreEtiquetas: Integer; EspacoEsquerda: Integer);
begin
  GravarLog('- DefinirDimensoes:'+
            '  Largura:'+IntToStr(Largura)+
            ', Altura:'+IntToStr(Altura)+
            ', EspacoEntreEtiquetas:'+IntToStr(EspacoEntreEtiquetas)+
            ', EspacoEsquerda:'+IntToStr(EspacoEsquerda) );

  fsETQ.Dimensoes.Largura := Largura;
  fsETQ.Dimensoes.Altura := Altura;
  fsETQ.Dimensoes.EspacoEntreEtiquetas := EspacoEntreEtiquetas;
  fsETQ.Dimensoes.EspacoEsquerda := EspacoEsquerda;
end;

end.



