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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
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
 {$DEFINE Device_Stream}
{$ENDIF}

unit ACBrDevice ;

interface

uses
  Classes, SysUtils, math, contnrs,
  synaser, blcksock,
  {$IfDef MSWINDOWS}
    ACBrWinUSBDevice,
  {$EndIf}
  {$IfDef COMPILER6_UP}
    {$IfDef MSWINDOWS}
      Windows,
    {$EndIf}
    DateUtils, Types, StrUtils,
  {$Else}
     Windows, ACBrD5,
  {$EndIf}
  {$IfNDef NOGUI}
    {$If defined(VisualCLX)}
       QForms,
    {$ElseIf defined(FMX)}
       FMX.Forms,
    {$Else}
       Forms,
    {$IfEnd}
  {$EndIf}
  {$If defined(VisualCLX)}
     QPrinters,
  {$ElseIf defined(FMX)}
     FMX.Printer,
     {$IfDef MSWINDOWS}
        Winapi.WinSpool,
     {$EndIf}
  {$ElseIf defined(FPC)}
     Printers,
     {$IfNDef NOGUI}
      OSPrinters,
     {$EndIf}
  {$ELSEIF DEFINED(POSIX)}
    Posix.Unistd,
  {$Else}
     Printers, WinSpool,
  {$IfEnd}
  ACBrConsts;

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

TACBrECFTipoBilhete = (tbRodIntermun,   //0x30 - Rodoviário Intermunicipal
                       tbFerIntermun,   //0x31 - Ferroviário Intermunicipal
                       tbAquIntermun,   //0x32 - Aquaviário Intermunicipal
                       tbRodInterest,   //0x33 - Rodoviário Interestadual
                       tbFerInterest,   //0x34 - Ferroviário Interestadual
                       tbAquInterest,   //0x35 - Aquaviário Interestadual
                       tbRodInternac,   //0x36 - Rodoviário Internacional
                       tbFerInternac,   //0x37 - Ferroviário Internacional
                       tbAquInternac    //0x38 - Aquaviário Internacional
                       ) ;

TACBrECFEstadoSet = set of TACBrECFEstado ;

TACBrGAVAberturaAntecipada = ( aaIgnorar , aaException, aaAguardar ) ;

TACBrETQUnidade = (etqMilimetros, etqPolegadas, etqDots, etqDecimoDeMilimetros );

TACBrETQDPI = (dpi203, dpi300, dpi600) ;

TACBrETQOrientacao = (orNormal, or270, or180, or90);

TACBrETQBarraExibeCodigo = (becPadrao, becSIM, becNAO);

TACBrETQBackFeed = (bfNone, bfOn, bfOff);

TACBrETQOrigem = (ogNone, ogTop, ogBottom);

{Criando o tipo enumerado para tipos de código de barras }
TACBrTipoCodBarra =  ( barEAN13, barEAN8, barSTANDARD, barINTERLEAVED,
                       barCODE128, barCODE39, barCODE93, barUPCA,
                       barCODABAR, barMSI, barCODE11 );

{Criando tipo enumerado para a finalidade do arquivo MFD}
TACBrECFFinalizaArqMFD = (finMF, finMFD, finTDM, finRZ, finRFD, finNFP,
                          finNFPTDM, finSintegra, finSPED);

{ Criando tipo enumerado para o tipo do contador }
TACBrECFTipoContador = (tpcCOO, tpcCRZ);

TACBrECFTipoDownloadMFD = (tdmfdTotal, tdmfdData, tdmfdCOO);

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

TACBrDeviceHookAtivar = procedure(const APort: String; Params: String) of object;
TACBrDeviceHookDesativar = procedure(const APort: String) of object;
TACBrDeviceHookEnviaString = procedure(const cmd: AnsiString) of object;
TACBrDeviceHookLeString = procedure(const NumBytes, ATimeOut: Integer; var Retorno: AnsiString) of object;

{ ACBrDevice é um componente apenas para usarmos o recurso de AutoExpand do
  ObjectInspector para SubComponentes, poderia ser uma Classe }

TACBrECFConfigBarras = class(TPersistent)
  private
    FMostrarCodigo: Boolean;
    FAltura: Integer;
    FLarguraLinha: Integer;
    FMargem: Integer;
  published
    property MostrarCodigo: Boolean read FMostrarCodigo write FMostrarCodigo;
    property LarguraLinha: Integer read FLarguraLinha write FLarguraLinha;
    property Altura: Integer read FAltura write FAltura;
    property Margem: Integer read FMargem write FMargem;
end;

{ TACBrTag }

TACBrTag = class
  private
    FAjuda: String;
    FEhBloco: Boolean;
    FNome: String;
    FSequencia: Integer;
    function GetTamanho: Integer;
    procedure SetAjuda(const AValue: String);
    procedure SetNome(const AValue: String);
  public
    constructor Create;

    property Sequencia: Integer read FSequencia write FSequencia;
    property EhBloco: Boolean read FEhBloco write FEhBloco;
    property Nome: String read FNome write SetNome;
    property Ajuda: String read FAjuda write SetAjuda;
    property Tamanho: Integer read GetTamanho;
end;

{ Lista de Objetos do tipo TACBrTag }

{ TACBrTags }

TACBrTags = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrTag);
    function GetObject (Index: Integer): TACBrTag;
    procedure Insert (Index: Integer; Obj: TACBrTag);
  public
    function New: TACBrTag;
    function Add (Obj: TACBrTag): Integer;
    property Objects [Index: Integer]: TACBrTag
      read GetObject write SetObject; default;

    function AcharTag( NomeTag: String): TACBrTag;
end;

TACBrTagOnTraduzirTag = procedure(const ATag: AnsiString;
  var TagTraduzida: AnsiString) of object ;
TACBrTagOnTraduzirTagBloco = procedure(const ATag, ConteudoBloco: AnsiString;
  var BlocoTraduzido: AnsiString) of object ;

{ TACBrTagProcessor }

TACBrTagProcessor = class
  private
    FIgnorarTags: Boolean;
    FOnTraduzirTag: TACBrTagOnTraduzirTag;
    FOnTraduzirTagBloco: TACBrTagOnTraduzirTagBloco;
    FTags: TACBrTags;
    FTraduzirTags: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function DecodificarTagsFormatacao(const AString: AnsiString): AnsiString;
    function TraduzirTag(const ATag: AnsiString): AnsiString;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString): AnsiString;

    procedure RetornarTags( AStringList: TStrings; IncluiAjuda: Boolean = True);

    procedure AddTags(ArrayTags: array of String; ArrayHelpTags: array of String;
       TagEhBloco: Boolean);

    property Tags: TACBrTags read FTags;
    property IgnorarTags: Boolean read FIgnorarTags write FIgnorarTags;
    property TraduzirTags: Boolean read FTraduzirTags write FTraduzirTags;

    property OnTraduzirTag: TACBrTagOnTraduzirTag
      read FOnTraduzirTag write FOnTraduzirTag;
    property OnTraduzirTagBloco: TACBrTagOnTraduzirTagBloco
      read FOnTraduzirTagBloco write FOnTraduzirTagBloco;

end;

TACBrDeviceType = (dtNenhum, dtFile, dtSerial, dtParallel, dtTCP, dtRawPrinter, dtHook, dtUSB);

{ TACBrDevice }

TACBrDevice = class( TComponent )
  private
    fsSerial : TBlockSerial ;
    fsSocket : TBlockSocket ;
    {$IfDef MSWINDOWS}
     fsWinUSB: TACBrUSBWinDeviceAPI;
    {$EndIf}

    fsPosImp: TPoint;
    fsHardFlow: Boolean;
    fsHookAtivar: TACBrDeviceHookAtivar;
    fsHookDesativar: TACBrDeviceHookDesativar;
    fsHookEnviaString: TACBrDeviceHookEnviaString;
    fsHookLeString: TACBrDeviceHookLeString;
    fsSoftFlow: Boolean;
    fsParity: Char ;
    fsData : Integer;
    fsBaud: Integer;
    fsStop: Integer ;
    fsPorta: String;
    fsNomeDocumento: String;
    fsDeviceType: TACBrDeviceType;
    fsTimeOut: Integer ;
    fsAtivo: Boolean;

    fsHandShake: TACBrHandShake;
    fsSendBytesCount: Integer;
    fsSendBytesInterval: Integer;
    fProcessMessages: Boolean;

    fTCPIP: String;
    fTCPPort: String;
    fsArqLOG: String;

    procedure ConfiguraSerial ;
    procedure EnviaStringSerial(const AString: AnsiString) ;
    procedure EnviaStringArquivo(const AString: AnsiString) ;
    procedure EnviaStringTCP(const AString: AnsiString);
    {$IfDef MSWINDOWS}
    procedure EnviaStringWinUSB(const AString: AnsiString);
    {$EndIf}
    procedure EnviaStringRaw(const AString: AnsiString);
    procedure EnviaStringHook(const AString: AnsiString);
    function GetNomeDocumento: String;
    function GetParamsString: String;
    function GetPrinterRawIndex: Integer;
    function GetPrinterFileName: String;
    function GetTimeOutMilissegundos: Integer;
    procedure SetArqLOG(AValue: String);

    {$IFDEF ThreadEnviaLPT}
    procedure EnviaStringThread( AString : AnsiString ) ;
    {$ENDIF}

    {$If DEFINED(FMX)}
    function GetLabelPrinterIndex(APrinterName: String): Integer;
    {$IfEnd}

    procedure SetBaud(const Value: Integer);
    procedure SetData(const Value: Integer);
    procedure SetDeviceType(AValue: TACBrDeviceType);
    procedure SetNomeDocumento(const AValue: String);
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
    procedure SetTimeOutMilissegundos(AValue: Integer);
    procedure TryCloseSocket;
  public
    property Serial: TBlockSerial read fsSerial;
    property Socket: TBlockSocket read fsSocket;
    {$IfDef MSWINDOWS}
     property WinUSB: TACBrUSBWinDeviceAPI read fsWinUSB;
    {$EndIf}
    property PosImp: TPoint read fsPosImp;

    procedure Assign(Source: TPersistent); override;

    property Ativo : Boolean read fsAtivo write SetAtivo ;

    property Porta: String  read fsPorta write SetPorta ;
    property DeviceType: TACBrDeviceType read fsDeviceType write SetDeviceType;
    property TimeOut : Integer read fsTimeOut write SetTimeOut ;
    property TimeOutMilissegundos: Integer read GetTimeOutMilissegundos
      write SetTimeOutMilissegundos;

    Function EmLinha( lTimeOut : Integer = 1) : Boolean  ;
    function DeduzirTipoPorta(const APorta: String): TACBrDeviceType;

    property ParamsString : String read GetParamsString write SetParamsString ;

    constructor Create(AOwner: TComponent); override ;
    destructor Destroy ; override ;

    procedure Ativar ;
    procedure Desativar ;
    Procedure EnviaString( const AString : AnsiString ) ;
    Procedure EnviaByte( const AByte : Byte ) ;
    Function LeString( ATimeOut: Integer=0; NumBytes: Integer=0; const Terminador: AnsiString = '' ): String;
    Function LeByte( ATimeOut: Integer=0 ): Byte;
    procedure Limpar ;
    Function BytesParaLer: Integer;

    Procedure ImprimePos(const Linha, Coluna : Integer; const AString: AnsiString) ;
    Procedure Eject ;

    Procedure SetDefaultValues ;

    Function IsSerialPort : Boolean;
    Function IsParallelPort: Boolean;
    Function IsTXTFilePort: Boolean;
    Function IsDLLPort: Boolean;
    Function IsUSBPort: Boolean;
    Function IsTCPPort: Boolean;
    Function IsRawPort: Boolean;

    procedure AcharPortasSeriais( const AStringList : TStrings;
       UltimaPorta : Integer = 64 ) ;
    {$IfDef MSWINDOWS}
    procedure DetectarTipoEProtocoloDispositivoUSB(var TipoHardware: TACBrUSBHardwareType;
       var ProtocoloACBr: Integer);
    {$EndIf}
    function DeviceToString(OnlyException: Boolean): String;

    procedure GravaLog(AString: AnsiString; Traduz :Boolean = False);
    function TemArqLog: Boolean;

    procedure DoException(E: Exception);
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

     property NomeDocumento : String read GetNomeDocumento write SetNomeDocumento;

     { propriedade que ativa/desativa o processamento de mensagens do windows }
     property ProcessMessages : Boolean read fProcessMessages
        write fProcessMessages default True ;

     property OnStatus : THookSerialStatus read GetOnStatus write SetOnStatus ;
     property HookAtivar : TACBrDeviceHookAtivar read fsHookAtivar
        write fsHookAtivar;
     property HookDesativar : TACBrDeviceHookDesativar read fsHookDesativar
        write fsHookDesativar;
     property HookEnviaString : TACBrDeviceHookEnviaString read fsHookEnviaString
        write fsHookEnviaString;
     property HookLeString : TACBrDeviceHookLeString read fsHookLeString
        write fsHookLeString;

     property ArqLOG : String read fsArqLOG write SetArqLOG ;
end ;

{ Essa classe é usada pela função EnviaStringThread para detectar se os Dados
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
    constructor Create(AOwner : TObject; const AString : String) ;
    property BytesSent : Integer read fsBytesSent ;
  end;

const
  estCupomAberto = [estVenda, estPagamento];
  CFailCount = 10;

implementation

Uses
  typinfo,
  ACBrUtil ;

{ TACBrTag }

function TACBrTag.GetTamanho: Integer;
begin
  Result := Length(FNome);
end;

procedure TACBrTag.SetAjuda(const AValue: String);
begin
  FAjuda := ACBrStr( AValue );
end;

procedure TACBrTag.SetNome(const AValue: String);
begin
  FNome := LowerCase(Trim(AValue));

  if LeftStr(FNome,1) <> '<' then
    FNome := '<'+FNome;

  if RightStr(AValue,1) <> '>' then
    FNome := FNome+'>';
end;

constructor TACBrTag.Create;
begin
  FEhBloco := False;
  FSequencia := 0;
end;

{ TACBrTags }

procedure TACBrTags.SetObject(Index: Integer; Item: TACBrTag);
begin
  inherited SetItem (Index, Item) ;
end;

function TACBrTags.GetObject(Index: Integer): TACBrTag;
begin
  //Result := inherited GetItem(Index) as TACBrTag;
  Result := TACBrTag(Get(Index));
end;

procedure TACBrTags.Insert(Index: Integer; Obj: TACBrTag);
begin
  inherited Insert(Index, Obj);
end;

function TACBrTags.New: TACBrTag;
begin
  Result := TACBrTag.Create;
  Add(Result);
end;

function TACBrTags.Add(Obj: TACBrTag): Integer;
begin
  Result := inherited Add(Obj) ;
  Obj.Sequencia := Count ;
end;

function TACBrTags.AcharTag(NomeTag: String): TACBrTag;
var
  I: Integer;
  ATag: TACBrTag;
begin
  NomeTag := LowerCase(NomeTag);
  Result := Nil;

  For I := 0 to Count-1 do
  begin
    ATag := Objects[I];
    if (ATag.Nome = NomeTag) then
    begin
      Result := ATag;
      Break;
    end;
  end;
end;

{ TACBrTagProcessor }

constructor TACBrTagProcessor.Create;
begin
  inherited Create;

  FTags := TACBrTags.create( True );
  FIgnorarTags := False;
  FTraduzirTags := True;
  FOnTraduzirTag := nil;
  FOnTraduzirTagBloco := nil;
end;

destructor TACBrTagProcessor.Destroy;
begin
  FTags.Free;

  inherited Destroy;
end;

function TACBrTagProcessor.DecodificarTagsFormatacao(const AString: AnsiString
  ): AnsiString;
Var
  Tag1, Tag2: String;
  Cmd : AnsiString ;
  PosTag1, LenTag1, PosTag2, FimTag : Integer ;
  ATag: TACBrTag;
begin
  Result := AString;
  if not TraduzirTags then exit ;

  Tag1    := '';
  PosTag1 := 0;

  AcharProximaTag( Result, 1, Tag1, PosTag1);
  while Tag1 <> '' do
  begin
    LenTag1  := Length( Tag1 );
    ATag     := FTags.AcharTag( Tag1 ) ;
    Tag2     := '' ;

    if ATag <> Nil then
    begin
      if (ATag.EhBloco) then  // Tag de Bloco, Procure pelo Fechamento
      begin
        Tag2    := '</'+ copy(Tag1, 2, LenTag1) ; // Calcula Tag de Fechamento
        PosTag2 := PosEx(Tag2, LowerCase(Result), PosTag1+LenTag1 );

        if PosTag2 = 0 then             // Não achou Tag de Fechamento
        begin
          Tag2 := '';
          Cmd  := '';
        end
        else
        begin
          Cmd := TraduzirTagBloco( Tag1, copy(Result, PosTag1+LenTag1, PosTag2-PosTag1-LenTag1) );

          // Faz da Tag1, todo o Bloco para fazer a substituição abaixo //
          LenTag1 := PosTag2-PosTag1+LenTag1+1;
          Tag1    := copy(Result, PosTag1, LenTag1 )
        end;
      end
      else
        Cmd := TraduzirTag( Tag1 ) ;
    end
    else
      Cmd := Tag1;   // Tag não existe nesse TagProcessor

    FimTag := PosTag1 + LenTag1 -1 ;

    if Cmd <> Tag1 then   // Houve mudança no conteudo  ? Se SIM, substitua
    begin
      Result := StuffString( Result, PosTag1, LenTag1, Cmd );
      FimTag := FimTag + (Length(Cmd) - LenTag1);
    end ;

    Tag1    := '';
    PosTag1 := 0;
    AcharProximaTag( Result, FimTag + 1, Tag1, PosTag1 );
  end ;
end;

function TACBrTagProcessor.TraduzirTag(const ATag: AnsiString): AnsiString;
begin
  Result := '';
  if (ATag = '') or IgnorarTags then exit ;

  if Assigned( FOnTraduzirTag ) then
    FOnTraduzirTag( ATag, Result);
end;

function TACBrTagProcessor.TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString
  ): AnsiString;
var
  AString: String;
begin
  Result := ConteudoBloco;
  if (ATag = '') or IgnorarTags or (ATag = cTagIgnorarTags) then
    exit ;

  { Chamada Recursiva, para no caso de "ConteudoBloco" ter TAGs não resolvidas
    dentro do Bloco }
  AString := DecodificarTagsFormatacao( ConteudoBloco ) ;

  if Assigned( FOnTraduzirTagBloco ) then
    FOnTraduzirTagBloco( ATag, AString, Result);
end;

procedure TACBrTagProcessor.RetornarTags(AStringList: TStrings;
  IncluiAjuda: Boolean);
var
  ALine: String;
  i: Integer;
begin
  AStringList.Clear;
  For i := 0 to FTags.Count-1 do
  begin
    ALine := FTags[i].Nome;
    if IncluiAjuda then
    begin
      if FTags[i].EhBloco then
        ALine := ALine + ' - Bloco';

      ALine := ALine + ' - '+FTags[i].Ajuda;
    end;

    AStringList.Add(ALine);
  end;
end;

procedure TACBrTagProcessor.AddTags(ArrayTags: array of String;
  ArrayHelpTags: array of String; TagEhBloco: Boolean);
var
  i: Integer;
begin
  For i := Low(ArrayTags) to High(ArrayTags) do
  begin
     with FTags.New do
     begin
        Nome := ArrayTags[i];
          if High(ArrayHelpTags) >= i then
            Ajuda := ArrayHelpTags[i];

        EhBloco := TagEhBloco;
     end;
  end;
end;


{ TACBrDevice }

constructor TACBrDevice.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

  { Classe SynaSer para acesso direto a Serial }
  fsSerial := TBlockSerial.Create ;
  fsSerial.RaiseExcept := True ;

  { Classe Synapse para envio por TCP }
  fsSocket := TTCPBlockSocket.Create;
  fsSocket.RaiseExcept := True;

  { Classe para envio por USB em Windows, usando a WinUSB}
  {$IfDef MSWINDOWS}
   fsWinUSB := TACBrUSBWinDeviceAPI.Create;
   fsWinUSB.LogFile := fsArqLOG;
  {$EndIf}

  { Variaveis Internas }
  fsPorta   := '' ;
  fsTimeOut := cTimeout ;

  fsSendBytesCount    := 0 ;
  fsSendBytesInterval := 0 ;

  fProcessMessages := True ;

  fsDeviceType      := dtNenhum;
  fsHookAtivar      := nil;
  fsHookDesativar   := nil;
  fsHookEnviaString := nil;
  fsHookLeString    := nil;
  fsNomeDocumento   := '';
  fsArqLOG          := '';

  SetDefaultValues ;
end;

destructor TACBrDevice.Destroy;
begin
  GravaLog('Destroy');
  fsSerial.Free ;
  IOResult ;

  fsSocket.Free;

  {$IfDef MSWINDOWS}
   fsWinUSB.Free;
  {$EndIf}

  inherited Destroy ;
end;

procedure TACBrDevice.SetDefaultValues;
begin
  GravaLog('SetDefaultValues');
  fsHardFlow := false ;
  fsSoftFlow := false ;
  fsHandShake:= hsNenhum ;
  fsParity   := 'N' ;
  fsData     := 8 ;
  fsBaud     := 9600 ;
  fsStop     := 0 ;

  fsPosImp.X := 0 ;
  fsPosImp.Y := 0 ;
end;


procedure TACBrDevice.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrDevice.Ativar;
var
  ip, NomeArq, ErrorMsg: String;
  p: Integer;
begin
  if fsAtivo then
    Exit;

  GravaLog('Ativar - Porta '+copy(GetEnumName(TypeInfo(TACBrDeviceType), integer(fsDeviceType)),3,20)+': '+fsPorta);

  if (fsPorta = '') or (fsDeviceType = dtNenhum) then
    DoException( Exception.Create(ACBrStr(cACBrDeviceAtivarPortaException)) );

  case fsDeviceType of
    dtHook:
      if Assigned(fsHookAtivar) then
        fsHookAtivar( fsPorta, DeviceToString(False));

    dtTCP:
      begin
        ip := copy(Porta,5 , 255);  //     "TCP:ip_maquina:porta"
        p := pos(':', ip);
        if p = 0 then
          p := Length(ip)+1;

        fTCPPort := copy(ip, p + 1, 5);
        if fTCPPort = '' then
          fTCPPort := '9100';

        fTCPIP := copy(ip, 1, p - 1);

        TryCloseSocket;

        GravaLog('  Socket.Connect('+fTCPIP+', '+fTCPPort+')');
        fsSocket.ConnectionTimeout := (TimeOut * 1000) ;
        fsSocket.Connect(fTCPIP, fTCPPort);
        fsSocket.Purge;
      end;

    dtSerial:
      begin
        try
          TryCloseSocket;  { Fecha se ficou algo aberto }

          GravaLog('  Serial.Connect('+fsPorta+')');
          fsSerial.DeadlockTimeout := (TimeOut * 1000) ;
          fsSerial.Connect( fsPorta ) ;
          ConfiguraSerial ;

          fsSerial.Purge ;  { Limpa Buffer de envio e recepção }
        except
          on E: ESynaSerError do
          begin
            if (fsSerial.LastError = 2) then
              ErrorMsg := Format(ACBrStr(cACBrDeviceAtivarPortaNaoEncontrada), [fsPorta])
            else
              ErrorMsg := E.Message;

            TryCloseSocket;

            DoException( ESynapseError.Create(ErrorMsg) );
          end;

          On E: Exception do
            DoException( Exception.Create(E.ClassName+': '+E.Message) );
        end ;
      end;

    {$IfDef MSWINDOWS}
    dtUSB:
      begin
        TryCloseSocket;

        GravaLog('  WinUSB.Connect('+fsPorta+')');
        fsWinUSB.Connect( fsPorta );
      end;
    {$EndIf}

    dtRawPrinter:
      GetPrinterRawIndex;

    dtFile, dtParallel:
      begin
        NomeArq := GetPrinterFileName;
        GravaLog('  NomeArq: '+NomeArq);

        { Tenta Abrir Arquivo/Porta para ver se existe e está disponivel}
        if IsTXTFilePort and FileExists(NomeArq) then
        begin
          GravaLog('  DeleteFile('+NomeArq+')');
          SysUtils.DeleteFile(NomeArq);
        end;

        try
          EnviaStringArquivo( '' );
        except
          on E: EFOpenError do
          begin
            ErrorMsg := '';
            if (fsDeviceType = dtParallel) then
            begin
              {$IfNDef MSWINDOWS}
              if not FileExists(fsPorta) then
                ErrorMsg := Format(ACBrStr(cACBrDeviceAtivarPortaNaoEncontrada), [fsPorta])
              else
              {$EndIf}
                ErrorMsg := Format(ACBrStr(cACBrDeviceAtivarPortaNaoAcessivel), [fsPorta]);
            end;

            if (ErrorMsg = '') then
              ErrorMsg := E.Message;

            DoException( EStreamError.Create(ErrorMsg) );
          end;

          On E: Exception do
            DoException( Exception.Create(E.ClassName+': '+E.Message) );
        end;
      end ;
  end;

  fsAtivo := true ;
end;

procedure TACBrDevice.ConfiguraSerial ;
begin
  if not fsSerial.InstanceActive then
    Exit;

  GravaLog('ConfiguraSerial');
  fsSerial.Config( fsBaud, fsData, fsParity, fsStop, fsSoftFlow, fsHardFlow);

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
end ;

procedure TACBrDevice.Desativar;
begin
  if not fsAtivo then
    Exit;

  GravaLog('Desativar');

  if (fsDeviceType = dtHook) then
  begin
    if Assigned(fsHookDesativar) then
      fsHookDesativar(fsPorta);
  end
  else
    TryCloseSocket;

  fsAtivo := false ;
end;

function TACBrDevice.GetOnStatus: THookSerialStatus;
begin
  Result := fsSerial.OnStatus;
end;

procedure TACBrDevice.SetOnStatus(const Value: THookSerialStatus);
begin
  fsSerial.OnStatus := Value;
end;

procedure TACBrDevice.SetBaud(const Value: Integer);
begin
  if fsBaud = Value then exit ;

  GravaLog('SetBaud('+IntToStr(Value)+')');

  if (Value < 50) or (Value > 4000000) then
     DoException( Exception.Create( ACBrStr(cACBrDeviceSetBaudException)));

  fsBaud := Value ;
  ConfiguraSerial ;
end;

procedure TACBrDevice.SetData(const Value: Integer);
begin
  if fsData = Value then exit ;

  GravaLog('SetData('+IntToStr(Value)+')');

  if (Value < 5) or (Value > 8) then
     DoException( Exception.Create( ACBrStr(cACBrDeviceSetDataException)));

   fsData := Value ;
   ConfiguraSerial ;
end;

procedure TACBrDevice.SetDeviceType(AValue: TACBrDeviceType);
begin
  if fsDeviceType = AValue then Exit;

  if TemArqLog then
    GravaLog('SetDeviceType('+GetEnumName(TypeInfo(TACBrDeviceType), integer(AValue))+')');

  if (AValue in [dtTCP, dtRawPrinter, dtHook, dtUSB]) then
  begin
    if (DeduzirTipoPorta(fsPorta) <> AValue) then
      DoException( Exception.Create( ACBrStr(cACBrDeviceSetTypeException)));
  end

  else if (AValue in [dtFile, dtSerial]) then
  begin
    if (DeduzirTipoPorta(fsPorta) in [dtFile, dtSerial]) then
      DoException( Exception.Create( ACBrStr(cACBrDeviceSetTypeException)));
  end;

  fsDeviceType := AValue;
end;

procedure TACBrDevice.SetNomeDocumento(const AValue: String);
begin
  GravaLog('SetNomeDocumento('+AValue+')');
  fsNomeDocumento := Trim(AValue);
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

  if TemArqLog then
    GravaLog('SetParity('+GetEnumName(TypeInfo(TACBrSerialParity), integer(Value))+')');

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

  if TemArqLog then
    GravaLog('SetStop('+GetEnumName(TypeInfo(TACBrSerialStop), integer(Value))+')');

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
  case fsDeviceType of
    dtTCP: Result := fsSocket.MaxSendBandwidth;
    dtSerial: Result := fsSerial.MaxSendBandwidth;
  else
    Result := 0;
  end;
end;

procedure TACBrDevice.SetMaxBandwidth(const Value: Integer);
begin
  GravaLog('SetMaxBandwidth('+IntToStr(Value)+')');

  case fsDeviceType of
    dtTCP: fsSocket.MaxSendBandwidth := Value;
    dtSerial: fsSerial.MaxSendBandwidth := Value;
  end;
end;

procedure TACBrDevice.TryCloseSocket;
begin
  case fsDeviceType of
    dtTCP:
      begin
        GravaLog('TryCloseSocket - TCP');
        try
          fsSocket.RaiseExcept := False;
          fsSocket.CloseSocket;
        finally
          fsSocket.RaiseExcept := True;
        end;
      end;

    dtSerial:
      begin
        GravaLog('TryCloseSocket - Serial');
        try
          fsSerial.RaiseExcept := False;
          GravaLog('  Serial.InstanceActive');
          if fsSerial.InstanceActive then
          begin
            GravaLog('  Serial.CloseSocket');
            fsSerial.CloseSocket ;
          end;
        finally
          fsSerial.RaiseExcept := True;
        end ;
      end ;

    {$IfDef MSWINDOWS}
    dtUSB:
      begin
        GravaLog('TryCloseSocket - WinUSB');
        fsWinUSB.Close;
      end;
    {$EndIf}
  end;
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
  GravaLog('SetHardFlow('+BoolToStr(Value, True)+')');

  if Value then
     HandShake := hsRTS_CTS
  else
     if HandShake = hsRTS_CTS then
        HandShake := hsNenhum ;
end;

procedure TACBrDevice.SetSoftFlow(const Value: Boolean);
begin
  GravaLog('SetSoftFlow('+BoolToStr(Value, True)+')');

  if Value then
     HandShake := hsXON_XOFF
  else
     if HandShake = hsXON_XOFF then
        HandShake := hsNenhum ;
end;

procedure TACBrDevice.SetHandShake(const Value: TACBrHandShake);
begin
  if TemArqLog then
    GravaLog('SetHandShake('+GetEnumName(TypeInfo(TACBrHandShake), integer(Value))+')');

  fsHardFlow  := (Value = hsRTS_CTS);
  fsSoftFlow  := (Value = hsXON_XOFF);

  fsHandShake := Value;
  ConfiguraSerial ;
end;

procedure TACBrDevice.SetPorta(const Value: String);
Var
  StrTemp : String ;
begin
  if fsPorta = Value then exit ;

  GravaLog('SetPorta('+Value+')');

  if Ativo then
    DoException( Exception.Create( ACBrStr(cACBrDeviceSetPortaException)));

  StrTemp := UpperCase( Value ) ;
  if (pos('LPT',StrTemp) = 1) or (pos('COM',StrTemp) = 1) then
     fsPorta := StrTemp
  else
     fsPorta := Value;

  fsDeviceType := DeduzirTipoPorta(fsPorta);
end;

{$IfDef FMX}
function TACBrDevice.GetLabelPrinterIndex(APrinterName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to Printer.Count - 1 do
    if AnsiContainsText(Printer.Printers[i].Title, APrinterName) then
      Exit(i);

  Result := -1;
end;
{$EndIf}

function TACBrDevice.DeduzirTipoPorta(const APorta: String): TACBrDeviceType;
var
  UPorta: String;
begin
  GravaLog('DeduzirTipoPorta('+APorta+')');

  UPorta := UpperCase(APorta);

  if APorta = '*' then
    Result := dtRawPrinter   // usará a impressora default
  else if (copy(UPorta, 1, 4) = 'TCP:') then
    Result := dtTCP
  else if (copy(UPorta, 1, 4) = 'RAW:') then
    Result := dtRawPrinter
  else if (RightStr(UPorta,4) = '.TXT') or (copy(UPorta, 1, 5) = 'FILE:') then
    Result := dtFile
  else if {$IFDEF LINUX}((pos('/dev/', APorta) = 1) and (Pos('/lp', APorta) > 4)){$ELSE}(Pos('LPT', UPorta) = 1){$ENDIF} then
    Result := dtParallel
  else if (copy(UPorta, 1, 3) = 'COM') or
       {$IFDEF MSWINDOWS}(copy(APorta,1,4) = '\\.\'){$ELSE}(pos('/dev/', APorta) = 1){$ENDIF} then
    Result := dtSerial
  else if (copy(UPorta, 1, 3) = 'DLL') then
    Result := dtHook
  else if (copy(UPorta, 1, 3) = 'USB') or
       (LowerCase(copy(APorta,1,7)) = '\\?\usb') then
    Result := dtUSB
  {$IfDef FMX}
  else if (GetLabelPrinterIndex(Aporta) > 0) then
  {$Else}
  else if (Printer.Printers.IndexOf(APorta) >= 0) then
  {$EndIf}
    Result := dtRawPrinter
  else
  begin
    if pos(PathDelim, APorta) > 0 then
      Result := dtFile
    else
      Result := dtNenhum;
  end;

  if TemArqLog then
    GravaLog('  '+GetEnumName(TypeInfo(TACBrDeviceType), integer(Result)));
end;

{$IfDef MSWINDOWS}
procedure TACBrDevice.DetectarTipoEProtocoloDispositivoUSB(
  var TipoHardware: TACBrUSBHardwareType; var ProtocoloACBr: Integer);
var
  i, f: Integer;
  uPorta, TipoPorta, DescPorta: String;
  DispositivoUSB: TACBrUSBWinDevice;
  Achou: Boolean;
begin
  ProtocoloACBr := 0;
  if not (DeviceType in [dtSerial, dtUSB]) then
    Exit;

  if (WinUSB.DeviceList.Count < 1) then
    WinUSB.FindUSBPrinters;

  if (WinUSB.DeviceList.Count < 1) then
    Exit;

  DispositivoUSB := Nil;
  uPorta := UpperCase(Porta);
  TipoPorta := copy(uPorta, 1, 3);
  DescPorta := copy(uPorta, 5, Length(uPorta));

  i := 0;
  Achou := False;
  while (not Achou) and (i < WinUSB.DeviceList.Count) do
  begin
    DispositivoUSB := WinUSB.DeviceList.Items[i];

    if (TipoPorta = 'COM') then
      Achou := (pos('('+TipoPorta+')', UpperCase(DispositivoUSB.FrendlyName)) > 0)
    else if (TipoPorta = '\\?') then
      Achou := (uPorta = UpperCase(copy(DispositivoUSB.DeviceInterface,1, Length(uPorta))))
    else if (TipoPorta = 'USB') then
      Achou := (DescPorta = UpperCase(copy(DispositivoUSB.DeviceName, 1, Length(DescPorta))));

    Inc(i);
  end;

  if Achou then
  begin
    TipoHardware := DispositivoUSB.DeviceKind;
    ProtocoloACBr := DispositivoUSB.ACBrProtocol;

    if (DeviceType = dtUSB) then
    begin
      // Se dispositivo Usa COM Virtual, prefira ela...
      i := pos('(COM',DispositivoUSB.FrendlyName);
      if (i > 0) then
      begin
        f := PosEx(')', DispositivoUSB.FrendlyName, i+1);
        if (f > 0) then
          Porta := copy(DispositivoUSB.FrendlyName, i+1, f-i-1);
      end;
    end;
  end;
end;
{$EndIf}

procedure TACBrDevice.SetTimeOut(const Value: Integer);
begin
  if Value = fsTimeOut then
     exit ;

  GravaLog('SetTimeOut('+IntToStr(Value)+')');

  if IsTCPPort then
  begin
    fsTimeOut := Value;
    fsSocket.ConnectionTimeout := (TimeOut * 1000);
  end
  else
  begin
    if Value < 1 then
      fsTimeOut := 1
    else
      fsTimeOut := Value ;

    if IsSerialPort then
      fsSerial.DeadlockTimeout := (TimeOut * 1000);

    {$IfDef MSWINDOWS}
    if IsUSBPort then
      fsWinUSB.TimeOut := (TimeOut * 1000);
    {$EndIf}
  end;
end;

function TACBrDevice.GetTimeOutMilissegundos: Integer;
begin
  if IsTCPPort then
    Result := fsSocket.ConnectionTimeout
  else if IsSerialPort then
    Result := fsSerial.DeadlockTimeout
  {$IfDef MSWINDOWS}
  else if IsUSBPort then
    Result := fsWinUSB.TimeOut
  {$EndIf}
  else
    Result := fsTimeOut;
end;

procedure TACBrDevice.SetArqLOG(AValue: String);
begin
  if fsArqLOG = AValue then Exit;
  fsArqLOG := AValue;
  {$IfDef MSWINDOWS}
  fsWinUSB.LogFile := AValue;
  {$EndIf}
end;

procedure TACBrDevice.SetTimeOutMilissegundos(AValue: Integer);
begin
  GravaLog('SetTimeOutMilissegundos('+IntToStr(AValue)+')');

  if IsTCPPort then
  begin
    fsSocket.ConnectionTimeout := AValue;
    fsTimeOut := Max(Trunc(AValue / 1000),0);
  end
  else if IsSerialPort then
  begin
    fsSerial.DeadlockTimeout := AValue;
    fsTimeOut := Max(Trunc(AValue / 1000),1);
  end
  {$IfDef MSWINDOWS}
  else if IsUSBPort then
  begin
    fsWinUSB.TimeOut := AValue;
    fsTimeOut := Max(Trunc(AValue / 1000),1);
  end
  {$EndIf}
  else
    fsTimeOut := AValue;
end;


function TACBrDevice.EmLinha( lTimeOut : Integer) : Boolean;
var TempoLimite : TDateTime ;
begin
  Result := True;

  GravaLog('EmLinha('+IntToStr(lTimeOut)+')');

  case fsDeviceType of
    dtFile:
      begin
        try
          {$IFDEF ThreadEnviaLPT}
          EnviaStringThread(#0);  { Tenta escrever algo (#0) na Porta/Arquivo }
          {$ENDIF}
          Result := true ;
        except
          Result := false ;
        end ;
      end;

    dtSerial:
      begin
        Result := False;
        if lTimeout < 1 then
          lTimeOut := 1;

        if fsSerial.InstanceActive then
        begin
          TempoLimite := IncSecond( now, lTimeOut);
          while (not Result) and (Now < TempoLimite) do
          begin
            case HandShake of
              hsRTS_CTS :
                Result := fsSerial.CTS;
              hsDTR_DSR :
                Result := fsSerial.DSR;
            else ;
              Result := True;    { Nao há sinal de HandShake para verificar }
            end;

            if not Result then
            begin
              {$IFNDEF NOGUI}
              if fProcessMessages then
                Application.ProcessMessages ;  { para redesenhar a tela do programa }
              {$ENDIF}
              Sleep(10) ;
            end;
          end;
        end;
      end;
  end;

  GravaLog('  '+BoolToStr(Result, True));
end;

function TACBrDevice.IsSerialPort: Boolean;
begin
  Result := (fsDeviceType = dtSerial);
end;

function TACBrDevice.IsParallelPort: Boolean;
begin
  Result := (fsDeviceType = dtParallel);
end;

function TACBrDevice.IsTCPPort: Boolean;
begin
  Result := (fsDeviceType = dtTCP);
end;

function TACBrDevice.IsRawPort: Boolean;
begin
  Result := (fsDeviceType = dtRawPrinter);
end;

function TACBrDevice.IsTXTFilePort: Boolean;
begin
  Result := (fsDeviceType = dtFile);
end;

function TACBrDevice.IsDLLPort : Boolean ;
begin
  Result := (fsDeviceType = dtHook);
end ;

function TACBrDevice.IsUSBPort: Boolean;
begin
  Result := (fsDeviceType = dtUSB);
end;

procedure TACBrDevice.AcharPortasSeriais(const AStringList : TStrings ;
   UltimaPorta : Integer) ;
var
   I     : Integer ;
   BS    : TBlockSerial ;
   UmaPorta : String ;
begin
   GravaLog('AcharPortasSeriais('+IntToStr(UltimaPorta)+')');
   AStringList.Clear;

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
     
  if (not OnlyException) or (SendBytesCount > 0) then
     Result := Result + ' SENDBYTESCOUNT='+IntToStr(SendBytesCount) ;

  if (not OnlyException) or (SendBytesInterval > 0) then
     Result := Result + ' SENDBYTESINTERVAL='+IntToStr(SendBytesInterval) ;

  Result := Trim(Result) ;
  GravaLog('  DeviceToString: '+Result);
end;

procedure TACBrDevice.GravaLog(AString: AnsiString; Traduz: Boolean);
begin
  if not TemArqLog then
    Exit;

  if Traduz then
    AString := TranslateUnprintable(AString);

  WriteLog(fsArqLOG, '-- '+FormatDateTime('dd/mm hh:nn:ss:zzz',now)+' '+ AString);
end;

function TACBrDevice.TemArqLog: Boolean;
begin
  Result := (fsArqLOG <> '');
end;

procedure TACBrDevice.DoException(E: Exception);
begin
  if Assigned(E) then
    GravaLog(E.ClassName+': '+E.Message);

  raise E;
end;

function TACBrDevice.GetParamsString: String;
begin
   Result := DeviceToString( True );
end;

function TACBrDevice.GetPrinterRawIndex: Integer;
var
  PrnName: String;
  PrnIndex: Integer;

  function RetornaNome(const ANome: string): string;
  begin
    Result := Copy(ANome, 3, Length(ANome));
    Result := Copy(Result, pos('\', Result) + 1, Length(Result));
  end;

  function RetornaPorta: Integer;
  {$IfDef MSWINDOWS}
   var
     I: Integer;
     VName: String;
  {$EndIf}
  begin
    {$IfDef FMX}
    Result := GetLabelPrinterIndex(PrnName);
    {$Else}
    Result := Printer.Printers.IndexOf(PrnName);
    {$EndIf}

    {$IfDef MSWINDOWS}
    if Result < 0 then
    begin
      for I := 0 to Pred(Printer{$IfNDef FMX}.Printers{$EndIf}.Count) do
      begin
        VName := Printer.Printers[I]{$IfDef FMX}.Title{$EndIf};
        if pos('\\', Copy(VName, 1, 2)) > 0 then
        begin
          if SameText(PrnName, RetornaNome(VName)) then
          begin
            Result := I;
            Break;
          end;
        end;
      end;
    end;
    {$EndIf}
  end;

begin
  GravaLog('GetPrinterRawIndex');

  PrnName := Porta;
  if (copy(UpperCase(PrnName), 1, 4) = 'RAW:') then
    PrnName := copy(PrnName, 5, Length(PrnName)) ;

  if (PrnName = '*') then
  begin
    {$IfDef FMX}
    if Assigned(Printer.ActivePrinter) then
    begin
      PrnName := Printer.ActivePrinter.Title;
      PrnIndex := RetornaPorta;
    end
    else
    begin
     PrnIndex := 0;
     if (Printer.Count = 0) then
       DoException( Exception.Create(ACBrStr(cACBrDeviceSemImpressoraPadrao)));
    end;
    {$Else}
    PrnIndex := Printer.PrinterIndex;
    if (PrnIndex < 0) then
    begin
      if Printer.Printers.Count > 0 then
        PrnIndex := 0
      else
        DoException( Exception.Create(ACBrStr(cACBrDeviceSemImpressoraPadrao)));
    end;
    {$EndIf}
  end
  else
  begin
    PrnIndex := RetornaPorta;

    if (PrnIndex < 0) then
      DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceImpressoraNaoEncontrada), [PrnName]));
  end;

  Result := PrnIndex;
  GravaLog('  '+IntToStr(Result));
end;

function TACBrDevice.GetPrinterFileName: String;
begin
  Result := Porta;
  if (copy(UpperCase(Result), 1, 5) = 'FILE:') then
    Result := copy(Result, 6, Length(Result)) ;

  GravaLog('GetPrinterFileName: '+Result);
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
  GravaLog('SetParamsString('+Value+')');
  SetDefaultValues ;

  Linha := Trim(UpperCase(Value)) ;

  Baud := StrToIntDef(GetValue(Linha,'BAUD'),Baud) ;

  S := GetValue(Linha,'PARITY') ;
  if S <> '' then
    if CharInSet(S[1], ['O','E','M','S','N']) then
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

  S := GetValue(Linha,'SENDBYTESCOUNT') ;
  SendBytesCount := StrToIntDef(S,SendBytesCount) ;

  S := GetValue(Linha,'SENDBYTESINTERVAL') ;
  SendBytesInterval := StrToIntDef(S,SendBytesInterval) ;
end;


procedure TACBrDevice.EnviaString( const AString: AnsiString);
begin
  GravaLog('EnviaString('+AString+')', True);

  case fsDeviceType of
    dtSerial:
      EnviaStringSerial(AString);
    dtHook:
      EnviaStringHook(AString);
    dtTCP:
      EnviaStringTCP(AString);
    {$IfDef MSWINDOWS}
    dtUSB:
      EnviaStringWinUSB(AString);
    {$EndIf}
    dtRawPrinter:
      EnviaStringRaw(AString);
    else
    begin
      {$IFNDEF ThreadEnviaLPT}
      EnviaStringArquivo(AString);
      {$ELSE}
      EnviaStringThread(AString);
      {$ENDIF} ;
    end ;
  end;
end;

procedure TACBrDevice.EnviaByte(const AByte: Byte);
begin
  GravaLog('EnviaByte('+IntToStr(AByte)+')');
  EnviaString(chr(AByte));
end;

function TACBrDevice.LeString(ATimeOut: Integer; NumBytes: Integer;
  const Terminador: AnsiString): String;
var
  Buffer: AnsiString;
  Fim: TDateTime;
  LenTer, LenBuf: Integer;
begin
  if TemArqLog then
    GravaLog('LeString('+IntToStr(ATimeOut)+', '+IntToStr(NumBytes)+', '+Terminador +')', True);

  Result := '';
  LenTer := Length(Terminador);

  if ATimeOut = 0 then
    ATimeOut := Self.TimeOut;

  case fsDeviceType of
    dtTCP:
      begin
        if (NumBytes > 0) then
          Result := fsSocket.RecvBufferStr( NumBytes, ATimeOut)
        else if (LenTer > 0) then
          Result := fsSocket.RecvTerminated( ATimeOut, Terminador)
        else
          Result := fsSocket.RecvPacket( ATimeOut );
      end;

    dtSerial:
      begin
        if (NumBytes > 0) then
          Result := fsSerial.RecvBufferStr( NumBytes, ATimeOut)
        else if (LenTer > 0) then
          Result := fsSerial.RecvTerminated( ATimeOut, Terminador)
        else
          Result := fsSerial.RecvPacket( ATimeOut );
      end;

    {$IfDef MSWINDOWS}
    dtUSB:
      begin
        if (NumBytes > 0) then
          Result := fsWinUSB.ReceiveNumBytes( NumBytes, ATimeOut)
        else if (LenTer > 0) then
          Result := fsWinUSB.ReceiveTerminated( Terminador, ATimeOut)
        else
          Result := fsWinUSB.ReceivePacket( ATimeOut );
      end;
    {$EndIf}

    dtHook:
      begin
        if Assigned( HookLeString ) then
        begin
          NumBytes := max(NumBytes,1);
          Fim := IncMilliSecond( Now, ATimeOut );
          repeat
            Buffer := '';
            HookLeString( NumBytes, ATimeOut, Buffer );
            LenBuf := Length(Buffer);
            if (LenTer > 0) and (LenBuf > 0) then
            begin
              if (RightStr(Buffer, LenTer) = Terminador) then
              begin
                SetLength(Buffer, (LenBuf-LenTer));
                NumBytes := 0; // força saida
              end;
            end;

            Result := Result + Buffer;
          until (Length(Result) >= NumBytes) or (now > Fim) ;
        end;
      end;
  end;

  GravaLog('  '+Result, True);
end;

function TACBrDevice.LeByte(ATimeOut: Integer): Byte;
Var
  Buffer: AnsiString;
begin
  GravaLog('LeByte('+IntToStr(ATimeOut)+')');
  Result := 0;

  if ATimeOut = 0 then
     ATimeOut := Self.TimeOut;

  case fsDeviceType of
    dtTCP:
      Result := fsSocket.RecvByte(ATimeOut);

    dtSerial:
      Result := fsSerial.RecvByte(ATimeOut);

    dtHook:
      begin
        if Assigned( HookLeString ) then
        begin
          Buffer := '';
          HookLeString( 1, ATimeOut, Buffer );
          if Length(Buffer) > 0 then
            Result := Ord( Buffer[1] );
        end;
      end;
  end;

  GravaLog('  '+IntToStr(Result));
end;

procedure TACBrDevice.Limpar;
begin
  GravaLog('Limpar');
  case fsDeviceType of
    dtSerial:
      fsSerial.Purge;
    dtTCP:
      fsSocket.Purge;
  end;
end;

function TACBrDevice.BytesParaLer: Integer;
begin
  case fsDeviceType of
    dtSerial:
      Result := fsSerial.WaitingDataEx;
    dtTCP:
      Result := fsSocket.WaitingDataEx;
    else
      Result := 0;
  end;
  GravaLog('BytesParaLer: '+IntToStr(Result));
end;

procedure TACBrDevice.EnviaStringSerial(const AString : AnsiString) ;
Var
  I, Max, BytesToSend, BytesSent, FailCount : Integer ;
  Buffer: AnsiString;
begin
  GravaLog('EnviaStringSerial('+AString+')', True);

  I   := 1 ;
  Max := Length(AString) ;
  FailCount := 0;

  while (I <= Max) and (FailCount < CFailCount) do
  begin
     BytesToSend := fsSendBytesCount ;
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

     if fsSendBytesInterval > 0 then
     begin
       GravaLog('  Sleep('+IntToStr(fsSendBytesInterval)+')');
       Sleep( fsSendBytesInterval ) ;
     end;

     I := I + BytesSent ;
  end ;

  if FailCount >= CFailCount then
    DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceEnviaStrFailCount), [Porta]));
end ;

procedure TACBrDevice.EnviaStringHook(const AString: AnsiString);
begin
  if Assigned( HookEnviaString ) then
  begin
    GravaLog('EnviaStringHook('+AString+')', True);
    HookEnviaString( AString );
  end;
end;

function TACBrDevice.GetNomeDocumento: String;
begin
  if fsNomeDocumento = '' then
     if not (csDesigning in ComponentState) then
        fsNomeDocumento := ClassName;

  Result := fsNomeDocumento;
end;

procedure TACBrDevice.EnviaStringTCP(const AString: AnsiString);
Var
  I, Max, NBytes : Integer ;
begin
  GravaLog('EnviaStringTCP('+AString+')', True);

  I   := 1 ;
  Max := Length(AString) ;
  NBytes := fsSendBytesCount ;
  if NBytes = 0 then
    NBytes := Max ;

  while I <= Max do
  begin
    GravaLog('  BytesToSend:'+IntToStr(NBytes));

    fsSocket.SendString( copy(AString, I, NBytes ) ) ;    { Envia por TCP }
    if fsSendBytesInterval > 0 then
    begin
      GravaLog('  Sleep('+IntToStr(fsSendBytesInterval)+')');
      Sleep( fsSendBytesInterval ) ;
    end;

    I := I + NBytes ;
  end ;
end;

{$IfDef MSWINDOWS}
procedure TACBrDevice.EnviaStringWinUSB(const AString: AnsiString);
Var
  I, Max, NBytes : Integer ;
begin
  GravaLog('EnviaStringWinUSB('+AString+')', True);

  I   := 1 ;
  Max := Length(AString) ;
  NBytes := fsSendBytesCount ;
  if NBytes = 0 then
    NBytes := Max ;

  while I <= Max do
  begin
    GravaLog('  BytesToSend:'+IntToStr(NBytes));

    fsWinUSB.SendData( copy(AString, I, NBytes ));
    if (fsSendBytesInterval > 0) then
    begin
      GravaLog('  Sleep('+IntToStr(fsSendBytesInterval)+')');
      Sleep( fsSendBytesInterval ) ;
    end;

    I := I + NBytes ;
  end ;
end;
{$EndIf}

procedure TACBrDevice.EnviaStringRaw(const AString: AnsiString);
var
  PrnIndex: Integer;
  {$IfNDef FPC}
   {$IfDef MSWINDOWS}
   PrnName: String;
   HandlePrn: THandle;
   N: DWORD;
   DocName: String;
    DocInfo1: TDocInfo1;
   {$Else}
    F: TextFile;
   {$EndIf}
  {$Else}
   Written: integer;
   OldRawMode: Boolean;
  {$EndIf}
begin
  GravaLog('EnviaStringRaw('+AString+')', True);
  PrnIndex := GetPrinterRawIndex;

  {$IfDef FPC}
   Printer.PrinterIndex := PrnIndex;
   Printer.Title := NomeDocumento;

   OldRawMode := Printer.RawMode;
   Printer.RawMode := True;
   try
     Printer.BeginDoc;
     Written := 0;
     Printer.Write(AString[1], Length(AString), Written);
     Printer.EndDoc;
   finally
     Printer.RawMode := OldRawMode;
   end;
  {$Else}
   {$IfDef MSWINDOWS}
    PrnName := Printer.Printers[PrnIndex]{$IfDef FMX}.Title{$EndIf};
    if not OpenPrinter(PChar(PrnName), HandlePrn, nil) then
      DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceImpressoraNaoEncontrada), [PrnName]));

    with DocInfo1 do
    begin
      DocName  := NomeDocumento;
      pDocName := PChar(DocName);
      pOutputFile := nil;
      pDataType := 'RAW';
    end;

    StartDocPrinter(HandlePrn, 1, @DocInfo1);
    WritePrinter(HandlePrn, PAnsiChar(AString), Length(AString), N);
    EndPagePrinter(HandlePrn);
    EndDocPrinter(HandlePrn);
    ClosePrinter(HandlePrn);
   {$Else}
    {$IfDef FMX}
     if (PrnIndex < Printer.Count) then
      Printer.ActivePrinter := Printer.Printers[PrnIndex];
    {$EndIf}
    AssignPrn(F);
    try
      ReWrite(F);
      Write(F,AString);
    finally
      CloseFile(F);
    end;
   {$EndIf}
  {$EndIf}
end;

procedure TACBrDevice.EnviaStringArquivo( const AString: AnsiString);
Var
  I, Max, NBytes: Integer ;
  NomeArq: String;
  {$IFDEF Device_Stream}
    CreateModeFlag: Integer;
    FS: TFileStream ;
    Buffer: AnsiString ;
  {$ELSE}
    ArqPrn: TextFile ;
  {$ENDIF}
begin
  GravaLog('EnviaStringArquivo('+AString+')', True);

  I   := 1 ;
  Max := Length(AString) ;
  NBytes := fsSendBytesCount ;
  if NBytes = 0 then
     NBytes := Max ;

  NomeArq := GetPrinterFileName;

  {$IFDEF Device_Stream}
    If IsTXTFilePort and FileExists(NomeArq) then
      CreateModeFlag := fmOpenReadWrite
    else
      CreateModeFlag := fmCreate;

    CreateModeFlag := CreateModeFlag or fmShareDenyWrite;
    // Tentando abrir o arquivo
    FS := TFileStream.Create( NomeArq, CreateModeFlag );

    try
       FS.Seek(0, soFromEnd);  // vai para EOF

       while I <= Max do
       begin
          GravaLog('  BytesToSend:'+IntToStr(NBytes));
          Buffer := copy(AString, I, NBytes ) ;

          FS.Write(Pointer(Buffer)^,Length(Buffer));

          if fsSendBytesInterval > 0 then
          begin
             GravaLog('  Sleep('+IntToStr(fsSendBytesInterval)+')');
             Sleep( fsSendBytesInterval ) ;
          end;

          I := I + NBytes ;
       end ;
    finally
       FS.Free ;
    end;
  {$ELSE}
    AssignFile( ArqPrn, NomeArq );
    try
       if IsTXTFilePort and FileExists(NomeArq) then
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

{$IFDEF ThreadEnviaLPT}
{ A ideia dessa Thread é testar se os dados estão sendo gravados com sucesso na
  Porta Paralela (ou arquivo). É criada uma Thread para "gravar" os dados em
  segundo plano, enquanto o programa monitora se as linhas estão sendo enviadas.
  Caso a Thread nao consiga enviar uma linha dentro do Timeout definido a Thread
  é cancelada e é gerado um TIMEOUT. Isso evita o "travamento" do programa
  quando a porta ou arquivo não estão prontos para a gravação com o comando
  Write() }
procedure TACBrDevice.EnviaStringThread(AString: AnsiString);
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
        DoException( Exception.Create( Format(ACBrStr(cACBrDeviceEnviaStrThreadException), [ Porta ] )));
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

  if Linha < fsPosImp.X then
     Eject ;

  if Linha > fsPosImp.X then
  begin
     Cmd := StringOfChar( LF, (Linha - fsPosImp.X) ) ;
     fsPosImp.X := Linha ;
  end ;

  if Coluna < fsPosImp.Y then
  begin
     Cmd := Cmd + CR ;
     fsPosImp.Y := 0 ;
  end ;

  if Coluna > fsPosImp.Y then
  begin
     Cmd := Cmd + StringOfChar( ' ', (Coluna - fsPosImp.Y) ) ;
     fsPosImp.Y := Coluna ;
  end ;

  EnviaString( Cmd + AString ) ;
  fsPosImp.Y := fsPosImp.Y + Length( AString );
end;

procedure TACBrDevice.Eject;
begin
  EnviaString( FF );
  fsPosImp.X := 0 ;
end;

{---------------------------- TACBrThreadEnviaLPT -----------------------------}
constructor TACBrThreadEnviaLPT.Create(AOwner : TObject; const AString: String ) ;
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

