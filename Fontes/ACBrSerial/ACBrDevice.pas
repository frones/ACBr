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

unit ACBrDevice ;

interface

uses
  Classes, SysUtils,
  synaser, blcksock,
  {$IfDef COMPILER6_UP}
   Types,
  {$Else}
   Windows, ACBrD5,
  {$EndIf}
  ACBrDeviceClass, ACBrDeviceSerial, ACBrDeviceTCP, ACBrDeviceLPT,
  ACBrDeviceHook, ACBrDeviceRaw,
  {$IfDef MSWINDOWS}
   ACBrDeviceWinUSB, ACBrWinUSBDevice,
  {$EndIf}
  {$IfDef HAS_BLUETOOTH}
   ACBrDeviceBlueTooth, System.Bluetooth.Components,
  {$EndIf}
  ACBrBase;

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

TACBrAlinhamento = (alDireita, alEsquerda, alCentro);

TACBrECFCHQEstado = (chqIdle, chqPosicione, chqImprimindo, chqFimImpressao, chqRetire, chqAutenticacao);

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

TACBrTags = class(TACBrObjectList)
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

TACBrDeviceType = (dtNenhum, dtFile, dtSerial, dtParallel, dtTCP, dtRawPrinter, dtHook, dtUSB, dtBlueTooth);

{ TACBrDevice }

TACBrDevice = class( TComponent )
  private
    fsDeviceAtivo: TACBrDeviceClass;
    fsDeviceNenhum: TACBrDeviceClass;
    fsDeviceSerial: TACBrDeviceSerial;
    fsDeviceTCP: TACBrDeviceTCP;
    fsDeviceLPT: TACBrDeviceLPT;
    fsDeviceHook: TACBrDeviceHook;
    fsDeviceRaw: TACBrDeviceRaw;
    {$IfDef MSWINDOWS}
     fsDeviceWinUSB: TACBrDeviceWinUSB;
    {$EndIf}
    {$IfDef HAS_BLUETOOTH}
     fsDeviceBlueTooth: TACBrDeviceBlueTooth;
    {$EndIf}

    fsPosImp: TPoint;
    fsPorta: String;
    fsNomeDocumento: String;
    fsDeviceType: TACBrDeviceType;
    fsTimeOutMilissegundos: Integer ;
    fsAtivo: Boolean;

    fsSendBytesCount: Integer;
    fsSendBytesInterval: Integer;
    fProcessMessages: Boolean;
    fsArqLOG: String;

    function GetBaud: Integer;
    function GetData: Integer;
    function GetDeviceTCP: TBlockSocket;

    {$IfDef MSWINDOWS}
     function GetDeviceWinUSB: TACBrUSBWinDeviceAPI;
    {$EndIf}
    {$IfDef HAS_BLUETOOTH}
     function GetDeviceBlueTooth: TBluetooth;
    {$EndIf}
    function GetHandShake: TACBrHandShake;
    function GetHardFlow: Boolean;
    function GetHookAtivar: TACBrDeviceHookAtivar;
    function GetHookDesativar: TACBrDeviceHookDesativar;
    function GetHookEnviaString: TACBrDeviceHookEnviaString;
    function GetHookLeString: TACBrDeviceHookLeString;
    function GetNomeDocumento: String;
    function GetParamsString: String;
    function GetDeviceSerial: TBlockSerial;
    function GetSoftFlow: Boolean;
    function GetTimeOut: Integer;
    function GetTimeOutMilissegundos: Integer;
    procedure SetArqLOG(AValue: String);

    procedure SetBaud(const AValue: Integer);
    procedure SetData(const AValue: Integer);
    procedure SetDeviceType(AValue: TACBrDeviceType);
    procedure SetHookAtivar(AValue: TACBrDeviceHookAtivar);
    procedure SetHookDesativar(AValue: TACBrDeviceHookDesativar);
    procedure SetHookEnviaString(AValue: TACBrDeviceHookEnviaString);
    procedure SetHookLeString(AValue: TACBrDeviceHookLeString);
    procedure SetNomeDocumento(const AValue: String);
    procedure SetHardFlow(const AValue: Boolean);
    function GetParity: TACBrSerialParity;
    procedure SetParity(const AValue: TACBrSerialParity);
    procedure SetSoftFlow(const AValue: Boolean);
    function GetStop: TACBrSerialStop;
    procedure SetStop(const AValue: TACBrSerialStop);
    procedure SetPorta(const AValue: String);
    procedure SetTimeOut(const AValue: Integer);
    procedure SetOnStatus(const AValue: THookSerialStatus);
    function GetOnStatus: THookSerialStatus;
    procedure SetAtivo(const Value: Boolean);
    procedure SetHandShake(const AValue: TACBrHandShake);
    procedure SetParamsString(const AValue: String);
    function GetMaxBandwidth: Integer;
    procedure SetMaxBandwidth(const AValue: Integer);
    procedure SetTimeOutMilissegundos(AValue: Integer);
  public
    property Serial: TBlockSerial read GetDeviceSerial;
    property Socket: TBlockSocket read GetDeviceTCP;
    {$IfDef MSWINDOWS}
     property WinUSB: TACBrUSBWinDeviceAPI read GetDeviceWinUSB;
    {$EndIf}
    {$IfDef HAS_BLUETOOTH}
     property BlueTooth: TBluetooth read GetDeviceBlueTooth;
    {$EndIf}

    property PosImp: TPoint read fsPosImp;

    procedure Assign(Source: TPersistent); override;

    property Ativo : Boolean read fsAtivo write SetAtivo ;

    property Porta: String  read fsPorta write SetPorta ;
    property DeviceType: TACBrDeviceType read fsDeviceType write SetDeviceType;
    property TimeOut : Integer read GetTimeOut write SetTimeOut ;
    property TimeOutMilissegundos: Integer read GetTimeOutMilissegundos
      write SetTimeOutMilissegundos;

    Function EmLinha(const ATimeOutSegundos : Integer = 1) : Boolean  ;
    function DeduzirTipoPorta(const APorta: String): TACBrDeviceType;

    property ParamsString : String read GetParamsString write SetParamsString ;

    constructor Create(AOwner: TComponent); override ;
    destructor Destroy ; override ;

    procedure Ativar ;
    procedure Desativar ;
    Procedure EnviaString( const AString : AnsiString ) ;
    Procedure EnviaByte( const AByte : Byte ) ;
    Function LeString(const ATimeOutMilissegundos: Integer=0; NumBytes: Integer=0;
      const Terminador: AnsiString = '' ): AnsiString;
    Function LeByte(const ATimeOutMilissegundos: Integer=0 ): Byte;
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

    procedure AcharPortasSeriais(const AStringList: TStrings; UltimaPorta: Integer = 64 );
    procedure AcharPortasRAW(const AStringList: TStrings);
    {$IfDef MSWINDOWS}
    procedure AcharPortasUSB(const AStringList: TStrings);
    procedure DetectarTipoEProtocoloDispositivoUSB(var TipoHardware: TACBrUSBHardwareType;
       var ProtocoloACBr: Integer);
    {$EndIf}
    {$IfDef HAS_BLUETOOTH}
    procedure AcharPortasBlueTooth(const AStringList: TStrings);
    {$EndIf}
    function DeviceToString(OnlyException: Boolean): String;

    procedure GravaLog(AString: AnsiString; Traduz :Boolean = False);
    function TemArqLog: Boolean;

    procedure DoException(E: Exception);
  published
     property Baud: Integer read GetBaud write SetBaud default 9600 ;
     property Data: Integer read GetData write SetData default 8 ;
     property Parity: TACBrSerialParity read GetParity write SetParity default pNone ;
     property Stop: TACBrSerialStop read GetStop write SetStop default s1 ;
     property HandShake: TACBrHandShake read GetHandShake write SetHandShake default hsNenhum ;
     property SoftFlow: Boolean read GetSoftFlow write SetSoftFlow default false ;
     property HardFlow: Boolean read GetHardFlow write SetHardFlow default false ;

     property MaxBandwidth: Integer read  GetMaxBandwidth write SetMaxBandwidth default 0 ;
     property SendBytesCount: Integer read  fsSendBytesCount write fsSendBytesCount  default 0 ;
     property SendBytesInterval: Integer read  fsSendBytesInterval write fsSendBytesInterval  default 0 ;

     property NomeDocumento : String read GetNomeDocumento write SetNomeDocumento;

     { propriedade que ativa/desativa o processamento de mensagens do windows }
     property ProcessMessages : Boolean read fProcessMessages
        write fProcessMessages default True ;

     property OnStatus : THookSerialStatus read GetOnStatus write SetOnStatus ;
     property HookAtivar : TACBrDeviceHookAtivar read GetHookAtivar write SetHookAtivar;
     property HookDesativar : TACBrDeviceHookDesativar read GetHookDesativar write SetHookDesativar;
     property HookEnviaString : TACBrDeviceHookEnviaString read GetHookEnviaString write SetHookEnviaString;
     property HookLeString : TACBrDeviceHookLeString read GetHookLeString write SetHookLeString;

     property ArqLOG : String read fsArqLOG write SetArqLOG ;
end ;

const
  estCupomAberto = [estVenda, estPagamento];

implementation

Uses
  typinfo, Math, StrUtils,
  ACBrUtil, ACBrConsts;

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
   inherited Items[Index] := Item;
end;

function TACBrTags.GetObject(Index: Integer): TACBrTag;
begin
  Result := TACBrTag(inherited Items[Index]);
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

  AcharProximaTag( String(Result), 1, Tag1, PosTag1);
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
        PosTag2 := PosEx(Tag2, LowerCase(String(Result)), PosTag1+LenTag1 );

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

  fsDeviceNenhum := TACBrDeviceClass.Create(Self);
  fsDeviceSerial := TACBrDeviceSerial.Create(Self);
  fsDeviceTCP := TACBrDeviceTCP.Create(Self);
  fsDeviceLPT := TACBrDeviceLPT.Create(Self);
  fsDeviceHook := TACBrDeviceHook.Create(Self);
  fsDeviceRaw := TACBrDeviceRaw.Create(Self);
  {$IfDef MSWINDOWS}
   fsDeviceWinUSB := TACBrDeviceWinUSB.Create(Self);
  {$EndIf}
  {$IfDef HAS_BLUETOOTH}
   fsDeviceBlueTooth := TACBrDeviceBlueTooth.Create(Self);
  {$EndIf}

  { Variaveis Internas }
  fsPorta   := '' ;
  fsTimeOutMilissegundos := cTimeout*1000;

  fsSendBytesCount := 0 ;
  fsSendBytesInterval := 0 ;
  fProcessMessages := True ;
  fsDeviceType := dtNenhum;
  fsDeviceAtivo := fsDeviceNenhum;
  fsNomeDocumento := '';
  fsArqLOG := '';

  SetDefaultValues ;
end;

destructor TACBrDevice.Destroy;
begin
  GravaLog('Destroy');
  fsDeviceNenhum.Free;
  fsDeviceSerial.Free ;
  fsDeviceTCP.Free;
  fsDeviceLPT.Free;
  fsDeviceHook.Free;
  fsDeviceRaw.Free;
  {$IfDef MSWINDOWS}
   fsDeviceWinUSB.Free;
  {$EndIf}
  {$IfDef HAS_BLUETOOTH}
   fsDeviceBlueTooth.Free;
  {$EndIf}

  inherited Destroy ;
end;

procedure TACBrDevice.SetDefaultValues;
begin
  GravaLog('SetDefaultValues');
  fsDeviceSerial.ValoresPadroes;
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
begin
  if fsAtivo then
    Exit;

  GravaLog('Ativar - Porta '+copy(GetEnumName(TypeInfo(TACBrDeviceType), integer(fsDeviceType)),3,20)+': '+fsPorta);

  if (fsPorta = '') or (fsDeviceType = dtNenhum) then
    DoException( Exception.Create(ACBrStr(cACBrDeviceAtivarPortaException)) );

  fsDeviceAtivo.Conectar(fsPorta, fsTimeOutMilissegundos);
  fsAtivo := true ;
end;

procedure TACBrDevice.Desativar;
begin
  if not fsAtivo then
    Exit;

  GravaLog('Desativar');
  fsDeviceAtivo.Desconectar();

  fsAtivo := false ;
end;

function TACBrDevice.GetOnStatus: THookSerialStatus;
begin
  Result := fsDeviceSerial.OnStatus;
end;

procedure TACBrDevice.SetOnStatus(const AValue: THookSerialStatus);
begin
  fsDeviceSerial.OnStatus := AValue;
end;

procedure TACBrDevice.SetBaud(const AValue: Integer);
begin
  fsDeviceSerial.Baud := AValue;
end;

procedure TACBrDevice.SetData(const AValue: Integer);
begin
  fsDeviceSerial.Data := AValue;
end;

procedure TACBrDevice.SetDeviceType(AValue: TACBrDeviceType);
begin
  if fsDeviceType = AValue then Exit;

  if TemArqLog then
    GravaLog('SetDeviceType('+GetEnumName(TypeInfo(TACBrDeviceType), integer(AValue))+')');

  if (AValue in [dtTCP, dtRawPrinter, dtHook, dtUSB, dtBlueTooth]) then
  begin
    if (DeduzirTipoPorta(fsPorta) <> AValue) then
      DoException( Exception.Create( ACBrStr(cACBrDeviceSetTypeException)));
  end

  else if (AValue in [dtFile, dtSerial]) then
  begin
    if not (DeduzirTipoPorta(fsPorta) in [dtFile, dtSerial]) then
      DoException( Exception.Create( ACBrStr(cACBrDeviceSetTypeException)));
  end;

  fsDeviceType := AValue;
  case fsDeviceType of
    dtTCP:
      fsDeviceAtivo := fsDeviceTCP;
    dtSerial:
      fsDeviceAtivo := fsDeviceSerial;
    dtHook:
      fsDeviceAtivo := fsDeviceHook;
    dtRawPrinter:
      fsDeviceAtivo := fsDeviceRaw;
    dtFile, dtParallel:
      fsDeviceAtivo := fsDeviceLPT;
    {$IfDef MSWINDOWS}
    dtUSB:
      fsDeviceAtivo := fsDeviceWinUSB;
    {$EndIf}
    {$IfDef HAS_BLUETOOTH}
    dtBlueTooth:
      fsDeviceAtivo := fsDeviceBlueTooth;
    {$EndIf}
    else
      fsDeviceAtivo := fsDeviceNenhum;
  end;

end;

procedure TACBrDevice.SetHookAtivar(AValue: TACBrDeviceHookAtivar);
begin
   fsDeviceHook.HookAtivar := AValue;
end;

procedure TACBrDevice.SetHookDesativar(AValue: TACBrDeviceHookDesativar);
begin
  fsDeviceHook.HookDesativar := AValue;
end;

procedure TACBrDevice.SetHookEnviaString(AValue: TACBrDeviceHookEnviaString);
begin
  fsDeviceHook.HookEnviaString := AValue;
end;

procedure TACBrDevice.SetHookLeString(AValue: TACBrDeviceHookLeString);
begin
  fsDeviceHook.HookLeString := AValue;
end;

procedure TACBrDevice.SetNomeDocumento(const AValue: String);
begin
  GravaLog('SetNomeDocumento('+AValue+')');
  fsNomeDocumento := Trim(AValue);
end;

function TACBrDevice.GetParity: TACBrSerialParity;
begin
  Result := fsDeviceSerial.Parity;
end;

procedure TACBrDevice.SetParity(const AValue: TACBrSerialParity);
begin
  fsDeviceSerial.Parity := AValue;
end;

function TACBrDevice.GetStop: TACBrSerialStop;
begin
  Result := fsDeviceSerial.Stop;
end;

procedure TACBrDevice.SetStop(const AValue: TACBrSerialStop );
begin
  fsDeviceSerial.Stop := AValue;
end;

function TACBrDevice.GetMaxBandwidth: Integer;
begin
  Result := fsDeviceAtivo.MaxSendBandwidth;
end;

procedure TACBrDevice.SetMaxBandwidth(const AValue: Integer);
begin
  GravaLog('SetMaxBandwidth('+IntToStr(AValue)+')');
  fsDeviceAtivo.MaxSendBandwidth := AValue;
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
     HookAtivar        := TACBrDevice(Source).HookAtivar;
     HookDesativar     := TACBrDevice(Source).HookDesativar;
  end;
end;

procedure TACBrDevice.SetHardFlow(const AValue: Boolean);
begin
  fsDeviceSerial.HardFlow := AValue;
end;

procedure TACBrDevice.SetSoftFlow(const AValue: Boolean);
begin
  fsDeviceSerial.SoftFlow := AValue;
end;

procedure TACBrDevice.SetHandShake(const AValue: TACBrHandShake);
begin
  fsDeviceSerial.HandShake := AValue;
end;

procedure TACBrDevice.SetPorta(const AValue: String);
Var
  PortaUp: String ;
begin
  if fsPorta = AValue then
    Exit;

  GravaLog('SetPorta('+AValue+')');
  if Ativo then
    DoException( Exception.Create( ACBrStr(cACBrDeviceSetPortaException)));

  fsPorta := Trim(AValue);
  PortaUp := UpperCase(fsPorta) ;
  if (pos('LPT',PortaUp) = 1) or (pos('COM',PortaUp) = 1) then
    fsPorta := PortaUp;

  DeviceType := DeduzirTipoPorta(fsPorta);
end;

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
  else if (copy(UPorta, 1, 4) = 'BTH:') then
    Result := dtBlueTooth
  else if (RightStr(UPorta,4) = '.TXT') or (copy(UPorta, 1, 5) = 'FILE:') then
    Result := dtFile
  else if {$IFDEF LINUX}
           ((pos('/dev/', APorta) = 1) and (Pos('/lp', APorta) > 4))
          {$ELSE}
           (Pos('LPT', UPorta) = 1)
          {$ENDIF} then
    Result := dtParallel
  else if (copy(UPorta, 1, 3) = 'COM') or
          {$IFDEF MSWINDOWS}
           (copy(APorta,1,4) = '\\.\')
          {$ELSE}
           (pos('/dev/', APorta) = 1)
          {$ENDIF} then
    Result := dtSerial
  else if (copy(UPorta, 1, 3) = 'DLL') then
    Result := dtHook
  else if (copy(UPorta, 1, 3) = 'USB') or
       (LowerCase(copy(APorta,1,7)) = '\\?\usb') then
    Result := dtUSB
  else if (fsDeviceRaw.GetLabelPrinterIndex(APorta) > 0) then
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
procedure TACBrDevice.AcharPortasUSB(const AStringList: TStrings);
begin
  fsDeviceWinUSB.AcharPortasUSB(AStringList);
end;

procedure TACBrDevice.DetectarTipoEProtocoloDispositivoUSB(
  var TipoHardware: TACBrUSBHardwareType; var ProtocoloACBr: Integer);
begin
  ProtocoloACBr := 0;
  if not (DeviceType in [dtSerial, dtUSB]) then
    Exit;

  fsDeviceWinUSB.DetectarTipoEProtocoloDispositivoUSB(TipoHardware, ProtocoloACBr);
end;
{$EndIf}

procedure TACBrDevice.SetTimeOut(const AValue: Integer);
var
  NovoTimeOut: Integer;
begin
  NovoTimeOut := (max(AValue,1) * 1000);
  if NovoTimeOut = fsTimeOutMilissegundos then
    Exit ;

  GravaLog('SetTimeOut('+IntToStr(AValue)+')');

  fsTimeOutMilissegundos := NovoTimeOut;
  fsDeviceAtivo.TimeOutMilissegundos := NovoTimeOut;
end;

function TACBrDevice.GetTimeOutMilissegundos: Integer;
begin
  Result := fsTimeOutMilissegundos;
end;

procedure TACBrDevice.SetArqLOG(AValue: String);
begin
  if fsArqLOG = AValue then Exit;
  fsArqLOG := AValue;
  {$IfDef MSWINDOWS}
  fsDeviceWinUSB.WinUSB.LogFile := AValue;
  {$EndIf}
end;

procedure TACBrDevice.SetTimeOutMilissegundos(AValue: Integer);
begin
  if AValue = fsTimeOutMilissegundos then
    Exit ;

  GravaLog('SetTimeOutMilissegundos('+IntToStr(AValue)+')');
  fsTimeOutMilissegundos := AValue;
  fsDeviceAtivo.TimeOutMilissegundos := AValue;
end;

function TACBrDevice.EmLinha(const ATimeOutSegundos: Integer): Boolean;
var
  NovoTimeOut: Integer;
begin
  GravaLog('EmLinha('+IntToStr(ATimeOutSegundos)+')');
  NovoTimeOut := max(ATimeOutSegundos,1)*1000;
  Result := fsDeviceAtivo.EmLinha(NovoTimeOut);
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

procedure TACBrDevice.AcharPortasRAW(const AStringList: TStrings);
begin
  fsDeviceRaw.AcharPortasRAW(AStringList);
end;

procedure TACBrDevice.AcharPortasSeriais(const AStringList : TStrings; UltimaPorta : Integer) ;
begin
  fsDeviceSerial.AcharPortasSeriais(AStringList, UltimaPorta);
end ;

function TACBrDevice.DeviceToString( OnlyException: Boolean): String;
begin
  Result := fsDeviceSerial.ParametrosSerial(OnlyException) ;
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
  Result := fsDeviceSerial.ParamsString;
end;

{$IfDef HAS_BLUETOOTH}
function TACBrDevice.GetDeviceBlueTooth: TBluetooth;
begin
  Result := fsDeviceBlueTooth.BlueTooth;
end;

procedure TACBrDevice.AcharPortasBlueTooth(const AStringList: TStrings);
begin
  fsDeviceBlueTooth.AcharPortasBlueTooth(AStringList);
end;
{$EndIf}

function TACBrDevice.GetDeviceSerial: TBlockSerial;
begin
  Result := fsDeviceSerial.Serial;
end;

function TACBrDevice.GetSoftFlow: Boolean;
begin
  Result := fsDeviceSerial.SoftFlow;
end;

function TACBrDevice.GetTimeOut: Integer;
begin
  Result := Max(Trunc(fsTimeOutMilissegundos / 1000),1);
end;

procedure TACBrDevice.SetParamsString(const AValue: String);
begin
  fsDeviceSerial.ParamsString := AValue;
end;

procedure TACBrDevice.EnviaString( const AString: AnsiString);
begin
  GravaLog('EnviaString'+AString+')', True);

  fsDeviceAtivo.EnviaString(AString);
end;

procedure TACBrDevice.EnviaByte(const AByte: Byte);
begin
  GravaLog('EnviaByte('+IntToStr(AByte)+')');
  fsDeviceAtivo.EnviaByte(AByte);
end;

function TACBrDevice.LeString(const ATimeOutMilissegundos: Integer;
  NumBytes: Integer; const Terminador: AnsiString): AnsiString;
var
  NovoTimeOut: Integer;
begin
  if ATimeOutMilissegundos <= 0 then
    NovoTimeOut := fsTimeOutMilissegundos
  else
    NovoTimeOut := ATimeOutMilissegundos;

  GravaLog('LeString('+IntToStr(NovoTimeOut)+', '+IntToStr(NumBytes)+', '+Terminador +')', True);
  Result := fsDeviceAtivo.LeString(NovoTimeOut, NumBytes, Terminador);
  GravaLog('  '+Result, True);
end;

function TACBrDevice.LeByte(const ATimeOutMilissegundos: Integer): Byte;
Var
  NovoTimeOut: Integer;
begin
  if ATimeOutMilissegundos <= 0 then
    NovoTimeOut := fsTimeOutMilissegundos
  else
    NovoTimeOut := ATimeOutMilissegundos;

  GravaLog('LeByte('+IntToStr(NovoTimeOut)+')');
  Result := fsDeviceAtivo.LeByte(NovoTimeOut);
  GravaLog('  '+IntToStr(Result));
end;

procedure TACBrDevice.Limpar;
begin
  GravaLog('Limpar');
  fsDeviceAtivo.Limpar;
end;

function TACBrDevice.BytesParaLer: Integer;
begin
  Result := fsDeviceAtivo.BytesParaLer;
  GravaLog('BytesParaLer: '+IntToStr(Result));
end;

function TACBrDevice.GetBaud: Integer;
begin
  Result := fsDeviceSerial.Baud;
end;

function TACBrDevice.GetData: Integer;
begin
   Result := fsDeviceSerial.Data;
end;

function TACBrDevice.GetDeviceTCP: TBlockSocket;
begin
  Result := fsDeviceTCP.Socket;
end;

{$IfDef MSWINDOWS}
function TACBrDevice.GetDeviceWinUSB: TACBrUSBWinDeviceAPI;
begin
  Result := fsDeviceWinUSB.WinUSB;
end;
{$EndIf}

function TACBrDevice.GetHandShake: TACBrHandShake;
begin
  Result := fsDeviceSerial.HandShake;
end;

function TACBrDevice.GetHardFlow: Boolean;
begin
  Result := fsDeviceSerial.HardFlow;
end;

function TACBrDevice.GetHookAtivar: TACBrDeviceHookAtivar;
begin
  Result := fsDeviceHook.HookAtivar;
end;

function TACBrDevice.GetHookDesativar: TACBrDeviceHookDesativar;
begin
 Result := fsDeviceHook.HookDesativar;
end;

function TACBrDevice.GetHookEnviaString: TACBrDeviceHookEnviaString;
begin
  Result := fsDeviceHook.HookEnviaString;
end;

function TACBrDevice.GetHookLeString: TACBrDeviceHookLeString;
begin
  Result := fsDeviceHook.HookLeString;
end;

function TACBrDevice.GetNomeDocumento: String;
begin
  if (fsNomeDocumento = '') then
    if not (csDesigning in ComponentState) then
      fsNomeDocumento := ClassName;

  Result := fsNomeDocumento;
end;

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

end.

