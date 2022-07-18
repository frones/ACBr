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

unit ACBrPosPrinterElginE1Service;

interface

uses
  Classes, SysUtils, Types,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  {$IfDef ANDROID}
   System.Messaging,
   Androidapi.JNI.Media,
  {$EndIf}
  ACBrDevice, ACBrPosPrinter, ACBrBase;

const
  CnModulo = 'Modulo';
  CnComando = 'Comando';
  CnFuncao = 'Funcao';
  CnParametros = 'Parametros';
  CmodImpressor = 'Impressor';

  CColunasImpressoraSmartPosElgin = 40;
  cTimeoutResp = 5000; // 5 seg
  CRequestCodeImpressora = 30;

type

  EACBrPosPrinterElginE1Service = class(Exception);

  {$IfDef ANDROID}
   TACBrPosPrinterElginErroMsg = procedure(const MsgErro: string) of object;
  {$EndIf}

  { TE1Funcao }

  TE1Funcao = class
  private
    FFuncao: String;
    FParametros: TACBrInformacoes;
  public
    constructor Create(NomeFuncao: String);
    destructor Destroy; override;

    property Funcao: String read FFuncao;
    property Parametros: TACBrInformacoes read FParametros;
  end;

  { TE1Comandos }

  TE1Comandos = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TE1Funcao>{$EndIf})
  private
    function GetItem(Index: Integer): TE1Funcao;
    procedure SetItem(Index: Integer; const Value: TE1Funcao);
  public
    function Add (Obj: TE1Funcao): Integer;
    procedure Insert (Index: Integer; Obj: TE1Funcao);
    function New(NomeFuncao: String): TE1Funcao;

    property Items[Index: Integer]: TE1Funcao read GetItem write SetItem; default;
  end;

  { TE1Json }

  TE1Json = class
  private
    FComandos: TE1Comandos;
    FModulo: String;
    function GetJSON: String;
    procedure SetJSON(AValue: String);
  public
    constructor Create(NomeModulo: String);
    destructor Destroy; override;
    procedure Clear;

    property Modulo: String read FModulo;
    property Comandos: TE1Comandos read FComandos;

    property JSON: String read GetJSON write SetJSON;
  end;

  TElginE1Printers = (prnI9, prnSmartPOS, prnM8);

  { TACBrPosPrinterElginE1Service }

  TACBrPosPrinterElginE1Service = class(TACBrPosPrinterClass)
  private
    fE1JSon: TE1Json;
    fModelo: TElginE1Printers;
    fPastaEntradaE1: String;
    fPastaSaidaE1: String;
    fSeqArqE1: Integer;
    fIPePortaE1: String;
    fREspostaE1: String;

    {$IfDef ANDROID}
     fMessageSubscriptionID: Integer;
     fOnErroImpressao: TACBrPosPrinterElginErroMsg;
    {$EndIf}
 protected
    procedure Imprimir(const LinhasImpressao: String; var Tratado: Boolean);
    {$IfDef ANDROID}
     procedure EnviarPorIntent(const LinhasImpressao: String);
     procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    {$Else}
     procedure EnviarPorTCP(const LinhasImpressao: String);
     procedure EnviarPorTXT(const LinhasImpressao: String);
    {$EndIf}

    procedure AddCmdAbreConexaoImpressora;
    procedure AddCmdFechaConexaoImpressora;
    procedure AddCmdImprimirTexto(const ConteudoBloco: String);
    procedure AddCmdPuloDeLinhas(const Linhas: Integer);
  public
    constructor Create(AOwner: TACBrPosPrinter);
    destructor Destroy; override;

    procedure Configurar; override;

    procedure AntesDecodificar(var ABinaryString: AnsiString); override;
    procedure AdicionarBlocoResposta(const ConteudoBloco: AnsiString); override;
    procedure DepoisDecodificar(var ABinaryString: AnsiString); override;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean;
      override;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; override;

    function ComandoFonte(TipoFonte: TACBrPosTipoFonte; Ligar: Boolean): AnsiString;
      override;
    function ComandoConfiguraModoPagina: AnsiString; override;
    function ComandoPosicionaModoPagina(APoint: TPoint): AnsiString; override;

    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
    function LerInfo: String; override;

    property E1JSon: TE1Json read fE1JSon;
    property Modelo: TElginE1Printers read fModelo write fModelo;
    property PastaEntradaE1: String read fPastaEntradaE1 write fPastaEntradaE1;
    property PastaSaidaE1: String read fPastaSaidaE1 write fPastaSaidaE1;
    property IPePortaE1: String read fIPePortaE1 write fIPePortaE1;
    property REspostaE1: string read fREspostaE1;
    {$IfDef ANDROID}
     property OnErroImpressao: TACBrPosPrinterElginErroMsg read fOnErroImpressao write fOnErroImpressao;
    {$EndIf}
  end;

implementation

uses
  StrUtils, Math, DateUtils,
  {$IfDef ANDROID}
  System.IOUtils,
  Androidapi.Helpers,  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  FMX.Platform.Android,
  {$Else}
   blcksock,
  {$EndIf}
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}
    Jsons,
  {$EndIf}
  ACBrUtil.Strings, ACBrConsts;

{ TE1Funcao }

constructor TE1Funcao.Create(NomeFuncao: String);
begin
  inherited Create;
  FFuncao := NomeFuncao;
  FParametros := TACBrInformacoes.Create;
end;

destructor TE1Funcao.Destroy;
begin
  FParametros.Free;
end;

{ TE1Comandos }

function TE1Comandos.GetItem(Index: Integer): TE1Funcao;
begin
  Result := TE1Funcao(inherited Items[Index]);
end;

procedure TE1Comandos.SetItem(Index: Integer; const Value: TE1Funcao);
begin
  inherited Items[Index] := Value;
end;

function TE1Comandos.Add(Obj: TE1Funcao): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TE1Comandos.Insert(Index: Integer; Obj: TE1Funcao);
begin
  inherited Insert(Index, Obj);
end;

function TE1Comandos.New(NomeFuncao: String): TE1Funcao;
begin
  Result := TE1Funcao.Create(NomeFuncao);
  inherited Add(Result);
end;

{ TE1Json }

constructor TE1Json.Create(NomeModulo: String);
begin
  inherited Create;
  FModulo := NomeModulo;
  FComandos := TE1Comandos.Create;
end;

destructor TE1Json.Destroy;
begin
  FComandos.Free;
  inherited Destroy;
end;

procedure TE1Json.Clear;
begin
  FComandos.Clear;
end;

function TE1Json.GetJSON: String;
var
  i, j: Integer;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon: TJsonObject;
   JSParametro: TJsonObject;
  {$Else}
   AJSon: TJson;
   JSComandos, JSParametros: TJsonArray;
   JSParamPair: TJsonPair;
   JSParametro: TJsonValue;
  {$EndIf}
   JSComando: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon := TJsonObject.Create;
   try
     AJSon[CnModulo] := FModulo;
     for i := 0 to FComandos.Count-1 do
     begin
       JSComando := AJSon.A[CnComando].AddObject;

       JSComando[CnFuncao] := FComandos[i].Funcao;
       if FComandos[i].Parametros.Count > 0 then
       begin
         JSParametro := JSComando.A[CnParametros].AddObject;

         for j := 0 to FComandos[i].Parametros.Count-1 do
         begin
           case FComandos[i].Parametros.Items[j].Tipo of
             tiBoolean:
               JSParametro.B[ FComandos[i].Parametros.Items[j].Nome ] := FComandos[i].Parametros.Items[j].AsBoolean;
             tiFloat:
               JSParametro.F[ FComandos[i].Parametros.Items[j].Nome ] := FComandos[i].Parametros.Items[j].AsFloat;
             tiInteger, tiInt64:
               JSParametro.I[ FComandos[i].Parametros.Items[j].Nome ] := FComandos[i].Parametros.Items[j].AsInteger;
           else
              JSParametro.S[ FComandos[i].Parametros.Items[j].Nome ] := FComandos[i].Parametros.Items[j].AsString;
           end;
         end;
       end;
     end;

     Result := AJSon.ToString;
   finally
     AJSon.Free;
   end;
  {$Else}
   AJSon := TJson.Create;
   try
     AJSon[CnModulo].AsString := FModulo;
     JSComandos := AJSon[CnComando].AsArray;

     for i := 0 to FComandos.Count-1 do
     begin
       JSComando := JSComandos.Add.AsObject;
       JSComando[CnFuncao].AsString := FComandos[i].Funcao;
       if FComandos[i].Parametros.Count > 0 then
       begin
         JSParametros := JSComando[CnParametros].AsArray;
         JSParametro := JSParametros.Add;

         for j := 0 to FComandos[i].Parametros.Count-1 do
         begin
           JSParamPair := JSParametro.AsObject.Add( FComandos[i].Parametros.Items[j].Nome);

           case FComandos[i].Parametros.Items[j].Tipo of
             tiBoolean:
               JSParamPair.Value.AsBoolean := FComandos[i].Parametros.Items[j].AsBoolean;
             tiFloat:
               JSParamPair.Value.AsNumber := FComandos[i].Parametros.Items[j].AsFloat;
             tiInteger, tiInt64:
               JSParamPair.Value.AsInteger := FComandos[i].Parametros.Items[j].AsInteger;
           else
              JSParamPair.Value.AsString := FComandos[i].Parametros.Items[j].AsString;
           end;
         end;
       end;
     end;

     Result := AJSon.Stringify;
   finally
     AJSon.Free;
   end;
  {$EndIf}
end;

procedure TE1Json.SetJSON(AValue: String);
var
  i, j, k: Integer;
  AInfo: TACBrInformacao;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon: TJsonObject;
  {$Else}
   AJSon: TJson;
   JSParamPair: TJsonPair;
  {$EndIf}
   JSComandos, JSParametros: TJsonArray;
   JSComando, JSParametro: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon := TJsonBaseObject.Parse(AValue) as TJsonObject;
   try
     FModulo := AJSon[CnModulo];
     JSComandos := AJSon.A[CnComando];

     For i := 0 to JSComandos.Count-1 do
     begin
       JSComando := JSComandos[i];
       JSParametros := JSComando.A[CnParametros];

       with FComandos.New(JSComando[CnFuncao]) do
       begin
         for j := 0 to JSParametros.Count-1 do
         begin
           JSParametro := JSParametros[j];

           for k := 0 to JSParametro.Count-1 do
           begin
             AInfo := Parametros.AddField(JSParametro.Names[k], '');
             case JSParametro.Items[k]^.Typ of
               jdtBool: AInfo.AsBoolean := JSParametro.Items[k]^.BoolValue;
               jdtInt, jdtLong, jdtULong, jdtFloat: AInfo.AsFloat := JSParametro.Items[k]^.FloatValue;
             else
               AInfo.AsString := JSParametro.Items[k]^.Value;
             end;
           end;
         end;
       end;
     end;
   finally
     AJSon.Free;
   end;
  {$Else}
   AJSon := TJson.Create;
   try
     AJSon.Parse(AValue);
     FModulo := AJSon.Values[CnModulo].AsString;
     JSComandos := AJSon.Values[CnComando].AsArray;

     For i := 0 to JSComandos.Count-1 do
     begin
       JSComando := JSComandos[i].AsObject;
       JSParametros := JSComando.Values[CnParametros].AsArray;

       with FComandos.New(JSComando.Values[CnFuncao].AsString) do
       begin
         for j := 0 to JSParametros.Count-1 do
         begin
           JSParametro := JSParametros[j].AsObject;

           for k := 0 to JSParametro.Count-1 do
           begin
             JSParamPair := JSParametro.Items[k];
             AInfo := Parametros.AddField(JSParamPair.Name, '');
             case JSParamPair.Value.ValueType of
               jvBoolean: AInfo.AsBoolean := JSParamPair.Value.AsBoolean;
               jvNumber: AInfo.AsFloat := JSParamPair.Value.AsNumber;
             else
               AInfo.AsString := JSParamPair.Value.AsString;
             end;
           end;
         end;
       end;
     end;
   finally
     AJSon.Free;
   end;
  {$EndIf}
end;

{ TACBrPosPrinterElginE1Service }

constructor TACBrPosPrinterElginE1Service.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fE1JSon := TE1Json.Create(CmodImpressor);
  fpModeloStr := 'PosPrinterElginE1Service';
  fModelo := prnSmartPOS;
  fPastaEntradaE1 := '';
  fPastaSaidaE1 := '';
  fIPePortaE1 := '';
  fREspostaE1 := '';
  fSeqArqE1 := 0;

  TagsNaoSuportadas.Add( cTagBarraMSI );
  TagsNaoSuportadas.Add( cTagLigaItalico );
  TagsNaoSuportadas.Add( cTagDesligaItalico );

  {$IfDef ANDROID}
   fMessageSubscriptionID := 0;
   fOnErroImpressao := Nil;
  {$EndIf}
end;

destructor TACBrPosPrinterElginE1Service.Destroy;
begin
  fE1JSon.Free;
  inherited Destroy;
end;

procedure TACBrPosPrinterElginE1Service.Configurar;
begin
  //fpPosPrinter.PaginaDeCodigo := pcNone;
  fpPosPrinter.Porta := 'NULL';
  Cmd.Clear;
  fpPosPrinter.OnEnviarStringDevice := Imprimir;
end;

procedure TACBrPosPrinterElginE1Service.AntesDecodificar(
  var ABinaryString: AnsiString);
begin
  // Troca todos Pulo de Linha, por Tag, para conseguir pegar os Blocos de impressão em TagProcessos
  ABinaryString := StringReplace(ABinaryString, Cmd.PuloDeLinha, cTagPulodeLinha, [rfReplaceAll]);
  AddCmdAbreConexaoImpressora;
end;

procedure TACBrPosPrinterElginE1Service.AdicionarBlocoResposta(
  const ConteudoBloco: AnsiString);
begin
  AddCmdImprimirTexto(ConteudoBloco);
end;

procedure TACBrPosPrinterElginE1Service.DepoisDecodificar(
  var ABinaryString: AnsiString);
begin
  AddCmdFechaConexaoImpressora;
  ABinaryString := fE1JSon.JSON;
end;

function TACBrPosPrinterElginE1Service.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString): Boolean;
begin
  TagTraduzida := '';

  if ((ATag = cTagZera) or (ATag = cTagReset)) then
  begin
    if not (fModelo in [prnSmartPOS, prnM8])  then
      fE1JSon.Comandos.New('InicializaImpressora')
  end

  else if ATag = cTagPuloDeLinhas then
  begin
    AddCmdPuloDeLinhas(fpPosPrinter.LinhasEntreCupons);
    Result := True;
  end

  else if ((ATag = cTagCorteParcial) or ( (ATag = cTagCorte) and (fpPosPrinter.TipoCorte = ctParcial) )) or
          ((ATag = cTagCorteTotal) or ( (ATag = cTagCorte) and (fpPosPrinter.TipoCorte = ctTotal) ) ) then
  begin
    AddCmdPuloDeLinhas(fpPosPrinter.LinhasEntreCupons);
    if fpPosPrinter.CortaPapel and (fModelo <> prnSmartPOS) then
      fE1JSon.Comandos.New('Corte').Parametros.AddField('avanco').AsInteger := 0;
    Result := True;
  end

  else if ATag = cTagAbreGaveta then
  begin
    if (fModelo <> prnSmartPOS)  then
      fE1JSon.Comandos.New('AbreGavetaElgin');
    Result := True;
  end

  else if ATag = cTagBeep then
  begin
    if not (fModelo in [prnSmartPOS, prnM8])  then
    begin
      with fE1JSon.Comandos.New('SinalSonoro') do
      begin
        Parametros.AddField('qtd').AsInteger := 1;
        Parametros.AddField('tempoInicio').AsInteger := 5; // seg
        Parametros.AddField('tempoFim').AsInteger := 0;
      end;
    end
    {$IfDef ANDROID}
    else
      // https://stackoverflow.com/questions/30938946/how-do-i-make-a-beep-sound-in-android-using-delphi-and-the-api
      TJToneGenerator.JavaClass.init( TJAudioManager.JavaClass.ERROR,
                                      TJToneGenerator.JavaClass.MAX_VOLUME)
        .startTone( TJToneGenerator.JavaClass.TONE_DTMF_0, 200 )
    {$EndIf};
    Result := True;
  end

  else if ATag = cTagLogotipo then
  begin
    if not fpPosPrinter.ConfigLogo.IgnorarLogo then
    begin
      with fE1JSon.Comandos.New('ImprimeImagemMemoria') do
      begin
        Parametros.AddField('key').AsInteger := fpPosPrinter.ConfigLogo.KeyCode1;
        Parametros.AddField('scala').AsInteger := fpPosPrinter.ConfigLogo.FatorX;
      end;
    end;
    Result := True;
  end

  else if ATag = cTagPulodeLinha then
  begin
    AddCmdPuloDeLinhas(1);
    Result := True;
  end

  else if ATag = cTagModoPaginaLiga then
    fE1JSon.Comandos.New('ModoPagina')

  else if ATag = cTagModoPaginaDesliga then
  begin
    fE1JSon.Comandos.New('ImprimeModoPagina');
    fE1JSon.Comandos.New('ModoPadrao');
  end

  else if ATag = cTagModoPaginaImprimir then
  begin
    fE1JSon.Comandos.New('ImprimeModoPagina');
    Result := True;
  end

  else
    Result := False;
end;

function TACBrPosPrinterElginE1Service.TraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString): Boolean;
var
  tipoCodBarras: Integer;
  ACodBar: AnsiString;
begin
  if ATag = cTagAbreGavetaEsp then
  begin
    with fE1JSon.Comandos.New('AbreGaveta') do
    begin
      Parametros.AddField('pino').AsInteger := max(min(StrToIntDef(ConteudoBloco,1),1),0);
      Parametros.AddField('ti').AsInteger := 50;
      Parametros.AddField('tf').AsInteger := 50;
    end;
    Result := True;
  end

  else if ATag = cTagQRCode then
  begin
    with fE1JSon.Comandos.New('ImpressaoQRCode') do
    begin
      Parametros.AddField('dados').AsString := ConteudoBloco;
      Parametros.AddField('tamanho').AsInteger := max(min(fpPosPrinter.ConfigQRCode.LarguraModulo, 6), 1);
      Parametros.AddField('nivelCorrecao').AsInteger := max(min(fpPosPrinter.ConfigQRCode.ErrorLevel, 4), 1);
    end;
    Result := True;
  end

  else if ATag = cTagBMP then
  begin
    if FileExists(ConteudoBloco) then
    begin
      with fE1JSon.Comandos.New('ImprimeImagemMemoria') do
      begin
        Parametros.AddField('key').AsString := ConteudoBloco;
        Parametros.AddField('scala').AsInteger := fpPosPrinter.ConfigLogo.FatorX;
      end;
    end
    else
      AddCmdImprimirTexto('Arquivo não encontrado: '+ConteudoBloco);

    Result := True;
  end

  else if (AnsiIndexText(ATag, cTAGS_BARRAS) >= 0) then
  begin
    tipoCodBarras := -1;
    ACodBar := ConteudoBloco;
    if (ATag = cTagBarraUPCA) then
    begin
      ACodBar := AnsiString(OnlyNumber(ConteudoBloco));
      if (Length(ACodBar) < 11) then
        ACodBar := PadLeftA(ACodBar, 11, '0');
      tipoCodBarras := 0;
    end
    else if (ATag = cTagBarraUPCE) then
    begin
      ACodBar := OnlyNumber(ConteudoBloco);
      tipoCodBarras := 1;
    end
    else if ATag = cTagBarraEAN13 then
    begin
      ACodBar := AnsiString(OnlyNumber(ConteudoBloco));
      if (Length(ACodBar) < 12) then
        ACodBar := PadLeftA(ACodBar, 12, '0');
      tipoCodBarras := 2;
    end
    else if ATag = cTagBarraEAN8 then
    begin
      ACodBar := AnsiString(OnlyNumber(ConteudoBloco));
      if (Length(ACodBar) < 7) then
        ACodBar := PadLeftA(ACodBar, 7, '0');
      tipoCodBarras := 3;
    end
    else if ATag = cTagBarraCode39 then
    begin
      ACodBar := AnsiString(OnlyCharsInSet(ConteudoBloco,
        ['0'..'9', 'A'..'Z', ' ', '$', '%', '*', '+', '-', '.', '/']));
      tipoCodBarras := 4;
    end
    else if ATag = cTagBarraInter then
    begin
      // Interleaved 2of5. Somente números, Tamanho deve ser PAR
      ACodBar := AnsiString(OnlyNumber(ConteudoBloco));
      if (Length(ACodBar) mod 2) <> 0 then  // Tamanho é Par ?
        ACodBar := '0' + ACodBar;
      tipoCodBarras := 5;
    end
    else if ATag = cTagBarraCodaBar then
    begin
      // Qualquer tamanho.. Aceita: 0~9, A~D, a~d, $, +, -, ., /, :
      ACodBar := AnsiString(OnlyCharsInSet(ConteudoBloco,
        ['0'..'9', 'A'..'D', 'a'..'d', '$', '+', '-', '.', '/', ':']));
      tipoCodBarras := 6;
    end
    else if ATag = cTagBarraCode93 then
    begin
      ACodBar := AnsiString(OnlyCharsInSet(ConteudoBloco, [#0..#127]));
      tipoCodBarras := 7;
    end
    else if (ATag = cTagBarraCode128) or (ATag = cTagBarraCode128b)  then
    begin
      ACodBar := AnsiString(OnlyCharsInSet(ConteudoBloco, [#0..#127]));
      ACodBar := '{B'+ACodBar;
      tipoCodBarras := 8;
    end
    else if (ATag = cTagBarraCode128a) then
    begin
      ACodBar := AnsiString(OnlyCharsInSet(ConteudoBloco, [#0..#127]));
      ACodBar := '{A'+ACodBar;
      tipoCodBarras := 8;
    end
    else if (ATag = cTagBarraCode128c) then
    begin
      ACodBar := AnsiString(OnlyNumber(ConteudoBloco));
      if (Length(ACodBar) mod 2) <> 0 then  // Tamanho deve ser Par
        ACodBar := '0' + ACodBar;

      ACodBar := '{C'+ACodBar;
      tipoCodBarras := 8;
    end;

    if tipoCodBarras >= 0 then
    begin
      with fE1JSon.Comandos.New('ImpressaoCodigoBarras') do
      begin
        Parametros.AddField('tipo').AsInteger := tipoCodBarras;
        Parametros.AddField('dados').AsString := ACodBar;
        Parametros.AddField('altura').AsInteger := IfThen(fpPosPrinter.ConfigBarras.Altura<=0, 50, max(min(fpPosPrinter.ConfigBarras.Altura, 255), 1));
        Parametros.AddField('largura').AsInteger := IfThen(fpPosPrinter.ConfigBarras.LarguraLinha<=0, 2, max(min(fpPosPrinter.ConfigBarras.LarguraLinha, 6), 1));
        Parametros.AddField('HRI').AsInteger := IfThen(fpPosPrinter.ConfigBarras.MostrarCodigo, 2, 4);
      end;
    end
    else
      AddCmdImprimirTexto(ConteudoBloco);

    Result := True;
  end

  else
    Result := False;

  if Result then
    BlocoTraduzido := '';
end;

function TACBrPosPrinterElginE1Service.ComandoFonte(
  TipoFonte: TACBrPosTipoFonte; Ligar: Boolean): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterElginE1Service.ComandoConfiguraModoPagina: AnsiString;
var
  direcao: Integer;
begin
  with fE1JSon.Comandos.New('DefineAreaImpressao') do
  begin
    Parametros.AddField('oHorizontal').AsInteger := fpPosPrinter.ConfigModoPagina.Esquerda;
    Parametros.AddField('oVertical').AsInteger := fpPosPrinter.ConfigModoPagina.Topo;
    Parametros.AddField('dHorizontal').AsInteger := fpPosPrinter.ConfigModoPagina.Largura;
    Parametros.AddField('dVertical').AsInteger := fpPosPrinter.ConfigModoPagina.Altura;
  end;

  case fpPosPrinter.ConfigModoPagina.Direcao of
    dirTopoParaBaixo: direcao := 3;
    dirDireitaParaEsquerda: direcao := 2;
    dirBaixoParaTopo: direcao := 1;
  else
    direcao := 0;
  end;
  fE1JSon.Comandos.New('DirecaoImpressao').Parametros.AddField('direcao').AsInteger := direcao;
  Result := '';
end;

function TACBrPosPrinterElginE1Service.ComandoPosicionaModoPagina(APoint: TPoint
  ): AnsiString;
begin
  fE1JSon.Comandos.New('PosicaoImpressaoHorizontal').Parametros.AddField('nLnH').AsInteger := APoint.X;
  fE1JSon.Comandos.New('PosicaoImpressaoVertical').Parametros.AddField('nLnH').AsInteger := APoint.Y;
  Result := '';
end;

procedure TACBrPosPrinterElginE1Service.LerStatus(var AStatus: TACBrPosPrinterStatus);

  function LerStatusImpressora(AParam: Integer): Integer;
  const
    cFuncaoStatus = 'StatusImpressora';
  var
    Tratado: Boolean;
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     AJSon: TJsonObject;
    {$Else}
     AJSon: TJson;
    {$EndIf}
  begin
    AddCmdAbreConexaoImpressora;
    fE1JSon.Comandos.New(cFuncaoStatus).Parametros.AddField('param').AsInteger := AParam;
    AddCmdFechaConexaoImpressora;

    Result := 0;
    Tratado := False;
    Imprimir(fE1JSon.JSON, Tratado);

    if (fREspostaE1 <> '') then
    begin
      {$IfDef USE_JSONDATAOBJECTS_UNIT}
       AJSon := TJsonBaseObject.Parse(fREspostaE1) as TJsonObject;
       try
         Result := AJSon.I[cFuncaoStatus];
       finally
         AJSon.Free;
       end;
      {$Else}
       AJSon := TJson.Create;
       try
         AJSon.Parse(fREspostaE1);
         Result := AJSon[cFuncaoStatus].AsInteger;
       finally
         AJSon.Free;
       end;
      {$EndIf}
    end;
  end;

var
  S: Integer;
begin
  S := LerStatusImpressora(3);
  if (S < 0) then
    AStatus := AStatus + [stErro];

  case S of
    6: AStatus := AStatus + [stPoucoPapel];
    7: AStatus := AStatus + [stSemPapel];
  end;

  if (S >= 0) and (Modelo = prnI9) then
  begin
    S := LerStatusImpressora(5);
    if (S > 0) then
    begin
      if TestBit(S, 0) then
        AStatus := AStatus + [stGavetaAberta];
      if TestBit(S, 1) then
        AStatus := AStatus + [stTampaAberta];
      if TestBit(S, 2) then
        AStatus := AStatus + [stPoucoPapel];
      if TestBit(S, 3) then
        AStatus := AStatus + [stSemPapel];
    end;
  end;

end;

function TACBrPosPrinterElginE1Service.LerInfo: String;
begin
  Result := inherited LerInfo;
end;


procedure TACBrPosPrinterElginE1Service.AddCmdAbreConexaoImpressora;
begin
  // Limpa o objeto com o E1JSON
  fE1JSon.Clear;

  with fE1JSon.Comandos.New('AbreConexaoImpressora') do
  begin
    Parametros.AddField('tipo').AsInteger := IfThen(fModelo=prnSmartPOS, 5, IfThen(fModelo=prnM8, 6, 1));
    Parametros.AddField('modelo').AsString := IfThen(fModelo=prnSmartPOS, 'SmartPOS', IfThen(fModelo=prnM8, 'M8', 'i9'));
    Parametros.AddField('conexao').AsString := IfThen(fModelo=prnI9, 'USB', '');
    Parametros.AddField('parametro').AsInteger := 0;
  end;
end;

procedure TACBrPosPrinterElginE1Service.AddCmdFechaConexaoImpressora;
begin
  fE1JSon.Comandos.New('FechaConexaoImpressora');
end;

procedure TACBrPosPrinterElginE1Service.AddCmdImprimirTexto(
  const ConteudoBloco: String);
var
  stilo, tamanho: Integer;
begin
  if (ConteudoBloco <> '') then
  begin
    with fE1JSon.Comandos.New('ImpressaoTexto') do
    begin
      Parametros.AddField('dados').AsString := ConteudoBloco;
      Parametros.AddField('posicao').AsInteger := IfThen(fpPosPrinter.Alinhamento = alDireita, 2,
                                                  IfThen(fpPosPrinter.Alinhamento = alCentro, 1, 0));

      stilo := 0;
      if (ftCondensado in fpPosPrinter.FonteStatus) or
         (ftFonteB in fpPosPrinter.FonteStatus) then
        stilo := stilo + 1;
      if ftSublinhado in fpPosPrinter.FonteStatus then
        stilo := stilo + 2;
      if ftInvertido in fpPosPrinter.FonteStatus then
        stilo := stilo + 4;
      if ftNegrito in fpPosPrinter.FonteStatus then
        stilo := stilo + 8;
      Parametros.AddField('stilo').AsInteger := stilo;

      tamanho := 0;
      if ftExpandido in fpPosPrinter.FonteStatus then
        tamanho := tamanho + 16;
      if ftAlturaDupla in fpPosPrinter.FonteStatus then
        tamanho := tamanho + 1;
      Parametros.AddField('tamanho').AsInteger := tamanho;
    end;
  end;
end;

procedure TACBrPosPrinterElginE1Service.AddCmdPuloDeLinhas(const Linhas: Integer
  );
begin
  fE1JSon.Comandos.New('AvancaPapel').
    Parametros.AddField('linhas').AsInteger := Linhas;
end;

procedure TACBrPosPrinterElginE1Service.Imprimir(const LinhasImpressao: String;
  var Tratado: Boolean);
begin
  Tratado := True;
  fREspostaE1 := '';
  {$IfDef ANDROID}
   EnviarPorIntent(LinhasImpressao)
  {$Else}
   if (fIPePortaE1 <> '') then
     EnviarPorTCP(LinhasImpressao)
   else
     EnviarPorTXT(LinhasImpressao);
  {$EndIf}
end;

{$IfDef ANDROID}
 procedure TACBrPosPrinterElginE1Service.EnviarPorIntent(const LinhasImpressao: String);
 var
   intentPrint: JIntent;
   //JSON: string;
 begin
//     JSON :=
//    '{"Modulo":"Impressor","Comando":[' +
//    '{"Funcao":"AbreConexaoImpressora","Parametros":[{"tipo":5,"modelo":"SmartPOS","conexao":"","parametro":0}]},' +
//    '{"Funcao":"AvancaPapel","Parametros":[{"linhas":1}]},' +
//    '{"Funcao":"ImpressaoTexto","Parametros":[{"dados":"Impressao Texto","posicao":1,"stilo":1,"tamanho":0}]},' +
//    '{"Funcao":"AvancaPapel","Parametros":[{"linhas":1}]},' +
//    '{"Funcao":"ImpressaoTexto","Parametros":[{"dados":"Impressao Texto Expandido","posicao":1,"stilo":0,"tamanho":17}]},' +
//    '{"Funcao":"AvancaPapel","Parametros":[{"linhas":1}]},' +
//    '{"Funcao":"ImpressaoTexto","Parametros":[{"dados":"Impressao de Texto Longo, muito Longo com mais linhas que a impressora suporta","posicao":0,"stilo":8,"tamanho":0}]},' +
//    '{"Funcao":"AvancaPapel","Parametros":[{"linhas":1}]},' +
//    '{"Funcao":"DefinePosicao","Parametros":[{"posicao":1}]},' +
//    '{"Funcao":"ImpressaoCodigoBarras","Parametros":[{"tipo":8,"dados":"{C0123456789","altura":20,"largura":1,"HRI":2}]},' +
//    '{"Funcao":"DefinePosicao","Parametros":[{"posicao":1}]},' +
//    '{"Funcao":"ImpressaoQRCode","Parametros":[{"dados":"www.clubeautomacaoelgin.com.br","tamanho":5,"nivelCorrecao":2}]},' +
//    '{"Funcao":"AvancaPapel","Parametros":[{"linhas":3}]},' +
//    '{"Funcao":"FechaConexaoImpressora"}]}';

   fMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityMessage);
   intentPrint := TJIntent.JavaClass.init(StringToJString('com.elgin.e1.intentservice.IMPRESSAO'));
   intentPrint.putExtra(StringToJString('direta'), StringToJString(LinhasImpressao));
   TAndroidHelper.Activity.startActivityForResult(intentPrint, CRequestCodeImpressora);
 end;

procedure TACBrPosPrinterElginE1Service.HandleActivityMessage(const Sender: TObject; const M: TMessage);
var
  resultIntent: JIntent;
  requestCode, resultCode: Integer;
  erroStr, retorno: String;
begin
  if M is TMessageResultNotification then
  begin
    TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, fMessageSubscriptionID);
    fMessageSubscriptionID := 0;

    if Assigned(fOnErroImpressao) then
    begin
      resultIntent := TMessageResultNotification(M).Value;
      requestCode := TMessageResultNotification(M).RequestCode;
      resultCode := TMessageResultNotification(M).ResultCode;

      if( requestCode = CRequestCodeImpressora ) then
      begin
        if  (resultCode = TJActivity.JavaClass.RESULT_OK) then
        begin
          retorno := JStringToString(resultIntent.getStringExtra(StringToJString('retorno')));

          if (retorno <> '1') then
          begin
            if resultIntent.hasExtra(StringToJString('erro')) then
            begin
              erroStr := 'Erro na impressão.' + sLineBreak +
                         JStringToString(resultIntent.getStringExtra(StringToJString('erro')));
              fOnErroImpressao(erroStr);
            end;
          end;
        end;
      end;
    end;
  end;
end;


{$Else}
 procedure TACBrPosPrinterElginE1Service.EnviarPorTCP(
   const LinhasImpressao: String);
 var
   ASocket: TBlockSocket;
   IP, Porta: String;
   Resp: AnsiString;
   p: Integer;
 begin
   fREspostaE1 := '';
   ASocket := TBlockSocket.Create;
   try
     IP := fIPePortaE1;
     p := pos(':',IP);
     if (p=0) then
       Porta := '89'
     else
     begin
       Porta := Copy(IP, P+1, Length(IP));
       IP := copy(IP, 1, P-1);
     end;

     ASocket.Connect(IP, Porta);
     ASocket.SendString(LinhasImpressao);
     Resp := ASocket.RecvPacket(cTimeoutResp);
     if (ASocket.LastError = 0) then
     begin
       fREspostaE1 := StringReplace(Resp, LF+'}'+LF+'{', ',', [rfReplaceAll]);
       //DEBUG
       //WriteToFile('c:\temp\E1JsonResp.txt', fREspostaE1);
     end;
   finally
     ASocket.Free;
   end;
 end;

 procedure TACBrPosPrinterElginE1Service.EnviarPorTXT(
   const LinhasImpressao: String);
 var
   ArqIN, ArqOUT: String;
   TempoLimite: TDateTime;
   SL: TStringList;
   Ok: Boolean;
 begin
   inc(fSeqArqE1);
   if fSeqArqE1 > 999 then
     fSeqArqE1 := 1;

   if (fPastaEntradaE1 = '') then
      ArqIN := ApplicationPath + 'E1'+PathDelim+'pathIN'
    else
      ArqIN := fPastaEntradaE1;
   ArqIN := PathWithDelim(ArqIN) + 'Comando' + IntToStrZero(fSeqArqE1, 3) + '.txt';

   if (fPastaSaidaE1 = '') then
      ArqOUT := ApplicationPath + 'E1'+PathDelim+'pathOUT'
    else
      ArqOUT := fPastaSaidaE1;
   ArqOUT := PathWithDelim(ArqOUT) + 'Comando' + IntToStrZero(fSeqArqE1, 3) + '*.txt';

   WriteToFile(ArqIN, LinhasImpressao);

   Ok := False;
   TempoLimite  := IncMilliSecond(Now, cTimeoutResp);
   SL := TStringList.Create;
   try
     while (Now < TempoLimite) do
     begin
       FindFiles(ArqOUT, SL);
       if (SL.Count > 0) then
       begin
         ArqOUT := SL[0];
         SL.Clear;
         SL.LoadFromFile(ArqOUT);
         fREspostaE1 := SL.Text;
         SysUtils.DeleteFile(ArqOUT);
         Ok := True;
         Break;
       end;

       Sleep(200);
     end;
   finally
     SL.Free;
   end;

   if (not Ok) then
     raise EACBrPosPrinterElginE1Service.Create('TimeOut');
 end;
{$EndIf}

end.


