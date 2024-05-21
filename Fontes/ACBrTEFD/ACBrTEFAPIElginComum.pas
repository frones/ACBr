{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Desenvolvido por:  MuriloS.A e Fernando Pasqueto                             }
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

unit ACBrTEFAPIElginComum;

interface

uses
  Classes, SysUtils,
  ACBrTEFAPIComum, ACBrJSON, ACBrTEFComum;

const
  CACBrTEFElgin_LIB = 'E1_Tef01.dll';
  ELGIN_OPERACAO_TEF = 0;
  ELGIN_OPERACAO_ADM = 1;
  ELGIN_OPERACAO_PIX = 2;
  ELGIN_COLETA_TIPO: array [0..4] of string =
    ('*',{Não exibivel podendo ser alfanumérico ou caracter especial}
    'A',{ Alfabético}'D',{Data/hora}'N',{Numérico/Decimais}'X'{Alfanumérico});

type

  Tsituacao = (tsSucesso, tsErro);

  TResultadoTransacao = record
    codigo: integer;
    sucesso: boolean;
    MsgElgin: string;
    MsgComercial: string;
  end;

const
  ELGIN_RESULT_TRANSACAO: array [0..9] of TResultadoTransacao =
    (
      (codigo: 0; sucesso: True{sucesso};
       MsgElgin: 'Com confirmação da Aplicação Comercial';
       MsgComercial: 'Confirmar transação'),
      (codigo: 1; sucesso: True{sucesso};
       MsgElgin: 'Sem confirmação da Aplicação Comercial';
       MsgComercial: 'Executar serviço'),
      (codigo: 2; sucesso: False{erro};
       MsgElgin: 'Sequencial inválido';
       MsgComercial: ''),
      (codigo: 3; sucesso: False{erro};
       MsgElgin: 'Transação cancelada pelo operador';
       MsgComercial: ''),
      (codigo: 4; sucesso: False{erro};
       MsgElgin: 'Transação cancelada pelo cliente';
       MsgComercial: ''),
      (codigo: 5; sucesso: False{erro};
       MsgElgin: 'Parâmetros insuficientes ou inválidos';
       MsgComercial: ''),
      (codigo: 6; sucesso: False{erro};
       MsgElgin: 'Problemas na conexão do ElginTef';
       MsgComercial: ''),
      (codigo: 7; sucesso: False{erro};
       MsgElgin: 'Problemas entre o ElginTef e a Rede';
       MsgComercial: ''),
      (codigo: 8; sucesso: False{erro};
       MsgElgin: 'Tempo limite de espera excedido';
       MsgComercial: 'Tempo limite de espera excedido'),
      (codigo: 9; sucesso: False{erro};
       MsgElgin: 'Problema desconhecido';
       MsgComercial: 'Cancelar transação')
    );

type
  {
  TACbrElginJsonTEF = Class
    retorno: String;
    sequencial: String;
    servico: String;

    identificadorPontoCaptura       : string;
    loja                            : string;
    nomeAC                          : string;
    nomeEstabelecimento             : string;
    statusClient                    : String;
    textoPinpad                     : String;
    versaoAC                        : String;

    automacao_coleta_opcao          : String;
    automacao_coleta_palavra_chave  : String;
    automacao_coleta_retorno        : String;
    automacao_coleta_sequencial     : String;
    automacao_coleta_tipo           : char;
    mensagemResultado               : String;
  End;


  TACbrElginJson = Class
    codigo: integer;
    mensagem: String;
    tef : TACbrElginJsonTEF;
  End;   }

  { TACBrTEFElginUtils }

  TACBrTEFElginUtils = class(TObject)
    // Métodos utilitários
  private
    class function JSPath(const aJSon: TACBrJSONObject;
      var aKEy: string): TACBrJSONObject;
  public
    class function DoFormataComprovante(const Match: string): string;
    class function FormataComprovante(const Comprovante: string): string;
    class function incrementarSequencial(const sequencial: string): string;
    class function getRetorno(const resp: string): string;
    class function FloatToJsonString(AValor: double): string;
    class function getComprovante(const resp: string; via: string): string;
    class function getSequencial(const resp: string): string;
    class function jsonify(jsonString: string): TACBrJSONObject;
    class function stringify(json: TACBrJSONObject): pansichar;
    class function getStringValue(json: TACBrJSONObject; const key: string): string;
    class function getIntegerValue(json: TACBrJSONObject; const key: string): integer;
    class function naoContem(const msg: string): boolean;
    class procedure Split(Delimiter: char; const Str: string; ListOfStrings: TStrings);
    class function FormatNumber(const AStr: string; dotIndex: integer = 2): string;
    class function MaskValor(const valor: string; dotIndex: integer): string;
    class function MaskDate(date: string; backSpace: boolean = False): string;
    class function RemoveNonNumericChars(const AStr: string): string;
    class function ExtractTextBetween(const Input, Delim1, Delim2: string): string;
    class function FormatRG(const rg: string): string;
    class function FormatCPF(const cpf: string): string;
    class function FormatCNPJ(const cnpj: string): string;
    class function FormatPhone(const phone: string): string;
    class function OperacaoAdminToElgin(TEF_OP: TACBrTEFOperacao): string;
    class function OperacaoAdminToElginInt(TEF_OP: TACBrTEFOperacao): integer;
  end;

  TACBrTEFRespElgin = class(TACBrTEFResp)
  public
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFElginAPI }

  TACBrTEFElginAPI = class
  private
    fpACBrTEFAPI: TACBrTEFAPIComum;
    FPathDLL: string;
    FInicializada: boolean;

    XGetProdutoTef: function(): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XGetClientTCP: function(): pansichar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XSetClientTCP: function(ip: PAnsiChar;
      porta: integer): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XConfigurarDadosPDV: function(textoPinpad: PAnsiChar;
      versaoAC: PAnsiChar;
      nomeEstabelecimento: PAnsiChar;
      loja: PAnsiChar;
      identificadorPontoCaptura: PAnsiChar): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XIniciarOperacaoTEF: function(dadosCaptura: PAnsiChar): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XRecuperarOperacaoTEF: function(dadosCaptura: PAnsiChar): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XRealizarPagamentoTEF: function(codigoOperacao: integer;
      dadosCaptura: PAnsiChar; novaTransacao: boolean): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XRealizarPixTEF: function(dadosCaptura: PAnsiChar;
      novaTransacao: boolean): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XRealizarAdmTEF: function(codigoOperacao: integer;
      dadosCaptura: PAnsiChar;
      novaTransacao: boolean): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XConfirmarOperacaoTEF: function(id: integer;
      acao: integer): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XFinalizarOperacaoTEF: function(id: integer): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XRealizarColetaPinPad: function(tipoColeta: integer;
      confirmar: boolean): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};
    XConfirmarCapturaPinPad: function(tipoCaptura: integer;
      dadosCaptura: PAnsiChar): PAnsiChar;
    {$IfDef MSWINDOWS} stdcall{$Else}cdecl{$EndIf};

    procedure SetInicializada(AValue: boolean);

    procedure Log(Msg: AnsiString);
    procedure LoadDLLFunctions;
    procedure UnLoadDLLFunctions;
  public
    constructor Create(aTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    function GetProdutoTef(): PAnsiChar;
    function GetClientTCP(): PAnsiChar;
    function SetClientTCP(ip: PAnsiChar; porta: Integer): PAnsiChar;

    function ConfigurarDadosPDV(textoPinpad: PAnsiChar;
      versaoAC: PAnsiChar;
      nomeEstabelecimento: PAnsiChar;
      loja: PAnsiChar;
      identificadorPontoCaptura: PAnsiChar): PAnsiChar;

    function IniciarOperacaoTEF(dadosCaptura: PAnsiChar): PAnsiChar;
    function RecuperarOperacaoTEF(dadosCaptura: PAnsiChar): PAnsiChar;
    function RealizarPagamentoTEF(codigoOperacao: Integer; dadosCaptura: PAnsiChar;
      novaTransacao: Boolean): PAnsiChar;
    function RealizarPixTEF(dadosCaptura: PAnsiChar; novaTransacao: Boolean): PAnsiChar;
    function RealizarAdmTEF(codigoOperacao: Integer; dadosCaptura: PAnsiChar;
      novaTransacao: Boolean): PAnsiChar;
    function ConfirmarOperacaoTEF(id: Integer; acao: Integer): PAnsiChar;
    function FinalizarOperacaoTEF(id: Integer): PAnsiChar;
    function RealizarColetaPinPad(tipoColeta: Integer; confirmar: Boolean): PAnsiChar;
    function ConfirmarCapturaPinPad(tipoCaptura: Integer; dadosCaptura: PAnsiChar): PAnsiChar;

    property PathDLL: string read fPathDLL write fPathDLL;
    property Inicializada: boolean read fInicializada write SetInicializada;
  end;

procedure ConteudoToPropertyElgin(AACBrTEFResp: TACBrTEFResp);

implementation

uses
  StrUtils, Math, DateUtils,
  ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO, ACBrUtil.DateTime;

procedure ConteudoToPropertyElgin(AACBrTEFResp: TACBrTEFResp);
var
  oJSON: TACBRJsonObject;
  lStr: string;
  sucessoInt: Integer;

  function JsonKey(key: string): string;
  begin
    Result := TACBrTEFElginUtils.getStringValue(oJson, key);
  end;

  function JsonKeyDateTime(Key: string): TDateTime;
  var
    s: string;
  begin
    s := JsonKey(Key);
    Result := ACBrUtil.DateTime.StringToDateTime(s, 'dd/mm/yyyy hh:mm:ss');
  end;

  function JsonKeyInt(key: string): integer;
  var
    sValue: string;
  begin
    sValue := TACBrTEFElginUtils.getStringValue(oJson, key);
    Result := StrToIntDef(sValue, 0);
  end;

  function JsonKeyFloat(key: string): double;
  var
    SVAlue: string;
    sInteiro, sDecimal: string;
  begin
    SVAlue := JsonKey(key);
    //SVAlue := PadLeft(SVAlue,3,'0');
    SDecimal := Copy(SVAlue, Length(SVAlue) - 1, 2);
    SInteiro := Copy(SVAlue, 1, Length(SVAlue) - 2);
    SVAlue := Format('%s,%s', [SInteiro, SDecimal]);
    Result := StrToFloatDef(SVAlue, 0);
  end;

  procedure ConteudoToParcelas;
  var
    parcela_valor, parcela_vencimento: string;
  begin
    AACBrTEFResp.QtdParcelas := StrToIntDef(JsonKey('tef.numeroParcelas'), 1);

    if AACBrTEFResp.TipoOperacao <> opParcelado then
      exit;

    case AnsiIndexStr(JsonKey('tef.tipoFinanciamento'),
        ['Estabelecimento', 'Administradora']) of
      0: AACBrTEFResp.ParceladoPor := parcADM;
      1: AACBrTEFResp.ParceladoPor := parcLoja;
    end;

    parcela_valor := JsonKey('tef.transacao_parcela_valor');
    parcela_vencimento := JsonKey('tef.transacao_parcela_vencimento');
  end;

  procedure ConteudoToComprovantes;
  var
    C1, C2: string;
  begin
    C1 := JsonKey('tef.comprovanteDiferenciadoPortador');
    C1 := TACBrTEFElginUtils.FormataComprovante(C1);

    C2 := JsonKey('tef.comprovanteDiferenciadoLoja');
    C2 := TACBrTEFElginUtils.FormataComprovante(C2);

    with AACBrTEFResp.ImagemComprovante2aVia do
    begin
      Clear;
      //append(C1);
      Text := StringToBinaryString(C1);
    end;

    with AACBrTEFResp.ImagemComprovante1aVia do
    begin
      Clear;
      //append(C2);
      Text := StringToBinaryString(C2);
    end;

    with AACBrTEFResp do
      QtdLinhasComprovante := max(ImagemComprovante1aVia.Count,
        ImagemComprovante2aVia.Count);
  end;

begin
  {Tratar respostas }
  oJSON := TACBrTEFElginUtils.jsonify(AACBrTEFResp.Conteudo.Conteudo.Text);
  if Assigned(oJSON) = False then
    exit;

  if JsonKey('tef.mensagemResultado') <> '' then
  begin
    AACBrTEFResp.TextoEspecialOperador := JsonKey('tef.mensagemResultado');
    AACBrTEFResp.TextoEspecialCliente := JsonKey('tef.mensagemResultado');
  end;

  AACBrTEFResp.Finalizacao := JsonKey('tef.sequencial');
  lStr := '';
  {informações}
  lStr := JsonKey('tef.automacao_coleta_retorno');
  if (lStr = '') then
  begin
    {transações}
    lStr := JsonKey('tef.resultadoTransacao');
  end;

  sucessoInt := StrToIntDef(lStr, 9);
  AACBrTEFResp.Sucesso := sucessoInt in [0, 1];
  AACBrTEFResp.Confirmar := sucessoInt in [0];

  lStr := JsonKey('tef.formaPagamento');
  case AnsiIndexStr(lStr, ['A vista', 'Parcelado', 'Pre-datado']) of
    0: AACBrTEFResp.TipoOperacao := opAvista;
    1: AACBrTEFResp.TipoOperacao := opParcelado;
    2: AACBrTEFResp.TipoOperacao := opPreDatado;
    else
      AACBrTEFResp.TipoOperacao := opOutras
  end;

  ConteudoToParcelas;
  ConteudoToComprovantes;

  lStr := JsonKey('tef.tipoCartao');
  case AnsiIndexStr(lStr, ['Debito', 'Credito']) of
    0: AACBrTEFResp.Debito := True;
    1: AACBrTEFResp.Credito := True;
  end;

  AACBrTEFResp.DataHoraTransacaoHost := JsonKeyDateTime('tef.dataHoraTransacao');
  AACBrTEFResp.NSU_TEF := JsonKey('tef.nsuTerminal');
  AACBrTEFResp.ValorTotal := JsonKeyFloat('tef.valorTotal');
  AACBrTEFResp.ValorOriginal := JsonKeyFloat('tef.valorTotal');
  AACBrTEFResp.NSU := JsonKey('tef.nsuTransacao');
  with AACBrTEFResp.NFCeSAT do
  begin
    DonoCartao := '';
    CNPJCredenciadora := JsonKey('tef.panMascarado');
    Bandeira := JsonKey('tef.nomeBandeira');
  end;
  AACBrTEFResp.Rede := JsonKey('tef.nomeProvedor');
  AACBrTEFResp.NumeroLoteTransacao := JsonKeyint('tef.transacao_codigo_vespague');
  AACBrTEFREsp.ModalidadePagto := JsonKey('tef.transacao_codigo_vespague');
end;

{ TACBrTEFElginAPI }

constructor TACBrTEFElginAPI.Create(aTEFAPI: TACBrTEFAPIComum);
begin
  fpACBrTEFAPI := aTEFAPI;
  fInicializada := False;
end;

destructor TACBrTEFElginAPI.Destroy;
begin
  inherited;
end;

procedure TACBrTEFElginAPI.Log(Msg: AnsiString);
begin
  fpACBrTEFAPI.GravarLog(msg);
end;

function TACBrTEFElginAPI.ConfigurarDadosPDV(textoPinpad: PAnsiChar;
  versaoAC: PAnsiChar; nomeEstabelecimento: PAnsiChar; loja: PAnsiChar;
  identificadorPontoCaptura: PAnsiChar): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xConfigurarDadosPDV) then
  begin
    Result := xConfigurarDadosPDV(textoPinpad, versaoAC, nomeEstabelecimento, loja, identificadorPontoCaptura);

    log('ConfigurarDadosPDV :' + TACBrTEFELginUtils.jsonify(Result).ToJson);
    log('--> textoPinpad :' + textoPinpad);
    log('--> versaoAC :' + versaoAC);
    log('--> nomeEstabelecimento :' + nomeEstabelecimento);
    log('--> loja :' + loja);
    log('--> identificadorPontoCaptura :' + identificadorPontoCaptura);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.ConfirmarCapturaPinPad(tipoCaptura: Integer;
  dadosCaptura: PAnsiChar): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xConfirmarCapturaPinPad) then
  begin
    Result := xConfirmarCapturaPinPad(tipoCaptura, dadosCaptura);

    log('ConfirmarCapturaPinPad: ' + TACBrTEFELginUtils.jsonify(Result).ToJson);
    log('--> tipoCaptura: ' + IntToStr(tipoCaptura));
    log('--> dadosCaptura: ' + dadosCaptura);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.ConfirmarOperacaoTEF(id: Integer; acao: Integer
  ): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xConfirmarOperacaoTEF) then
  begin
    Result := xConfirmarOperacaoTEF(id, acao);

    log('ConfirmarOperacaoTEF :' + Result);
    log('--> id: ' + IntToStr(id));
    log('--> acao: ' + IntToStr(acao));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.FinalizarOperacaoTEF(id: Integer): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xFinalizarOperacaoTEF) then
  begin
    Result := xFinalizarOperacaoTEF(id);

    log('FinalizarOperacaoTEF : ' + Result);
    log('--> id: ' + IntToStr(id));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.GetClientTCP: PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xGetClientTCP) then
  begin
    Result := xGetClientTCP();

    Log('GetClientTCP: ' + Result);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.GetProdutoTef: PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xGetProdutoTef) then
  begin
    Result := xGetProdutoTef();

    Log('GetProdutoTef: ' + Result);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.IniciarOperacaoTEF(dadosCaptura: PAnsiChar): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(XIniciarOperacaoTEF) then
  begin
    Result := XIniciarOperacaoTEF(dadosCaptura);

    log('IniciarOperacaoTEF : ' + TACBrTEFELginUtils.jsonify((Result)).ToJson);
    log('--> dadosCaptura : ' + TACBrTEFELginUtils.jsonify(dadosCaptura).ToJson);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.RealizarAdmTEF(codigoOperacao: Integer;
  dadosCaptura: PAnsiChar; novaTransacao: Boolean): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(XRealizarAdmTEF) then
  begin
    Result := XRealizarAdmTEF(codigoOperacao, dadosCaptura, novaTransacao);

    log('RealizarAdmTEF: ' + Result);
    log('--> codigoOperacao: ' + IntToStr(codigoOperacao));
    log('--> dadosCaptura: ' + dadosCaptura);
    log('--> novaTransacao:  ' + BoolToStr(novaTransacao));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.RealizarColetaPinPad(tipoColeta: Integer;
  confirmar: Boolean): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xRealizarColetaPinPad) then
  begin
    Result := xRealizarColetaPinPad(tipoColeta, confirmar);

    log('RealizarColetaPinPad: ' + TACBrTEFELginUtils.jsonify((Result)).ToJson);
    log('--> tipoColeta: ' + IntToStr(tipoColeta));
    log('--> confirmar: ' + BoolToStr(confirmar));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.RealizarPagamentoTEF(codigoOperacao: Integer;
  dadosCaptura: PAnsiChar; novaTransacao: Boolean): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xRealizarPagamentoTEF) then
  begin
    Result := xRealizarPagamentoTEF(codigoOperacao, dadosCaptura, novaTransacao);

    log('RealizarPagamentoTEF: ' + Result);
    log('--> codigoOperacao : ' + IntToStr(codigoOperacao));
    log('--> dadosCaptura : ' + dadosCaptura);
    log('--> novaTransacao : ' + BoolToStr(novaTransacao));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.RealizarPixTEF(dadosCaptura: PAnsiChar;
  novaTransacao: Boolean): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xRealizarPixTEF) then
  begin
    Result := xRealizarPixTEF(dadosCaptura, novaTransacao);

    Log('RealizarPixTEF: ' + Result);
    Log('--> dadosCaptura: ' + dadosCaptura);
    Log('--> novaTransacao: ' + BoolToStr(novaTransacao));
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.RecuperarOperacaoTEF(dadosCaptura: PAnsiChar): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xRecuperarOperacaoTEF) then
  begin
    Result := xRecuperarOperacaoTEF(dadosCaptura);

    log('RecuperarOperacaoTEF : ' + Result);
    log('--> dadosCaptura : ' + dadosCaptura);
  end
  else
    Result := nil;
end;

function TACBrTEFElginAPI.SetClientTCP(ip: PAnsiChar; porta: Integer
  ): PAnsiChar;
begin
  LoadDLLFunctions;

  if Assigned(xSetClientTCP) then
  begin
    Result := xSetClientTCP(ip, porta);

    log('xSetClientTCP :' + Result);
    log('-->{ip : ' + utf8string(ip) + ', porta :' + IntToStr(porta) + '}');
  end
  else
    Result := nil;
end;

procedure TACBrTEFElginAPI.SetInicializada(AValue: boolean);
begin
  if fInicializada = AValue then
    Exit;

  if AValue then
    LoadDLLFunctions
  else
    UnLoadDLLFunctions;
end;

procedure TACBrTEFElginAPI.LoadDLLFunctions;

  procedure ElginFunctionDetect(FuncName: AnsiString; var LibPointer: Pointer);
  var
    sLibName: string;
  begin
    if not Assigned(LibPointer) then
    begin
      // Verifica se exite o caminho das DLLs
      sLibName := '';
      if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

      // Concatena o caminho se exitir mais o nome da DLL.
      sLibName := sLibName + CACBrTEFElgin_LIB;
      if not FunctionDetect(sLibName, FuncName, LibPointer) then
      begin
        log('func(falha) : ' + FuncName + 'path:' + sLibName);
        LibPointer := nil;
        raise EACBrTEFErro.Create(ACBrStr('Erro ao carregar a função:' + FuncName + ' de: ' + sLibName));
      end;

      log('func(ok):' + FuncName + 'path:' + sLibName);
    end;
  end;

begin
  if fInicializada then
    Exit;

  ElginFunctionDetect('SetClientTCP', @XSetClientTCP);
  ElginFunctionDetect('ConfigurarDadosPDV', @XConfigurarDadosPDV);
  ElginFunctionDetect('IniciarOperacaoTEF', @XIniciarOperacaoTEF);
  ElginFunctionDetect('RecuperarOperacaoTEF', @XRecuperarOperacaoTEF);
  ElginFunctionDetect('RealizarPagamentoTEF', @XRealizarPagamentoTEF);
  ElginFunctionDetect('RealizarPixTEF', @XRealizarPixTEF);
  ElginFunctionDetect('RealizarAdmTEF', @XRealizarAdmTEF);
  ElginFunctionDetect('ConfirmarOperacaoTEF', @XConfirmarOperacaoTEF);
  ElginFunctionDetect('FinalizarOperacaoTEF', @XFinalizarOperacaoTEF);
  ElginFunctionDetect('RealizarColetaPinPad', @XRealizarColetaPinPad);
  ElginFunctionDetect('ConfirmarCapturaPinPad', @XConfirmarCapturaPinPad);

  fInicializada := True;
end;

procedure TACBrTEFElginAPI.UnLoadDLLFunctions;
var
  sLibName: string;
begin
  if not fInicializada then
    Exit;

  sLibName := '';
  if Length(PathDLL) > 0 then
    sLibName := PathWithDelim(PathDLL);

  UnLoadLibrary(sLibName + CACBrTEFElgin_Lib);

  xSetClientTCP := nil;
  xConfigurarDadosPDV := nil;
  xIniciarOperacaoTEF := nil;
  xRecuperarOperacaoTEF := nil;
  xRealizarPagamentoTEF := nil;
  xRealizarPixTEF := nil;
  xRealizarAdmTEF := nil;
  xConfirmarOperacaoTEF := nil;
  xFinalizarOperacaoTEF := nil;
  xRealizarColetaPinPad := nil;
  xConfirmarCapturaPinPad := nil;

  fInicializada := False;
end;

{ TACBrTEFRespElgin }

procedure TACBrTEFRespElgin.ConteudoToProperty;
begin
  inherited;
  ConteudoToPropertyElgin(Self);
end;

{ TACBrTEFElginUtils }

class function TACBrTEFElginUtils.DoFormataComprovante(const Match: string
  ): string;
const
  TabelaASCii:
    array  [0..13] of string =
    ('0',      //   0  00000000  U+0000  \0  byte nulo
    char(0),   //     1  00000001  U+0001    não usamos
    char(0),   //     2  00000010  U+0002    não usamos
    char(0),   //     3  00000011  U+0003    não usamos
    char(0),   //     4  00000100  U+0004    não usamos
    char(0),   //     5  00000101  U+0005    não usamos
    char(0),   //     6  00000110  U+0006    não usamos
    'a',       //     7  00000111  U+0007  \a  apito
    'b',       //     8  00001000  U+0008  \b  backspace
    't',       //     9  00001001  U+0009  \t  tabulação
    'n',       //    10  00001010  U+000A  \n  fim de linha
    'v',       //    11  00001011  U+000B  \v  tab vertical
    'f',       //    12  00001100  U+000C  \f  fim de página
    'r');      //    13  00001101  U+000D  \r  carriage return
var
  sLast, sCurr, sV: string;
  len, idx, iDec: integer;
begin
  len := length(match);
  sLast := '';
  sCurr := '';
  sV := '';

  for idx := 0 to (len - 1) do
  begin
    if idx > 0 then
      sLast := copy(match, idx, 1);//match [idx ];

    sCurr := copy(match, idx + 1, 1);//match [idx];
    if sLast = '\' then
    begin
      iDec := AnsiIndexStr(sCurr, TabelaASCii);
      if iDec >= 0 then
        sCurr := char(iDec);
    end;
    sV := sV + sCurr;
  end;

  Result := sV;
end;

class function TACBrTEFElginUtils.ExtractTextBetween(
  const Input, Delim1, Delim2: string): string;
var
  aPos, bPos: integer;
begin
  Result := '';
  aPos := Pos(Delim1, Input);
  if aPos > 0 then
  begin
    bPos := PosEx(Delim2, Input, aPos + Length(Delim1));
    if bPos > 0 then
      Result := Copy(Input, aPos + Length(Delim1), bPos - (aPos + Length(Delim1)));
  end;
end;

class function TACBrTEFElginUtils.FloatToJsonString(AValor: double): string;
var
  iPos: integer;
  sValor: string;
begin
  sValor := CurrToStrF(AVAlor, ffCurrency, 2);
  iPos := pos(sValor, ',');
  if iPos > 0 then
    sValor := PadRight(sValor, (length(sValor) - iPos), '0');

  Result := sValor;
end;

class function TACBrTEFElginUtils.FormataComprovante(const Comprovante: string
  ): string;
begin
  Result := DoFormataComprovante(Comprovante);
  //TRegEx.Replace(Comprovante,'(\\.)',DoFormataComprovante );
end;

class function TACBrTEFElginUtils.FormatCNPJ(const cnpj: string): string;
begin
  Result := copy(cnpj, 1, 2) + '.' + copy(cnpj, 3, 3) +
    '.' + copy(cnpj, 6, 3) + '/' + copy(cnpj, 9, 4) +
    '-' + copy(cnpj, 13, 2);
end;

class function TACBrTEFElginUtils.FormatCPF(const cpf: string): string;
begin
  Result := copy(cpf, 1, 3) + '.' + copy(cpf, 4, 3) + '.' +
            copy(cpf, 7, 3) + '.' + copy(cpf, 10, 2);
end;

class function TACBrTEFElginUtils.FormatNumber(const AStr: string;
  dotIndex: integer): string;
var
  dotPos: integer;
  cleanString: string;
  num: double;
  len: integer;
begin
  cleanString := RemoveNonNumericChars(AStr);

  if Length(cleanString) > 8 then
    Result := '0.00';

  if cleanString = '' then
    cleanstring := '0'
  else
    cleanString := IntToStr(StrToInt(cleanString));

  len := Length(cleanString);
  if len <= 1 then
    Result := '0.0' + cleanString
  else if len <= 2 then
    Result := '0.' + cleanString
  else
  begin
    dotPos := Length(AStr) - dotIndex;
    Result := Copy(cleanString, 1, dotPos) + '.' + Copy(cleanString, dotPos + 1, 2);
  end;
end;

class function TACBrTEFElginUtils.FormatPhone(const phone: string): string;
begin
  Result := '(' + copy(phone, 1, 2) + ') ' + copy(phone, 3, 5) + '-' + copy(phone, 8, 4);
end;

class function TACBrTEFElginUtils.FormatRG(const rg: string): string;
begin
  Result := copy(rg, 1, 2) + '.' + copy(rg, 3, 3) + '.' +
    copy(rg, 6, 3) + '-' + copy(rg, 9, 1);
end;

class function TACBrTEFElginUtils.getComprovante(const resp: string; via: string): string;
begin
  if via = 'loja' then
    Result := getStringValue(jsonify(resp), 'tef.comprovanteDiferenciadoLoja')
  else if via = 'cliente' then
    Result := getStringValue(jsonify(resp), 'tef.comprovanteDiferenciadoPortador')
  else
    Result := '';
end;

class function TACBrTEFElginUtils.getIntegerValue(json: TACBrJSONObject;
  const key: string): integer;
var
  Value: integer;
begin
  if (json.ValueExists(key)) then
  begin
    json.Value(Key, Value);
    Result := Value;
  end
  else
    Result := -1;
end;

class function TACBrTEFElginUtils.getRetorno(const resp: string): string;
begin
  Result := getStringValue(jsonify(resp), 'tef.retorno');
end;

class function TACBrTEFElginUtils.getSequencial(const resp: string): string;
begin
  Result := getStringValue(jsonify(resp), 'tef.sequencial');
end;

class function TACBrTEFElginUtils.getStringValue(json: TACBrJSONObject;
  const key: string): string;
var
  sValue, sKey: string;
  jsO: TACBrJSONObject;
begin
  sKey := key;
  jsO := JSPath(json, sKey);

  if Assigned(jsO) then
  begin
    if (jsO.ValueExists(sKey)) then
    begin
      sValue := jsO.AsString[sKey];
      Result := sValue; //copy(valueSt, 2, valueSt.Length-2);
    end
    else
      Result := '';
  end;
end;

class function TACBrTEFElginUtils.incrementarSequencial(const sequencial: string): string;
var
  seq: integer;
begin
  try
    seq := StrToInt(sequencial) + 1;
    Result := IntToStr(seq);
  except
    on Exception: EConvertError do
      Result := ''; // sequencial informado não numérico
  end;
end;

class function TACBrTEFElginUtils.jsonify(jsonString: string): TACBrJSONObject;
begin
  Result := TACBrJSONObject.Parse(UTF8ToNativeString(jsonString));
end;

class function TACBrTEFElginUtils.JSPath(const aJSon: TACBrJSONObject;
  var aKEy: string): TACBrJSONObject;
var
  iPosObj: integer;
  sO: string;
begin
  iPosObj := Pos('.', akey);
  Result := ajson;
  if iPosObj <= 0 then
    exit;

  sO := Copy(akey, 1, iPosObj - 1);
  akey := Copy(akey, iPosObj + 1, length(akey) - 1);

  Result := ajson.AsJSONObject[sO];
end;

class function TACBrTEFElginUtils.MaskDate(date: string; backSpace: boolean): string;
var
  cleanDate: string;
  len: integer;
begin
  cleanDate := removenonNumericChars(date);
  len := length(cleanDate);

  if len >= 5 then
    Result := copy(cleanDate, 1, 2) + '/' + copy(cleanDate, 3, 2) + '/' + copy(cleanDate, 5, 1)
  else
    Result := cleanDate;
end;

class function TACBrTEFElginUtils.MaskValor(const valor: string; dotIndex: integer): string;
var
  len, dotPos: integer;
  input: string;
begin
  input := removeNonNumericChars(valor);
  if input = '' then
    input := '0';

  input := IntToStr(StrToInt(input));
  len := length(input);

  if len <= 1 then
  begin
    Result := '0.' + input;
  end
  else if (len <= 3) and (dotIndex = 3) then
  begin
    if len = 3 then Result := '0.' + input;
    if len = 2 then Result := '0.0' + input;
    if len = 1 then Result := '0.00';
  end
  else
  begin
    dotPos := len - dotIndex;
    Result := Copy(input, 1, dotPos) + '.' + Copy(input, dotPos + 1, len);
  end;
end;

class function TACBrTEFElginUtils.naoContem(const msg: string): boolean;
const
  strings: array [0..4] of string =
    ('AGUARDE', 'FINALIZADA', 'PASSAGEM', 'CANCELADA', 'APROVADA');
var
  idx, P: integer;
  element: string;
  contem: boolean;
begin
  contem := False;

  for idx := 0 to 4 do
  begin
    element := strings[idx];
    P := Pos(element, msg);
    if P = 0 then
      contem := True
    else
    begin
      contem := False;
      break;
    end;
  end;

  Result := contem;
end;

class function TACBrTEFElginUtils.OperacaoAdminToElgin(TEF_OP: TACBrTEFOperacao): string;
begin
  case TEF_OP of
    tefopNenhuma: Result := '0';          // Adm  --> Perguntar operacao
    tefopCancelamento: Result := '1';     // Adm  --> Cancelamento
    tefopPagamento: Result := '2';        // Adm  --> Pendencias
    tefopReimpressao: Result := '3';      // Adm  --> Reimpressao
    else
      Result := '0';
  end;
end;

class function TACBrTEFElginUtils.OperacaoAdminToElginInt(TEF_OP: TACBrTEFOperacao): integer;
var
  sV: string;
begin
  sV := OperacaoAdminToElgin(TEF_OP);
  Result := StrToIntDef(sV, 0);
end;

class function TACBrTEFElginUtils.RemoveNonNumericChars(const AStr: string): string;
begin
  Result := OnlyNumber(AStr);
end;

class procedure TACBrTEFElginUtils.Split(Delimiter: char; const Str: string;
  ListOfStrings: TStrings);
begin
  ACBrUtil.Strings.AddDelimitedTextToList(Str, Delimiter, ListOfStrings);
end;

class function TACBrTEFElginUtils.stringify(json: TACBrJSONObject): PAnsiChar;
var
  sJS: string;
begin
  sJS := json.ToJson;
  Result := PAnsiChar(AnsiString(sJS));
end;

end.
