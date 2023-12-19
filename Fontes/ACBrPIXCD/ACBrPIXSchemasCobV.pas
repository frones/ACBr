{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasCobV;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase, ACBrPIXSchemasCob, ACBrPIXSchemasPix,
  ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation;

resourcestring
  sErroDescontoDataFixaLimit = 'Limite de descontoDataFixa atingido (3)';

type

  TACBrPIXValoresModalidade = (
    pvmNenhum,
    pvmValorFixo,
    pvmPercentual
  );

  TACBrPIXDescontoModalidade = (
    pdmNenhum,
    pdmValorFixo,             // 1: Valor Fixo até a data informada
    pdmPercentual,            // 2: Percentual até a data informada
    pdmValorDiaCorrido,       // 3: Valor por antecipação dia corrido
    pdmValorDiaUtil,          // 4: Valor por antecipação dia útil
    pdmPercentualDiaCorrido,  // 5: Percentual por antecipação dia corrido
    pdmPercentualDiaUtil      // 6: Percentual por antecipação dia útil
  );

  TACBrPIXJurosModalidade = (
    pjmNenhum,
    pjmValorDiasCorridos,          // 1: Valor (dias corridos)
    pjmPercentualDiaDiasCorridos,  // 2: Percentual ao dia (dias corridos)
    pjmPercentualMesDiasCorridos,  // 3: Percentual ao mês (dias corridos)
    pjmPercentualAnoDiasCorridos,  // 4: Percentual ao ano (dias corridos)
    pjmValorDiasUteis,             // 5: Valor (dias úteis)
    pjmPercentualDiaDiasUteis,     // 6: Percentual ao dia (dias úteis)
    pjmPercentualMesDiasUteis,     // 7: Percentual ao mês (dias úteis)
    pjmPercentualAnoDiasUteis      // 8: Percentual ao ano (dias úteis)
  );

  { TACBrPIXCalendarioCobVBase }

  TACBrPIXCalendarioCobVBase = class(TACBrPIXSchema)
  private
    fcriacao: TDateTime;
    fcriacao_Bias: Integer;
    fdataDeVencimento: TDateTime;
    fvalidadeAposVencimento: Integer;
    procedure SetCriacao(AValue: TDateTime);
    procedure SetDataDeVencimento(AValue: TDateTime);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property criacao: TDateTime read fcriacao write SetCriacao;
    property criacao_Bias: Integer read fcriacao_Bias write fcriacao_Bias;
    property dataDeVencimento: TDateTime read fdataDeVencimento write SetDataDeVencimento;
    property validadeAposVencimento: Integer read fvalidadeAposVencimento write fvalidadeAposVencimento;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCalendarioCobVBase);
  end;

  { TACBrPIXCalendarioCobVSolicitada }

  TACBrPIXCalendarioCobVSolicitada = class(TACBrPIXCalendarioCobVBase)
  public
    property dataDeVencimento;
    property validadeAposVencimento;
  end;


  { TACBrPIXCalendarioCobVGerada }

  TACBrPIXCalendarioCobVGerada = class(TACBrPIXCalendarioCobVSolicitada)
  public
    property criacao;
    property criacao_Bias;
  end;

  { TACBrPIXModalidadeValor }

  TACBrPIXModalidadeValor = class(TACBrPIXSchema)
  private
    fmodalidade: TACBrPIXValoresModalidade;
    fvalorPerc: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    procedure Assign(aSource: TACBrPIXModalidadeValor);
    function IsEmpty: Boolean; override;

    property modalidade: TACBrPIXValoresModalidade read fmodalidade write fmodalidade;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
  end;

  { TACBrPIXJuros }

  TACBrPIXJuros = class(TACBrPIXSchema)
  private
    fmodalidade: TACBrPIXJurosModalidade;
    fvalorPerc: Currency;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    procedure Assign(aSource: TACBrPIXJuros);
    function IsEmpty: Boolean; override;

    property modalidade: TACBrPIXJurosModalidade read fmodalidade write fmodalidade;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
  end;

  { TACBrPIXDescontoDataFixa }

  TACBrPIXDescontoDataFixa = class(TACBrPIXSchema)
  private
    fdata: TDateTime;
    fvalorPerc: Currency;
    procedure SetData(AValue: TDateTime);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXDescontoDataFixa);
    function IsEmpty: Boolean; override;

    property data: TDateTime read fdata write SetData;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
  end;

  { TACBrPIXDescontosDataFixa }

  TACBrPIXDescontosDataFixa = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXDescontoDataFixa;
    procedure SetItem(Index: Integer; Value: TACBrPIXDescontoDataFixa);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ADescontoDataFixa: TACBrPIXDescontoDataFixa): Integer;
    Procedure Insert(Index: Integer; ADescontoDataFixa: TACBrPIXDescontoDataFixa);
    function New: TACBrPIXDescontoDataFixa;
    property Items[Index: Integer]: TACBrPIXDescontoDataFixa read GetItem write SetItem; default;
  end;


  { TACBrPIXDesconto }

  TACBrPIXDesconto = class(TACBrPIXSchema)
  private
    fmodalidade: TACBrPIXDescontoModalidade;
    fvalorPerc: Currency;
  protected
    fdescontosDataFixa: TACBrPIXDescontosDataFixa;

    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(aSource: TACBrPIXDesconto);
    function IsEmpty: Boolean; override;

    property modalidade: TACBrPIXDescontoModalidade read fmodalidade write fmodalidade;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
    property descontosDataFixa: TACBrPIXDescontosDataFixa read fdescontosDataFixa;
  end;

  { TACBrPIXCobVValor }

  TACBrPIXCobVValor = class(TACBrPIXSchema)
  private
    fabatimento: TACBrPIXModalidadeValor;
    fdesconto: TACBrPIXDesconto;
    fjuros: TACBrPIXJuros;
    fmulta: TACBrPIXModalidadeValor;
    foriginal: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVValor);

    property original: Currency read foriginal write foriginal;
    property multa: TACBrPIXModalidadeValor read fmulta;
    property juros: TACBrPIXJuros read fjuros;
    property abatimento: TACBrPIXModalidadeValor read fabatimento;
    property desconto: TACBrPIXDesconto read fdesconto;
  end;

  { TACBrPIXCobVSolicitada }

  TACBrPIXCobVSolicitada = class(TACBrPIXCobBaseCopiaCola)
  private
    fcalendario: TACBrPIXCalendarioCobVSolicitada;
    fdevedor: TACBrPIXDadosDevedor;
    floc: TACBrPIXLocationCobSolicitada;
    fvalor: TACBrPIXCobVValor;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVSolicitada);
    function LoadFromIni(aIniStr: String): Boolean; override;

    property calendario: TACBrPIXCalendarioCobVSolicitada read fcalendario;
    property devedor: TACBrPIXDadosDevedor read fdevedor;
    property loc: TACBrPIXLocationCobSolicitada read floc;
    property valor: TACBrPIXCobVValor read fvalor;
  end;

  { TACBrPIXCobVRevisada }

  TACBrPIXCobVRevisada = class(TACBrPIXCobVSolicitada)
  private
    fstatus: TACBrPIXStatusCobranca;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVRevisada);
    function LoadFromIni(aIniStr: String): Boolean; override;

    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
  end;

  { TACBrPIXCobVGerada }

  TACBrPIXCobVGerada = class(TACBrPIXCobBaseCopiaCola)
  private
    fcalendario: TACBrPIXCalendarioCobVGerada;
    fdevedor: TACBrPIXDadosDevedor;
    floc: TACBrPIXLocationCompleta;
    frecebedor: TACBrPIXDadosRecebedor;
    frevisao: Integer;
    fstatus: TACBrPIXStatusCobranca;
    ftxId: String;
    fvalor: TACBrPIXCobVValor;
    procedure SetRevisao(AValue: Integer);
    procedure SetTxId(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVGerada);

    property calendario: TACBrPIXCalendarioCobVGerada read fcalendario;
    property txId: String read ftxId write SetTxId;
    property revisao: Integer read frevisao write SetRevisao;
    property devedor: TACBrPIXDadosDevedor read fdevedor;
    property recebedor: TACBrPIXDadosRecebedor read frecebedor;
    property loc: TACBrPIXLocationCompleta read floc;
    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
    property valor: TACBrPIXCobVValor read fvalor;
  end;

  { TACBrPIXCobVCompleta }

  TACBrPIXCobVCompleta = class(TACBrPIXCobVGerada)
  private
    fpix: TACBrPIXArray;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXCobVCompleta);

    property pix: TACBrPIXArray read fpix;
  end;

  { TACBrPIXCobVCompletaArray }

  TACBrPIXCobVCompletaArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobVCompleta;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobVCompleta);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ACobV: TACBrPIXCobVCompleta): Integer;
    Procedure Insert(Index: Integer; ACobV: TACBrPIXCobVCompleta);
    function New: TACBrPIXCobVCompleta;
    property Items[Index: Integer]: TACBrPIXCobVCompleta read GetItem write SetItem; default;
  end;

  function ValoresModalidadeToString(aValue: TACBrPIXValoresModalidade): String;
  function DescontoModalidadeToString(aValue: TACBrPIXDescontoModalidade): String;
  function JurosModalidadeToString(aValue: TACBrPIXJurosModalidade): String;

implementation

uses
  DateUtils, Math, IniFiles,
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

function ValoresModalidadeToString(aValue: TACBrPIXValoresModalidade): String;
begin
  case aValue of
    pvmValorFixo: Result := 'Valor Fixo';
    pvmPercentual: Result := 'Percentual';
  else
    Result := 'Nenhum';
  end;
end;

function DescontoModalidadeToString(aValue: TACBrPIXDescontoModalidade): String;
begin
  case aValue of
    pdmValorFixo: Result := ACBrStr('Valor Fixo até a data informada');
    pdmPercentual: Result := ACBrStr('Percentual até a data informada');
    pdmValorDiaCorrido: Result := ACBrStr('Valor por antecipação dia corrido');
    pdmValorDiaUtil: Result := ACBrStr('Valor por antecipação dia útil');
    pdmPercentualDiaCorrido: Result := ACBrStr('Percentual por antecipação dia corrido');
    pdmPercentualDiaUtil: Result := ACBrStr('Percentual por antecipação dia útil');
  else
    Result := 'Nenhum desconto aplicado';
  end;
end;

function JurosModalidadeToString(aValue: TACBrPIXJurosModalidade): String;
begin
  case aValue of
    pjmValorDiasCorridos: Result := 'Valor (dias corridos)';
    pjmPercentualDiaDiasCorridos: Result := 'Percentual ao dia (dias corridos)';
    pjmPercentualMesDiasCorridos: Result := ACBrStr('Percentual ao mês (dias corridos)');
    pjmPercentualAnoDiasCorridos: Result := 'Percentual ao ano (dias corridos)';
    pjmValorDiasUteis: Result := ACBrStr('Valor (dias úteis)');
    pjmPercentualDiaDiasUteis: Result := ACBrStr('Percentual ao dia (dias úteis)');
    pjmPercentualMesDiasUteis: Result := ACBrStr('Percentual ao mês (dias úteis)');
    pjmPercentualAnoDiasUteis: Result := ACBrStr('Percentual ao ano (dias úteis)');
  else
    Result := 'Nenhum juros aplicado';
  end;
end;

{ TACBrPIXJuros }

procedure TACBrPIXJuros.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (fmodalidade <> pjmNenhum) then
    aJSon.AddPair('modalidade', Ord(fmodalidade));
  if (fvalorPerc > 0) then
    aJSon.AddPair('valorPerc', FormatarValorPIX(fvalorPerc));
end;

procedure TACBrPIXJuros.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  {$IfDef FPC}i := 0;{$EndIf}
  aJSon
    .Value('modalidade', i)
    .Value('valorPerc', fvalorPerc);
  fmodalidade := TACBrPIXJurosModalidade(i);
end;

constructor TACBrPIXJuros.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TACBrPIXJuros.Clear;
begin
  fmodalidade := pjmNenhum;
  fvalorPerc := 0;
end;

procedure TACBrPIXJuros.Assign(aSource: TACBrPIXJuros);
begin
  fmodalidade := aSource.modalidade;
  fvalorPerc := aSource.valorPerc;
end;

function TACBrPIXJuros.IsEmpty: Boolean;
begin
  Result := (fmodalidade = pjmNenhum) and (fvalorPerc = 0);
end;

{ TACBrPIXCalendarioCobVBase }

constructor TACBrPIXCalendarioCobVBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCalendarioCobVBase.Clear;
begin
  fcriacao := 0;
  fcriacao_Bias := 0;
  fdataDeVencimento := 0;
  fvalidadeAposVencimento := 0;
end;

function TACBrPIXCalendarioCobVBase.IsEmpty: Boolean;
begin
  Result := (fcriacao = 0) and
            (fcriacao_Bias = 0) and
            (fdataDeVencimento = 0) and
            (fvalidadeAposVencimento = 0);
end;

procedure TACBrPIXCalendarioCobVBase.Assign(Source: TACBrPIXCalendarioCobVBase);
begin
  fcriacao := Source.criacao;
  fcriacao_Bias := Source.criacao_Bias;
  fdataDeVencimento := Source.dataDeVencimento;
  fvalidadeAposVencimento := Source.validadeAposVencimento;
end;

procedure TACBrPIXCalendarioCobVBase.SetDataDeVencimento(AValue: TDateTime);
begin
  if fdataDeVencimento = AValue then
    Exit;
  fdataDeVencimento := DateOf(AValue);
end;

procedure TACBrPIXCalendarioCobVBase.SetCriacao(AValue: TDateTime);
begin
  if fcriacao = AValue then
    Exit;
  fcriacao := DateOf(AValue);
end;

procedure TACBrPIXCalendarioCobVBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if (fcriacao > 0) then
    AJSon.AddPair('criacao', DateTimeToIso8601(fcriacao, BiasToTimeZone(fcriacao_Bias)));

  AJSon.AddPairISODate('dataDeVencimento', fdataDeVencimento, False);

  if (fvalidadeAposVencimento > 0) then
    AJSon.AddPair('validadeAposVencimento', fvalidadeAposVencimento);
end;

procedure TACBrPIXCalendarioCobVBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wC: String;
begin
  {$IfDef FPC}wC := EmptyStr;{$EndIf}

  AJSon
    .Value('criacao', wC)
    .ValueISODate('dataDeVencimento', fdataDeVencimento)
    .Value('validadeAposVencimento', fvalidadeAposVencimento);

  if NaoEstaVazio(wC) then
  begin
    fcriacao := Iso8601ToDateTime(wC);
    fcriacao_Bias := TimeZoneToBias(wC);
  end;
end;

{ TACBrPIXModalidadeValor }

constructor TACBrPIXModalidadeValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXModalidadeValor.Clear;
begin
  fmodalidade := pvmNenhum;
  fvalorPerc := 0;
end;

procedure TACBrPIXModalidadeValor.Assign(aSource: TACBrPIXModalidadeValor);
begin
  fmodalidade := aSource.modalidade;
  fvalorPerc := aSource.valorPerc;
end;

function TACBrPIXModalidadeValor.IsEmpty: Boolean;
begin
  Result := (fmodalidade = pvmNenhum) and (fvalorPerc = 0);
end;

procedure TACBrPIXModalidadeValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if (fmodalidade <> pvmNenhum) then
    AJSon.AddPair('modalidade', Ord(fmodalidade));
  if (fvalorPerc > 0) then
    AJSon.AddPair('valorPerc', FormatarValorPIX(fvalorPerc));
end;

procedure TACBrPIXModalidadeValor.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  i: Integer;
begin
  {$IfDef FPC}i := 0;{$EndIf}
  AJSon
    .Value('modalidade', i)
    .Value('valorPerc', fvalorPerc);
   fmodalidade := TACBrPIXValoresModalidade(i);
end;

{ TACBrPIXDescontoDataFixa }

constructor TACBrPIXDescontoDataFixa.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDescontoDataFixa.Clear;
begin
  fdata := 0;
  fvalorPerc := 0;
end;

procedure TACBrPIXDescontoDataFixa.Assign(Source: TACBrPIXDescontoDataFixa);
begin
  fdata := Source.data;
  fvalorPerc := Source.valorPerc;
end;

function TACBrPIXDescontoDataFixa.IsEmpty: Boolean;
begin
  Result := (fdata = 0) or (fvalorPerc = 0);
end;

procedure TACBrPIXDescontoDataFixa.SetData(AValue: TDateTime);
begin
  if fdata = AValue then
    Exit;
  fdata := DateOf(AValue);
end;

procedure TACBrPIXDescontoDataFixa.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXDescontoDataFixa) then
    Assign(TACBrPIXDescontoDataFixa(ASource));
end;

procedure TACBrPIXDescontoDataFixa.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('data', FormatDateTime('yyyy-mm-dd', fdata))
    .AddPair('valorPerc', FormatarValorPIX(fvalorPerc));
end;

procedure TACBrPIXDescontoDataFixa.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .ValueISODate('data', fdata)
    .Value('valorPerc', fvalorPerc);
end;

{ TACBrPIXDescontosDataFixa }

function TACBrPIXDescontosDataFixa.GetItem(Index: Integer): TACBrPIXDescontoDataFixa;
begin
  Result := TACBrPIXDescontoDataFixa(inherited Items[Index]);
end;

procedure TACBrPIXDescontosDataFixa.SetItem(Index: Integer;
  Value: TACBrPIXDescontoDataFixa);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXDescontosDataFixa.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXDescontosDataFixa.Add(ADescontoDataFixa: TACBrPIXDescontoDataFixa): Integer;
begin
  if (Count >= 3) then
    raise EACBrPixException.Create(ACBrStr(sErroDescontoDataFixaLimit));

  Result := inherited Add(ADescontoDataFixa);
end;

procedure TACBrPIXDescontosDataFixa.Insert(Index: Integer;
  ADescontoDataFixa: TACBrPIXDescontoDataFixa);
begin
  inherited Insert(Index, ADescontoDataFixa);
end;

function TACBrPIXDescontosDataFixa.New: TACBrPIXDescontoDataFixa;
begin
  Result := TACBrPIXDescontoDataFixa.Create;
  Self.Add(Result);
end;


{ TACBrPIXDesconto }

constructor TACBrPIXDesconto.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fdescontosDataFixa := TACBrPIXDescontosDataFixa.Create('descontoDataFixa');
end;

destructor TACBrPIXDesconto.Destroy;
begin
  fdescontosDataFixa.Free;
  inherited Destroy;
end;

procedure TACBrPIXDesconto.Clear;
begin
  fvalorPerc := 0;         
  fmodalidade := pdmNenhum;
  fdescontosDataFixa.Clear;
end;

procedure TACBrPIXDesconto.Assign(aSource: TACBrPIXDesconto);
begin
  fmodalidade := aSource.modalidade;
  fdescontosDataFixa.Assign(aSource.descontosDataFixa);
end;

function TACBrPIXDesconto.IsEmpty: Boolean;
begin
  Result := (fvalorPerc = 0) and (fmodalidade = pdmNenhum) and fdescontosDataFixa.IsEmpty;
end;

procedure TACBrPIXDesconto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pdmNenhum) then
    Exit;

  aJSon.AddPair('modalidade', Ord(modalidade));

  { Modalidades 1 e 2 devem enviar a lista de descontosDataFixa }
  { Modalidades 3, 4, 5 e 6 devem enviar valorPerc }
  if (Ord(modalidade) >= 3) then
    aJSon.AddPair('valorPerc', FormatarValorPIX(valorPerc))
  else if (Ord(modalidade) <= 2) then
    descontosDataFixa.WriteToJSon(AJSon);
end;

procedure TACBrPIXDesconto.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin 
  {$IfDef FPC}i := 0;{$EndIf}
  aJSon.Value('modalidade', i);
  modalidade := TACBrPIXDescontoModalidade(i);

  if (i >= 3) then
    aJSon.Value('valorPerc', fvalorPerc)
  else if (i <= 2) then
    descontosDataFixa.ReadFromJSon(aJSon);
end;

{ TACBrPIXCobVValor }

constructor TACBrPIXCobVValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fabatimento := TACBrPIXModalidadeValor.Create('abatimento');
  fdesconto := TACBrPIXDesconto.Create('desconto');
  fjuros := TACBrPIXJuros.Create('juros');
  fmulta := TACBrPIXModalidadeValor.Create('multa');
  Clear;
end;

destructor TACBrPIXCobVValor.Destroy;
begin
  fabatimento.Free;
  fdesconto.Free;
  fjuros.Free;
  fmulta.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVValor.Clear;
begin
  foriginal := 0;
  fabatimento.Clear;
  fdesconto.Clear;
  fjuros.Clear;
  fmulta.Clear;
end;

function TACBrPIXCobVValor.IsEmpty: Boolean;
begin
  Result := (foriginal = 0) and
            fabatimento.IsEmpty and
            fdesconto.IsEmpty and
            fjuros.IsEmpty and
            fmulta.IsEmpty;
end;

procedure TACBrPIXCobVValor.Assign(Source: TACBrPIXCobVValor);
begin
  foriginal := Source.original;
  fabatimento.Assign(Source.abatimento);
  fdesconto.Assign(Source.desconto);
  fjuros.Assign(Source.juros);
  fmulta.Assign(Source.multa);
end;

procedure TACBrPIXCobVValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('original', FormatarValorPIX(foriginal));

  fabatimento.WriteToJSon(AJSon);
  fdesconto.WriteToJSon(AJSon);
  fjuros.WriteToJSon(AJSon);
  fmulta.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVValor.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('original', foriginal);

  fabatimento.ReadFromJSon(AJSon);
  fdesconto.ReadFromJSon(AJSon);
  fjuros.ReadFromJSon(AJSon);
  fmulta.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVSolicitada }

constructor TACBrPIXCobVSolicitada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobVSolicitada.Create('calendario');
  fdevedor := TACBrPIXDadosDevedor.Create('devedor');
  floc := TACBrPIXLocationCobSolicitada.Create('loc');
  fvalor := TACBrPIXCobVValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobVSolicitada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVSolicitada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

function TACBrPIXCobVSolicitada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobVSolicitada.Assign(Source: TACBrPIXCobVSolicitada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

function TACBrPIXCobVSolicitada.LoadFromIni(aIniStr: String): Boolean;
var
  wSecao: String;
  wIni: TMemIniFile;
  i: Integer;
begin
  Result := False;

  wIni := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(aIniStr, wIni);

    wSecao := 'CobVSolicitada';
    chave := wIni.ReadString(wSecao, 'chave', EmptyStr);
    solicitacaoPagador := wIni.ReadString(wSecao, 'solicitacaoPagador', EmptyStr);

    fcalendario.criacao := wIni.ReadDateTime(wSecao, 'criacao', 0);
    fcalendario.criacao_Bias := wIni.ReadInteger(wSecao, 'criacaoBias', 0);
    fcalendario.dataDeVencimento := wIni.ReadDateTime(wSecao, 'dataDeVencimento', 0);
    fcalendario.validadeAposVencimento := wIni.ReadInteger(wSecao, 'validadeAposVencimento', 0);

    fvalor.original := wIni.ReadFloat(wSecao, 'valorOriginal', 0);

    fvalor.multa.modalidade := TACBrPIXValoresModalidade(wIni.ReadInteger(wSecao, 'multaModalidade', 0));
    fvalor.multa.valorPerc := wIni.ReadFloat(wSecao, 'multaValorPercentual', 0);

    fvalor.juros.modalidade := TACBrPIXJurosModalidade(wIni.ReadInteger(wSecao, 'jurosModalidade', 0));
    fvalor.juros.valorPerc := wIni.ReadFloat(wSecao, 'jurosValorPercentual', 0);

    fvalor.abatimento.modalidade := TACBrPIXValoresModalidade(wIni.ReadInteger(wSecao, 'abatimentoModalidade', 0));
    fvalor.abatimento.valorPerc := wIni.ReadFloat(wSecao, 'abatimentoValorPercentual', 0);

    fvalor.desconto.modalidade := TACBrPIXDescontoModalidade(wIni.ReadInteger(wSecao, 'descontoModalidade', 0));
    fvalor.desconto.valorPerc := wIni.ReadFloat(wSecao, 'descontoValorPercentual', 0);

    devedor.cpf := wIni.ReadString(wSecao, 'devedorCPF', EmptyStr);
    devedor.cnpj := wIni.ReadString(wSecao, 'devedorCNPJ', EmptyStr);
    devedor.nome := wIni.ReadString(wSecao, 'devedorNome', EmptyStr);

    i := 1;
    wSecao := 'infoAdicionais' + IntToStrZero(i, 3);
    while wIni.SectionExists(wSecao) do
    begin
      with infoAdicionais.New do
      begin
        nome := wini.ReadString(wSecao, 'nome', EmptyStr);
        valor := wini.ReadString(wSecao, 'valor', EmptyStr);
      end;

      Inc(i);
      wSecao := 'infoAdicionais' + IntToStrZero(i, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TACBrPIXCobVSolicitada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  fcalendario.WriteToJSon(AJSon);
  fdevedor.WriteToJSon(AJSon);
  floc.WriteToJSon(AJSon);
  fvalor.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVSolicitada.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  fcalendario.ReadFromJSon(AJSon);
  fdevedor.ReadFromJSon(AJSon);
  floc.ReadFromJSon(AJSon);
  fvalor.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVRevisada }

procedure TACBrPIXCobVRevisada.Clear;
begin
  fstatus := stcNENHUM;
  inherited Clear;
end;

function TACBrPIXCobVRevisada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fstatus = stcNENHUM);
end;

procedure TACBrPIXCobVRevisada.Assign(Source: TACBrPIXCobVRevisada);
begin
  inherited Assign(Source);
  fstatus := Source.status;
end;

function TACBrPIXCobVRevisada.LoadFromIni(aIniStr: String): Boolean;
var
  wSecao: String;
  wIni: TMemIniFile;
  i: Integer;
begin
  Result := False;

  wIni := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(aIniStr, wIni);

    wSecao := 'CobVRevisada';
    chave := wIni.ReadString(wSecao, 'chave', EmptyStr);
    solicitacaoPagador := wIni.ReadString(wSecao, 'solicitacaoPagador', EmptyStr);

    fcalendario.criacao := wIni.ReadDateTime(wSecao, 'criacao', 0);
    fcalendario.criacao_Bias := wIni.ReadInteger(wSecao, 'criacaoBias', 0);
    fcalendario.dataDeVencimento := wIni.ReadDateTime(wSecao, 'dataDeVencimento', 0);
    fcalendario.validadeAposVencimento := wIni.ReadInteger(wSecao, 'validadeAposVencimento', 0);

    fvalor.original := wIni.ReadFloat(wSecao, 'valorOriginal', 0);

    fvalor.multa.modalidade := TACBrPIXValoresModalidade(wIni.ReadInteger(wSecao, 'multaModalidade', 0));
    fvalor.multa.valorPerc := wIni.ReadFloat(wSecao, 'multaValorPercentual', 0);

    fvalor.juros.modalidade := TACBrPIXJurosModalidade(wIni.ReadInteger(wSecao, 'jurosModalidade', 0));
    fvalor.juros.valorPerc := wIni.ReadFloat(wSecao, 'jurosValorPercentual', 0);

    fvalor.abatimento.modalidade := TACBrPIXValoresModalidade(wIni.ReadInteger(wSecao, 'abatimentoModalidade', 0));
    fvalor.abatimento.valorPerc := wIni.ReadFloat(wSecao, 'abatimentoValorPercentual', 0);

    fvalor.desconto.modalidade := TACBrPIXDescontoModalidade(wIni.ReadInteger(wSecao, 'descontoModalidade', 0));
    fvalor.desconto.valorPerc := wIni.ReadFloat(wSecao, 'descontoValorPercentual', 0);

    devedor.cpf := wIni.ReadString(wSecao, 'devedorCPF', EmptyStr);
    devedor.cnpj := wIni.ReadString(wSecao, 'devedorCNPJ', EmptyStr);
    devedor.nome := wIni.ReadString(wSecao, 'devedorNome', EmptyStr);

    fstatus := TACBrPIXStatusCobranca(wIni.ReadInteger(wSecao, 'status', 0));

    i := 1;
    wSecao := 'infoAdicionais' + IntToStrZero(i, 3);
    while wIni.SectionExists(wSecao) do
    begin
      with infoAdicionais.New do
      begin
        nome := wini.ReadString(wSecao, 'nome', EmptyStr);
        valor := wini.ReadString(wSecao, 'valor', EmptyStr);
      end;

      Inc(i);
      wSecao := 'infoAdicionais' + IntToStrZero(i, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TACBrPIXCobVRevisada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  if (fstatus <> stcNENHUM) then
    AJSon.AddPair('status', PIXStatusCobrancaToString(fstatus));
end;

procedure TACBrPIXCobVRevisada.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  AJSon.Value('status', s);
  fstatus := StringToPIXStatusCobranca(s);
end;

{ TACBrPIXCobVGerada }

constructor TACBrPIXCobVGerada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobVGerada.Create('calendario');
  fdevedor := TACBrPIXDadosDevedor.Create('devedor');
  frecebedor := TACBrPIXDadosRecebedor.Create('recebedor');
  floc := TACBrPIXLocationCompleta.Create('loc');
  fvalor := TACBrPIXCobVValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobVGerada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  frecebedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVGerada.Clear;
begin
  inherited Clear;
  frevisao := 0;
  fstatus := stcNENHUM;
  ftxId := '';
  fcalendario.Clear;
  fdevedor.Clear;
  frecebedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

function TACBrPIXCobVGerada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (frevisao = 0) and
            (fstatus = stcNENHUM) and
            (ftxId = '') and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            frecebedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobVGerada.Assign(Source: TACBrPIXCobVGerada);
begin
  inherited Assign(Source);
  frevisao := Source.revisao;
  fstatus := Source.status;
  ftxId := Source.txId;
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  frecebedor.Assign(Source.recebedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

procedure TACBrPIXCobVGerada.SetRevisao(AValue: Integer);
begin
  if frevisao = AValue then
    Exit;
  frevisao := max(AValue,0);
end;

procedure TACBrPIXCobVGerada.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobVGerada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);

  fcalendario.WriteToJSon(AJSon);
  fdevedor.WriteToJSon(AJSon);
  frecebedor.WriteToJSon(AJSon);
  floc.WriteToJSon(AJSon);
  fvalor.WriteToJSon(AJSon);

  AJSon
    .AddPair('txid', ftxId)
    .AddPair('revisao', frevisao)
    .AddPair('status', PIXStatusCobrancaToString(fstatus));
end;

procedure TACBrPIXCobVGerada.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  fcalendario.ReadFromJSon(AJSon);
  fdevedor.ReadFromJSon(AJSon);
  frecebedor.ReadFromJSon(AJSon);
  floc.ReadFromJSon(AJSon);
  fvalor.ReadFromJSon(AJSon);

  AJSon
    .Value('txid', ftxId)
    .Value('revisao', frevisao)
    .Value('status', s);
  fstatus := StringToPIXStatusCobranca(s);
end;


{ TACBrPIXCobVCompleta }

constructor TACBrPIXCobVCompleta.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpix := TACBrPIXArray.Create('pix');
  Clear;
end;

destructor TACBrPIXCobVCompleta.Destroy;
begin
  fpix.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVCompleta.Clear;
begin
  fpix.Clear;
  inherited Clear;
end;

function TACBrPIXCobVCompleta.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fpix.IsEmpty;
end;

procedure TACBrPIXCobVCompleta.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXCobVCompleta) then
    Assign(TACBrPIXCobVCompleta(ASource));
end;

procedure TACBrPIXCobVCompleta.Assign(Source: TACBrPIXCobVCompleta);
begin
  inherited Assign(Source);
  fpix.Assign(Source.pix);
end;

procedure TACBrPIXCobVCompleta.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  fpix.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVCompleta.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  fpix.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVCompletaArray }

function TACBrPIXCobVCompletaArray.GetItem(Index: Integer
  ): TACBrPIXCobVCompleta;
begin
  Result := TACBrPIXCobVCompleta(inherited Items[Index]);
end;

procedure TACBrPIXCobVCompletaArray.SetItem(Index: Integer; Value: TACBrPIXCobVCompleta);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobVCompletaArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobVCompletaArray.Add(ACobV: TACBrPIXCobVCompleta): Integer;
begin
  Result := inherited Add(ACobV);
end;

procedure TACBrPIXCobVCompletaArray.Insert(Index: Integer; ACobV: TACBrPIXCobVCompleta);
begin
  inherited Insert(Index, ACobV);
end;

function TACBrPIXCobVCompletaArray.New: TACBrPIXCobVCompleta;
begin
  Result := TACBrPIXCobVCompleta.Create('');
  Self.Add(Result);
end;

end.

