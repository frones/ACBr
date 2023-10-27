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

unit ACBrPIXSchemasCob;

interface

uses
  Classes, SysUtils, ACBrBase, ACBrPIXBase, ACBrJSON, ACBrPIXSchemasCalendario,
  ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation, ACBrPIXSchemasPix;

resourcestring
  sErroInfoAdicLimit = 'Limite de infoAdicionais atingido (50)';

type

  { TACBrPIXInfoAdicional }

  TACBrPIXInfoAdicional = class(TACBrPIXSchema)
  private
    fnome: String;
    fvalor: String;
    procedure SetNome(AValue: String);
    procedure SetValor(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXInfoAdicional);

    property nome: String read fnome write SetNome;
    property valor: String read fvalor write SetValor;
  end;

  { TACBrPIXInfoAdicionalArray }

  TACBrPIXInfoAdicionalArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXInfoAdicional;
    procedure SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
    procedure Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
    function New: TACBrPIXInfoAdicional;
    property Items[Index: Integer]: TACBrPIXInfoAdicional read GetItem write SetItem; default;
    function Find(const ANome: String): TACBrPIXInfoAdicional;
  end;

  { TACBrPIXRetirada }

  TACBrPIXRetirada = class(TACBrPIXSchema)
  private
    fsaque: TACBrPIXSaqueTroco;
    ftroco: TACBrPIXSaqueTroco;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRetirada);

    property saque: TACBrPIXSaqueTroco read fsaque;
    property troco: TACBrPIXSaqueTroco read ftroco;
  end;

  { TACBrPIXCobValor }

  TACBrPIXCobValor = class(TACBrPIXSchema)
  private
    fmodalidadeAlteracao: Boolean;
    foriginal: Currency;
    fretirada: TACBrPIXRetirada;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobValor);

    property original: Currency read foriginal write foriginal;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property retirada: TACBrPIXRetirada read fretirada;
  end;

  { TACBrPIXCobBase }

  TACBrPIXCobBase = class(TACBrPIXSchema)
  private
    fchave: String;
    finfoAdicionais: TACBrPIXInfoAdicionalArray;
    fsolicitacaoPagador: String;
    procedure SetChave(AValue: String);
    procedure SetSolicitacaoPagador(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobBase);

    property chave: String read fchave write SetChave;
    property solicitacaoPagador: String read fsolicitacaoPagador write SetSolicitacaoPagador;
    property infoAdicionais: TACBrPIXInfoAdicionalArray read finfoAdicionais;
  end;

  { TACBrPIXCobSolicitada }

  TACBrPIXCobSolicitada = class(TACBrPIXCobBase)
  private
    fcalendario: TACBrPIXCalendarioCobSolicitada;
    fdevedor: TACBrPIXDevedor;
    floc: TACBrPIXLocationCobSolicitada;
    fvalor: TACBrPIXCobValor;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobSolicitada);

    property calendario: TACBrPIXCalendarioCobSolicitada read fcalendario;
    property devedor: TACBrPIXDevedor read fdevedor;
    property loc: TACBrPIXLocationCobSolicitada read floc;
    property valor: TACBrPIXCobValor read fvalor;
  end;

  { TACBrPIXCobRevisada }

  TACBrPIXCobRevisada = class(TACBrPIXCobSolicitada)
  private
    fstatus: TACBrPIXStatusCobranca;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRevisada);

    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
  end;

  { TACBrPIXCobBaseCopiaCola }

  TACBrPIXCobBaseCopiaCola = class(TACBrPIXCobBase)
  private
    fpixCopiaECola: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobBaseCopiaCola);

    property pixCopiaECola: String read fpixCopiaECola write fpixCopiaECola;
  end;

  { TACBrPIXCobGerada }

  TACBrPIXCobGerada = class(TACBrPIXCobBaseCopiaCola)
  private
    fcalendario: TACBrPIXCalendarioCobGerada;
    fdevedor: TACBrPIXDevedor;
    floc: TACBrPIXLocationCompleta;
    flocation: String;
    frevisao: Integer;
    fstatus: TACBrPIXStatusCobranca;
    ftxId: String;
    fvalor: TACBrPIXCobValor;
    procedure SetRevisao(AValue: Integer);
    procedure SetTxId(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobGerada);

    property calendario: TACBrPIXCalendarioCobGerada read fcalendario;
    property txId: String read ftxId write SetTxId;
    property revisao: Integer read frevisao write SetRevisao;
    property devedor: TACBrPIXDevedor read fdevedor;
    property loc: TACBrPIXLocationCompleta read floc;
    property location: String read flocation write flocation;
    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
    property valor: TACBrPIXCobValor read fvalor;
  end;

  { TACBrPIXCobCompleta }

  TACBrPIXCobCompleta = class(TACBrPIXCobGerada)
  private
    fpix: TACBrPIXArray;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXCobCompleta);

    property pix: TACBrPIXArray read fpix;
  end;

  { TACBrPIXCobCompletaArray }

  TACBrPIXCobCompletaArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobCompleta;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobCompleta);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ACob: TACBrPIXCobCompleta): Integer;
    Procedure Insert(Index: Integer; ACob: TACBrPIXCobCompleta);
    function New: TACBrPIXCobCompleta;
    property Items[Index: Integer]: TACBrPIXCobCompleta read GetItem write SetItem; default;
  end;


implementation

uses
  DateUtils, Math,
  ACBrUtil.Strings,
  ACBrConsts, ACBrPIXUtil;

{ TACBrPIXInfoAdicional }

constructor TACBrPIXInfoAdicional.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXInfoAdicional.Clear;
begin
  fnome := '';
  fvalor := '';
end;

function TACBrPIXInfoAdicional.IsEmpty: Boolean;
begin
  Result := (fnome = '') and (fvalor = '');
end;

procedure TACBrPIXInfoAdicional.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXInfoAdicional) then
    Assign(TACBrPIXInfoAdicional(ASource));
end;

procedure TACBrPIXInfoAdicional.Assign(Source: TACBrPIXInfoAdicional);
begin
  fnome := Source.nome;
  fvalor := Source.valor;
end;

procedure TACBrPIXInfoAdicional.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('nome', fnome)
    .AddPair('valor', fvalor);
end;

procedure TACBrPIXInfoAdicional.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('nome', fnome)
    .Value('valor', fvalor);
end;

procedure TACBrPIXInfoAdicional.SetNome(AValue: String);
begin
  if fnome = AValue then
    Exit;
  fnome := copy(Trim(AValue), 1, 50);
end;

procedure TACBrPIXInfoAdicional.SetValor(AValue: String);
begin
  if fvalor = AValue then
    Exit;
  fvalor := copy(AValue, 1, 200);
end;

{ TACBrPIXInfoAdicionalArray }

function TACBrPIXInfoAdicionalArray.GetItem(Index: Integer): TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional(inherited Items[Index]);
end;

procedure TACBrPIXInfoAdicionalArray.SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXInfoAdicionalArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXInfoAdicionalArray.Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
begin
  if Count >= 50 then
    raise EACBrPixException.Create(ACBrStr(sErroInfoAdicLimit));

  Result := inherited Add(AInfoAdicional);
end;

procedure TACBrPIXInfoAdicionalArray.Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
begin
  inherited Insert(Index, AInfoAdicional);
end;

function TACBrPIXInfoAdicionalArray.New: TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional.Create('');
  Self.Add(Result);
end;

function TACBrPIXInfoAdicionalArray.Find(const ANome: String): TACBrPIXInfoAdicional;
var
  i: Integer;
  s: String;
begin
  Result := Nil;
  s := LowerCase(ANome);
  for i := 0 to Count-1 do
  begin
    if (LowerCase(Items[i].nome) = s) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

{ TACBrPIXRetirada }

constructor TACBrPIXRetirada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fsaque := TACBrPIXSaqueTroco.Create('saque', 'prestadorDoServicoDeSaque');
  ftroco := TACBrPIXSaqueTroco.Create('troco', 'prestadorDoServicoDeSaque');
end;

destructor TACBrPIXRetirada.Destroy;
begin
  fsaque.Free;
  ftroco.Free;
  inherited Destroy;
end;

procedure TACBrPIXRetirada.Clear;
begin
  fsaque.Clear;
  ftroco.Clear;
end;

function TACBrPIXRetirada.IsEmpty: Boolean;
begin
  Result := fsaque.IsEmpty and
            ftroco.IsEmpty;
end;

procedure TACBrPIXRetirada.Assign(Source: TACBrPIXRetirada);
begin
  fsaque.Assign(Source.saque);
  ftroco.Assign(Source.troco);
end;

procedure TACBrPIXRetirada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if not saque.IsEmpty then
    saque.WriteToJSon(AJSon)
  else if not troco.IsEmpty then
    troco.WriteToJSon(AJSon);
end;

procedure TACBrPIXRetirada.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  if (not Assigned(AJSon)) then
    Exit;

  fsaque.ReadFromJSon(AJSon);
  ftroco.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobValor }

constructor TACBrPIXCobValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fretirada := TACBrPIXRetirada.Create('retirada');
  Clear;
end;

destructor TACBrPIXCobValor.Destroy;
begin
  fretirada.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobValor.Clear;
begin
  foriginal := 0;
  fmodalidadeAlteracao := False;
  fretirada.Clear;
end;

function TACBrPIXCobValor.IsEmpty: Boolean;
begin
  Result := (foriginal = 0) and  // (fmodalidadeAlteracao = False) and
            fretirada.IsEmpty;
end;

procedure TACBrPIXCobValor.Assign(Source: TACBrPIXCobValor);
begin
  foriginal := Source.original;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fretirada.Assign(Source.retirada);
end;

procedure TACBrPIXCobValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('original', FormatarValorPIX(original));
  if modalidadeAlteracao then
    AJSon.AddPair('modalidadeAlteracao', 1);

  fretirada.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobValor.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wAux: Integer;
begin
  {$IFDEF FPC}wAux := 0;{$ENDIF}
  AJSon
    .Value('original', foriginal)
    .Value('modalidadeAlteracao', wAux);

  fmodalidadeAlteracao := (wAux = 1);
  fretirada.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobBase }

constructor TACBrPIXCobBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  finfoAdicionais := TACBrPIXInfoAdicionalArray.Create('infoAdicionais');
  Clear;
end;

destructor TACBrPIXCobBase.Destroy;
begin
  finfoAdicionais.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobBase.Clear;
begin
  fchave := '';
  fsolicitacaoPagador := '';
  finfoAdicionais.Clear;
end;

function TACBrPIXCobBase.IsEmpty: Boolean;
begin
  Result := (fchave = '') and
            (fsolicitacaoPagador = '') and
            finfoAdicionais.IsEmpty;
end;

procedure TACBrPIXCobBase.Assign(Source: TACBrPIXCobBase);
begin
  fchave := Source.chave;
  fsolicitacaoPagador := Source.solicitacaoPagador;
  finfoAdicionais.Assign(Source.infoAdicionais);
end;

procedure TACBrPIXCobBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('chave', fchave, False)
    .AddPair('solicitacaoPagador', fsolicitacaoPagador, False);
  finfoAdicionais.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobBase.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('chave', fchave)
    .Value('solicitacaoPagador', fsolicitacaoPagador);

  finfoAdicionais.ReadFromJSon(AJSon);
end;

procedure TACBrPIXCobBase.SetChave(AValue: String);
var
  e: String;
begin
  if fchave = AValue then
    Exit;

  e := ValidarChave(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fchave := AValue;
end;

procedure TACBrPIXCobBase.SetSolicitacaoPagador(AValue: String);
begin
  if fsolicitacaoPagador = AValue then
    Exit;
  fsolicitacaoPagador := copy(AValue, 1, 140);
end;

{ TACBrPIXCobSolicitada }

constructor TACBrPIXCobSolicitada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobSolicitada.Create('calendario');
  fdevedor := TACBrPIXDevedor.Create('devedor');
  floc := TACBrPIXLocationCobSolicitada.Create('loc');
  fvalor := TACBrPIXCobValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobSolicitada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobSolicitada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

function TACBrPIXCobSolicitada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobSolicitada.Assign(Source: TACBrPIXCobSolicitada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

procedure TACBrPIXCobSolicitada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  fcalendario.WriteToJSon(AJSon);
  fdevedor.WriteToJSon(AJSon);
  floc.WriteToJSon(AJSon);
  fvalor.WriteToJSon(AJSon);
  inherited DoWriteToJSon(AJSon);
end;

procedure TACBrPIXCobSolicitada.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  fcalendario.ReadFromJSon(AJSon);
  fdevedor.ReadFromJSon(AJSon);
  floc.ReadFromJSon(AJSon);
  fvalor.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobRevisada }

procedure TACBrPIXCobRevisada.Clear;
begin
  inherited Clear;
  fstatus := stcNENHUM;
end;

function TACBrPIXCobRevisada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fstatus = stcNENHUM);
end;

procedure TACBrPIXCobRevisada.Assign(Source: TACBrPIXCobRevisada);
begin
  inherited Assign(Source);
  fstatus := Source.status;
end;

procedure TACBrPIXCobRevisada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  if (fstatus <> stcNENHUM) then
    AJSon.AddPair('status', PIXStatusCobrancaToString(fstatus));
end;

procedure TACBrPIXCobRevisada.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  AJSon.Value('status', s);
  fstatus := StringToPIXStatusCobranca(s);
 end;

{ TACBrPIXCobBaseCopiaCola }

procedure TACBrPIXCobBaseCopiaCola.Clear;
begin
  inherited Clear;
  fpixCopiaECola := '';
end;

function TACBrPIXCobBaseCopiaCola.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fpixCopiaECola = '');
end;

procedure TACBrPIXCobBaseCopiaCola.Assign(Source: TACBrPIXCobBaseCopiaCola);
begin
  inherited Assign(Source);
  fpixCopiaECola := Source.pixCopiaECola;
end;

procedure TACBrPIXCobBaseCopiaCola.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  AJSon.AddPair('pixCopiaECola', fpixCopiaECola, False);
end;

procedure TACBrPIXCobBaseCopiaCola.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  AJSon.Value('pixCopiaECola', fpixCopiaECola);
end;

{ TACBrPIXCobGerada }

constructor TACBrPIXCobGerada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobGerada.Create('calendario');
  fdevedor := TACBrPIXDevedor.Create('devedor');
  floc := TACBrPIXLocationCompleta.Create('loc');
  fvalor := TACBrPIXCobValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobGerada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobGerada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  floc.Clear;
  fvalor.Clear;
  flocation := '';
  frevisao := 0;
  fstatus := stcNENHUM;
  ftxId := '';
end;

function TACBrPIXCobGerada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (flocation = '') and
            (frevisao = 0) and
            (fstatus = stcNENHUM) and
            (ftxId = '') and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobGerada.Assign(Source: TACBrPIXCobGerada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
  flocation := Source.location;
  frevisao := Source.revisao;
  fstatus := Source.status;
  ftxId := Source.txId;
end;

procedure TACBrPIXCobGerada.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);

  fcalendario.WriteToJSon(AJSon);
  fdevedor.WriteToJSon(AJSon);
  floc.WriteToJSon(AJSon);
  fvalor.WriteToJSon(AJSon);

  AJSon
    .AddPair('txid', ftxId, False)
    .AddPair('revisao', frevisao, False)
    .AddPair('location', flocation, False)
    .AddPair('status', PIXStatusCobrancaToString(fstatus), False);
end;

procedure TACBrPIXCobGerada.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  fcalendario.ReadFromJSon(AJSon);
  fdevedor.ReadFromJSon(AJSon);
  floc.ReadFromJSon(AJSon);
  fvalor.ReadFromJSon(AJSon);

  AJSon
    .Value('txid', ftxId)
    .Value('revisao', frevisao)
    .Value('location', flocation)
    .Value('status', s);

  fstatus := StringToPIXStatusCobranca(s);
end;

procedure TACBrPIXCobGerada.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and IsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobGerada.SetRevisao(AValue: Integer);
begin
  if frevisao = AValue then
    Exit;
  frevisao := max(AValue,0);
end;

{ TACBrPIXCobCompleta }

constructor TACBrPIXCobCompleta.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpix := TACBrPIXArray.Create('pix');
  Clear;
end;

destructor TACBrPIXCobCompleta.Destroy;
begin
  fpix.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobCompleta.Clear;
begin
  fpix.Clear;
  inherited Clear;
end;

function TACBrPIXCobCompleta.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fpix.IsEmpty;
end;

procedure TACBrPIXCobCompleta.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXCobCompleta) then
    Assign(TACBrPIXCobCompleta(ASource));
end;

procedure TACBrPIXCobCompleta.Assign(Source: TACBrPIXCobCompleta);
begin
  inherited Assign(Source);
  fpix.Assign(Source.pix);
end;

procedure TACBrPIXCobCompleta.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  fpix.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobCompleta.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);
  fpix.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobCompletaArray }

function TACBrPIXCobCompletaArray.GetItem(Index: Integer): TACBrPIXCobCompleta;
begin
  Result := TACBrPIXCobCompleta(inherited Items[Index]);
end;

procedure TACBrPIXCobCompletaArray.SetItem(Index: Integer; Value: TACBrPIXCobCompleta);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobCompletaArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobCompletaArray.Add(ACob: TACBrPIXCobCompleta): Integer;
begin
  Result := inherited Add(ACob);
end;

procedure TACBrPIXCobCompletaArray.Insert(Index: Integer; ACob: TACBrPIXCobCompleta);
begin
  inherited Insert(Index, ACob);
end;

function TACBrPIXCobCompletaArray.New: TACBrPIXCobCompleta;
begin
  Result := TACBrPIXCobCompleta.Create('');
  Self.Add(Result);
end;

end.

