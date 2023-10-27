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

unit ACBrPIXSchemasPix;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase, ACBrPIXBase, ACBrPIXSchemasDevolucao;

type

  { TACBrPIXValor }

  TACBrPIXValor = class(TACBrPIXSchema)
  private
    fvalor: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXValor);

    property valor: Currency read fvalor write fvalor;
  end;

  { TACBrPIXSaqueTroco }

  TACBrPIXSaqueTroco = class(TACBrPIXSchema)
  private
    fprestadorStr: String;
    fmodalidadeAgente: TACBrPIXModalidadeAgente;
    fmodalidadeAlteracao: Boolean;
    fprestadorDoServicoDeSaque: Integer;
    fvalor: Currency;
    procedure SetprestadorDoServicoDeSaque(AValue: Integer);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String; prestadorStr: String); reintroduce;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXSaqueTroco);

    property valor: Currency read fvalor write fvalor;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property modalidadeAgente: TACBrPIXModalidadeAgente read fmodalidadeAgente write fmodalidadeAgente;
    property prestadorDoServicoDeSaque: Integer read fprestadorDoServicoDeSaque write SetprestadorDoServicoDeSaque;
  end;

  { TACBrPIXComponentesValor }

  TACBrPIXComponentesValor = class(TACBrPIXSchema)
  private
    fabatimento: TACBrPIXValor;
    fdesconto: TACBrPIXValor;
    fjuros: TACBrPIXValor;
    fmulta: TACBrPIXValor;
    foriginal: TACBrPIXValor;
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
    procedure Assign(Source: TACBrPIXComponentesValor);

    property original: TACBrPIXValor read foriginal;
    property saque: TACBrPIXSaqueTroco read fsaque;
    property troco: TACBrPIXSaqueTroco read ftroco;
    property juros: TACBrPIXValor read fjuros;
    property multa: TACBrPIXValor read fmulta;
    property abatimento: TACBrPIXValor read fabatimento;
    property desconto: TACBrPIXValor read fdesconto;
  end;

  { TACBrPIX }

  TACBrPIX = class(TACBrPIXSchema)
  private
    fchave: String;
    fcomponentesValor: TACBrPIXComponentesValor;
    fdevolucoes: TACBrPIXDevolucoes;
    fendToEndId: String;
    fhorario: TDateTime;
    fhorario_Bias: Integer;
    finfoPagador: String;
    ftxid: String;
    fvalor: Currency;
    procedure SetChave(AValue: String);
    procedure SetendToEndId(const AValue: String);
    procedure SetinfoPagador(AValue: String);
    procedure SetTxid(const AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIX);

    property endToEndId: String read fendToEndId write SetendToEndId;   //"Id fim a fim da transação"
    property txid: String read ftxid write SetTxid;
    property valor: Currency read fvalor write fvalor;
    property componentesValor: TACBrPIXComponentesValor read fcomponentesValor;
    property chave: String read fchave write SetChave;
    property horario: TDateTime read fhorario write fhorario;
    property horario_Bias: Integer read fhorario_Bias write fhorario_Bias;
    property infoPagador: String read finfoPagador write SetinfoPagador;
    property devolucoes: TACBrPIXDevolucoes read fdevolucoes;
  end;

  { TACBrPIXArray }

  TACBrPIXArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIX;
    procedure SetItem(Index: Integer; Value: TACBrPIX);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ADevolucao: TACBrPIX): Integer;
    Procedure Insert(Index: Integer; ADevolucao: TACBrPIX);
    function New: TACBrPIX;
    property Items[Index: Integer]: TACBrPIX read GetItem write SetItem; default;
  end;

implementation

uses
  Math,
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXValor }

constructor TACBrPIXValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXValor.Clear;
begin
  fvalor := 0;
end;

function TACBrPIXValor.IsEmpty: Boolean;
begin
  Result := (fvalor = 0);
end;

procedure TACBrPIXValor.Assign(Source: TACBrPIXValor);
begin
  fvalor := Source.valor;
end;

procedure TACBrPIXValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('valor', FormatarValorPIX(fvalor));
end;

procedure TACBrPIXValor.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  fvalor := StringToFloatDef(AJSon.AsString['valor'], 0);
end;

{ TACBrPIXSaqueTroco }

constructor TACBrPIXSaqueTroco.Create(const ObjectName: String;
  prestadorStr: String);
begin
  inherited Create(ObjectName);
  fprestadorStr := prestadorStr;
end;

procedure TACBrPIXSaqueTroco.Clear;
begin
  fvalor := -1;
  fmodalidadeAlteracao := False;
  fmodalidadeAgente := maNENHUM;
  fprestadorDoServicoDeSaque := -1;
end;

function TACBrPIXSaqueTroco.IsEmpty: Boolean;
begin
  Result := (fmodalidadeAgente = maNENHUM);
  //(fvalor < 0) and // (fmodalidadeAlteracao = False) and
  //          (fmodalidadeAgente = maNENHUM) and
  //          (fprestadorDoServicoDeSaque < 0);
end;

procedure TACBrPIXSaqueTroco.Assign(Source: TACBrPIXSaqueTroco);
begin
  fvalor := Source.valor;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fmodalidadeAgente := Source.modalidadeAgente;
  fprestadorDoServicoDeSaque := Source.prestadorDoServicoDeSaque;
end;

procedure TACBrPIXSaqueTroco.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if (fvalor >= 0) then
    AJSon.AddPair('valor', FormatarValorPIX(fvalor));
  AJSon.AddPair('modalidadeAlteracao', IfThen(fmodalidadeAlteracao, 1, 0));
  if (fmodalidadeAgente <> maNENHUM) then
    AJSon.AddPair('modalidadeAgente', PIXModalidadeAgenteToString(fmodalidadeAgente));
  if (fprestadorDoServicoDeSaque >= 0) then
    AJSon.AddPair(fprestadorStr, IntToStrZero(fprestadorDoServicoDeSaque, 8));
end;

procedure TACBrPIXSaqueTroco.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  fvalor := StringToFloatDef(AJSon.AsString['valor'], 0 );
  fmodalidadeAlteracao := (AJSon.AsInteger['modalidadeAlteracao'] = 1);
  fmodalidadeAgente := StringToPIXModalidadeAgente(AJSon.AsString['modalidadeAgente']);
  fprestadorDoServicoDeSaque := StrToIntDef(AJSon.AsString[fprestadorStr], -1);
end;

procedure TACBrPIXSaqueTroco.SetprestadorDoServicoDeSaque(AValue: Integer);
var
  e: String;
begin
  if (fprestadorDoServicoDeSaque = AValue) then
    Exit;

  e := ValidarPSS(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fprestadorDoServicoDeSaque := AValue;
end;

{ TACBrPIXComponentesValor }

constructor TACBrPIXComponentesValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fabatimento := TACBrPIXValor.Create('abatimento');
  fdesconto := TACBrPIXValor.Create('desconto');
  fjuros := TACBrPIXValor.Create('juros');
  fmulta := TACBrPIXValor.Create('multa');
  foriginal := TACBrPIXValor.Create('original');
  fsaque := TACBrPIXSaqueTroco.Create('saque', 'prestadorDeServicoDeSaque');
  ftroco := TACBrPIXSaqueTroco.Create('troco', 'prestadorDeServicoDeSaque');
end;

destructor TACBrPIXComponentesValor.Destroy;
begin
  fabatimento.Free;
  fdesconto.Free;
  fjuros.Free;
  fmulta.Free;
  foriginal.Free;
  fsaque.Free;
  ftroco.Free;
  inherited Destroy;
end;

procedure TACBrPIXComponentesValor.Clear;
begin
  fabatimento.Clear;
  fdesconto.Clear;
  fjuros.Clear;
  fmulta.Clear;
  foriginal.Clear;
  fsaque.Clear;
  ftroco.Clear;
end;

function TACBrPIXComponentesValor.IsEmpty: Boolean;
begin
  Result := fabatimento.IsEmpty and
            fdesconto.IsEmpty and
            fjuros.IsEmpty and
            fmulta.IsEmpty and
            foriginal.IsEmpty and
            fsaque.IsEmpty and
            ftroco.IsEmpty;
end;

procedure TACBrPIXComponentesValor.Assign(Source: TACBrPIXComponentesValor);
begin
  fabatimento.Assign(Source.abatimento);
  fdesconto.Assign(Source.desconto);
  fjuros.Assign(Source.juros);
  fmulta.Assign(Source.multa);
  foriginal.Assign(Source.original);
  fsaque.Assign(Source.saque);
  ftroco.Assign(Source.troco);
end;

procedure TACBrPIXComponentesValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  foriginal.WriteToJSon(AJSon);
  fsaque.WriteToJSon(AJSon);
  ftroco.WriteToJSon(AJSon);
  fjuros.WriteToJSon(AJSon);
  fmulta.WriteToJSon(AJSon);
  fabatimento.WriteToJSon(AJSon);
  fdesconto.WriteToJSon(AJSon);
end;

procedure TACBrPIXComponentesValor.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  c: Currency;
begin
  {$IFDEF FPC}c := 0;{$ENDIF}
  if AJSon.IsJSONObject('original') then
    foriginal.ReadFromJSon(AJSon)
  else
  begin
    AJSon.Value('original', c);
    foriginal.valor := c;
  end;

  fsaque.ReadFromJSon(AJSon);
  ftroco.ReadFromJSon(AJSon);
  fjuros.ReadFromJSon(AJSon);
  fmulta.ReadFromJSon(AJSon);
  fabatimento.ReadFromJSon(AJSon);
  fdesconto.ReadFromJSon(AJSon);
end;

{ TACBrPIX }

constructor TACBrPIX.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcomponentesValor := TACBrPIXComponentesValor.Create('componentesValor');
  fdevolucoes := TACBrPIXDevolucoes.Create('devolucoes');
  Clear;
end;

destructor TACBrPIX.Destroy;
begin
  fcomponentesValor.Free;
  fdevolucoes.Free;
  inherited Destroy;
end;

procedure TACBrPIX.Clear;
begin
  fendToEndId := '';
  ftxid := '';
  fvalor := 0;
  fchave := '';
  fhorario := 0;
  fhorario_Bias := 0;
  finfoPagador := '';
  ftxid := '';
  fvalor := 0;

  fcomponentesValor.Clear;
  fdevolucoes.Clear
end;

function TACBrPIX.IsEmpty: Boolean;
begin
  Result := (fendToEndId = '') and
            (ftxid = '') and
            (fvalor = 0) and
            (fchave = '') and
            (fhorario = 0) and
            (fhorario_Bias = 0) and
            (finfoPagador = '') and
            (ftxid = '') and
            (fvalor = 0) and
            fcomponentesValor.IsEmpty and
            fdevolucoes.IsEmpty;
end;

procedure TACBrPIX.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIX) then
    Assign(TACBrPIX(ASource));
end;

procedure TACBrPIX.Assign(Source: TACBrPIX);
begin
  fendToEndId := Source.endToEndId;
  ftxid := Source.txid;
  fvalor := Source.valor;
  fchave := Source.chave;
  fhorario := Source.horario;
  fhorario_Bias := Source.horario_Bias;
  finfoPagador := Source.infoPagador;
  ftxid := Source.txid;
  fvalor := Source.valor;

  fcomponentesValor.Assign(Source.componentesValor);
  fdevolucoes.Assign(Source.devolucoes);
end;

procedure TACBrPIX.SetendToEndId(const AValue: String);
var
  e: String;
begin
  if (fendToEndId = AValue) then
    Exit;

  e := ValidarEndToEndId(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fendToEndId := AValue;
end;

procedure TACBrPIX.SetinfoPagador(AValue: String);
begin
  if finfoPagador = AValue then
    Exit;
  finfoPagador := copy(AValue, 1, 140);
end;

procedure TACBrPIX.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('endToEndId', fendToEndId);
  if NaoEstaVazio(ftxid) then
    AJSon.AddPair('txid', ftxid);
  AJSon.AddPair('valor', FormatarValorPIX(fvalor));
  fcomponentesValor.WriteToJSon(AJSon);
  if NaoEstaVazio(fchave) then
    AJSon.AddPair('chave', fchave);
  AJSon.AddPair('horario', DateTimeToIso8601(fhorario, BiasToTimeZone(fhorario_Bias)));
  AJSon.AddPair('infoPagador', finfoPagador);
  fdevolucoes.WriteToJSon(AJSon);
end;

procedure TACBrPIX.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  AJSon
    .Value('endToEndId', fendToEndId)
    .Value('txid', ftxid)
    .Value('valor', fvalor)
    .Value('chave', fchave)
    .Value('horario', s)
    .Value('infoPagador', finfoPagador);

  if NaoEstaVazio(s) then
  begin
    fhorario := Iso8601ToDateTime(s);
    fhorario_Bias := TimeZoneToBias(s);
  end;

  fdevolucoes.ReadFromJSon(AJSon);
  fcomponentesValor.ReadFromJSon(AJSon);
end;

procedure TACBrPIX.SetChave(AValue: String);
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

procedure TACBrPIX.SetTxid(const AValue: String);
var
  e, s: String;
begin
  if ftxid = AValue then
    Exit;

  if (AValue = cMPMValueNotInformed) then
  begin
    fTxId := '';
    Exit;
  end;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

{ TACBrPIXArray }

function TACBrPIXArray.GetItem(Index: Integer): TACBrPIX;
begin
  Result := TACBrPIX(inherited Items[Index]);
end;

procedure TACBrPIXArray.SetItem(Index: Integer; Value: TACBrPIX);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXArray.Add(ADevolucao: TACBrPIX): Integer;
begin
  Result := inherited Add(ADevolucao);
end;

procedure TACBrPIXArray.Insert(Index: Integer; ADevolucao: TACBrPIX);
begin
  inherited Insert(Index, ADevolucao);
end;

function TACBrPIXArray.New: TACBrPIX;
begin
  Result := TACBrPIX.Create('');
  Self.Add(Result);
end;

end.

