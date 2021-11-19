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

//{$undef USE_JSONDATAOBJECTS_UNIT}

unit ACBrPIXSchemasCobranca;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrBase, ACBrPIXBase;

type

  { TACBrPIXPessoaFisica }

  TACBrPIXPessoaFisica = class
  private
    fcpf: String;
    fnome: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXPessoaFisica);

    property cpf: String read fcpf write fcpf;
    property nome: String read fnome write fnome;
  end;

  { TACBrPIXPessoaJuridica }

  TACBrPIXPessoaJuridica = class
  private
    fcnpj: String;
    fnome: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXPessoaJuridica);

    property cnpj: String read fcnpj write fcnpj;
    property nome: String read fnome write fnome;
  end;

  { TACBrPIXDevedor }

  TACBrPIXDevedor = class
  private
    fcnpj: String;
    fcpf: String;
    fnome: String;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXDevedor);

    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property nome: String read fnome write fnome;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXCalendario }

  TACBrPIXCalendario = class
  private
    fapresentacao: TDateTime;
    fcriacao: TDateTime;
    fexpiracao: Integer;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXCalendario);

    // "Tempo de vida da cobrança, especificado em segundos."
    property criacao: TDateTime read fcriacao write fcriacao;
    property apresentacao: TDateTime read fapresentacao write fapresentacao;
    property expiracao: Integer read fexpiracao write fexpiracao;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXInfoAdicional }

  TACBrPIXInfoAdicional = class
  private
    fnome: String;
    fvalor: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXInfoAdicional);

    property nome: String read fnome write fnome;
    property valor: String read fvalor write fvalor;
  end;

  { TACBrPIXInfoAdicionais }

  TACBrPIXInfoAdicionais = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TACBrPIXInfoAdicional;
    procedure SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
  public
    procedure Assign(Source: TACBrPIXInfoAdicionais);
    Function Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
    Procedure Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
    function New: TACBrPIXInfoAdicional;
    property Items[Index: Integer]: TACBrPIXInfoAdicional read GetItem write SetItem; default;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXSaqueTroco }

  TACBrPIXSaqueTroco = class
  private
    fmodalidadeAgente: TACBrPIXModalidadeAgente;
    fmodalidadeAlteracao: Boolean;
    fprestadorDoServicoDeSaque: String;
    fvalor: Currency;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXSaqueTroco);

    property valor: Currency read fvalor write fvalor;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property modalidadeAgente: TACBrPIXModalidadeAgente read fmodalidadeAgente write fmodalidadeAgente;
    property prestadorDoServicoDeSaque: String read fprestadorDoServicoDeSaque write fprestadorDoServicoDeSaque;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXRetirada }

  TACBrPIXRetirada = class
  private
    fsaque: TACBrPIXSaqueTroco;
    ftroco: TACBrPIXSaqueTroco;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TACBrPIXRetirada);

    property saque: TACBrPIXSaqueTroco read fsaque;
    property troco: TACBrPIXSaqueTroco read ftroco;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXCobValor }

  TACBrPIXCobValor = class
  private
    fmodalidadeAlteracao: Boolean;
    foriginal: Currency;
    fretirada: TACBrPIXRetirada;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TACBrPIXCobValor);

    property original: Currency read foriginal write foriginal;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property retirada: TACBrPIXRetirada read fretirada;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXCobBase }

  TACBrPIXCobBase = class
  private
    frevisao: Integer;
    fassinatura: String;
    fchave: String;
    finfoAdicionais: TACBrPIXInfoAdicionais;
    fsolicitacaoPagador: String;
    fstatus: TACBrPIXStatusCobranca;
    ftxid: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TACBrPIXCobBase);

    property revisao: Integer read frevisao write frevisao;
    property chave: String read fchave write fchave;
    property txid: String read ftxid write ftxid;
    property solicitacaoPagador: String read fsolicitacaoPagador write fsolicitacaoPagador;
    property infoAdicionais: TACBrPIXInfoAdicionais read finfoAdicionais;
    property assinatura: String read fassinatura write fassinatura;
    property status: TACBrPIXStatusCobranca read fstatus write fstatus;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXCobSolicitada }

  TACBrPIXCobSolicitada = class(TACBrPIXCobBase)
  private
    fcalendario: TACBrPIXCalendario;
    fdevedor: TACBrPIXDevedor;
    fvalor: TACBrPIXCobValor;

    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TACBrPIXCobSolicitada);

    property calendario: TACBrPIXCalendario read fcalendario;
    property devedor: TACBrPIXDevedor read fdevedor;
    property valor: TACBrPIXCobValor read fvalor;

    property AsJSON: String read GetAsJSON write SetAsJSON;
  end;

function PIXStatusToString(AStatus: TACBrPIXStatusCobranca): String;
function StringToPIXStatus(AString: String): TACBrPIXStatusCobranca;

function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
function StringToPIXModalidadeAgente(AString: String): TACBrPIXModalidadeAgente;

implementation

uses
  StrUtils, DateUtils, Math,
  ACBrUtil, ACBrConsts, ACBrPIXUtil;

function PIXStatusToString(AStatus: TACBrPIXStatusCobranca): String;
begin
  case AStatus of
    stcATIVA: Result := 'ATIVA';
    stcCONCLUIDA: Result := 'CONCLUIDA';
    stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := 'REMOVIDA_PELO_USUARIO_RECEBEDOR';
    stcREMOVIDA_PELO_PSP: Result := 'REMOVIDA_PELO_PSP';
  else
    Result := '';
  end;
end;

function StringToPIXStatus(AString: String): TACBrPIXStatusCobranca;
begin
  if (AString = 'ATIVA') then
    Result := stcATIVA
  else if (AString = 'CONCLUIDA') then
    Result := stcCONCLUIDA
  else if (AString = 'REMOVIDA_PELO_USUARIO_RECEBEDOR') then
    Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR
  else if (AString = 'REMOVIDA_PELO_PSP') then
    Result := stcREMOVIDA_PELO_PSP
  else
    Result := stcNENHUM;
end;

function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
begin
  case AModalidadeAgente of
    maAGTEC: Result := 'AGTEC';
    maAGTOT: Result := 'AGTOT';
    maAGPSS: Result := 'AGPSS';
  else
    Result := '';
  end;
end;

function StringToPIXModalidadeAgente(AString: String): TACBrPIXModalidadeAgente;
begin
  if (AString = 'AGTEC') then
    Result := maAGTEC
  else if (AString = 'AGTOT') then
    Result := maAGTOT
  else if (AString = 'AGPSS') then
    Result := maAGPSS
  else
    Result := maNENHUM;
end;


{ TACBrPIXPessoaFisica }

constructor TACBrPIXPessoaFisica.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXPessoaFisica.Clear;
begin
  fcpf := '';
  fnome := '';
end;

procedure TACBrPIXPessoaFisica.Assign(Source: TACBrPIXPessoaFisica);
begin
  fcpf := Source.cpf;
  fnome := Source.nome;
end;

{ TACBrPIXPessoaJuridica }

constructor TACBrPIXPessoaJuridica.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXPessoaJuridica.Clear;
begin
  fcnpj := '';
  fnome := '';
end;

procedure TACBrPIXPessoaJuridica.Assign(Source: TACBrPIXPessoaJuridica);
begin
  fcnpj := Source.cnpj;
  fnome := Source.nome;
end;

{ TACBrPIXDevedor }

constructor TACBrPIXDevedor.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXDevedor.Clear;
begin
  fcpf := '';
  fcnpj := '';
  fnome := '';
end;

procedure TACBrPIXDevedor.Assign(Source: TACBrPIXDevedor);
begin
  fcpf := Source.cpf;
  fcnpj := Source.cnpj;
  fnome := Source.nome;
end;

procedure TACBrPIXDevedor.WriteToJSon(AJSon: TJsonObject);
var
  jsd: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsd := AJSon.O['devedor'];
   if (cnpj <> '') then
     jsd.S['cnpj'] := cnpj
   else if (cpf <> '') then
     jsd.S['cpf'] := cnpj;
   jsd.S['nome'] := nome;
  {$Else}
   jsd := AJSon['devedor'].AsObject;
   if (cnpj <> '') then
     jsd['cnpj'].AsString := cnpj
   else if (cpf <> '') then
     jsd['cpf'].AsString := cnpj;
   jsd['nome'].AsString := nome;
  {$EndIf}
end;

procedure TACBrPIXDevedor.ReadFromJSon(AJSon: TJsonObject);
var
  jsd: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsd := AJSon.O['devedor'];
   cnpj := jsd.S['cnpj'];
   cpf  := jsd.S['cpf'];
   nome := jsd.S['nome'];
  {$Else}
   jsd := AJSon['devedor'].AsObject;
   cnpj := jsd['cnpj'].AsString;
   cpf  := jsd['cpf'].AsString;
   nome := jsd['nome'].AsString;
  {$EndIf}
end;

procedure TACBrPIXDevedor.SetCnpj(AValue: String);
begin
  if fcnpj = AValue then Exit;
  fcnpj := OnlyNumber(AValue);
end;

procedure TACBrPIXDevedor.SetCpf(AValue: String);
begin
  if fcpf = AValue then Exit;
  fcpf := OnlyNumber(AValue);
end;

{ TACBrPIXCalendario }

constructor TACBrPIXCalendario.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXCalendario.Clear;
begin
  fcriacao := 0;
  fapresentacao := 0;
  fexpiracao := 0;
end;

procedure TACBrPIXCalendario.Assign(Source: TACBrPIXCalendario);
begin
  fcriacao := Source.criacao;
  fapresentacao := Source.apresentacao;
  fexpiracao := Source.expiracao;
end;

procedure TACBrPIXCalendario.WriteToJSon(AJSon: TJsonObject);
var
  jsc: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsc := AJSon.O['calendario'];
   if (criacao <> 0) then
     jsc.S['criacao'] := DateTimeToIso8601(criacao);
   if (apresentacao <> 0) then
     jsc.S['apresentacao'] := DateTimeToIso8601(apresentacao);
   if (expiracao > 0) then
     jsc.I['expiracao'] := expiracao;
  {$Else}
   jsc := AJSon['calendario'].AsObject;
   if (criacao <> 0) then
     jsc['criacao'].AsString := DateTimeToIso8601(criacao);
   if (apresentacao <> 0) then
     jsc['apresentacao'].AsString := DateTimeToIso8601(apresentacao);
   if (expiracao > 0) then
     jsc['expiracao'].AsInteger := expiracao;
  {$EndIf}
end;

procedure TACBrPIXCalendario.ReadFromJSon(AJSon: TJsonObject);
var
  jsc: TJsonObject;
  s: String;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsc := AJSon.O['calendario'];
   s := jsc.S['criacao'];
   if (s <> '') then
     criacao := Iso8601ToDateTime(s);
   s := jsc.S['apresentacao'];
   if (s <> '') then
     apresentacao := Iso8601ToDateTime(s);
   expiracao := jsc.I['expiracao'];
  {$Else}
   jsc := AJSon['calendario'].AsObject;
   s := jsc['criacao'].AsString;
   if (s <> '') then
     criacao := Iso8601ToDateTime(s);
   s := jsc['apresentacao'].AsString;
   if (s <> '') then
     apresentacao := Iso8601ToDateTime(s);
   expiracao := jsc['expiracao'].AsInteger;
  {$EndIf}
end;

{ TACBrPIXInfoAdicional }

constructor TACBrPIXInfoAdicional.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXInfoAdicional.Clear;
begin
  fnome := '';
  fvalor := '';
end;

procedure TACBrPIXInfoAdicional.Assign(Source: TACBrPIXInfoAdicional);
begin
  fnome := Source.nome;
  fvalor := Source.valor;
end;

{ TACBrPIXInfoAdicionais }

function TACBrPIXInfoAdicionais.GetItem(Index: Integer): TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional(inherited Items[Index]);
end;

procedure TACBrPIXInfoAdicionais.SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
begin
  inherited Items[Index] := Value;
end;

procedure TACBrPIXInfoAdicionais.Assign(Source: TACBrPIXInfoAdicionais);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    New.Assign(Source[i]);
end;

function TACBrPIXInfoAdicionais.Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
begin
  Result := inherited Add(AInfoAdicional);
end;

procedure TACBrPIXInfoAdicionais.Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
begin
  inherited Insert(Index, AInfoAdicional);
end;

function TACBrPIXInfoAdicionais.New: TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional.Create;
  Self.Add(Result);
end;

procedure TACBrPIXInfoAdicionais.WriteToJSon(AJSon: TJsonObject);
var
  i: Integer;
  ia: TACBrPIXInfoAdicional;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.A['infoAdicionais'].Clear;
   for i := 0 to Count-1 do
   begin
     ia := Items[i];
     with AJSon.A['infoAdicionais'].AddObject do
     begin
       S['nome'] := ia.nome;
       S['valor'] := ia.valor;
     end;
   end;
  {$Else}
   AJSon['infoAdicionais'].AsArray.Clear;
   for i := 0 to Count-1 do
   begin
     ia := Items[i];
     with AJSon['infoAdicionais'].AsArray.Add.AsObject do
     begin
       Values['nome'].AsString := ia.nome;
       Values['valor'].AsString := ia.valor;
     end;
   end;
  {$EndIf}
end;

procedure TACBrPIXInfoAdicionais.ReadFromJSon(AJSon: TJsonObject);
var
  ja: TJsonArray;
  i: Integer;
  jai: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   ja := AJSon.A['infoAdicionais'];
   for i := 0 to ja.Count-1 do
   begin
     jai := ja.O[i];
     with New do
     begin
       nome := jai.S['nome'];
       valor := jai.S['valor'];
     end;
   end;
  {$Else}
   ja := AJSon['infoAdicionais'].AsArray;
   for i := 0 to ja.Count-1 do
   begin
     jai := ja[i].AsObject;
     with New do
     begin
       nome := jai['nome'].AsString;
       valor := jai['valor'].AsString;
     end;
   end;
  {$EndIf}
end;

{ TACBrPIXSaqueTroco }

constructor TACBrPIXSaqueTroco.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXSaqueTroco.Clear;
begin
  fvalor := 0;
  fmodalidadeAlteracao := False;
  fmodalidadeAgente := maAGTEC;
  fprestadorDoServicoDeSaque := ''
end;

procedure TACBrPIXSaqueTroco.Assign(Source: TACBrPIXSaqueTroco);
begin
  fvalor := Source.valor;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fmodalidadeAgente := Source.modalidadeAgente;
  fprestadorDoServicoDeSaque := Source.prestadorDoServicoDeSaque;
end;

procedure TACBrPIXSaqueTroco.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['valor'] := FormatarValorPIX(valor);
   AJSon.I['modalidadeAlteracao'] := IfThen(modalidadeAlteracao, 1, 0);
   AJSon.S['modalidadeAgente'] := PIXModalidadeAgenteToString(modalidadeAgente);
   AJSon.S['prestadorDoServicoDeSaque'] := prestadorDoServicoDeSaque;
  {$Else}
   AJSon['valor'].AsString := FormatarValorPIX(valor);
   AJSon['modalidadeAlteracao'].AsInteger := IfThen(modalidadeAlteracao, 1, 0);
   AJSon['modalidadeAgente'].AsString := PIXModalidadeAgenteToString(modalidadeAgente);
   AJSon['prestadorDoServicoDeSaque'].AsString := prestadorDoServicoDeSaque;
  {$EndIf}
end;

procedure TACBrPIXSaqueTroco.ReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   valor := StringToFloatDef( AJSon.S['valor'], 0 );
   modalidadeAlteracao := (AJSon.I['modalidadeAlteracao'] = 1);
   modalidadeAgente := StringToPIXModalidadeAgente( AJSon.S['modalidadeAgente'] );
   prestadorDoServicoDeSaque := AJSon.S['prestadorDoServicoDeSaque'];
  {$Else}
   valor := StringToFloatDef( AJSon['valor'].AsString, 0 );
   modalidadeAlteracao := (AJSon['modalidadeAlteracao'].AsInteger = 1);
   modalidadeAgente := StringToPIXModalidadeAgente( AJSon['modalidadeAgente'].AsString );
   prestadorDoServicoDeSaque := AJSon['prestadorDoServicoDeSaque'].AsString;
  {$EndIf}
end;

{ TACBrPIXRetirada }

constructor TACBrPIXRetirada.Create;
begin
  inherited;
  fsaque := TACBrPIXSaqueTroco.Create;
  ftroco := TACBrPIXSaqueTroco.Create;
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

procedure TACBrPIXRetirada.Assign(Source: TACBrPIXRetirada);
begin
  fsaque.Assign(Source.saque);
  ftroco.Assign(Source.troco);
end;

procedure TACBrPIXRetirada.WriteToJSon(AJSon: TJsonObject);
var
  s: String;
  st: TACBrPIXSaqueTroco;
  jsr, jsst: TJsonObject;
begin
  s := '';
  st := Nil;
  if (saque.modalidadeAgente <> maNENHUM) then
  begin
    st := saque;
    s := 'saque';
  end
  else if (troco.modalidadeAgente <> maNENHUM) then
  begin
    st := troco;
    s := 'troco';
  end;

  if (st <> nil) then
  begin
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     jsr := AJSon.O['retirada'];
     jsst := jsr.O[s];
    {$Else}
     jsr := AJSon['retirada'].AsObject;
     jsst := jsr[s].AsObject;
    {$EndIf}
    st.WriteToJSon(jsst);
  end;
end;

procedure TACBrPIXRetirada.ReadFromJSon(AJSon: TJsonObject);
var
  jsr, jss, jst: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsr := AJSon.O['retirada'];
   jss := jsr.O['saque'];
   jst := jsr.O['troco'];
   saque.ReadFromJSon(jss);
   troco.ReadFromJSon(jst);
  {$Else}
   jsr := AJSon['retirada'].AsObject;
   jss := jsr['saque'].AsObject;
   jst := jsr['troco'].AsObject;
   saque.ReadFromJSon(jss);
   troco.ReadFromJSon(jst);
  {$EndIf}
end;

{ TACBrPIXCobValor }

constructor TACBrPIXCobValor.Create;
begin
  inherited;
  fretirada := TACBrPIXRetirada.Create;
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

procedure TACBrPIXCobValor.Assign(Source: TACBrPIXCobValor);
begin
  foriginal := Source.original;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fretirada.Assign(Source.retirada);
end;

procedure TACBrPIXCobValor.WriteToJSon(AJSon: TJsonObject);
var
  jsv: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsv := AJSon.O['valor'];
   jsv.S['original'] := FormatarValorPIX(original);
   jsv.I['modalidadeAlteracao'] := IfThen(modalidadeAlteracao, 1, 0);
  {$Else}
   jsv := AJSon['valor'].AsObject;
   jsv['original'].AsString := FormatarValorPIX(original);
   jsv['modalidadeAlteracao'].AsInteger := IfThen(modalidadeAlteracao, 1, 0);
  {$EndIf}
  retirada.WriteToJSon(jsv);
end;

procedure TACBrPIXCobValor.ReadFromJSon(AJSon: TJsonObject);
var
  jsv: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsv := AJSon.O['valor'];
   original := StringToFloatDef( jsv.S['original'], 0 );
   modalidadeAlteracao := (jsv.I['modalidadeAlteracao'] = 1);
  {$Else}
   jsv := AJSon['valor'].AsObject;
   original := StringToFloatDef( jsv['original'].AsString, 0 );
   modalidadeAlteracao := (jsv['modalidadeAlteracao'].AsInteger = 1);
  {$EndIf}
  retirada.ReadFromJSon(jsv);
end;

{ TACBrPIXCobBase }

constructor TACBrPIXCobBase.Create;
begin
  inherited;
  finfoAdicionais := TACBrPIXInfoAdicionais.Create();
  Clear;
end;

destructor TACBrPIXCobBase.Destroy;
begin
  finfoAdicionais.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobBase.Clear;
begin
  frevisao := 0;
  fchave := '';
  ftxid := '';
  fassinatura := '';
  fsolicitacaoPagador := '';
  fstatus := stcNENHUM;
  finfoAdicionais.Clear;
end;

procedure TACBrPIXCobBase.Assign(Source: TACBrPIXCobBase);
begin
  frevisao := Source.revisao;
  fchave := Source.chave;
  ftxid := Source.txid;
  fassinatura := Source.assinatura;
  fsolicitacaoPagador := Source.solicitacaoPagador;
  fstatus := Source.status;
  finfoAdicionais.Assign(Source.infoAdicionais);
end;

procedure TACBrPIXCobBase.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (frevisao > 0) then
     AJSon.I['revisao'] := frevisao;
   if (fchave <> '') then
     AJSon.S['chave'] := fchave;
   if (ftxid <> '') then
     AJSon.S['txid'] := ftxid;
   if (fsolicitacaoPagador <> '') then
     AJSon.S['solicitacaoPagador'] := fsolicitacaoPagador;
   finfoAdicionais.WriteToJSon(AJSon);
   if (fassinatura <> '') then
     AJSon.S['assinatura'] := fassinatura;
   if (fstatus <> stcNENHUM) then
     AJSon.S['status'] := PIXStatusToString(fstatus);
  {$Else}
   if (frevisao > 0) then
     AJSon['revisao'].AsInteger := frevisao;
   if (fchave <> '') then
     AJSon['chave'].AsString := fchave;
   if (ftxid <> '') then
     AJSon['txid'].AsString := ftxid;
   if (fsolicitacaoPagador <> '') then
     AJSon['solicitacaoPagador'].AsString := fsolicitacaoPagador;
   finfoAdicionais.WriteToJSon(AJSon);
   if (fassinatura <> '') then
     AJSon['assinatura'].AsString := fassinatura;
   if (fstatus <> stcNENHUM) then
     AJSon['status'].AsString := PIXStatusToString(fstatus);
  {$EndIf}
end;

procedure TACBrPIXCobBase.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   frevisao := AJSon.I['revisao'];
   fchave := AJSon.S['chave'];
   fsolicitacaoPagador := AJSon.S['solicitacaoPagador'];
   ftxid := AJSon.S['txid'];
   fassinatura := AJSon.S['assinatura'];
   fstatus := StringToPIXStatus(AJSon.S['status']);
   finfoAdicionais.ReadFromJSon(AJSon);
  {$Else}
   frevisao := AJSon['revisao'].AsInteger;
   fchave := AJSon['chave'].AsString;
   fsolicitacaoPagador := AJSon['solicitacaoPagador'].AsString;
   ftxid := AJSon['txid'].AsString;
   fassinatura := AJSon['assinatura'].AsString;
   fstatus := StringToPIXStatus(AJSon['status'].AsString);
   finfoAdicionais.ReadFromJSon(AJSon);
  {$EndIf}
end;

{ TACBrPIXCobSolicitada }

constructor TACBrPIXCobSolicitada.Create;
begin
  inherited;
  fcalendario := TACBrPIXCalendario.Create;
  fdevedor := TACBrPIXDevedor.Create;
  fvalor := TACBrPIXCobValor.Create;
  Clear;
end;

destructor TACBrPIXCobSolicitada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobSolicitada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  fvalor.Clear;
end;

procedure TACBrPIXCobSolicitada.Assign(Source: TACBrPIXCobSolicitada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  fvalor.Assign(Source.valor);
end;

function TACBrPIXCobSolicitada.GetAsJSON: String;
var
  js: TJsonObject;
begin
  js := TJsonObject.Create;
  try
    inherited WriteToJSon(js);
    fcalendario.WriteToJSon(js);
    fdevedor.WriteToJSon(js);
    fvalor.WriteToJSon(js);

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     Result := js.ToJSON();
    {$Else}
     Result := js.Stringify;
    {$EndIf}
  finally
    js.Free;
  end;
end;

procedure TACBrPIXCobSolicitada.SetAsJSON(AValue: String);
var
  js: TJsonObject;

  procedure _ReadFromJSon(AJSon: TJsonObject);
  begin
    inherited ReadFromJSon(AJSon);
    fcalendario.ReadFromJSon(AJSon);
    fdevedor.ReadFromJSon(AJSon);
    fvalor.ReadFromJSon(AJSon);
  end;

begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(AValue) as TJsonObject;
   try
     _ReadFromJSon(js);
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     js.Parse(AValue);
     _ReadFromJSon(js);
   finally
     js.Free;
   end;
  {$EndIf}
end;

end.

