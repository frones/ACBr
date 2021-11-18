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
  end;

  TACBrPIXModalidadeAgente = ( maNENHUM, maAGTEC, maAGTOT, maAGPSS );

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

    function GetAsJSON: String; virtual;
    procedure SetAsJSON(AValue: String); virtual;
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

    property AsJSON: String read GetAsJSON write SetAsJSON;
  end;

  { TACBrPIXCobSolicitada }

  TACBrPIXCobSolicitada = class(TACBrPIXCobBase)
  private
    fcalendario: TACBrPIXCalendario;
    fdevedor: TACBrPIXDevedor;
    fvalor: TACBrPIXCobValor;

    function GetAsJSON: String; override;
    procedure SetAsJSON(AValue: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TACBrPIXCobSolicitada);

    property calendario: TACBrPIXCalendario read fcalendario;
    property devedor: TACBrPIXDevedor read fdevedor;
    property valor: TACBrPIXCobValor read fvalor;
  end;

function PIXStatusToString(AStatus: TACBrPIXStatusCobranca): String;
function StringToPIXStatus(AString: String): TACBrPIXStatusCobranca;

function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
function StringToPIXModalidadeAgente(AString: String): TACBrPIXModalidadeAgente;

implementation

uses
  StrUtils, DateUtils, Math,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
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

{ TACBrPIXCobBase }

function TACBrPIXCobBase.GetAsJSON: String;
var
  jo: TJsonObject;
  i: Integer;
  ia: TACBrPIXInfoAdicional;
begin
  jo := TJsonObject.Create;
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     if (frevisao > 0) then
       jo.I['revisao'] := frevisao;
     if (fchave <> '') then
       jo.S['chave'] := fchave;
     if (ftxid <> '') then
       jo.S['txid'] := ftxid;
     if (fsolicitacaoPagador <> '') then
       jo.S['solicitacaoPagador'] := fsolicitacaoPagador;

     for i := 0 to finfoAdicionais.Count-1 do
     begin
       ia := finfoAdicionais[i];
       with jo.A['infoAdicionais'].AddObject do
       begin
         S['nome'] := ia.nome;
         S['valor'] := ia.valor;
       end;
     end;

     if (fassinatura <> '') then
       jo.S['assinatura'] := fassinatura;
     if (fstatus <> stcNENHUM) then
       jo.S['status'] := PIXStatusToString(fstatus);

     Result := jo.ToJSON();
    {$Else}
     if (frevisao > 0) then
       jo['revisao'].AsInteger := frevisao;
     if (fchave <> '') then
       jo['chave'].AsString := fchave;
     if (ftxid <> '') then
       jo['txid'].AsString := ftxid;
     if (fsolicitacaoPagador <> '') then
       jo['solicitacaoPagador'].AsString := fsolicitacaoPagador;

     for i := 0 to finfoAdicionais.Count-1 do
     begin
       ia := finfoAdicionais[i];
       with jo['infoAdicionais'].AsArray.Add.AsObject do
       begin
         Values['nome'].AsString := ia.nome;
         Values['valor'].AsString := ia.valor;
       end;
     end;

     if (fassinatura <> '') then
       jo['assinatura'].AsString := fassinatura;
     if (fstatus <> stcNENHUM) then
       jo['status'].AsString := PIXStatusToString(fstatus);

     Result := jo.Stringify;
    {$EndIf}
  finally
    jo.Free;
  end;
end;

procedure TACBrPIXCobBase.SetAsJSON(AValue: String);
var
  jo, jai: TJsonObject;
  ja: TJsonArray;
  i: Integer;
begin
  Clear;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jo := TJsonObject.Parse(AValue) as TJsonObject;
   try
     frevisao := jo.I['revisao'];
     fchave := jo.S['chave'];
     fsolicitacaoPagador := jo.S['solicitacaoPagador'];
     ftxid := jo.S['txid'];
     fassinatura := jo.S['assinatura'];
     fstatus := StringToPIXStatus(jo.S['status']);
     ja := jo.A['infoAdicionais'];
     for i := 0 to ja.Count-1 do
     begin
       jai := ja.O[i];
       with finfoAdicionais.New do
       begin
         nome := jai.S['nome'];
         valor := jai.S['valor'];
       end;
     end;
   finally
     jo.Free;
   end;
  {$Else}
   jo := TJsonObject.Create;
   try
     jo.Parse(AValue);

     frevisao := jo['revisao'].AsInteger;
     fchave := jo['chave'].AsString;
     fsolicitacaoPagador := jo['solicitacaoPagador'].AsString;
     ftxid := jo['txid'].AsString;
     fassinatura := jo['assinatura'].AsString;
     fstatus := StringToPIXStatus(jo['status'].AsString);
     ja := jo['infoAdicionais'].AsArray;
     for i := 0 to ja.Count-1 do
     begin
       jai := ja[i].AsObject;
       with finfoAdicionais.New do
       begin
         nome := jai['nome'].AsString;
         valor := jai['valor'].AsString;
       end;
     end;
   finally
     jo.Free;
   end;
  {$EndIf}
end;

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

{ TACBrPIXCobSolicitada }

function TACBrPIXCobSolicitada.GetAsJSON: String;
var
  js, jsc, jsd, jsv, jsr, jsst: TJsonObject;
  st: TACBrPIXSaqueTroco;
  s, u: String;
begin
  s := inherited GetAsJSON;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(s) as TJsonObject;
   try
     with fcalendario do
     begin
       jsc := js.O['calendario'];
       if (criacao <> 0) then
         jsc.S['criacao'] := DateTimeToIso8601(criacao);
       if (apresentacao <> 0) then
         jsc.S['apresentacao'] := DateTimeToIso8601(apresentacao);
       if (expiracao > 0) then
         jsc.I['expiracao'] := expiracao;
     end;

     with fdevedor do
     begin
       jsd := js.O['devedor'];
       if (cnpj <> '') then
         jsd.S['cnpj'] := cnpj
       else if (cpf <> '') then
         jsd.S['cpf'] := cnpj;

       jsd.S['nome'] := nome;
     end;

     with fvalor do
     begin
       jsv := js.O['valor'];
       jsv.S['original'] := FormatarValorPIX(original);
       jsv.I['modalidadeAlteracao'] := IfThen(modalidadeAlteracao, 1, 0);

       with retirada do
       begin
         s := '';
         st := Nil;
         if  (saque.modalidadeAgente <> maNENHUM) then
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
           jsr := jsv.O['retirada'];
           jsst := jsr.O[s];

           jsst.S['valor'] := FormatarValorPIX(st.valor);
           jsst.I['modalidadeAlteracao'] := IfThen(st.modalidadeAlteracao, 1, 0);
           jsst.S['modalidadeAgente'] := PIXModalidadeAgenteToString(st.modalidadeAgente);
           jsst.S['prestadorDoServicoDeSaque'] := st.prestadorDoServicoDeSaque;
         end;
       end;
     end;

     Result := js.ToJSON();
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     u := js.Decode(s);
     js.Parse(u);

     with fcalendario do
     begin
       jsc := js['calendario'].AsObject;
       if (criacao <> 0) then
         jsc['criacao'].AsString := DateTimeToIso8601(criacao);
       if (apresentacao <> 0) then
         jsc['apresentacao'].AsString := DateTimeToIso8601(apresentacao);
       if (expiracao > 0) then
         jsc['expiracao'].AsInteger := expiracao;

     end;

     with fdevedor do
     begin
       jsd := js['devedor'].AsObject;
       if (cnpj <> '') then
         jsd['cnpj'].AsString := cnpj
       else if (cpf <> '') then
         jsd['cpf'].AsString := cnpj;

       jsd['nome'].AsString := nome;
     end;

     with fvalor do
     begin
       jsv := js['valor'].AsObject;
       jsv['original'].AsString := FormatarValorPIX(original);
       jsv['modalidadeAlteracao'].AsInteger := IfThen(modalidadeAlteracao, 1, 0);

       with retirada do
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
           jsr := jsv['retirada'].AsObject;
           jsst := jsr[s].AsObject;

           jsst['valor'].AsString := FormatarValorPIX(st.valor);
           jsst['modalidadeAlteracao'].AsInteger := IfThen(st.modalidadeAlteracao, 1, 0);
           jsst['modalidadeAgente'].AsString := PIXModalidadeAgenteToString(st.modalidadeAgente);
           jsst['prestadorDoServicoDeSaque'].AsString := st.prestadorDoServicoDeSaque;
         end;
       end;
     end;

     Result := js.Stringify;
   finally
     js.Free;
   end;
  {$EndIf}
end;

procedure TACBrPIXCobSolicitada.SetAsJSON(AValue: String);
var
  js, jsr, jsc, jsd, jsv, jss, jst: TJsonObject;
  jsa: TJsonArray;
  s: String;
begin
  inherited SetAsJSON(AValue);

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(AValue) as TJsonObject;
   try
     with fcalendario do
     begin
       jsc := js.O['calendario'];
       s := jsc.S['criacao'];
       if (s <> '') then
         criacao := Iso8601ToDateTime(s);
       s := jsc.S['apresentacao'];
       if (s <> '') then
         apresentacao := Iso8601ToDateTime(s);
       expiracao := jsc.I['expiracao'];
     end;

     with fdevedor do
     begin
       jsd := js.O['devedor'];
       cnpj := jsd.S['cnpj'];
       cpf  := jsd.S['cpf'];
       nome := jsd.S['nome'];
     end;

     with fvalor do
     begin
       jsv := js.O['valor'];
       original := StringToFloatDef( jsv.S['original'], 0 );
       modalidadeAlteracao := (jsv.I['modalidadeAlteracao'] = 1);

       with retirada do
       begin
         jsr := jsv.O['retirada'];
         jss := jsr.O['saque'];
         jst := jsr.O['troco'];

         saque.valor := StringToFloatDef( jss.S['valor'], 0 );
         saque.modalidadeAlteracao := (jss.I['modalidadeAlteracao'] = 1);
         saque.modalidadeAgente := StringToPIXModalidadeAgente( jss.S['modalidadeAgente'] );
         saque.prestadorDoServicoDeSaque := jss.S['prestadorDoServicoDeSaque'];

         troco.valor := StringToFloatDef( jst.S['valor'], 0 );
         troco.modalidadeAlteracao := (jst.I['modalidadeAlteracao'] = 1);
         troco.modalidadeAgente := StringToPIXModalidadeAgente( jst.S['modalidadeAgente'] );
         troco.prestadorDoServicoDeSaque := jst.S['prestadorDoServicoDeSaque'];
       end;
     end;
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     js.Parse(AValue);

     with fcalendario do
     begin
       jsc := js['calendario'].AsObject;
       s := jsc['criacao'].AsString;
       if (s <> '') then
         criacao := Iso8601ToDateTime(s);
       s := jsc['apresentacao'].AsString;
       if (s <> '') then
         apresentacao := Iso8601ToDateTime(s);
       expiracao := jsc['expiracao'].AsInteger;
     end;

     with fdevedor do
     begin
       jsd := js['devedor'].AsObject;
       cnpj := jsd['cnpj'].AsString;
       cpf  := jsd['cpf'].AsString;
       nome := jsd['nome'].AsString;
     end;

     with fvalor do
     begin
       jsv := js['valor'].AsObject;
       original := StringToFloatDef( jsv['original'].AsString, 0 );
       modalidadeAlteracao := (jsv['modalidadeAlteracao'].AsInteger = 1);

       with retirada do
       begin
         jsr := jsv['retirada'].AsObject;
         jss := jsr['saque'].AsObject;
         jst := jsr['troco'].AsObject;

         saque.valor := StringToFloatDef( jss['valor'].AsString, 0 );
         saque.modalidadeAlteracao := (jss['modalidadeAlteracao'].AsInteger = 1);
         saque.modalidadeAgente := StringToPIXModalidadeAgente( jss['modalidadeAgente'].AsString );
         saque.prestadorDoServicoDeSaque := jss['prestadorDoServicoDeSaque'].AsString;

         troco.valor := StringToFloatDef( jst['valor'].AsString, 0 );
         troco.modalidadeAlteracao := (jst['modalidadeAlteracao'].AsInteger = 1);
         troco.modalidadeAgente := StringToPIXModalidadeAgente( jst['modalidadeAgente'].AsString );
         troco.prestadorDoServicoDeSaque := jst['prestadorDoServicoDeSaque'].AsString;
       end;
     end;
   finally
     js.Free;
   end;
  {$EndIf}
end;

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

end.

