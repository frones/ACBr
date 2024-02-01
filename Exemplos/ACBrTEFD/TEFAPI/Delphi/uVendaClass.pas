{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit uVendaClass;

interface

uses
  Classes, SysUtils, contnrs, IniFiles;

const
  cPagamentos: array[0..6] of array [0..1] of String =
     ( ('01','Dinheiro'),
       ('02','Cheque'),
       ('03','Cartão de Crédito'),
       ('04','Cartão de Débito'),
       ('05','Carteira Digital'),
       ('06','Vale Refeição'),
       ('99','Outros') );

type
  TStatusVenda = (stsLivre, stsIniciada, stsEmPagamento, stsCancelada, stsAguardandoTEF, stsOperacaoTEF, stsFinalizada);

  { TPagamento }

  TPagamento = class
  private
    FAcrescimo: Double;
    FCancelada: Boolean;
    FConfirmada: Boolean;
    FDesconto: Double;
    FHora: TDateTime;
    FRedeCNPJ: String;
    FNSU: String;
    FRede: String;
    FSaque: Double;
    FTipoPagamento: String;
    FValorPago: Currency;

  public
    constructor Create;
    procedure Clear;

    property TipoPagamento: String read FTipoPagamento write FTipoPagamento;
    property ValorPago: Currency read FValorPago write FValorPago;
    property Hora: TDateTime read FHora write FHora;
    property NSU: String read FNSU write FNSU;
    property Rede: String read FRede write FRede;
    property RedeCNPJ: String read FRedeCNPJ write FRedeCNPJ;
    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property Desconto: Double read FDesconto write FDesconto;
    property Saque: Double read FSaque write FSaque;
    property Confirmada: Boolean read FConfirmada write FConfirmada;
    property Cancelada: Boolean read FCancelada write FCancelada;
  end;

  { TListaPagamentos }

  TListaPagamentos = class(TObjectList)
  private
    function GetTotalPago: Double;
  protected
    procedure SetObject(Index: Integer; Item: TPagamento);
    function GetObject(Index: Integer): TPagamento;
  public
    function New: TPagamento;
    function Add(Obj: TPagamento): Integer;
    procedure Insert(Index: Integer; Obj: TPagamento);
    property Objects[Index: Integer]: TPagamento read GetObject write SetObject; default;

    property TotalPago: Double read GetTotalPago;
    function AcharPagamento(const ARede, ANSU: String; AValor: Double): TPagamento;
    function ConfirmarPagamento(const ARede, ANSU: String; AValor: Double): Boolean;
    function CancelarPagamento(const ARede, ANSU: String; AValor: Double): Boolean;
  end;

  { TVenda }

  TVenda = class
  private
    FArqVenda: String;
    FDHInicio: TDateTime;
    FNumOperacao: Integer;
    FStatus: TStatusVenda;
    FValorInicial: Currency;
    FTotalAcrescimo: Currency;
    FTotalDesconto: Currency;
    FPagamentos: TListaPagamentos;
    function GetTotalVenda: Currency;
    function GetTotalPago: Currency;
    function GetTroco: Currency;

    function SecPag(i: integer): String;
  public
    constructor Create(const ArqVenda: String);
    destructor Destroy; override;
    procedure Clear;

    procedure Gravar;
    procedure Ler;

    property NumOperacao: Integer read FNumOperacao write FNumOperacao;
    property DHInicio: TDateTime read FDHInicio write FDHInicio;
    property Status: TStatusVenda read FStatus write FStatus;

    property ValorInicial: Currency read FValorInicial write FValorInicial;
    property TotalDesconto: Currency read FTotalDesconto write FTotalDesconto;
    property TotalAcrescimo: Currency read FTotalAcrescimo write FTotalAcrescimo;
    property TotalVenda: Currency read GetTotalVenda;

    property Pagamentos: TListaPagamentos read FPagamentos;
    property TotalPago: Currency read GetTotalPago;
    property Troco: Currency read GetTroco;
  end;


Function DescricaoTipoPagamento(const ATipoPagamento: String): String;

implementation

uses
  math;

function DescricaoTipoPagamento(const ATipoPagamento: String): String;
var
  l, i: Integer;
begin
  Result := '';
  l := Length(cPagamentos)-1;
  For i := 0 to l do
  begin
    if ATipoPagamento = cPagamentos[i,0] then
    begin
      Result := cPagamentos[i,1];
      Break;
    end;
  end;
end;

{ TPagamento }

constructor TPagamento.Create;
begin
  inherited;
  Clear;
end;

procedure TPagamento.Clear;
begin
  FTipoPagamento := cPagamentos[0,0];
  FHora := 0;
  FValorPago := 0;
  FNSU := '';
  FRede := '';
  FConfirmada := False;
  FCancelada := False;
  FRedeCNPJ := '';
  FDesconto := 0;
  FAcrescimo := 0;
  FSaque := 0;
end;

{ TListaPagamentos }

function TListaPagamentos.GetTotalPago: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
  begin
    with Objects[I] do
    begin
      if not Cancelada then
        Result := Result + ValorPago;
    end;
  end;

  Result := RoundTo(Result, -2);
end;

procedure TListaPagamentos.SetObject(Index: Integer; Item: TPagamento);
begin
  inherited SetItem(Index, Item);
end;

function TListaPagamentos.GetObject(Index: Integer): TPagamento;
begin
  Result := inherited GetItem(Index) as TPagamento;
end;

function TListaPagamentos.New: TPagamento;
begin
  Result := TPagamento.Create;
  Result.Hora := Now;
  Add(Result);
end;

function TListaPagamentos.Add(Obj: TPagamento): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TListaPagamentos.Insert(Index: Integer; Obj: TPagamento);
begin
  inherited Insert(Index, Obj);
end;

function TListaPagamentos.AcharPagamento(const ARede, ANSU: String;
  AValor: Double): TPagamento;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to Count-1 do
  begin
    if (ARede = Objects[i].Rede) and
       (ANSU = Objects[i].NSU) and
       (AValor = Objects[i].ValorPago) then
    begin
      Result := Objects[i];
      Break;
    end;
  end;
end;

function TListaPagamentos.ConfirmarPagamento(const ARede, ANSU: String;
  AValor: Double): Boolean;
var
  APag: TPagamento;
begin
  APag := AcharPagamento(ARede, ANSU, AValor);
  if Assigned(APag) then
  begin
    APag.Confirmada := True;
    Result := True;
  end
  else
    Result := False;
end;

function TListaPagamentos.CancelarPagamento(const ARede, ANSU: String;
  AValor: Double): Boolean;
var
  APag: TPagamento;
begin
  APag := AcharPagamento(ARede, ANSU, AValor);
  if Assigned(APag) then
  begin
    APag.Cancelada := True;
    Result := True;
  end
  else
    Result := False;
end;

{ TVenda }

constructor TVenda.Create(const ArqVenda: String);
begin
  FArqVenda := ArqVenda;
  FPagamentos := TListaPagamentos.Create;
  Clear;
end;

destructor TVenda.Destroy;
begin
  FPagamentos.Free;
  inherited Destroy;
end;

procedure TVenda.Clear;
begin
  FNumOperacao := 0;
  FStatus := stsLivre;
  FDHInicio := 0;
  FValorInicial := 0;
  FTotalAcrescimo := 0;
  FTotalDesconto := 0;
  FPagamentos.Clear;
end;

function TVenda.SecPag(i: integer): String;
begin
  Result := 'Pagto'+FormatFloat('000',i);
end;

procedure TVenda.Gravar;
var
  Ini: TMemIniFile;
  ASecPag: String;
  i: Integer;
begin
  Ini := TMemIniFile.Create(FArqVenda);
  try
    Ini.Clear;
    Ini.WriteInteger('Venda','NumOperacao', FNumOperacao);
    Ini.WriteDateTime('Venda','DHInicio', FDHInicio);
    Ini.WriteInteger('Venda','Status', Integer(FStatus));
    Ini.WriteFloat('Valores','ValorInicial', FValorInicial);
    Ini.WriteFloat('Valores','TotalAcrescimo', FTotalAcrescimo);
    Ini.WriteFloat('Valores','TotalDesconto', FTotalDesconto);

    i := 0;
    while i < Pagamentos.Count do
    begin
      ASecPag := SecPag(i);
      Ini.WriteString(ASecPag,'TipoPagamento',Pagamentos[i].TipoPagamento);
      Ini.WriteFloat(ASecPag,'Valor', Pagamentos[i].ValorPago);
      Ini.WriteDateTime(ASecPag,'Hora', Pagamentos[i].Hora);
      Ini.WriteString(ASecPag,'NSU', Pagamentos[i].NSU);
      Ini.WriteString(ASecPag,'Rede', Pagamentos[i].Rede);
      Ini.WriteString(ASecPag,'RedeCNPJ', Pagamentos[i].RedeCNPJ);
      Ini.WriteFloat(ASecPag,'Acrescimo', Pagamentos[i].Acrescimo);
      Ini.WriteFloat(ASecPag,'Desconto', Pagamentos[i].Desconto);
      Ini.WriteFloat(ASecPag,'Saque', Pagamentos[i].Saque);
      Ini.WriteBool(ASecPag,'Confirmada', Pagamentos[i].Confirmada);
      Ini.WriteBool(ASecPag,'Cancelada', Pagamentos[i].Cancelada);
      Inc(i);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TVenda.Ler;
var
  Ini: TMemIniFile;
  i: Integer;
  APag: TPagamento;
  ASecPag: String;
begin
  Clear;
  Ini := TMemIniFile.Create(FArqVenda);
  try
    FNumOperacao := Ini.ReadInteger('Venda','NumOperacao', 0);
    FDHInicio := Ini.ReadDateTime('Venda','DHInicio', Now);
    FStatus := TStatusVenda(Ini.ReadInteger('Venda','Status', 0));
    FValorInicial := Ini.ReadFloat('Valores','ValorInicial', 0);
    FTotalAcrescimo := Ini.ReadFloat('Valores','TotalAcrescimo', 0);
    FTotalDesconto := Ini.ReadFloat('Valores','TotalDesconto', 0);

    i := 1;
    ASecPag := SecPag(i);
    while Ini.SectionExists(ASecPag) do
    begin
      APag := TPagamento.Create;
      APag.TipoPagamento := Ini.ReadString(ASecPag,'TipoPagamento','99');
      APag.ValorPago := Ini.ReadFloat(ASecPag,'Valor', 0);
      APag.Hora := Ini.ReadDateTime(ASecPag,'Hora', 0);
      APag.NSU := Ini.ReadString(ASecPag,'NSU', '');
      APag.Rede := Ini.ReadString(ASecPag,'Rede', '');
      APag.RedeCNPJ := Ini.ReadString(ASecPag,'RedeCNPJ', '');
      APag.Acrescimo := Ini.ReadFloat(ASecPag,'Acrescimo', 0);
      APag.Desconto := Ini.ReadFloat(ASecPag,'Desconto', 0);
      APag.Saque := Ini.ReadFloat(ASecPag,'Saque', 0);
      APag.Confirmada := Ini.ReadBool(ASecPag,'Confirmada', False);
      APag.Cancelada := Ini.ReadBool(ASecPag,'Cancelada', False);

      Pagamentos.Add(APag);

      Inc(i);
      ASecPag := SecPag(i);
    end;
  finally
    Ini.Free;
  end;
end;

function TVenda.GetTotalPago: Currency;
begin
  Result := Pagamentos.TotalPago;
end;

function TVenda.GetTroco: Currency;
begin
  Result := TotalPago - TotalVenda;
end;

function TVenda.GetTotalVenda: Currency;
begin
  Result := FValorInicial - FTotalDesconto + FTotalAcrescimo;
end;

end.

