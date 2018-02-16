{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2005 Anderson Rogerio Bejatto               }
{                                                                              }
{ Colaboradores nesse arquivo:          Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 19/09/2011: Régys Borges da Silveira
|*  - Primeira Versao ACBrCargaBal
******************************************************************************}

unit ACBrCargaBal;

{$I ACBr.inc}

interface

uses
  ACBrBase, ACBrUtil,
  SysUtils, Classes, Contnrs;

type
  EACBrCargaBal = class(Exception);
  TACBrCargaBalTipoVenda = (tpvPeso, tpvUnidade, tpvEAN13);
  TACBrCargaBalModelo = (modFilizola, modToledo, modUrano, modUranoS, modToledoMGV5, modToledoMGV6, modUranoURF32);
  TACBrCargaBalProgresso = procedure(Mensagem: String; ProgressoAtual, ProgressoTotal: Integer) of object;
  TACBrCargaBalTipoValidade = (tpvDias, tpvMeses);

  // nutricional
  TACBrCargaBalNutriUndPorcao = (tpGramas, tpMililitros, tpUnidades);
  TACBrCargaBalNutriPartdecimal = (tpPara0, tpPara14, tpPara13, tpPara12, tpPara23, tpPara34);
  TACBrCargaBalNutriMedCaseira = (tpColherSopa, tpColherCafe, tpColherCha, tpXicara, tpDeXicara, tpUnidade, tpPacote, tpFatia,
                                  tpFatiaFina, tpPedaco, tpFolha, tpPao, tpBiscoito, tpBisnaguinha, tpDisco, tpCopo, tpPorcao,
                                  tpTablete, tpSache, tpAlmodega, tpBife, tpFile, tpConcha, tpBala, tpPratoFundo, tpPitada, tpLata);


  TACBrCargaBalSetor = class
  private
    FCodigo: Integer;
    FDescricao: String;
  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
  end;

  TACBrCargaBalNutricional = class
  private
    fCodigo: Integer;
    FDescricao : String;
    FQtd: Integer;
    FUndPorcao: TACBrCargaBalNutriUndPorcao;
    FPartInteira: Integer;
    FPartDecimal: TACBrCargaBalNutriPartdecimal;
    FMedCaseira: TACBrCargaBalNutriMedCaseira;
    FValorEnergetico: Integer;
    FCarboidrato: Currency;
    FProteina: Currency;
    FGorduraTotal: Currency;
    FGorduraSaturada: Currency;
    FGorduraTrans: Currency;
    FFibra: Currency;
    FSodio: Currency;

  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Qtd: Integer read FQtd write FQtd;
    property UndPorcao: TACBrCargaBalNutriUndPorcao read FUndPorcao write FUndPorcao;
    property PartInteira: Integer read FPartInteira write FPartInteira;
    property PartDecimal: TACBrCargaBalNutriPartdecimal read FPartDecimal write FPartDecimal;
    property MedCaseira: TACBrCargaBalNutriMedCaseira read FMedCaseira write FMedCaseira;
    property ValorEnergetico: Integer read FValorEnergetico write FValorEnergetico;
    property Carboidrato: Currency read FCarboidrato write FCarboidrato;
    property Proteina: Currency read FProteina write FProteina;
    property GorduraTotal: Currency read FGorduraTotal write FGorduraTotal;
    property GorduraSaturada: Currency read FGorduraSaturada write FGorduraSaturada;
    property GorduraTrans: Currency read FGorduraTrans write FGorduraTrans;
    property Fibra: Currency read FFibra write FFibra;
    property Sodio: Currency read FSodio write FSodio;
  end;


//Taras kleberson
  TACBrCargaBalTaras = class
  private
    fCodigo: Integer;
    FDescricao : String;
    FValor : Currency;

  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Valor: Currency read FValor write FValor;
  end;

  TACBrCargaBalFornecedor = class
  private
    FCodigo: Smallint;
    FObservacao : String; // 100 Bytes
    FDescricao1 : String; // 56 Bytes
    FDescricao2 : String; // 56 Bytes
    FDescricao3 : String; // 56 Bytes
    FDescricao4 : String; // 56 Bytes
    FDescricao5 : String; // 56 Bytes
  public
    constructor Create;
    property Codigo: Smallint read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Descricao1: String read FDescricao1 write FDescricao1;
    property Descricao2: String read FDescricao2 write FDescricao2;
    property Descricao3: String read FDescricao3 write FDescricao3;
    property Descricao4: String read FDescricao4 write FDescricao4;
    property Descricao5: String read FDescricao5 write FDescricao5;
  end;


  TACBrCargaBalItem = class
  private
    FTecla: Integer;
    FReceita: String;
    FValorVenda: Currency;
    FModeloEtiqueta: Smallint;
    FDescricao: String;
    FCodigo: Integer;
    FTipo: TACBrCargaBalTipoVenda;
    FValidade: Smallint;
    FTipoValidade: TACBrCargaBalTipoValidade;
    FSetor: TACBrCargaBalSetor;
    FNutricional: TACBrCargaBalNutricional;
    FTara: TACBrCargaBalTaras;
    FFornecedor: TACBrCargaBalFornecedor;
    FCodigoTexto1: Integer;
    FCodigoTexto2: Integer;
    FCodigoTexto3: Integer;
    FCodigoInfoNutr: Integer;
    FCodigoTara: Integer;
    FCodigoFornecedor: Smallint;
  Public
    constructor Create;
    destructor Destroy; override;
    property Setor: TACBrCargaBalSetor read FSetor write FSetor;
    property ModeloEtiqueta: Smallint read FModeloEtiqueta write FModeloEtiqueta;
    property Tipo: TACBrCargaBalTipoVenda read FTipo write FTipo;
    property TipoValidade: TACBrCargaBalTipoValidade read FTipoValidade write FTipoValidade;
    property Codigo: Integer read FCodigo write FCodigo;
    property ValorVenda: Currency read FValorVenda write FValorVenda;
    property Validade: Smallint read FValidade write FValidade;
    property Descricao: String read FDescricao write FDescricao;
    property Receita: String read FReceita write FReceita;
    property Tecla: Integer read FTecla write FTecla;
    property Nutricional: TACBrCargaBalNutricional Read FNutricional Write FNutricional;
    property Tara: TACBrCargaBalTaras Read FTara Write FTara;
    property Fornecedor: TACBrCargaBalFornecedor Read FFornecedor Write FFornecedor;
    property CodigoTexto1: Integer read FCodigoTexto1 write FCodigoTexto1;
    property CodigoTexto2: Integer read FCodigoTexto2 write FCodigoTexto2;
    property CodigoTexto3: Integer read FCodigoTexto3 write FCodigoTexto3;
    property CodigoInfoNutr: Integer read FCodigoInfoNutr write FCodigoInfoNutr;
    property CodigoTara: Integer Read FCodigoTara Write FCodigoTara Default 0;
    property CodigoFornecedor: Smallint Read FCodigoFornecedor Write FCodigoFornecedor Default 0;
  end;

  TACBrCargaBalItens = class(TObjectList)
  private
    function GetItem(Index: Integer): TACBrCargaBalItem;
    procedure SetItem(Index: Integer; const Value: TACBrCargaBalItem);
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Clear; override;
    function New: TACBrCargaBalItem;
    property Items[Index: Integer]: TACBrCargaBalItem read GetItem write SetItem; Default;
  end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrCargaBal = class( TACBrComponent )
  private
    FOnProgresso: TACBrCargaBalProgresso;
    FProdutos: TACBrCargaBalItens;
    FModelo: TACBrCargaBalModelo;
    procedure Progresso(const AMensagem: String; const AContAtual, AContTotal: Integer);

    function RFill(Str: string; Tamanho: Integer = 0; Caracter: Char = ' '): string; overload;
    function LFIll(Str: string; Tamanho: Integer = 0; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Currency; Tamanho: Integer; Decimais: Integer = 2; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Integer; Tamanho: Integer; Caracter: Char = '0'): string; overload;


    function GetNomeArquivoNutricional: String;
    function GetNomeArquivoProduto: String;
    function GetNomeArquivoReceita: String;
    function GetNomeArquivoSetor: String;
    function GetNomeArquivoTaras: String;
    function GetNomeArquivoFornecedor: String;

    function GetNomeArquivoRelacaoProdutoNutricional: String;
    function GetNomeArquivoRelacaoProdutoReceita: String;

    function GetTipoProdutoFilizola(Tipo: TACBrCargaBalTipoVenda): String;
    function GetTipoProdutoToledo(Tipo: TACBrCargaBalTipoVenda): String;
    function GetTipoProdutoUrano(Tipo: TACBrCargaBalTipoVenda): string;
    function GetTipoProdutoUranoURF32(Tipo: TACBrCargaBalTipoVenda): string;
    function CalcularSoma(const xStr: string): integer;
    function GetModeloStr: string;

    function GetTipoValidadeProdutoUranoURF32(Tipo: TACBrCargaBalTipoValidade): string;

    procedure PreencherFilizola(Arquivo, Setor, Nutricional, Receita: TStringList);
    procedure PreencherToledo(Arquivo, Nutricional, Receita, Tara, Fornecedor, Setor: TStringList; Versao: integer = 0);
    procedure PreencherUrano(Arquivo: TStringList);
    procedure PreencherUranoS(Arquivo: TStringList);
    procedure PreencherUranoURF32(Arquivo, Nutricional, Receita, RelacaoProdutoNutricional, RelacaoProdutoReceita: TStringList);

    function GetNutriUndPorcaoToledo(Tipo: TACBrCargaBalNutriUndPorcao): String;
    function GetNutriPartDecimalToledo(Tipo: TACBrCargaBalNutriPartdecimal): String;
    function GetNutriMedCaseiraToledo(Tipo: TACBrCargaBalNutriMedCaseira): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GerarArquivos(const ADiretorio: String);
  published
    property Modelo: TACBrCargaBalModelo read FModelo write FModelo;
    property ModeloStr: String read GetModeloStr;
    property Produtos: TACBrCargaBalItens read FProdutos write FProdutos;
    property OnProgresso: TACBrCargaBalProgresso read FOnProgresso write FOnProgresso;
  end;

implementation

uses
  ACBrConsts;

{ TACBrCargaBalSetor }
constructor TACBrCargaBalSetor.Create;
begin
  FCodigo    := 0;
  FDescricao := EmptyStr;
end;

{ TACBrCargaBalNutricional }
constructor TACBrCargaBalNutricional.Create;
begin
  fCodigo := 0;
  FQtd    := 0;
  FUndPorcao := tpGramas;
  FPartInteira := 0;
  FPartDecimal := tpPara0;
  FMedCaseira := tpColherSopa;
  FValorEnergetico := 0;
  FCarboidrato := 0;
  FProteina := 0;
  FGorduraTotal := 0;
  FGorduraSaturada := 0;
  FGorduraTrans := 0;
  FFibra := 0;
  FSodio := 0;
end;


{TACBrCargaBalTaras}
constructor TACBrCargaBalTaras.Create;
begin
  fCodigo:= 0;
  FValor := 0;
end;


{ TACBrCargaBalItem }
constructor TACBrCargaBalItem.Create;
begin
  // Criacao da propriedade Setor
  FSetor := TACBrCargaBalSetor.Create;

  // Iniciar os campos de valores
  FCodigo          := 0;
  FDescricao       := EmptyStr;
  FTipo            := tpvPeso;
  FValorVenda      := 0.00;
  FModeloEtiqueta  := 0;
  FReceita         := EmptyStr;
  FValidade        := 0;

  FNutricional := TACBrCargaBalNutricional.Create;
  FTara := TACBrCargaBalTaras.Create;
  FFornecedor := TACBrCargaBalFornecedor.Create;

  FCodigoTexto1    := 0;
  FCodigoTexto2    := 0;
  FCodigoTexto3    := 0;
  FCodigoInfoNutr  := 0;
end;

destructor TACBrCargaBalItem.Destroy;
begin
  FSetor.Free;
  FNutricional.Free;
  FTara.Free;
  inherited;
end;

{ TACBrCargaBalItens }

procedure TACBrCargaBalItens.Clear;
begin
  inherited Clear;
end;

constructor TACBrCargaBalItens.create;
begin
  inherited Create(True);
end;

destructor TACBrCargaBalItens.destroy;
begin
  Clear;
  inherited Destroy;
end;

function TACBrCargaBalItens.GetItem(Index: Integer): TACBrCargaBalItem;
begin
  Result := TACBrCargaBalItem(inherited Items[Index]);
end;

function TACBrCargaBalItens.New: TACBrCargaBalItem;
begin
  Result := TACBrCargaBalItem.Create;
  Add(Result);
end;

procedure TACBrCargaBalItens.SetItem(Index: Integer;
  const Value: TACBrCargaBalItem);
begin
  Put(Index, Value);
end;

{ TACBrCargaBal }
constructor TACBrCargaBal.Create(AOwner: TComponent);
begin
  inherited;
  FProdutos := TACBrCargaBalItens.Create;
end;

destructor TACBrCargaBal.Destroy;
begin
  FProdutos.Free;
  inherited;
end;

function TACBrCargaBal.RFill(Str: string; Tamanho: Integer = 0;
  Caracter: Char = ' '): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := Str + StringOfChar(Caracter, Tamanho - Length(Str));
end;

function TACBrCargaBal.LFill(Str: string; Tamanho: Integer = 0;
  Caracter: Char = '0'): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := StringOfChar(Caracter, Tamanho - length(Str)) + Str;
end;

function TACBrCargaBal.LFill(Valor: Currency; Tamanho: Integer;
  Decimais: Integer = 2; Caracter: Char = '0'): string;
var
  i, p: Integer;
begin
  p := 1;

  for i := 1 to Decimais do
    p := p * 10;

  Result := LFill(Trunc(Valor * p), Tamanho, Caracter);
end;

function TACBrCargaBal.LFill(Valor: Integer; Tamanho: Integer;
  Caracter: Char = '0'): string;
begin
  Result := LFill(IntToStr(Valor), Tamanho, Caracter);
end;

function TACBrCargaBal.GetNomeArquivoTaras: String;
begin
  case FModelo of
    modToledo,
    modToledoMGV5, modToledoMGV6 : Result := 'TARA.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoProduto: String;
begin
  case FModelo of
    modFilizola   : Result := 'CADTXT.TXT';
    modToledo     : Result := 'TXITENS.TXT';
    modUrano      : Result := 'PRODUTOS.TXT';
    modUranoS     : Result := 'PRODUTOS.TXT';
    modToledoMGV5,
    modToledoMGV6 : Result := 'ITENSMGV.TXT';
    modUranoURF32 : Result := 'PRODUTOS.TXT';
  end;
end;

function TACBrCargaBal.GetNutriUndPorcaoToledo(Tipo: TACBrCargaBalNutriUndPorcao): String;
begin
   case Tipo of
    tpGramas     : Result := '0';
    tpMililitros : Result := '1';
    tpUnidades   : Result := '2';
   end;
end;

function TACBrCargaBal.GetNutriPartDecimalToledo(Tipo: TACBrCargaBalNutriPartdecimal): String;
begin
   case Tipo of
    tpPara0  : Result := '0';
    tpPara14 : Result := '1';
    tpPara13 : Result := '2';
    tpPara12 : Result := '3';
    tpPara23 : Result := '4';
    tpPara34 : Result := '5';
   end;
end;


function TACBrCargaBal.GetTipoProdutoToledo(Tipo: TACBrCargaBalTipoVenda): String;
begin
  case Tipo of
    tpvPeso    : Result := '0';
    tpvUnidade : Result := '1';
    tpvEAN13   : Result := '2';
  end;
end;

function TACBrCargaBal.GetNutriMedCaseiraToledo(Tipo: TACBrCargaBalNutriMedCaseira): String;
begin
  case tipo of
    tpColherSopa  : Result := '00';
    tpColherCafe  : Result := '01';
    tpColherCha   : Result := '02';
    tpXicara      : Result := '03';
    tpDeXicara    : Result := '04';
    tpUnidade     : Result := '05';
    tpPacote      : Result := '06';
    tpFatia       : Result := '07';
    tpFatiaFina   : Result := '08';
    tpPedaco      : Result := '09';
    tpFolha       : Result := '10';
    tpPao         : Result := '11';
    tpBiscoito    : Result := '12';
    tpBisnaguinha : Result := '13';
    tpDisco       : Result := '14';
    tpCopo        : Result := '15';
    tpPorcao      : Result := '16';
    tpTablete     : Result := '17';
    tpSache       : Result := '18';
    tpAlmodega    : Result := '19';
    tpBife        : Result := '20';
    tpFile        : Result := '21';
    tpConcha      : Result := '22';
    tpBala        : Result := '23';
    tpPratoFundo  : Result := '24';
    tpPitada      : Result := '25';
    tpLata        : Result := '26';
    end;
end;

function TACBrCargaBal.GetTipoProdutoFilizola(Tipo: TACBrCargaBalTipoVenda): String;
begin
  case Tipo of
    tpvPeso    : Result := 'P';
    tpvUnidade : Result := 'U';
  end;
end;

function TACBrCargaBal.GetTipoProdutoUrano(Tipo: TACBrCargaBalTipoVenda): string;
begin
  case Tipo of
    tpvPeso    : Result:='P';
    tpvUnidade : Result:='U';
  end;
end;

function TACBrCargaBal.GetTipoProdutoUranoURF32(Tipo: TACBrCargaBalTipoVenda): string;
begin
  case Tipo of
    tpvPeso    : Result:='0';
    tpvUnidade : Result:='6';
  end;
end;

function TACBrCargaBal.GetNomeArquivoSetor: String;
begin
  // Toledo e Urano nao possuem arquivo de setor a parte
  case FModelo of
    modFilizola : Result := 'SETORTXT.TXT';
    modToledoMGV5,
    modToledoMGV6 : Result := 'DEPTO.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoReceita: String;
begin
  // Urano nao possue arquivo de Receita a parte. EXCETO URANO URF32
  case FModelo of
    modFilizola : Result := 'REC_ASS.TXT';
    modToledoMGV5,
    modToledoMGV6: Result := 'TXINFO.TXT';
    modUranoURF32: Result := 'RECEITAS.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoFornecedor: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6 : Result := 'TXFORN.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoNutricional: String;
begin
  // A urano nao possuem arquivo nutricional a parte das informações
  // são incluídas no mesmo arquivo de itens.
  case FModelo of
    modToledoMGV5,
    modToledoMGV6 : Result := 'INFNUTRI.TXT';
    modFilizola : Result := 'NUTRI.TXT';
    modUranoURF32: Result := 'INFORMACOESNUTRICIONAIS.TXT';
  end;
end;

function TACBrCargaBal.CalcularSoma(const xStr: string): Integer;
var
  I, Vl: Integer;
begin
  result:=0;
  Vl:=0;
  if Length(xStr)<1 then
    exit;
  for I:=1 to Length(xStr) do
  begin
    Vl:=Vl+Ord(xStr[I]);
  end;
  result:=Vl;
end;


procedure TACBrCargaBal.PreencherFilizola(Arquivo, Setor, Nutricional, Receita: TStringList);
var
  i, Total: Integer;
  areceita:string;
begin
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    Arquivo.Add(
      LFIll(Produtos[i].Codigo, 6) +
      GetTipoProdutoFilizola(Produtos[i].Tipo) +
      RFIll(Produtos[i].Descricao, 22) +
      LFIll(Produtos[i].ValorVenda, 7, 2) +
      LFIll(Produtos[i].Validade, 3)
    );

    Setor.Add(
      RFill(Produtos[i].Setor.Descricao, 12) +
      LFIll(Produtos[i].Codigo, 6) +
      LFIll(i + 1, 4) +
      LFill(Produtos[i].Tecla, 3)
    );

    Nutricional.Add(LFIll(Produtos[i].Codigo,6) +
     RFill(Produtos[i].Nutricional.Descricao,35) +
     LFIll(Produtos[i].Nutricional.ValorEnergetico,5) +
     LFIll(0,4) +
     LFIll(Produtos[i].Nutricional.Carboidrato,5,1) +
     LFIll(0,4) +
     LFIll(Produtos[i].Nutricional.Proteina,5,1) +
     LFIll(0,4) +
     LFIll(Produtos[i].Nutricional.GorduraTotal,5,1) +
     LFIll(0,4) +
     LFIll(Produtos[i].Nutricional.GorduraSaturada,5,1)+
     LFIll(0,4)+
     LFIll(Produtos[i].Nutricional.GorduraTrans,5,1) +
     LFIll(0,4)+
     LFIll(Produtos[i].Nutricional.Fibra,5,1)+
     LFIll(0,4)+
     RFill('*****',5)+
     RFill('****',4) +
     RFill('*****',5) +
     RFill('****',4) +
     LFIll(Produtos[i].Nutricional.Sodio,5,1)+
     LFIll(0,4)

     );

    // receita
    areceita:=RFill(' ',12)+LFIll(Produtos[i].Codigo,6)+LFIll(Produtos[i].Codigo,6)+RFill(Produtos[i].Receita,840)+'@';
    if (Length(Produtos[i].Receita)>2) and (Receita.IndexOf(areceita)<0) then
       Receita.Add(areceita);

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherToledo(Arquivo, Nutricional, Receita, Tara, Fornecedor, Setor: TStringList; versao:integer);
var
  i, Total: Integer;
  ANutri, AReceita, ATara, AFornecedor, ASetor: string;
begin
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    if Versao=0 then
    begin
      Arquivo.Add(
        LFIll(Produtos[i].Setor.Codigo, 2) +
        LFIll(Produtos[i].ModeloEtiqueta, 2) +
        GetTipoProdutoToledo(Produtos[i].Tipo) +
        LFIll(Produtos[i].Codigo, 6) +
        LFIll(Produtos[i].ValorVenda, 6, 2) +
        LFIll(Produtos[i].Validade, 3) +
        RFIll(Produtos[i].Descricao, 50) +
        RFIll(Produtos[i].Receita, 250)
      );
    end else
    begin
      if Modelo = modToledoMGV5 then
      begin
        // ITENSMGV.TXT - VERSÃO 2
        Arquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          LFIll(Produtos[i].Codigo, 6)+ // codigo inf extra
          LFIll('0', 4)+ // codigo imagem
          LFIll(Produtos[i].Nutricional.Codigo,6)+ // codigo inf nutricional
          RFill('1', 1)+ // imprime data de validade
          RFill('1', 1)+ // imprime data embalagem
          LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
          //LFIll('0', 4)+ // codigo fornecedor
          lFill('0', 12)+ // lote
          lFill('0', 11)+ // codigo especial
          LFIll('0', 1)+ // versao do preco
          LFIll('0', 4)+ // codigo do som
          LFIll(IntToStr(Produtos[i].CodigoTara),4)+ // codigo da tara
          //LFIll('0', 4)+ // codigo da tara
          LFIll('0', 4)+ // codigo da fracionador
          LFIll('0', 4)+ // Código do Campo Extra 1
          LFIll('0', 4)+ // Código do Campo Extra 2
          LFIll('0', 4)+ // Código da Conservação
          LFIll('0', 12) // EAN-13, quando utilizado Tipo de Produto EAN-13
        );
      end else
      begin
        // ITENSMGV.TXT - VERSÃO 3
        Arquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          LFIll(Produtos[i].Codigo, 6)+ // codigo inf extra
          LFIll('0', 4)+ // codigo imagem
          LFIll(Produtos[i].Nutricional.Codigo,6)+ // codigo inf nutricional
          RFill('1', 1)+ // imprime data de validade
          RFill('1', 1)+ // imprime data embalagem
          LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
          //LFIll('0', 4)+ // codigo fornecedor
          lFill('0', 12)+ // lote
          lFill('0', 11)+ // codigo especial
          LFIll('0', 1)+ // versao do preco
          LFIll('0', 4)+ // codigo do som
          LFIll(IntToStr(Produtos[i].CodigoTara),4)+ // codigo da tara
          //LFIll('0', 4)+ // codigo da tara
          LFIll('0', 4)+ // codigo da fracionador
          LFIll('0', 4)+ // Código do Campo Extra 1
          LFIll('0', 4)+ // Código do Campo Extra 2
          LFIll('0', 4)+ // Código da Conservação
          LFIll('0', 12) // EAN-13, quando utilizado Tipo de Produto EAN-13

          (* Novos campos para o arquivo do MGV6 - De acordo com a observação
            eles podem estar inexistentes no arquivo, pois serão considerados como não associados.*)

          {LFIll('0', 6)+ // Percentual de Glaciamento
          LFIll('0', 2)+ // Sequencia de departamentos Associados. Ex: Para associar departamentos 2 e 5: |0205|
          LFIll('', 35)+ // Descritivo do Item – Terceira Linha
          LFIll('', 35)+ // Descritivo do Item – Quarta Linha
          LFIll('0', 4)+ // Código do Campo Extra 3
          LFIll('0', 4)+ // Código do Campo Extra 4
          LFIll('0', 6) // Código da mídia (Prix 6 Touch)}
        );
      end;

      // receita
      AReceita:=LFIll(Produtos[i].Codigo, 6) + RFill('', 100) + RFill(Produtos[i].Receita, 840);
      if (Length(Produtos[i].Receita) > 2) and (Receita.IndexOf(AReceita) < 0) then
         Receita.Add(AReceita);

      ANutri:= 'N'+ LFIll(Produtos[i].Nutricional.Codigo, 6) +
      '0' +
      LFIll(Produtos[i].Nutricional.Qtd, 3) +
      GetNutriUndPorcaoToledo(Produtos[i].Nutricional.UndPorcao) +
      LFIll(Produtos[i].Nutricional.PartInteira, 2) +
      GetNutriPartDecimalToledo(Produtos[i].Nutricional.PartDecimal) +
      GetNutriMedCaseiraToledo(Produtos[i].Nutricional.MedCaseira) +
      LFIll(Produtos[i].Nutricional.ValorEnergetico, 4) +
      LFIll(Produtos[i].Nutricional.Carboidrato, 4, 1) +
      LFIll(Produtos[i].Nutricional.Proteina, 3, 1) +
      LFIll(Produtos[i].Nutricional.GorduraTotal, 3, 1) +
      LFIll(Produtos[i].Nutricional.GorduraSaturada, 3, 1) +
      LFIll(Produtos[i].Nutricional.GorduraTrans, 3, 1) +
      LFIll(Produtos[i].Nutricional.Fibra, 3, 1) +
      LFIll(Produtos[i].Nutricional.Sodio, 5, 1);

      if (Produtos[i].Nutricional.Codigo > 0) and (Nutricional.IndexOf(ANutri) < 0) then
         Nutricional.Add(ANutri);


      ATara:= LFIll(Produtos[i].Tara.Codigo, 4) + LFIll(Produtos[i].Tara.Valor, 6, 3)+
              RFIll(Produtos[i].Tara.Descricao, 20);

      if (Produtos[i].Tara.Codigo > 0) and (Tara.IndexOf(ATara) < 0) THEN
         Tara.Add(ATara);

      AFornecedor := LFIll(Produtos[i].Fornecedor.Codigo, 4) + RFIll(Produtos[i].Fornecedor.Observacao, 100) +
        RFill(Produtos[i].Fornecedor.Descricao1, 56) +
        RFill(Produtos[i].Fornecedor.Descricao2, 56) +
        RFill(Produtos[i].Fornecedor.Descricao3, 56) +
        RFill(Produtos[i].Fornecedor.Descricao4, 56) +
        RFill(Produtos[i].Fornecedor.Descricao5, 56);

      if ((Produtos[i].Fornecedor.Codigo > 0) and (Fornecedor.IndexOf(AFornecedor) < 0))then
        Fornecedor.Add(AFornecedor);

      ASetor := LFIll(Produtos[i].Setor.Codigo, 2) + RFIll(Produtos[i].Setor.Descricao, 40);

      if ((Produtos[i].Setor.Codigo > 0) and (Setor.IndexOf(ASetor) < 0)) then
        Setor.Add(ASetor);
    end;
    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;

end;

procedure TACBrCargaBal.PreencherUrano(Arquivo: TStringList);
var
  i, Total, xtam: Integer;
  xnutric: string;
begin
  //modelo do arquivo: serve somente para as novas balanças urano (linha top e topmax)
  //0x10+0x02+codigo[5]+pesagem[35]+chksum[4]+0x03+0x13+0x10
                //      pesagem[35]=tipoproduto[1]+descricao[20]+preco[9]+validade[4]+tipovalidade[1]

  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    //linha do produto
    Arquivo.Add(#10#02 +
      LFIll(Produtos[i].Codigo, 5) +
      GetTipoProdutoUrano(Produtos[i].Tipo) +
      RFIll(Produtos[i].Descricao, 20) +
      FormatCurr('000000.00', Produtos[i].ValorVenda) +
      LFIll(Produtos[i].Validade, 4) +
      'D'
    );
    xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
    Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;

    if Produtos[i].Nutricional.FQtd>0 then
    begin
      //linha da informação nutricional
      //0x11+0x02+codigo[5]+pesagem[35]+informacoes nutricionais[258]+chksum[4]+0x03+0x13+0x10
                                      //informacoes nutricionais[258]=1 linha de 41 caracteres para porção
                                      // e 8 linhas de 21 caracteres para [calorias,carboidratos,proteínas,gorduras totais,
                                      //                                   gorduras saturadas,gordura trans,fibra alimentar,
                                      //                                   sódio].

      Arquivo.Add(#11#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        RFIll('' {Produtos[i].Nutricional.FUndPorcao}, 209)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    if Produtos[i].Receita <> '' then
    begin
    //linha da receita
    //0x12+0x02+codigo[5]+pesagem[35]+informacoes adicionais[615]+chksum[4]+0x03+0x13+0x10
                                     //informacoes adicionais[615]=15 linhas de 41 caracteres.
      xnutric:='';

      if Produtos[i].Nutricional.FQtd<=0 then
        xnutric := RFill('', 209);

      Arquivo.Add(#12#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        xnutric +
        RFIll(Produtos[i].Receita, 615)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherUranoS(Arquivo: TStringList);
var
  i, Total, xtam: Integer;
  xnutric: string;
  inform : string;
begin
  //modelo do arquivo: serve somente para as novas balanças urano (linha top e topmax)
  //0x10+0x02+codigo[5]+pesagem[35]+chksum[4]+0x03+0x13+0x10
                //      pesagem[35]=tipoproduto[1]+descricao[20]+preco[9]+validade[4]+tipovalidade[1]

  DecimalSeparator := '.';
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    //linha do produto
    inform := LFIll(Produtos[i].Codigo, 6) +
    GetTipoProdutoUrano(Produtos[i].Tipo) +
    RFIll(Produtos[i].Descricao, 30) +
    FormatCurr('000000.00', Produtos[i].ValorVenda) +
    LFIll(Produtos[i].Validade, 4) + 'D0000                            ';
    xtam := CalcularSoma(inform);
    Arquivo.Add( ^P + ^B + inform + LowerCase(IntToHex(xtam, 4)) + ^C);

    if Produtos[i].Nutricional.FQtd>0 then
    begin
      //linha da informação nutricional
      //0x11+0x02+codigo[5]+pesagem[35]+informacoes nutricionais[258]+chksum[4]+0x03+0x13+0x10
                                      //informacoes nutricionais[258]=1 linha de 41 caracteres para porção
                                      // e 8 linhas de 21 caracteres para [calorias,carboidratos,proteínas,gorduras totais,
                                      //                                   gorduras saturadas,gordura trans,fibra alimentar,
                                      //                                   sódio].

      Arquivo.Add(#11#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        RFIll('' {Produtos[i].Nutricional.FUndPorcao}, 209)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    if Produtos[i].Receita <> '' then
    begin
    //linha da receita
    //0x12+0x02+codigo[5]+pesagem[35]+informacoes adicionais[615]+chksum[4]+0x03+0x13+0x10
                                    //informacoes adicionais[615]=15 linhas de 41 caracteres.
      xnutric:='';
      

      if Produtos[i].Nutricional.FQtd<=0 then
        xnutric := RFill('', 209);

      Arquivo.Add(#12#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        xnutric +
        RFIll(Produtos[i].Receita, 615)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherUranoURF32(Arquivo, Nutricional, Receita, RelacaoProdutoNutricional, RelacaoProdutoReceita: TStringList);
var
  i, iTotal: Integer;
  sReceita:string;
  function TrocaPontoPorVirgula(sString: String): String;
  var
    iPos :Integer;
    sRetorno :String;
  begin
    sRetorno := '';
    for iPos := 1 to Length(sString) do
      begin
        if sString[iPos] <> '.' then
          sRetorno := sRetorno + sString[iPos]
        else
          sRetorno := sRetorno + ',';
      end;
     Result := sRetorno;
  end;

  function ValorComVirgula(nValor:Currency):string;
  var
    sTemp:string;
  begin
    sTemp := LFIll(FormatFloat('###,###,##0.00', nValor), 9, ' ');
    Result := TrocaPontoPorVirgula(sTemp);
  end;

begin
  iTotal := Produtos.Count;

  for i := 0 to iTotal - 1 do
    begin
      Arquivo.Add(LFIll(Produtos[i].Codigo, 6, ' ') +          // Código
                  '*' +                                        // Flag * para transmitir
                  GetTipoProdutoUranoURF32(Produtos[i].Tipo) + // Tipo
                  RFIll(Produtos[i].Descricao, 20, ' ') +      // Nome
                  ValorComVirgula(Produtos[i].ValorVenda) +    // Preço
                  LFIll(Produtos[i].Validade, 5, ' ') +        // Validade
                  LFIll(GetTipoValidadeProdutoUranoURF32(Produtos[i].TipoValidade), 1)   // Validade 2 - Indica se é M=Meses ou D=Dias
                  );

      // Informações Nutricionais
      Nutricional.Add(LFIll(Produtos[i].Codigo,5, ' ') +
                      LFIll(Produtos[i].Nutricional.Qtd,6) +
                      LFIll(Produtos[i].Nutricional.ValorEnergetico,5) +
                      LFIll(Produtos[i].Nutricional.Carboidrato,6,1) +
                      LFIll(Produtos[i].Nutricional.Proteina,6,1) +
                      LFIll(Produtos[i].Nutricional.GorduraTotal,6,1) +
                      LFIll(Produtos[i].Nutricional.GorduraSaturada,6,1)+
                      LFIll(Produtos[i].Nutricional.GorduraTrans,6,1) +
                      LFIll(Produtos[i].Nutricional.Fibra,6,1)+
                      LFIll(Produtos[i].Nutricional.Sodio,5)
                      );

      if Produtos[i].Nutricional.Qtd > 0 then
        begin
          RelacaoProdutoNutricional.Add(LFIll(Produtos[i].Codigo,6, ' ') +
                                        LFIll(Produtos[i].Codigo,5, ' ')
                                        );
        end;

      // Receita
      sReceita := LFIll(Produtos[i].Codigo,4) +
                  ' ' + // Flag * para transmitir
                  RFill(Produtos[i].Receita,254);

      if (Length(Produtos[i].Receita) > 2) and (Receita.IndexOf(sReceita) < 0) then
        begin
          Receita.Add(sReceita);
          RelacaoProdutoReceita.Add(LFIll(Produtos[i].Codigo,6, ' ') +
                                    LFIll(Produtos[i].Codigo,5, ' ')
                                        );

        end;

      Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, iTotal);
    end;
end;

procedure TACBrCargaBal.Progresso(const AMensagem: String; const AContAtual,
  AContTotal: Integer);
begin
  if Assigned(FOnProgresso) then
    FOnProgresso(AMensagem, AContAtual, AContTotal);
end;

procedure TACBrCargaBal.GerarArquivos(const ADiretorio: String);
var
  Produto, Setor, Receita, Nutricional, Tara, Fornecedor: TStringList;
  RelacaoProdutoNutricional, RelacaoProdutoReceita: TStringList;
  NomeArquivo: TFileName;
  Total: integer;
begin
  if Trim(ADiretorio) = EmptyStr then
    raise EACBrCargaBal.Create(ACBrStr('Informe o diretório onde serão gerados os arquivos de carga!'));

  if not DirectoryExists(ADiretorio) then
    raise EACBrCargaBal.Create(ACBrStr('Diretorio informado não existe!'));

  if Self.Produtos.Count = 0 then
    raise EACBrCargaBal.Create(ACBrStr('Não foram informados os produtos para a geração!'));

  Produto := TStringList.Create;
  Produto.Clear;

  Setor := TStringList.Create;
  Setor.Clear;

  Receita := TStringList.Create;
  Receita.Clear;

  Tara := TStringList.Create;
  Tara.Clear;

  Nutricional := TStringList.Create;
  Nutricional.Clear;

  RelacaoProdutoNutricional := TStringList.Create;
  RelacaoProdutoNutricional.Clear;

  RelacaoProdutoReceita := TStringList.Create;
  RelacaoProdutoReceita.Clear;

  Fornecedor := TStringList.Create;
  Fornecedor.Clear;

  try
    Total := Self.Produtos.Count;
    Progresso(ACBrStr('Iniciando a geração dos arquivos'), 0, Total);

    // Varre os registros gerando o arquivo em lista
    case FModelo of
      modFilizola   : PreencherFilizola(Produto, Setor, Nutricional, Receita);
      modToledo     : PreencherToledo(Produto, Nutricional, Receita, Tara, nil, nil);
      modUrano      : PreencherUrano(Produto);
      modUranoS     : PreencherUranoS(Produto);
      modToledoMGV5,
      modToledoMGV6 : PreencherToledo(Produto, Nutricional, Receita, Tara, Fornecedor, Setor, 1);
      modUranoURF32 : PreencherUranoURF32(Produto, Nutricional, Receita, RelacaoProdutoNutricional, RelacaoProdutoReceita);
    end;

    // Monta o nome do arquivo de produtos seguindo o padrao da balanca
    if Produto.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoProduto;
      Produto.SaveToFile(NomeArquivo);
    end;

    // Gerar arquivo de setores se houverem dados e o arquivo for separado
    if Setor.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoSetor;
      Setor.SaveToFile(NomeArquivo);
    end;

    // Gerar arquivo de receitas se houverem dados e o arquivo for separado
    if Receita.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoReceita;
      Receita.SaveToFile(NomeArquivo);
    end;

    // Gerar arquivo de Nutricionais se houverem dados e o arquivo for separado
    if Nutricional.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoNutricional;
      Nutricional.SaveToFile(NomeArquivo);
    end;

    // Gerar arquivo de relação entre Produto e Nutricionais se houverem dados e o arquivo for separado
    if RelacaoProdutoNutricional.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoRelacaoProdutoNutricional;
      RelacaoProdutoNutricional.SaveToFile(NomeArquivo);
    end;

    // Gerar arquivo de relção entre Produto e Receita se houverem dados e o arquivo for separado
    if RelacaoProdutoReceita.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoRelacaoProdutoReceita;
      RelacaoProdutoReceita.SaveToFile(NomeArquivo);
    end;

    //Gerar arquivo de taras(peso de embalagens)
    if Tara.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoTaras;
      Tara.SaveToFile(NomeArquivo);
    end;

    //Gerar arquivo de fornecedor
    if Fornecedor.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoFornecedor;
      Fornecedor.SaveToFile(NomeArquivo);
    end;

    Progresso('Terminado', Total, Total);
  finally
    FreeAndNil(Produto);
    FreeAndNil(Setor);
    FreeAndNil(Receita);
    FreeAndNil(Nutricional);    
    FreeAndNil(RelacaoProdutoNutricional);
    FreeAndNil(RelacaoProdutoReceita);
    FreeAndNil(Tara);
    FreeAndNil(Fornecedor);
  end;
end;

function TACBrCargaBal.GetModeloStr: string;
begin
 case fModelo of
   modFilizola   : result := 'Filizola';
   modToledo     : result := 'Toledo';
   modUrano      : result := 'Urano';
   modUranoS     : result := 'Urano (S)';
   modToledoMGV5 : result := 'ToledoMGV5';
   modUranoURF32 : result := 'Urano URF32';
 end;
end;

function TACBrCargaBal.GetTipoValidadeProdutoUranoURF32(
  Tipo: TACBrCargaBalTipoValidade): string;
begin
  case Tipo of
    tpvDias    : Result := 'D';
    tpvMeses   : Result := 'M';
  end;                         
end;

function TACBrCargaBal.GetNomeArquivoRelacaoProdutoNutricional: String;
begin
  case FModelo of
    modUranoURF32: Result := 'PRODUTOSINFORMACOESNUTRICIONAIS.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoRelacaoProdutoReceita: String;
begin
  case FModelo of
    modUranoURF32: Result := 'PRODUTOSRECEITAS.TXT';
  end;
end;

{ TACBrCargaBalFornecedor }

constructor TACBrCargaBalFornecedor.Create;
begin
  FCodigo := 0;
  FObservacao := EmptyStr;
  FDescricao1 := EmptyStr;
  FDescricao2 := EmptyStr;
  FDescricao3 := EmptyStr;
  FDescricao4 := EmptyStr;
  FDescricao5 := EmptyStr;
end;

end.
