{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreGNRE;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pgnreConversao;

type
  TGNRE                     = class;
  TCampoExtraCollection     = class;
  TCampoExtraCollectionItem = class;
  TCampoExtra               = class;
  TReferencia               = class;

  TCampoExtraCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TCampoExtraCollectionItem;
    procedure SetItem(Index: Integer; Value: TCampoExtraCollectionItem);
  public
    function Add: TCampoExtraCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TCampoExtraCollectionItem;
    property Items[Index: Integer]: TCampoExtraCollectionItem read GetItem write SetItem; default;
  end;

  TCampoExtraCollectionItem = class(TObject)
  private
    FCampoExtra: TCampoExtra;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property CampoExtra: TCampoExtra read FCampoExtra write FCampoExtra;
  end;

  TCampoExtra = class(TObject)
  private
    Fcodigo: Integer;
    Ftipo: string;
    Fvalor: string;
  public
    property codigo: Integer read Fcodigo write Fcodigo;
    property tipo: string read Ftipo write Ftipo;
    property valor: string read Fvalor write Fvalor;
  end;

  TReferencia = class(TObject)
  private
    Fperiodo: Integer;
    Fmes: string;
    Fano: Integer;
    Fparcela: Integer;
  public
    property periodo: Integer read Fperiodo write Fperiodo;
    property mes: string read Fmes write Fmes;
    property ano: Integer read Fano write Fano;
    property parcela: Integer read Fparcela write Fparcela;
  end;

  TGNRE = class(TObject)
  private
    Fc01_UfFavorecida: string;
    Fc02_receita: Integer;
    Fc25_detalhamentoReceita: Integer;
    Fc26_produto: Integer;
    Fc27_tipoIdentificacaoEmitente: Integer;
    Fc03_idContribuinteEmitente: string;
    Fc28_tipoDocOrigem: Integer;
    Fc04_docOrigem: string;
    Fc06_valorPrincipal: Currency;
    Fc10_valorTotal: Currency;
    Fc14_dataVencimento: TDateTime;
    Fc15_convenio: string;
    Fc16_razaoSocialEmitente: string;
    Fc17_inscricaoEstadualEmitente: string;
    Fc18_enderecoEmitente: string;
    Fc19_municipioEmitente: string;
    Fc20_ufEnderecoEmitente: string;
    Fc21_cepEmitente: string;
    Fc22_telefoneEmitente: string;
    Fc34_tipoIdentificacaoDestinatario: Integer;
    Fc35_idContribuinteDestinatario: string;
    Fc36_inscricaoEstadualDestinatario: string;
    Fc37_razaoSocialDestinatario: string;
    Fc38_municipioDestinatario: string;
    Fc33_dataPagamento: TDateTime;
    Freferencia: TReferencia;
    FcamposExtras: TCampoExtraCollection;
    Fc42_identificadorGuia: string;
    // usado na versão 2.00
    FtipoGNRE: TTipoGNRE;
    FValorFECP: Currency;
    FTotalFECP: Currency;
    FMultaICMS: Currency;
    FMultaFECP: Currency;
    FJurosICMS: Currency;
    FJurosFECP: Currency;
    FAtualMonetICMS: Currency;
    FAtualMonetFECP: Currency;
    FnumeroControle: String;
  public
    constructor Create;
    destructor Destroy; override;

    property c01_UfFavorecida: string read Fc01_UfFavorecida write Fc01_UfFavorecida;
    property c02_receita: Integer read Fc02_receita write Fc02_receita;
    property c25_detalhamentoReceita: Integer read Fc25_detalhamentoReceita write Fc25_detalhamentoReceita;
    property c26_produto: Integer read Fc26_produto write Fc26_produto;
    property c27_tipoIdentificacaoEmitente: Integer read Fc27_tipoIdentificacaoEmitente write Fc27_tipoIdentificacaoEmitente;
    property c03_idContribuinteEmitente: string read Fc03_idContribuinteEmitente write Fc03_idContribuinteEmitente;
    property c28_tipoDocOrigem: Integer read Fc28_tipoDocOrigem write Fc28_tipoDocOrigem;
    property c04_docOrigem: string read Fc04_docOrigem write Fc04_docOrigem;
    property c06_valorPrincipal: Currency read Fc06_valorPrincipal write Fc06_valorPrincipal;
    property c10_valorTotal: Currency read Fc10_valorTotal write Fc10_valorTotal;
    property c14_dataVencimento: TDateTime read Fc14_dataVencimento write Fc14_dataVencimento;
    property c15_convenio: string read Fc15_convenio write Fc15_convenio;
    property c16_razaoSocialEmitente: string read Fc16_razaoSocialEmitente write Fc16_razaoSocialEmitente;
    property c17_inscricaoEstadualEmitente: string read Fc17_inscricaoEstadualEmitente write Fc17_inscricaoEstadualEmitente;
    property c18_enderecoEmitente: string read Fc18_enderecoEmitente write Fc18_enderecoEmitente;
    property c19_municipioEmitente: string read Fc19_municipioEmitente write Fc19_municipioEmitente;
    property c20_ufEnderecoEmitente: string read Fc20_ufEnderecoEmitente write Fc20_ufEnderecoEmitente;
    property c21_cepEmitente: string read Fc21_cepEmitente write Fc21_cepEmitente;
    property c22_telefoneEmitente: string read Fc22_telefoneEmitente write Fc22_telefoneEmitente;
		property c34_tipoIdentificacaoDestinatario: Integer read Fc34_tipoIdentificacaoDestinatario write Fc34_tipoIdentificacaoDestinatario;
		property c35_idContribuinteDestinatario: string read Fc35_idContribuinteDestinatario write Fc35_idContribuinteDestinatario;
    property c36_inscricaoEstadualDestinatario: string read Fc36_inscricaoEstadualDestinatario write Fc36_inscricaoEstadualDestinatario;
		property c37_razaoSocialDestinatario: string read Fc37_razaoSocialDestinatario write Fc37_razaoSocialDestinatario;
		property c38_municipioDestinatario: string read Fc38_municipioDestinatario write Fc38_municipioDestinatario;
		property c33_dataPagamento: TDateTime read Fc33_dataPagamento write Fc33_dataPagamento;
    property referencia: TReferencia read Freferencia write Freferencia;
    property camposExtras: TCampoExtraCollection read FcamposExtras write FcamposExtras;
    property c42_identificadorGuia: string read Fc42_identificadorGuia write Fc42_identificadorGuia;
    // usado na versão 2.00
    property tipoGNRE: TTipoGNRE read FtipoGNRE write FtipoGNRE;
    property ValorFECP: Currency read FValorFECP write FValorFECP;
    property TotalFECP: Currency read FTotalFECP write FTotalFECP;
    property MultaICMS: Currency read FMultaICMS write FMultaICMS;
    property MultaFECP: Currency read FMultaFECP write FMultaFECP;
    property JurosICMS: Currency read FJurosICMS write FJurosICMS;
    property JurosFECP: Currency read FJurosFECP write FJurosFECP;
    property AtualMonetICMS: Currency read FAtualMonetICMS write FAtualMonetICMS;
    property AtualMonetFECP: Currency read FAtualMonetFECP write FAtualMonetFECP;
    property numeroControle: string read FnumeroControle write FnumeroControle;
  end;

implementation

{ TGNRE }

constructor TGNRE.Create;
begin
  inherited Create;

  Fc01_UfFavorecida := '';
  Fc02_receita := 0;
  Fc25_detalhamentoReceita := 0;
  Fc26_produto := 0;
  Fc27_tipoIdentificacaoEmitente := 0;
  Fc03_idContribuinteEmitente := '';
  Fc28_tipoDocOrigem := 0;
  Fc04_docOrigem := '0';
  Fc06_valorPrincipal := 0;
  Fc10_valorTotal := 0;
  Fc14_dataVencimento := 0;
  Fc15_convenio := '';
  Fc16_razaoSocialEmitente := '';
  Fc17_inscricaoEstadualEmitente := '';
  Fc18_enderecoEmitente := '';
  Fc19_municipioEmitente := '';
  Fc20_ufEnderecoEmitente := '';
  Fc21_cepEmitente := '';
  Fc22_telefoneEmitente := '';
  Fc34_tipoIdentificacaoDestinatario := 0;
  Fc35_idContribuinteDestinatario := '';
  Fc36_inscricaoEstadualDestinatario := '';
  Fc37_razaoSocialDestinatario := '';
  Fc38_municipioDestinatario := '';
  Fc33_dataPagamento := 0;
  Freferencia := TReferencia.Create;
  FcamposExtras := TCampoExtraCollection.Create;
  Fc42_identificadorGuia := '';
end;

destructor TGNRE.Destroy;
begin
  Freferencia.Free;
  FcamposExtras.Free;

  inherited Destroy;
end;

{ TCampoExtraCollection }

function TCampoExtraCollection.Add: TCampoExtraCollectionItem;
begin
  Result := Self.New;
end;

function TCampoExtraCollection.GetItem(
  Index: Integer): TCampoExtraCollectionItem;
begin
  Result := TCampoExtraCollectionItem(inherited Items[Index]);
end;

function TCampoExtraCollection.New: TCampoExtraCollectionItem;
begin
  Result := TCampoExtraCollectionItem.Create();
  Self.Add(Result);
end;

procedure TCampoExtraCollection.SetItem(Index: Integer;
  Value: TCampoExtraCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TCampoExtraCollectionItem }

constructor TCampoExtraCollectionItem.Create;
begin
  FCampoExtra := TCampoExtra.Create;
end;

destructor TCampoExtraCollectionItem.Destroy;
begin
  FCampoExtra.Free;
  inherited;
end;

end.

