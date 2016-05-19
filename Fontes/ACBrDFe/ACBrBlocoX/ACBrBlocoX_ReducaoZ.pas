{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{******************************************************************************}


{$I ACBr.inc}

unit ACBrBlocoX_ReducaoZ;

interface

uses
  ACBrBlocoX_Comum, Classes, SysUtils, StrUtils;

type
  TACBrBlocoX_Totalizador = class(TCollectionItem)
  private
    FIdentificacao: String;
    FValor: Double;
    FProdutos: TACBrBlocoX_Produtos;
    FServicos: TACBrBlocoX_Servicos;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Identificacao: String read FIdentificacao write FIdentificacao;
    property Valor: Double read FValor write FValor;
    property Produtos: TACBrBlocoX_Produtos read FProdutos write FProdutos;
    property Servicos: TACBrBlocoX_Servicos read FServicos write FServicos;
  end;

  TACBrBlocoX_Totalizadores = class(TOwnedCollection)
  private
    FProdutos: TACBrBlocoX_Produtos;
    FServicos: TACBrBlocoX_Servicos;
    function GetItem(Index: integer): TACBrBlocoX_Totalizador;
    procedure SetItem(Index: integer; const Value: TACBrBlocoX_Totalizador);
  public
    function Add: TACBrBlocoX_Totalizador;
    function Insert(Index: integer): TACBrBlocoX_Totalizador;

    property Items[Index: integer]: TACBrBlocoX_Totalizador read GetItem write SetItem; default;
  end;

  TACBrBlocoX_ReducaoZ = class(TACBrBlocoX_BaseFile)
  private
    FVendaBrutaDiaria: Double;
    FCRO: Integer;
    FGT: Double;
    FCRZ: Integer;
    FCOO: Integer;
    FDataReferencia: TDateTime;
    FTotalizadoresParciais: TACBrBlocoX_Totalizadores;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;

    property DataReferencia: TDateTime read FDataReferencia write FDataReferencia;
    property CRZ: Integer read FCRZ write FCRZ;
    property COO: Integer read FCOO write FCOO;
    property CRO: Integer read FCRO write FCRO;
    property VendaBrutaDiaria: Double read FVendaBrutaDiaria write FVendaBrutaDiaria;
    property GT: Double read FGT write FGT;
    property TotalizadoresParciais: TACBrBlocoX_Totalizadores read FTotalizadoresParciais write FTotalizadoresParciais;
  end;

implementation

uses
  pcnConversao, pcnGerador, ACBrUtil, ACBrBlocoX;


{ TACBrBlocoX_Totalizadores }

function TACBrBlocoX_Totalizadores.Add: TACBrBlocoX_Totalizador;
begin
  Result := TACBrBlocoX_Totalizador(inherited Add);
end;

function TACBrBlocoX_Totalizadores.GetItem(Index: integer): TACBrBlocoX_Totalizador;
begin
  Result := TACBrBlocoX_Totalizador(inherited Items[Index]);
end;

function TACBrBlocoX_Totalizadores.Insert(Index: integer): TACBrBlocoX_Totalizador;
begin
  Result := TACBrBlocoX_Totalizador(inherited Insert(Index));
end;

procedure TACBrBlocoX_Totalizadores.SetItem(Index: integer;
  const Value: TACBrBlocoX_Totalizador);
begin
  Items[Index].Assign(Value);
end;

{ TACBrBlocoX_ReducaoZ }

constructor TACBrBlocoX_ReducaoZ.Create(AOwner: TComponent);
begin
  inherited;
  FTotalizadoresParciais := TACBrBlocoX_Totalizadores.Create(Self, TACBrBlocoX_Totalizador);
end;

destructor TACBrBlocoX_ReducaoZ.Destroy;
begin
  FTotalizadoresParciais.Free;
  inherited;
end;

procedure TACBrBlocoX_ReducaoZ.GerarXML(const Assinar: Boolean);
var
  I, X: Integer;
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('ReducaoZ Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  GerarDadosEstabelecimento;
  GerarDadosPafECF;

  FGerador.wGrupo('Ecf');
  with TACBrBlocoX(FACBrBlocoX).ECF do
  begin
    FGerador.wCampo(tcStr, '', 'NumeroFabricacao', 0, 0, 1, NumeroFabricacao);
    FGerador.wCampo(tcStr, '', 'Tipo', 0, 0, 1, Tipo);
    FGerador.wCampo(tcStr, '', 'Marca', 0, 0, 1, Marca);
    FGerador.wCampo(tcStr, '', 'Modelo', 0, 0, 1, Modelo);
    FGerador.wCampo(tcStr, '', 'Versao', 0, 0, 1, Versao);
    FGerador.wCampo(tcStr, '', 'Caixa', 0, 0, 1, Caixa);
  end;

  FGerador.wGrupo('DadosReducaoZ');
  FGerador.wCampo(tcStr, '', 'DataReferencia', 0, 0, 1, FORMATDATETIME('yyyy-mm-dd',DataReferencia));
  FGerador.wCampo(tcStr, '', 'CRZ', 1, 6, 1, CRZ);
  FGerador.wCampo(tcStr, '', 'COO', 1, 6, 1, COO);
  FGerador.wCampo(tcStr, '', 'CRO', 1, 9, 1, CRO);
  FGerador.wCampo(tcStr, '', 'VendaBrutaDiaria', 1, 14, 1, FormatFloat('0.00',VendaBrutaDiaria));
  FGerador.wCampo(tcStr, '', 'GT', 1, 18, 1, FormatFloat('0.00',GT));

  if TotalizadoresParciais.Count > 0 then
  begin
    FGerador.wGrupo('TotalizadoresParciais');

    for I := 0 to TotalizadoresParciais.Count - 1 do
    begin
      FGerador.wGrupo('TotalizadorParcial');
      FGerador.wCampo(tcStr, '', 'Nome', 0,  0, 1, TotalizadoresParciais[I].Identificacao);
      FGerador.wCampo(tcStr, '', 'Valor',       1, 11, 1, FormatFloat('0.00',TotalizadoresParciais[I].Valor));

      with TotalizadoresParciais[I] do
      begin
        FGerador.wGrupo('ProdutosServicos');
        if Produtos.Count > 0then
        begin
          for X := 0 to Produtos.Count - 1 do
          begin
            FGerador.wGrupo('Produto');
            FGerador.wCampo(tcStr, '', 'Descricao',     0, 0, 1, Produtos[X].Descricao);
            FGerador.wCampo(tcStr, '', 'Codigo',        0, 0, 1, Produtos[X].Codigo.Numero);
            FGerador.wCampo(tcStr, '', 'CodigoTipo',    0, 0, 1,   TipoCodigoToStr(Produtos[X].Codigo.Tipo));
            FGerador.wCampo(tcStr, '', 'Quantidade',    0, 0, 1, FormatFloat('0.00',Produtos[X].Quantidade));
            FGerador.wCampo(tcStr, '', 'Unidade',       0, 0, 1, Produtos[X].Unidade);
            FGerador.wCampo(tcStr, '', 'ValorUnitario', 0, 0, 1, FormatFloat('0.00',Produtos[X].ValorUnitario));
            FGerador.wGrupo('/Produto');
          end;
        end;

        if Servicos.Count > 0 then
        begin
          for X := 0 to Servicos.Count - 1 do
          begin
            FGerador.wGrupo('Servico');
            FGerador.wCampo(tcStr, '', 'Descricao',     0, 0, 1, Servicos[X].Descricao);
            FGerador.wCampo(tcStr, '', 'Codigo',        0, 0, 1, Servicos[X].Codigo.Numero);
            FGerador.wCampo(tcStr, '', 'CodigoTipo',    0, 0, 1, TipoCodigoToStr(Servicos[X].Codigo.Tipo));
            FGerador.wCampo(tcStr, '', 'Quantidade',    0, 0, 1, FormatFloat('0.00',Servicos[X].Quantidade));
            FGerador.wCampo(tcStr, '', 'Unidade',       0, 0, 1, Servicos[X].Unidade);
            FGerador.wCampo(tcStr, '', 'ValorUnitario', 0, 0, 1, FormatFloat('0.00',Servicos[X].ValorUnitario));
            FGerador.wGrupo('/Servico');
          end;
        end;
        FGerador.wGrupo('/ProdutosServicos');
      end;
      FGerador.wGrupo('/TotalizadorParcial');
    end;

    FGerador.wGrupo('/TotalizadoresParciais');
  end;

  FGerador.wGrupo('/DadosReducaoZ');
  FGerador.wGrupo('/Ecf');

  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/ReducaoZ');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'ReducaoZ', 'Mensagem');
end;

procedure TACBrBlocoX_ReducaoZ.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_Totalizador }

constructor TACBrBlocoX_Totalizador.Create(Collection: TCollection);
begin
  inherited;
  FProdutos := TACBrBlocoX_Produtos.Create(Self, TACBrBlocoX_Produto);
  FServicos := TACBrBlocoX_Servicos.Create(Self, TACBrBlocoX_Servico);
end;

destructor TACBrBlocoX_Totalizador.Destroy;
begin
  FProdutos.Free;
  FServicos.Free;
  inherited;
end;

end.
