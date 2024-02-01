{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrBlocoX_Estoque;

interface

uses
  ACBrBlocoX_Comum, Classes, SysUtils, StrUtils;

type
  TACBrBlocoX_Estoque = class(TACBrBlocoX_BaseFile)
  private
    FDataReferencia: TDateTime;
    FProdutos: TACBrBlocoX_Produtos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;

    property DataReferencia: TDateTime read FDataReferencia write FDataReferencia;
    property Produtos: TACBrBlocoX_Produtos read FProdutos write FProdutos;
  end;

implementation

uses
  ACBrBlocoX, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrDFeConsts,
  pcnConversao, pcnGerador;

{ TACBrBlocoX_Estoque }

constructor TACBrBlocoX_Estoque.Create(AOwner: TComponent);
begin
  inherited;
  FProdutos := TACBrBlocoX_Produtos.Create(Self, TACBrBlocoX_Produto);
end;

destructor TACBrBlocoX_Estoque.Destroy;
begin
  FProdutos.Free;
  inherited;
end;

procedure TACBrBlocoX_Estoque.GerarXML(const Assinar: Boolean);
var
  I: Integer;
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('Estoque Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  GerarDadosEstabelecimento;
  GerarDadosPafECF;

  FGerador.wGrupo('DadosEstoque');
  FGerador.wCampo(tcStr, '', 'DataReferencia', 0, 0, 1, FORMATDATETIME('yyyy-mm-dd', DataReferencia));

  if Produtos.Count > 0 then
  begin
    FGerador.wGrupo('Produtos');
    for I := 0 to Produtos.Count - 1 do
    begin
      FGerador.wGrupo('Produto');
      FGerador.wCampo(tcStr, '', 'Descricao', 0, 0, 1, Produtos[I].Descricao);

      case TACBrBlocoX(FACBrBlocoX).Configuracoes.VersaoER of
           erv0204 :
           begin
                FGerador.wCampo(tcStr, '', 'Codigo', 0, 0, 1, Produtos[I].Codigo.CodigoProprio);
                FGerador.wCampo(tcStr, '', 'CodigoTipo', 0, 0, 1, TipoCodigoToStr(Produtos[I].Codigo.Tipo));
                FGerador.wCampo(tcStr, '', 'Quantidade', 1, 20, 1, formatfloat('0.000',Abs(Produtos[I].Quantidade)));
           end;
      else
//           erv0205, erv0206 :
           begin
              with Produtos[I] do begin
                FGerador.wCampo(tcStr, '', 'CodigoGTIN', 0, 0, 1, Produtos[I].Codigo.CodigoGTIN);
                FGerador.wCampo(tcStr, '', 'CodigoCEST', 0, 0, 1, Produtos[I].Codigo.CodigoCEST);
                FGerador.wCampo(tcStr, '', 'CodigoNCMSH', 0, 0, 1,Produtos[I].Codigo.CodigoNCMSH);
                FGerador.wCampo(tcStr, '', 'CodigoProprio', 0, 0, 1,Produtos[I].Codigo.CodigoProprio);
                FGerador.wCampo(tcStr, '', 'Quantidade', 1, 20, 1, formatfloat('0.000',Abs(Produtos[I].Quantidade)));
                FGerador.wCampo(tcStr, '', 'QuantidadeTotalAquisicao', 1, 1, 1, formatfloat('0.000',Produtos[I].QuantidadeTotalAquisicaoMercadoria));
              end;
           end;
      end;
      FGerador.wCampo(tcStr, '', 'Unidade', 0, 0, 1, Produtos[I].Unidade);
      FGerador.wCampo(tcStr, '', 'ValorUnitario', 1, 20, 1, formatfloat('0.000',Produtos[I].ValorUnitario));
      case TACBrBlocoX(FACBrBlocoX).Configuracoes.VersaoER of
           erv0204:
           begin
             //Não tem adição
           end;
      else
//           erv0205, erv0206 :
           begin
              FGerador.wCampo(tcStr, '', 'ValorTotalAquisicao', 1, 1, 1, formatfloat('0.00',Produtos[I].ValorTotalAquisicaoMercadoria));
              FGerador.wCampo(tcStr, '', 'ValorTotalICMSDebitoFornecedor', 1, 1, 1, formatfloat('0.00',Produtos[I].ValorTotalICMSDebitoFornecedor));
              FGerador.wCampo(tcStr, '', 'ValorBaseCalculoICMSST', 1, 1, 1, formatfloat('0.00',Produtos[I].ValorBaseCalculoICMSST) );
              FGerador.wCampo(tcStr, '', 'ValorTotalICMSST', 1, 1, 1, formatfloat('0.00',Produtos[I].ValorTotalICMSST));
           end;
      end;

      FGerador.wCampo(tcStr, '', 'SituacaoTributaria', 1, 1, 1, SituacaoTributariaToStr(Produtos[I].SituacaoTributaria));

      if ((Produtos[I].SituacaoTributaria in [stIsento, stNaoTributado, stSubstTributaria]) and (Produtos[I].Aliquota = 0)) then
        FGerador.wCampo(tcStr, '', 'Aliquota', 4, 4, 1, '')
      else
        FGerador.wCampo(tcStr, '', 'Aliquota', 4, 4, 1, FormatFloat('0.00',Produtos[I].Aliquota));

      FGerador.wCampo(tcStr, '', 'IsArredondado', 1, 1, 1, IfThen(Produtos[I].IndicadorArredondamento, 'true', 'false'));
      FGerador.wCampo(tcStr, '', 'Ippt', 1, 1, 1, IpptToStr(Produtos[I].Ippt));
      FGerador.wCampo(tcStr, '', 'SituacaoEstoque', 1, 1, 1, IfThen(Produtos[I].Quantidade >= 0, 'Positivo', 'Negativo'));

      FGerador.wGrupo('/Produto');
    end;
    FGerador.wGrupo('/Produtos');
  end;

  FGerador.wGrupo('/DadosEstoque');
  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/Estoque');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'Estoque', 'Mensagem');
end;

procedure TACBrBlocoX_Estoque.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

end.

