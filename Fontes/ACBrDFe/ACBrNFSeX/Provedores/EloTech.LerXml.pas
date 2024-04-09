{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit EloTech.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlDocument, ACBrXmlBase,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_EloTech203 }

  TNFSeR_EloTech203 = class(TNFSeR_ABRASFv2)
  protected

    procedure LerListaServicos(const ANode: TACBrXmlNode); override;
    procedure LerServicos(const ANode: TACBrXmlNode); override;
    procedure LerDadosDeducao(const ANode: TACBrXmlNode; Item: Integer);
  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     EloTech
//==============================================================================

{ TNFSeR_EloTech203 }

procedure TNFSeR_EloTech203.LerListaServicos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ListaItensServico');

  if AuxNode <> nil then
    LerServicos(AuxNode);
end;

procedure TNFSeR_EloTech203.LerServicos(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  CodigoItemServico: string;
  Ok: Boolean;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('ItemServico');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      CodigoItemServico := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ItemListaServico'), tcStr);
      ItemListaServico := NormatizarItemListaServico(CodigoItemServico);

      CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Descricao'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      Tributavel := FpAOwner.StrToSimNao(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('Tributavel'), tcStr));
      Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe2);
      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorUnitario'), tcDe2);
      DescontoCondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorDesconto'), tcDe2);
      ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorLiquido'), tcDe2);

      LerDadosDeducao(ANodes[i], i);
    end;
  end;
end;

procedure TNFSeR_EloTech203.LerDadosDeducao(const ANode: TACBrXmlNode; Item: Integer);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DadosDeducao');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.ItemServico[Item].DadosDeducao do
    begin
      TipoDeducao := FpAOwner.StrToTipoDeducao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoDeducao'), tcStr));

      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if CpfCnpj = '' then
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);

      NumeroNotaFiscalReferencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroNotaFiscalReferencia'), tcStr);
      ValorTotalNotaFiscal := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorTotalNotaFiscal'), tcDe2);
      PercentualADeduzir := ObterConteudo(AuxNode.Childrens.FindAnyNs('PercentualADeduzir'), tcDe2);
      ValorADeduzir := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorADeduzir'), tcDe2);
    end;
  end;
end;

end.
