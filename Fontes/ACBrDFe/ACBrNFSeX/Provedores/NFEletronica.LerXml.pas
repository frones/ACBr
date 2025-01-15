{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit NFEletronica.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlDocument,
  ACBrNFSeXLerXml_ABRASFv1;

type
  { TNFSeR_NFEletronica }

  TNFSeR_NFEletronica = class(TNFSeR_ABRASFv1)
  protected
    {
    // Usados para a leitura do XML do RPS
    procedure LerValores(const ANode: TACBrXmlNode); override;
    procedure LerServico(const ANode: TACBrXmlNode); override;
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode); override;
    procedure LerTomador(const ANode: TACBrXmlNode); override;
    procedure Ler_CondicaoPagamento(const ANode: TACBrXmlNode);
    }
    {
    // Usados para a leitura do XML da NFSe
    }
    procedure Ler_NF(const ANode: TACBrXmlNode);
    procedure Ler_Totais(const ANode: TACBrXmlNode);
    procedure Ler_Retencao(const ANode: TACBrXmlNode);
    procedure Ler_Prestador(const ANode: TACBrXmlNode);
    procedure Ler_Tomador(const ANode: TACBrXmlNode);
    procedure Ler_servico(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrUtil.Base,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     NFEletronica
//==============================================================================

{ TNFSeR_NFEletronica }

function TNFSeR_NFEletronica.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;
  FpAOwner.ConfigGeral.DetalharServico := False;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('</NFSE>', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.tpXML := tpXml;

  if tpXML = txmlNFSe then
  begin
    Result := LerXmlNfse(XmlNode);
    FpAOwner.ConfigGeral.DetalharServico := True;
  end
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_NFEletronica.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  Ler_NF(ANode.Childrens.FindAnyNs('NF'));
  {
    <cod_pag>1</cod_pag>
    <local_serv>0</local_serv>
    <cod_isencao>0</cod_isencao>
    <f_simples>0</f_simples>
  }
  Ler_Totais(ANode.Childrens.FindAnyNs('Totais'));

  NFSe.Servico.Valores.AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_iss'), tcDe4);

  Ler_Retencao(ANode.Childrens.FindAnyNs('Retencao'));

  NFSe.CondicaoPagamento.InstrucaoPagamento := ObterConteudo(ANode.Childrens.FindAnyNs('InstrucaoPagamento'), tcStr);
  NFSe.CondicaoPagamento.DataCriacao := ObterConteudo(ANode.Childrens.FindAnyNs('dt_cria'), tcDat);
  NFSe.CondicaoPagamento.DataVencimento := ObterConteudo(ANode.Childrens.FindAnyNs('dt_vecto'), tcDat);
  NFSe.CondicaoPagamento.QtdParcela := ObterConteudo(ANode.Childrens.FindAnyNs('num_parc'), tcInt);

  Ler_Prestador(ANode.Childrens.FindAnyNs('Prestador'));
  Ler_Tomador(ANode.Childrens.FindAnyNs('Tomador'));
  Ler_servico(ANode.Childrens.FindAnyNs('servico'));

  NFSe.Assinatura := ObterConteudo(ANode.Childrens.FindAnyNs('Assinatura'), tcStr);

  with NFSe.Servico.Valores do
  begin
    ValorLiquidoNfse := ValorServicos -
        (ValorDeducoes + DescontoCondicionado +
         DescontoIncondicionado + ValorIssRetido);

    ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                            DescontoIncondicionado;
  end;
end;

procedure TNFSeR_NFEletronica.Ler_NF(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFSe.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('Numero'), tcStr);
  NFSe.DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('Data_Emissao'), tcDat);
  NFSe.IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('Referencia'), tcStr);
//  <RPS>0</RPS>
  NFSe.Servico.ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('codigo_servico'), tcStr);
  NFSe.Servico.ItemListaServico := NormatizarItemListaServico(NFSe.Servico.ItemListaServico);
  NFSe.Servico.xItemListaServico := ItemListaServicoDescricao(NFSe.Servico.ItemListaServico);
end;

procedure TNFSeR_NFEletronica.Ler_Totais(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFSe.Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('Valor_Total'), tcDe4);
  NFSe.Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('Deducao_Base'), tcDe4);
  NFSe.Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('ISS_incluso'), tcDe4);
end;

procedure TNFSeR_NFEletronica.Ler_Retencao(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFSe.Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('ISS'), tcDe4);
  NFSe.Servico.Valores.AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('IRRF'), tcDe4);
  NFSe.Servico.Valores.AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('PIS'), tcDe4);
  NFSe.Servico.Valores.AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('COFINS'), tcDe4);
  NFSe.Servico.Valores.AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('CSLL'), tcDe4);
  NFSe.Servico.Valores.AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('INSS'), tcDe4);
end;

procedure TNFSeR_NFEletronica.Ler_Prestador(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFSe.Prestador.RazaoSocial  := ObterConteudo(ANode.Childrens.FindAnyNs('Nome'), tcStr);

  NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
  NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);

  NFSe.Prestador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFSe.Prestador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('Numero'), tcStr);
  NFSe.Prestador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('Complemento'), tcStr);
  NFSe.Prestador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('Bairro'), tcStr);
  NFSe.Prestador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('Municipio'), tcStr);
  NFSe.Prestador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);

  NFSe.Prestador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('Email'), tcStr);
end;

procedure TNFSeR_NFEletronica.Ler_Tomador(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFSe.Tomador.RazaoSocial  := ObterConteudo(ANode.Childrens.FindAnyNs('Nome'), tcStr);

  NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('Inscr_Municipal'), tcStr);
  NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('Inscr_Estadual'), tcStr);

  NFSe.Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFSe.Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero'), tcStr);
  NFSe.Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complemento'), tcStr);
  NFSe.Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro'), tcStr);
  NFSe.Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('municipio'), tcStr);
  NFSe.Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);

  NFSe.Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_tomador'), tcStr);
end;

procedure TNFSeR_NFEletronica.Ler_servico(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ANodes := ANode.Childrens.FindAllAnyNs('item');

  NFSe.Servico.ItemServico.Clear;

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('qtde'), tcDe4);
      Unidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('unid'), tcStr);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('descricao'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_unit'), tcDe6);
      ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor'), tcDe10);
    end;
  end;
end;

end.
