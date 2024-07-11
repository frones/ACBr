{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit SigISSWeb.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_SigISSWeb }

  TNFSeR_SigISSWeb = class(TNFSeRClass)
  protected

  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.DateTime, ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SigISSWeb
//==============================================================================

{ TNFSeR_SigISSWeb }

function TNFSeR_SigISSWeb.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('<numero_nf>', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_SigISSWeb.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  with NFSe do
  begin
    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_prestador'), tcStr);
    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('pessoa_prestador'), tcStr);

    if aValor = 'J' then
      Prestador.IdentificacaoPrestador.Tipo := tpPJdoMunicipio
    else
      Prestador.IdentificacaoPrestador.Tipo := tpPF;

    Prestador.IdentificacaoPrestador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_prestador'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_prestador'), tcStr);

    Prestador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_prestador'), tcStr);

    Prestador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_prestador'), tcStr);
    Prestador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_prestador'), tcStr);
    Prestador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_prestador'), tcStr);
    Prestador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_prestador'), tcStr);
    Prestador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_prestador'), tcStr);
    Prestador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_prestador'), tcStr);

    Prestador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_prestador'), tcStr);
    Prestador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_prestador'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('exterior_dest'), tcStr);
    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_destinatario'), tcStr);

    if aValor = '0' then
    begin
      if Length(Tomador.IdentificacaoTomador.CpfCnpj) = 14 then
        Tomador.IdentificacaoTomador.Tipo :=  tpPJdoMunicipio
      else
        Tomador.IdentificacaoTomador.Tipo := tpPF;
    end
    else
      Tomador.IdentificacaoTomador.Tipo := tpPJforaPais;

    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_destinatario'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_destinatario'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_destinatario'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_destinatario'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_destinatario'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complemento_ende_destinatario'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_destinatario'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_destinatario'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_destinatario'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_destinatario'), tcStr);
    Tomador.Endereco.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('pais_destinatario'), tcStr);

    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_destinatario'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_destinatario'), tcStr);

    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_nf'), tcStr);
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('data_emissao'), tcDatVcto);

    Servico.xFormaPagamento := ObterConteudo(ANode.Childrens.FindAnyNs('forma_de_pagamento'), tcStr);
    Servico.Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('descricao'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    Servico.ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('id_codigo_servico'), tcStr);

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valor_nf'), tcDe2);
    Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('deducao'), tcDe2);
    Servico.Valores.ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('valor_servico'), tcDe2);
    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('iss_retido'), tcStr);

    if aValor = 'S' then
      NFSe.Servico.Valores.IssRetido := stRetencao
    else
    if aValor = 'F' then
      NFSe.Servico.Valores.IssRetido := stRetidoForaMunicipio
    else
    if aValor = 'D' then
      NFSe.Servico.Valores.IssRetido := stDevidoForaMunicipioNaoRetido
    else
      NFSe.Servico.Valores.IssRetido := stNormal;

    if NFSe.Servico.Valores.IssRetido in [stRetidoForaMunicipio, stDevidoForaMunicipioNaoRetido] then
      NFSe.NaturezaOperacao := no2
    else
      NFSe.NaturezaOperacao := no1;

    Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_iss'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_iss'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_pis'), tcDe2);
    Servico.Valores.AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_pis'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('valor_pis'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_cofins'), tcDe2);
    Servico.Valores.AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_cofins'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('valor_cofins'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_csll'), tcDe2);
    Servico.Valores.AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_csll'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('valor_csll'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_irrf'), tcDe2);
    Servico.Valores.AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_irrf'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('valor_irrf'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_inss'), tcDe2);
    Servico.Valores.AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_inss'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_inss'), tcDe2);

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
      Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
      Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

    ValoresNfse.ValorLiquidoNfse := Servico.Valores.ValorServicos;

//    <regime>V</regime>
//    <cancelada>N</cancelada>
//    <nf_avulsa>N</nf_avulsa>

    verAplic := ObterConteudo(ANode.Childrens.FindAnyNs('sistema_gerador'), tcStr);

    IdentificacaoRps.Serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie_rps'), tcStr);
    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('rps'), tcStr);

    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('codigo'), tcStr);
  end;

  LerCampoLink;
end;

function TNFSeR_SigISSWeb.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  with NFSe do
  begin
    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_prestador'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('exterior_dest'), tcStr);
    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_destinatario'), tcStr);

    if aValor = '0' then
    begin
      if Length(Tomador.IdentificacaoTomador.CpfCnpj) = 14 then
        Tomador.IdentificacaoTomador.Tipo :=  tpPJdoMunicipio
      else
        Tomador.IdentificacaoTomador.Tipo := tpPF;
    end
    else
      Tomador.IdentificacaoTomador.Tipo := tpPJforaPais;

    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_destinatario'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_destinatario'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_destinatario'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_destinatario'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_destinatario'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complemento_ende_destinatario'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_destinatario'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_destinatario'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_destinatario'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_destinatario'), tcStr);
    Tomador.Endereco.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('pais_destinatario'), tcStr);

    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_destinatario'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_destinatario'), tcStr);

    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_nf'), tcStr);
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('data_emissao'), tcDat);
//    <forma_de_pagamento></forma_de_pagamento>

    Servico.Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('descricao'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);
    Servico.ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('id_codigo_servico'), tcStr);

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valor_nf'), tcDe2);
    Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('deducao'), tcDe2);
    Servico.Valores.ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('valor_servico'), tcDe2);
    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('iss_retido'), tcStr);

    if aValor = 'S' then
      NFSe.Servico.Valores.IssRetido := stRetencao
    else
      NFSe.Servico.Valores.IssRetido := stNormal;

    Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_iss'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_iss'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_pis'), tcDe2);
    Servico.Valores.AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_pis'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('valor_pis'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_cofins'), tcDe2);
    Servico.Valores.AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_cofins'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('valor_cofins'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_csll'), tcDe2);
    Servico.Valores.AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_csll'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('valor_csll'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_irrf'), tcDe2);
    Servico.Valores.AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_irrf'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('valor_irrf'), tcDe2);

    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_inss'), tcDe2);
    Servico.Valores.AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_inss'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_inss'), tcDe2);

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

//    <cancelada>N</cancelada>

    verAplic := ObterConteudo(ANode.Childrens.FindAnyNs('sistema_gerador'), tcStr);

    IdentificacaoRps.Serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie_rps'), tcStr);
    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('rps'), tcStr);
  end;
end;

end.
