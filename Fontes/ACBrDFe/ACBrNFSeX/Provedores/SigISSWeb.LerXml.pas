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
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

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

  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe do
  begin
    with Prestador do
    begin
      IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_prestador'), tcStr);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('pessoa_prestador'), tcStr);

      if aValor = 'J' then
        IdentificacaoPrestador.Tipo := tpPJdoMunicipio
      else
        IdentificacaoPrestador.Tipo := tpPF;

      IdentificacaoPrestador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_prestador'), tcStr);
      IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_prestador'), tcStr);

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_prestador'), tcStr);

      with Endereco do
      begin
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_prestador'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_prestador'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_prestador'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_prestador'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_prestador'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_prestador'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_prestador'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_prestador'), tcStr);
      end;
    end;

    with Tomador do
    begin
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('exterior_dest'), tcStr);
      IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_destinatario'), tcStr);

      if aValor = '0' then
      begin
        if Length(IdentificacaoTomador.CpfCnpj) = 14 then
          IdentificacaoTomador.Tipo :=  tpPJdoMunicipio
        else
          IdentificacaoTomador.Tipo := tpPF;
      end
      else
        IdentificacaoTomador.Tipo := tpPJforaPais;

      IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_destinatario'), tcStr);
      IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_destinatario'), tcStr);

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_destinatario'), tcStr);

      with Endereco do
      begin
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_destinatario'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_destinatario'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complemento_ende_destinatario'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_destinatario'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_destinatario'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_destinatario'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_destinatario'), tcStr);
        xPais := ObterConteudo(ANode.Childrens.FindAnyNs('pais_destinatario'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_destinatario'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_destinatario'), tcStr);
      end;
    end;

    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_nf'), tcStr);
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('valor_nf'), tcDe2);
    end;

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('data_emissao'), tcDatVcto);
//    <forma_de_pagamento></forma_de_pagamento>

    with Servico do
    begin
      Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('descricao'), tcStr);
      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('id_codigo_servico'), tcStr);

      with Valores do
      begin
        ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('deducao'), tcDe2);
        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valor_servico'), tcDe2);
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('iss_retido'), tcStr);

        if aValor = 'S' then
          NFSe.Servico.Valores.IssRetido := stRetencao
        else
          NFSe.Servico.Valores.IssRetido := stNormal;

        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_iss'), tcDe2);
        ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_iss'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_pis'), tcDe2);
        AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_pis'), tcDe2);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('valor_pis'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_cofins'), tcDe2);
        AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_cofins'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('valor_cofins'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_csll'), tcDe2);
        AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_csll'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('valor_csll'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_irrf'), tcDe2);
        AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_irrf'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('valor_irrf'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_inss'), tcDe2);
        AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_inss'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_inss'), tcDe2);
      end;
    end;

//    <regime>V</regime>
//    <cancelada>N</cancelada>
//    <nf_avulsa>N</nf_avulsa>

    verAplic := ObterConteudo(ANode.Childrens.FindAnyNs('sistema_gerador'), tcStr);

    with IdentificacaoRps do
    begin
      Serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie_rps'), tcStr);
      Numero := ObterConteudo(ANode.Childrens.FindAnyNs('rps'), tcStr);
    end;

    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('codigo'), tcStr);
  end;

  LerCampoLink;
end;

function TNFSeR_SigISSWeb.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe do
  begin
    with Prestador do
    begin
      IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_prestador'), tcStr);
    end;

    with Tomador do
    begin
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('exterior_dest'), tcStr);
      IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj_cpf_destinatario'), tcStr);

      if aValor = '0' then
      begin
        if Length(IdentificacaoTomador.CpfCnpj) = 14 then
          IdentificacaoTomador.Tipo :=  tpPJdoMunicipio
        else
          IdentificacaoTomador.Tipo := tpPF;
      end
      else
        IdentificacaoTomador.Tipo := tpPJforaPais;

      IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie_destinatario'), tcStr);
      IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im_destinatario'), tcStr);

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razao_social_destinatario'), tcStr);

      with Endereco do
      begin
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('endereco_destinatario'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_ende_destinatario'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complemento_ende_destinatario'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairro_destinatario'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep_destinatario'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidade_destinatario'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('uf_destinatario'), tcStr);
        xPais := ObterConteudo(ANode.Childrens.FindAnyNs('pais_destinatario'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone_destinatario'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('email_destinatario'), tcStr);
      end;
    end;

    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numero_nf'), tcStr);
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('data_emissao'), tcDat);
//    <forma_de_pagamento></forma_de_pagamento>

    with Servico do
    begin
      Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('descricao'), tcStr);
      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('id_codigo_servico'), tcStr);

      with Valores do
      begin
        ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('valor_nf'), tcDe2);
        ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('deducao'), tcDe2);
        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valor_servico'), tcDe2);
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('iss_retido'), tcStr);

        if aValor = 'S' then
          NFSe.Servico.Valores.IssRetido := stRetencao
        else
          NFSe.Servico.Valores.IssRetido := stNormal;

        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_iss'), tcDe2);
        ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_iss'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_pis'), tcDe2);
        AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_pis'), tcDe2);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('valor_pis'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_cofins'), tcDe2);
        AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_cofins'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('valor_cofins'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_csll'), tcDe2);
        AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_csll'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('valor_csll'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_irrf'), tcDe2);
        AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_irrf'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('valor_irrf'), tcDe2);

        BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('bc_inss'), tcDe2);
        AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('aliq_inss'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('valor_inss'), tcDe2);
      end;
    end;

//    <cancelada>N</cancelada>

    verAplic := ObterConteudo(ANode.Childrens.FindAnyNs('sistema_gerador'), tcStr);

    with IdentificacaoRps do
    begin
      Serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie_rps'), tcStr);
      Numero := ObterConteudo(ANode.Childrens.FindAnyNs('rps'), tcStr);
    end;
  end;
end;

end.
