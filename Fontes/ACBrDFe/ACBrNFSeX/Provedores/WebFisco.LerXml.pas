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

unit WebFisco.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_WebFisco }

  TNFSeR_WebFisco = class(TNFSeRClass)
  protected

  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     WebFisco
//==============================================================================

{ TNFSeR_WebFisco }

function TNFSeR_WebFisco.LerXml: Boolean;
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

  if (Pos('Nfe', Arquivo) > 0) then
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

function TNFSeR_WebFisco.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('nfenumero'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('nfedata'), tcStr);

    aValor := aValor + ' ' + ObterConteudo(ANode.Childrens.FindAnyNs('nfehora'), tcStr);
    DataEmissao := StrToDateTime(aValor);

    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('nfeautenticacao'), tcStr);
    SituacaoNfse := StrToStatusNFSe(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('nfestatus'), tcStr));

//      <xsd:element name="nfecontrole" type="xsd:string"/>

    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('emitentecnpj'), tcStr);
      end;

//      ValorReceitaBruta := ObterConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);

//      DataInicioAtividade := ObterConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('nfenumerorps'), tcStr);

    Link := ObterConteudo(ANode.Childrens.FindAnyNs('nfelink'), tcStr);

    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('nfeobservacoes'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariorsocial'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocnpj'), tcStr);
        InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioinscest'), tcStr);
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioinscmun'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioendereco'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarionumero'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocomplemento'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariobairro'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocidade'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioestado'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocep'), tcStr);
      end;
      {
      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);
      end;
      }
    end;

    with Servico do
    begin
//      CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
//      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

      with Valores do
      begin
//        if ObterConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
//          IssRetido := stRetencao
//        else
//          IssRetido := stNormal;

        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalor'), tcDe2);
        ValorLiquidoNfse := ValorServicos;
        ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaliss'), tcDe2);
        ValorIssRetido := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalissretido'), tcDe2);
        DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaldescincondicional'), tcDe2);
        DescontoCondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaldescoutros'), tcDe2);

        AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqpis'), tcDe2);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalpis'), tcDe2);
        AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqcofins'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalcofins'), tcDe2);
        AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqcsll'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalcsll'), tcDe2);
        AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqinss'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalinss'), tcDe2);

        AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqirrf'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalirrf'), tcDe2);

//      <xsd:element name="nfevaltributavel" type="xsd:string"/>

        i := 0;
        repeat
          Inc(i);
          aValor := ObterConteudo(ANode.Childrens.FindAnyNs('nfeitemserv' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqserv' + IntToStr(i)), tcDe2);
              ValorUnitario := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalserv' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ObterConteudo(ANode.Childrens.FindAnyNs('nfedescricaoservicos'), tcStr);

            end;
          end;
        until aValor = '';
      end;
    end;
    {
      // Campos que constam no retorno da consulta
      <xsd:element name="nfenumfatura1" type="xsd:string"/>
      <xsd:element name="nfedatfatura1" type="xsd:string"/>
      <xsd:element name="nfevalfatura1" type="xsd:string"/>
      <xsd:element name="nfenumfatura2" type="xsd:string"/>
      <xsd:element name="nfedatfatura2" type="xsd:string"/>
      <xsd:element name="nfevalfatura2" type="xsd:string"/>
      <xsd:element name="nfenumfatura3" type="xsd:string"/>
      <xsd:element name="nfedatfatura3" type="xsd:string"/>
      <xsd:element name="nfevalfatura3" type="xsd:string"/>
      <xsd:element name="nfenumfatura4" type="xsd:string"/>
      <xsd:element name="nfedatfatura4" type="xsd:string"/>
      <xsd:element name="nfevalfatura4" type="xsd:string"/>
      <xsd:element name="nfenumfatura5" type="xsd:string"/>
      <xsd:element name="nfedatfatura5" type="xsd:string"/>
      <xsd:element name="nfevalfatura5" type="xsd:string"/>
      <xsd:element name="nfenumfatura6" type="xsd:string"/>
      <xsd:element name="nfedatfatura6" type="xsd:string"/>
      <xsd:element name="nfevalfatura6" type="xsd:string"/>
     }
  end;
end;

function TNFSeR_WebFisco.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  i: Integer;
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('usr'), tcStr);
      end;

      ValorReceitaBruta := ObterConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);
      DataInicioAtividade := ObterConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('ctr'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('cnpjn'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj'), tcStr);
        InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie'), tcStr);
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('lgr'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('num'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('cpl'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bai'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cid'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('est'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);
      end;
    end;

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dat'), tcDatVcto);

    with Servico do
    begin
      CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

      with Valores do
      begin
        if ObterConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;

        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('val'), tcDe2);
        ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('iss'), tcDe2);
        ValorIssRetido := ObterConteudo(ANode.Childrens.FindAnyNs('issret'), tcDe2);
        DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('desci'), tcDe2);
        DescontoCondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('desco'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('inss'), tcDe2);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('pis'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('cofins'), tcDe2);
        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('alqser1'), tcDe2);

        i := 0;
        repeat
          Inc(i);
          aValor := ObterConteudo(ANode.Childrens.FindAnyNs('item' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq' + IntToStr(i)), tcDe2);
              ValorUnitario := ObterConteudo(ANode.Childrens.FindAnyNs('val' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ObterConteudo(ANode.Childrens.FindAnyNs('txt'), tcStr);
            end;
          end;
        until aValor = '';
      end;
    end;
  end;
end;

end.
