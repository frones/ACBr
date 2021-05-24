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
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
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

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

//  FpVersao := ConfigGeral.VersaoProv;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);
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
    Numero := ProcessarConteudo(ANode.Childrens.Find('nfenumero'), tcStr);

    aValor := ProcessarConteudo(ANode.Childrens.Find('nfedata'), tcStr);

    aValor := aValor + ' ' + ProcessarConteudo(ANode.Childrens.Find('nfehora'), tcStr);
    DataEmissao := StrToDateTime(aValor);

    CodigoVerificacao := ProcessarConteudo(ANode.Childrens.Find('nfeautenticacao'), tcStr);

    Status := StrToStatusRPS(Ok, ProcessarConteudo(ANode.Childrens.Find('nfestatus'), tcStr));

//      <xsd:element name="nfecontrole" type="xsd:string"/>

    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        Cnpj := ProcessarConteudo(ANode.Childrens.Find('emitentecnpj'), tcStr);
      end;

//      ValorReceitaBruta := ProcessarConteudo(ANode.Childrens.Find('sssrecbr'), tcDe2);

//      DataInicioAtividade := ProcessarConteudo(ANode.Childrens.Find('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ProcessarConteudo(ANode.Childrens.Find('nfenumerorps'), tcStr);

    Link := ProcessarConteudo(ANode.Childrens.Find('nfelink'), tcStr);

    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.Find('nfeobservacoes'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.Find('destinatariorsocial'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.Find('destinatariocnpj'), tcStr);
        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.Find('destinatarioinscest'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('destinatarioinscmun'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ProcessarConteudo(ANode.Childrens.Find('destinatarioendereco'), tcStr);
        Numero := ProcessarConteudo(ANode.Childrens.Find('destinatarionumero'), tcStr);
        Complemento := ProcessarConteudo(ANode.Childrens.Find('destinatariocomplemento'), tcStr);
        Bairro := ProcessarConteudo(ANode.Childrens.Find('destinatariobairro'), tcStr);
        xMunicipio := ProcessarConteudo(ANode.Childrens.Find('destinatariocidade'), tcStr);
        UF := ProcessarConteudo(ANode.Childrens.Find('destinatarioestado'), tcStr);
        CEP := ProcessarConteudo(ANode.Childrens.Find('destinatariocep'), tcStr);
      end;
      {
      with Contato do
      begin
        Telefone := ProcessarConteudo(ANode.Childrens.Find('fon'), tcStr);
        Email := ProcessarConteudo(ANode.Childrens.Find('mail'), tcStr);
      end;
      }
    end;

    with Servico do
    begin
//      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.Find('loc'), tcStr);
//      ItemListaServico := ProcessarConteudo(ANode.Childrens.Find('iteser1'), tcStr);

      with Valores do
      begin
//        if ProcessarConteudo(ANode.Childrens.Find('ret'), tcStr) = 'SIM' then
//          IssRetido := stRetencao
//        else
//          IssRetido := stNormal;

        ValorServicos := ProcessarConteudo(ANode.Childrens.Find('nfevalor'), tcDe2);
        ValorLiquidoNfse := ValorServicos;
        ValorIss := ProcessarConteudo(ANode.Childrens.Find('nfevaliss'), tcDe2);
        ValorIssRetido := ProcessarConteudo(ANode.Childrens.Find('nfevalissretido'), tcDe2);
        DescontoIncondicionado := ProcessarConteudo(ANode.Childrens.Find('nfevaldescincondicional'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(ANode.Childrens.Find('nfevaldescoutros'), tcDe2);

        AliquotaPis := ProcessarConteudo(ANode.Childrens.Find('nfealiqpis'), tcDe2);
        ValorPis := ProcessarConteudo(ANode.Childrens.Find('nfevalpis'), tcDe2);
        AliquotaCofins := ProcessarConteudo(ANode.Childrens.Find('nfealiqcofins'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('nfevalcofins'), tcDe2);
        AliquotaCsll := ProcessarConteudo(ANode.Childrens.Find('nfealiqcsll'), tcDe2);
        ValorCsll := ProcessarConteudo(ANode.Childrens.Find('nfevalcsll'), tcDe2);
        AliquotaInss := ProcessarConteudo(ANode.Childrens.Find('nfealiqinss'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.Find('nfevalinss'), tcDe2);

        AliquotaIr := ProcessarConteudo(ANode.Childrens.Find('nfealiqirrf'), tcDe2);
        ValorIr := ProcessarConteudo(ANode.Childrens.Find('nfevalirrf'), tcDe2);

//      <xsd:element name="nfevaltributavel" type="xsd:string"/>

        i := 0;
        repeat
          Inc(i);
          aValor := ProcessarConteudo(ANode.Childrens.Find('nfeitemserv' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ProcessarConteudo(ANode.Childrens.Find('nfealiqserv' + IntToStr(i)), tcDe2);
              ValorUnitario := ProcessarConteudo(ANode.Childrens.Find('nfevalserv' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ProcessarConteudo(ANode.Childrens.Find('nfedescricaoservicos'), tcStr);

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
        Cnpj := ProcessarConteudo(ANode.Childrens.Find('usr'), tcStr);
      end;

      ValorReceitaBruta := ProcessarConteudo(ANode.Childrens.Find('sssrecbr'), tcDe2);

      DataInicioAtividade := ProcessarConteudo(ANode.Childrens.Find('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ProcessarConteudo(ANode.Childrens.Find('ctr'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.Find('cnpjn'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.Find('cnpj'), tcStr);
        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.Find('ie'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('im'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ProcessarConteudo(ANode.Childrens.Find('lgr'), tcStr);
        Numero := ProcessarConteudo(ANode.Childrens.Find('num'), tcStr);
        Complemento := ProcessarConteudo(ANode.Childrens.Find('cpl'), tcStr);
        Bairro := ProcessarConteudo(ANode.Childrens.Find('bai'), tcStr);
        xMunicipio := ProcessarConteudo(ANode.Childrens.Find('cid'), tcStr);
        UF := ProcessarConteudo(ANode.Childrens.Find('est'), tcStr);
        CEP := ProcessarConteudo(ANode.Childrens.Find('cep'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ProcessarConteudo(ANode.Childrens.Find('fon'), tcStr);
        Email := ProcessarConteudo(ANode.Childrens.Find('mail'), tcStr);
      end;
    end;

    DataEmissao := ProcessarConteudo(ANode.Childrens.Find('dat'), tcDatVcto);

    with Servico do
    begin
      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.Find('loc'), tcStr);
      ItemListaServico := ProcessarConteudo(ANode.Childrens.Find('iteser1'), tcStr);

      with Valores do
      begin
        if ProcessarConteudo(ANode.Childrens.Find('ret'), tcStr) = 'SIM' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;

        ValorServicos := ProcessarConteudo(ANode.Childrens.Find('val'), tcDe2);
        ValorIss := ProcessarConteudo(ANode.Childrens.Find('iss'), tcDe2);
        ValorIssRetido := ProcessarConteudo(ANode.Childrens.Find('issret'), tcDe2);
        DescontoIncondicionado := ProcessarConteudo(ANode.Childrens.Find('desci'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(ANode.Childrens.Find('desco'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.Find('inss'), tcDe2);
        ValorPis := ProcessarConteudo(ANode.Childrens.Find('pis'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('cofins'), tcDe2);
        Aliquota := ProcessarConteudo(ANode.Childrens.Find('alqser1'), tcDe2);

        i := 0;
        repeat
          Inc(i);
          aValor := ProcessarConteudo(ANode.Childrens.Find('item' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ProcessarConteudo(ANode.Childrens.Find('aliq' + IntToStr(i)), tcDe2);
              ValorUnitario := ProcessarConteudo(ANode.Childrens.Find('val' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ProcessarConteudo(ANode.Childrens.Find('txt'), tcStr);
            end;
          end;
        until aValor = '';
      end;
    end;
  end;
end;

end.
