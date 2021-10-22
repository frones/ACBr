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
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('Nfe', xRetorno) > 0) then
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
    Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfenumero'), tcStr);

    aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfedata'), tcStr);

    aValor := aValor + ' ' + ProcessarConteudo(ANode.Childrens.FindAnyNs('nfehora'), tcStr);
    DataEmissao := StrToDateTime(aValor);

    CodigoVerificacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfeautenticacao'), tcStr);
    Status := StrToStatusRPS(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('nfestatus'), tcStr));

//      <xsd:element name="nfecontrole" type="xsd:string"/>

    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        Cnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('emitentecnpj'), tcStr);
      end;

//      ValorReceitaBruta := ProcessarConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);

//      DataInicioAtividade := ProcessarConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfenumerorps'), tcStr);

    Link := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfelink'), tcStr);

    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfeobservacoes'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariorsocial'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariocnpj'), tcStr);
        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatarioinscest'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatarioinscmun'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatarioendereco'), tcStr);
        Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatarionumero'), tcStr);
        Complemento := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariocomplemento'), tcStr);
        Bairro := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariobairro'), tcStr);
        xMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariocidade'), tcStr);
        UF := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatarioestado'), tcStr);
        CEP := ProcessarConteudo(ANode.Childrens.FindAnyNs('destinatariocep'), tcStr);
      end;
      {
      with Contato do
      begin
        Telefone := ProcessarConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
        Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);
      end;
      }
    end;

    with Servico do
    begin
//      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
//      ItemListaServico := ProcessarConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

      with Valores do
      begin
//        if ProcessarConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
//          IssRetido := stRetencao
//        else
//          IssRetido := stNormal;

        ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalor'), tcDe2);
        ValorLiquidoNfse := ValorServicos;
        ValorIss := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevaliss'), tcDe2);
        ValorIssRetido := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalissretido'), tcDe2);
        DescontoIncondicionado := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevaldescincondicional'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevaldescoutros'), tcDe2);

        AliquotaPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqpis'), tcDe2);
        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalpis'), tcDe2);
        AliquotaCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqcofins'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalcofins'), tcDe2);
        AliquotaCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqcsll'), tcDe2);
        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalcsll'), tcDe2);
        AliquotaInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqinss'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalinss'), tcDe2);

        AliquotaIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqirrf'), tcDe2);
        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalirrf'), tcDe2);

//      <xsd:element name="nfevaltributavel" type="xsd:string"/>

        i := 0;
        repeat
          Inc(i);
          aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfeitemserv' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfealiqserv' + IntToStr(i)), tcDe2);
              ValorUnitario := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfevalserv' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ProcessarConteudo(ANode.Childrens.FindAnyNs('nfedescricaoservicos'), tcStr);

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
        Cnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('usr'), tcStr);
      end;

      ValorReceitaBruta := ProcessarConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);
      DataInicioAtividade := ProcessarConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);
    end;

    IdentificacaoRps.Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('ctr'), tcStr);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('cnpjn'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('cnpj'), tcStr);
        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.FindAnyNs('ie'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('im'), tcStr);
      end;

      with Endereco do
      begin
        Endereco := ProcessarConteudo(ANode.Childrens.FindAnyNs('lgr'), tcStr);
        Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('num'), tcStr);
        Complemento := ProcessarConteudo(ANode.Childrens.FindAnyNs('cpl'), tcStr);
        Bairro := ProcessarConteudo(ANode.Childrens.FindAnyNs('bai'), tcStr);
        xMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('cid'), tcStr);
        UF := ProcessarConteudo(ANode.Childrens.FindAnyNs('est'), tcStr);
        CEP := ProcessarConteudo(ANode.Childrens.FindAnyNs('cep'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ProcessarConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
        Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);
      end;
    end;

    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('dat'), tcDatVcto);

    with Servico do
    begin
      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
      ItemListaServico := ProcessarConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

      with Valores do
      begin
        if ProcessarConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;

        ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('val'), tcDe2);
        ValorIss := ProcessarConteudo(ANode.Childrens.FindAnyNs('iss'), tcDe2);
        ValorIssRetido := ProcessarConteudo(ANode.Childrens.FindAnyNs('issret'), tcDe2);
        DescontoIncondicionado := ProcessarConteudo(ANode.Childrens.FindAnyNs('desci'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(ANode.Childrens.FindAnyNs('desco'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('inss'), tcDe2);
        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('pis'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('cofins'), tcDe2);
        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('alqser1'), tcDe2);

        i := 0;
        repeat
          Inc(i);
          aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('item' + IntToStr(i)), tcStr);

          if aValor <> '' then
          begin
            ItemServico.New;
            with ItemServico[i-1] do
            begin
              ItemListaServico := aValor;
              Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('aliq' + IntToStr(i)), tcDe2);
              ValorUnitario := ProcessarConteudo(ANode.Childrens.FindAnyNs('val' + IntToStr(i)), tcDe2);

              if i = 1 then
                Descricao := ProcessarConteudo(ANode.Childrens.FindAnyNs('txt'), tcStr);
            end;
          end;
        until aValor = '';
      end;
    end;
  end;
end;

end.
