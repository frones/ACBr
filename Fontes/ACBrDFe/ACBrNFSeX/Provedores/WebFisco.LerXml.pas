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
  ACBrUtil.Base, ACBrUtil.DateTime;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     WebFisco
//==============================================================================

{ TNFSeR_WebFisco }

function TNFSeR_WebFisco.LerXml: Boolean;
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

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

  FreeAndNil(FDocument);
end;

function TNFSeR_WebFisco.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  i: Integer;
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('nfenumero'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('nfedata'), tcStr);

    aValor:=StringReplace(aValor, '-', '/', [rfReplaceAll]);

    aValor := aValor + 'T' + ObterConteudo(ANode.Childrens.FindAnyNs('nfehora'), tcStr);
    DataEmissao := EncodeDataHora(aValor);

    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('nfeautenticacao'), tcStr);

    SituacaoNfse := snNormal;

    if UpperCase(ObterConteudo(ANode.Childrens.FindAnyNs('nfestatus'), tcStr)) = 'SIM' then
      SituacaoNfse := snCancelado;

//      <xsd:element name="nfecontrole" type="xsd:string"/>
    InfID.ID := ObterConteudo(ANode.Childrens.FindAnyNs('nfecontrole'), tcStr);

    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('emitentecnpj'), tcStr);

//      ValorReceitaBruta := ObterConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);

//      DataInicioAtividade := ObterConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('nfenumerorps'), tcStr);

    Link := ObterConteudo(ANode.Childrens.FindAnyNs('nfelink'), tcStr);

    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

    MotivoCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('nfeobservacoes'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariorsocial'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocnpj'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioinscest'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioinscmun'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioendereco'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarionumero'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocomplemento'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariobairro'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocidade'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('destinatarioestado'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('destinatariocep'), tcStr);
    {
    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);
    }

    i := 0;

    repeat
      Inc(i);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('nfenumfatura' + IntToStr(i)), tcStr);

      if aValor <> '' then
      begin
        CondicaoPagamento.Parcelas.New;

        CondicaoPagamento.Parcelas[i-1].Parcela := aValor;
        CondicaoPagamento.Parcelas[i-1].DataVencimento := ObterConteudo(ANode.Childrens.FindAnyNs('nfedatfatura' + IntToStr(i)), tcDat);
        CondicaoPagamento.Parcelas[i-1].Valor := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalfatura' + IntToStr(i)), tcDe2);
      end;
    until aValor = '';

//      CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
//      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

//        if ObterConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
//          IssRetido := stRetencao
//        else
//          IssRetido := stNormal;

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalor'), tcDe2);
    Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos;
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaliss'), tcDe2);
    Servico.Valores.ValorIssRetido := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalissretido'), tcDe2);
    Servico.Valores.DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaldescincondicional'), tcDe2);
    Servico.Valores.DescontoCondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('nfevaldescoutros'), tcDe2);

    Servico.Valores.AliquotaPis := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqpis'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalpis'), tcDe2);
    Servico.Valores.AliquotaCofins := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqcofins'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalcofins'), tcDe2);
    Servico.Valores.AliquotaCsll := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqcsll'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalcsll'), tcDe2);
    Servico.Valores.AliquotaInss := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqinss'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalinss'), tcDe2);

    Servico.Valores.AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqirrf'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalirrf'), tcDe2);

//      <xsd:element name="nfevaltributavel" type="xsd:string"/>
    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis + Servico.Valores.ValorCofins +
      Servico.Valores.ValorInss + Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

    i := 0;

    repeat
      Inc(i);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('nfeitemserv' + IntToStr(i)), tcStr);

      if aValor <> '' then
      begin
        Servico.ItemServico.New;

        Servico.ItemServico[i-1].ItemListaServico := aValor;
        Servico.ItemServico[i-1].Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('nfealiqserv' + IntToStr(i)), tcDe2);
        Servico.ItemServico[i-1].ValorUnitario := ObterConteudo(ANode.Childrens.FindAnyNs('nfevalserv' + IntToStr(i)), tcDe2);

        if i = 1 then
        begin
          Servico.ItemServico[i-1].Descricao := ObterConteudo(ANode.Childrens.FindAnyNs('nfedescricaoservicos'), tcStr);
          Servico.ItemServico[i-1].Descricao := StringReplace(Servico.ItemServico[i-1].Descricao, FpQuebradeLinha,
                                  sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        end;

        Servico.ItemServico[i-1].Quantidade := 1;
        Servico.ItemServico[i-1].ValorTotal := Servico.ItemServico[i-1].ValorUnitario;
      end;
    until aValor = '';
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
    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('usr'), tcStr);

    Prestador.ValorReceitaBruta := ObterConteudo(ANode.Childrens.FindAnyNs('sssrecbr'), tcDe2);
    Prestador.DataInicioAtividade := ObterConteudo(ANode.Childrens.FindAnyNs('ssdtini'), tcDatVcto);

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('ctr'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('cnpjn'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpj'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('ie'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('im'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('lgr'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('num'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('cpl'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bai'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cid'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('est'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('cep'), tcStr);

    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fon'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('mail'), tcStr);

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dat'), tcDatVcto);

    Servico.CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('loc'), tcStr);
    Servico.ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('iteser1'), tcStr);

    if ObterConteudo(ANode.Childrens.FindAnyNs('ret'), tcStr) = 'SIM' then
      Servico.Valores.IssRetido := stRetencao
    else
      Servico.Valores.IssRetido := stNormal;

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('val'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('iss'), tcDe2);
    Servico.Valores.ValorIssRetido := ObterConteudo(ANode.Childrens.FindAnyNs('issret'), tcDe2);
    Servico.Valores.DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('desci'), tcDe2);
    Servico.Valores.DescontoCondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('desco'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('inss'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('pis'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('cofins'), tcDe2);
    Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('alqser1'), tcDe2);

    i := 0;
    repeat
      Inc(i);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('item' + IntToStr(i)), tcStr);

      if aValor <> '' then
      begin
        Servico.ItemServico.New;

        Servico.ItemServico[i-1].ItemListaServico := aValor;
        Servico.ItemServico[i-1].Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('aliq' + IntToStr(i)), tcDe2);
        Servico.ItemServico[i-1].ValorUnitario := ObterConteudo(ANode.Childrens.FindAnyNs('val' + IntToStr(i)), tcDe2);

        if i = 1 then
        begin
          Servico.ItemServico[i-1].Descricao := ObterConteudo(ANode.Childrens.FindAnyNs('txt'), tcStr);
          Servico.ItemServico[i-1].Descricao := StringReplace(Servico.ItemServico[i-1].Descricao, FpQuebradeLinha,
                                sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
    until aValor = '';
  end;
end;

end.
