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

unit SoftPlan.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_SoftPlan }

  TNFSeR_SoftPlan = class(TNFSeRClass)
  protected
    procedure LerItensServico(const ANode: TACBrXmlNode);
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
//     SoftPlan
//==============================================================================

{ TNFSeR_SoftPlan }

procedure TNFSeR_SoftPlan.LerItensServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('itensServico');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('itemServico');

    NFSe.Servico.ItemServico.Clear;

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Aliquota := ObterConteudo(ANodes[i].Childrens.FindAnyNs('aliquota'), tcDe2);
        BaseCalculo := ObterConteudo(ANodes[i].Childrens.FindAnyNs('baseCalculo'), tcDe2);
        // <codigoCNAE>19377702</codigoCNAE>
        CodServ := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cst'), tcStr);
  			// <descricaoCNAE>Estúdios de extração de árvores</descricaoCNAE>
        Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('descricaoServico'), tcStr);
        CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('idCNAE'), tcStr);
        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('quantidade'), tcDe2);
        ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valorTotal'), tcDe2);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valorUnitario'), tcDe2);
      end;
    end;
  end;
end;

function TNFSeR_SoftPlan.LerXml: Boolean;
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

  if (Pos('xmlNfpse', Arquivo) > 0) then
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

function TNFSeR_SoftPlan.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then
    Exit;

  with NFSe do
  begin
    Prestador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairroPrestador'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairroTomador'), tcStr);
    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('baseCalculo'), tcDe2);
  	// <baseCalculoSubstituicao>0</baseCalculoSubstituicao>
    Servico.CodigoCnae := ObterConteudo(ANode.Childrens.FindAnyNs('cfps'), tcStr);
    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpjPrestador'), tcStr);
    Tomador.Endereco.CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('codigoMunicipioTomador'), tcStr);
    Prestador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('codigoPostalPrestador'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('codigoPostalTomador'), tcStr);
    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('codigoVerificacao'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complementoEnderecoTomador'), tcStr);
    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('dadosAdicionais'), tcStr);
    NfseCancelamento.DataHora := ObterConteudo(ANode.Childrens.FindAnyNs('dataCancelamento'), tcDat);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dataEmissao'), tcDat);
 	  // <dataProcessamento>2017-01-15</dataProcessamento>
    Prestador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailPrestador'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailTomador'), tcStr);

    if ObterConteudo(ANode.Childrens.FindAnyNs('homologacao'), tcStr) = 'true' then
      Producao := snNao
    else
      Producao := snSim;

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('identificacao'), tcStr);
    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('identificacaoTomador'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoMunicipalPrestador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoMunicipalTomador'), tcStr);

    LerItensServico(ANode);

    Prestador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('logradouroPrestador'), tcStr);
    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('logradouroTomador'), tcStr);
    MotivoCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('motivoCancelamento'), tcStr);
    Prestador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('nomeMunicipioPrestador'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('nomeMunicipioTomador'), tcStr);
   	// <numeroAEDF>000000</numeroAEDF>
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroEnderecoTomador'), tcStr);
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroSerie'), tcStr);
    Tomador.Endereco.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('paisTomador'), tcStr);
    Prestador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaoSocialPrestador'), tcStr);
    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaoSocialTomador'), tcStr);
  	// <statusNFPSe>0</statusNFPSe>
    Prestador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('telefonePrestador'), tcStr);
    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('telefoneTomador'), tcStr);
    Prestador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('ufPrestador'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('ufTomador'), tcStr);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valorISSQN'), tcDe2);
  	// <valorISSQNSubstituicao>0</valorISSQNSubstituicao>
    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valorTotalServicos'), tcDe2);
    Situacao := ObterConteudo(ANode.Childrens.FindAnyNs('statusNFPSe'), tcInt);
  end;
end;

function TNFSeR_SoftPlan.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then
   Exit;

  with NFSe do
  begin
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairroTomador'), tcStr);
    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('baseCalculo'), tcDe2);
  	// <baseCalculoSubstituicao>0</baseCalculoSubstituicao>
    Servico.CodigoCnae := ObterConteudo(ANode.Childrens.FindAnyNs('cfps'), tcStr);
    Tomador.Endereco.CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('codigoMunicipioTomador'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('codigoPostalTomador'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complementoEnderecoTomador'), tcStr);
    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('dadosAdicionais'), tcStr);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dataEmissao'), tcDat);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailTomador'), tcStr);
    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('identificacao'), tcStr);
    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('identificacaoTomador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoMunicipalTomador'), tcStr);

    LerItensServico(ANode);

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('logradouroTomador'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('nomeMunicipioTomador'), tcStr);
   	// <numeroAEDF>000000</numeroAEDF>
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroEnderecoTomador'), tcStr);
    Tomador.Endereco.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('paisTomador'), tcStr);
    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaoSocialTomador'), tcStr);
    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('telefoneTomador'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('ufTomador'), tcStr);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valorISSQN'), tcDe2);
  	// <valorISSQNSubstituicao>0</valorISSQNSubstituicao>
    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valorTotalServicos'), tcDe2);
  end;
end;

end.
