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
        CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('codigoCNAE'), tcStr);
        CodServ := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cst'), tcStr);
  			// <descricaoCNAE>Estúdios de extração de árvores</descricaoCNAE>
        Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('descricaoServico'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
//        CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('idCNAE'), tcStr);
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
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

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

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

  FreeAndNil(FDocument);
end;

function TNFSeR_SoftPlan.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  ValorLiq: Double;
begin
  Result := True;

  if not Assigned(ANode) then
    Exit;

  with NFSe do
  begin
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroSerie'), tcStr);
    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('codigoVerificacao'), tcStr);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dataEmissao'), tcDat);
 	  // <dataProcessamento>2017-01-15</dataProcessamento>
    NfseCancelamento.DataHora := ObterConteudo(ANode.Childrens.FindAnyNs('dataCancelamento'), tcDat);
    MotivoCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('motivoCancelamento'), tcStr);
    Servico.CFPS := ObterConteudo(ANode.Childrens.FindAnyNs('cfps'), tcStr);
    Situacao := ObterConteudo(ANode.Childrens.FindAnyNs('statusNFPSe'), tcInt);
    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('baseCalculo'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('valorISSQN'), tcDe2);
    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('valorTotalServicos'), tcDe2);

    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cnpjPrestador'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoMunicipalPrestador'), tcStr);
    Prestador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaoSocialPrestador'), tcStr);
    Prestador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('logradouroPrestador'), tcStr);
    Prestador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairroPrestador'), tcStr);
    Prestador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('nomeMunicipioPrestador'), tcStr);
    Prestador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('codigoPostalPrestador'), tcStr);
    Prestador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('ufPrestador'), tcStr);
    Prestador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailPrestador'), tcStr);
    Prestador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('telefonePrestador'), tcStr);

    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('identificacao'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('identificacaoTomador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoMunicipalTomador'), tcStr);
    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaoSocialTomador'), tcStr);
    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('logradouroTomador'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroEnderecoTomador'), tcStr);
    Tomador.Endereco.Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('complementoEnderecoTomador'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairroTomador'), tcStr);
    Tomador.Endereco.CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('codigoMunicipioTomador'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('nomeMunicipioTomador'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('codigoPostalTomador'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('ufTomador'), tcStr);
    Tomador.Endereco.CodigoPais := ObterConteudo(ANode.Childrens.FindAnyNs('paisTomador'), tcInt);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailTomador'), tcStr);
    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('telefoneTomador'), tcStr);

    if ObterConteudo(ANode.Childrens.FindAnyNs('homologacao'), tcStr) = 'true' then
      Producao := snNao
    else
      Producao := snSim;

    LerItensServico(ANode);

  	// <baseCalculoSubstituicao>0</baseCalculoSubstituicao>
    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('dadosAdicionais'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  	// <valorISSQNSubstituicao>0</valorISSQNSubstituicao>

    if NFSe.Servico.Valores.ValorIss <> 0 then
      NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIss
    else
      NFSe.Servico.Valores.ValorIssRetido := 0;

    ValorLiq := NFSe.Servico.Valores.ValorServicos - NFSe.Servico.Valores.ValorIssRetido;

    NFSe.Servico.Valores.ValorLiquidoNfse := ValorLiq;

    NFSe.Servico.Valores.ValorTotalNotaFiscal := NFSe.Servico.Valores.ValorServicos;
  end;

  LerCampoLink;
end;

function TNFSeR_SoftPlan.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then
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
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
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
