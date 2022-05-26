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

unit SoftPlan.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXGravarXml;

type
  { TNFSeW_SoftPlan }

  TNFSeW_SoftPlan = class(TNFSeWClass)
  protected
//    function GerarChaveRPS: TACBrXmlNode;
//    function GerarCPFCNPJTomador: TACBrXmlNode;
//    function GerarEnderecoTomador: TACBrXmlNode;
//    function GerarCPFCNPJIntermediario: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SoftPlan
//==============================================================================

{ TNFSeW_SoftPlan }

function TNFSeW_SoftPlan.GerarXml: Boolean;
var
  NFSeNode{, xmlNode}: TACBrXmlNode;
  TipoRPS, Situacao, aliquota, ISSRetido, sISSRetidoInter: String;
begin
  Configuracao;

  Opcoes.SuprimirDecimais := True;
  Opcoes.DecimalChar := '.';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;

  NFSeNode := CreateElement('RPS');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.LoteRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Assinatura', 1, 2000, 1,
                                                          NFSe.Assinatura, ''));

//  xmlNode := GerarChaveRPS;
//  NFSeNode.AppendChild(xmlNode);

  TipoRPS := EnumeradoToStr(NFSe.IdentificacaoRps.Tipo,
                      ['RPS','RPS-M','RPS-C'], [trRPS, trNFConjugada, trCupom]);

  Situacao := EnumeradoToStr(NFSe.StatusRps, ['N', 'C'], [srNormal, srCancelado]);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoRPS', 1, 5, 1, TipoRPS, ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'DataEmissao', 1, 10, 1,
                                                         NFse.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'StatusRPS', 1, 1, 1, Situacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TributacaoRPS', 1, 1, 1,
                           TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS), ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorServicos', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorDeducoes', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorPIS', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCOFINS', 1, 15, 0,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorINSS', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorIR', 1, 15, 0,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCSLL', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodigoServico', 1, 5, 1,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

  if NFSe.Servico.Valores.Aliquota > 0 then
  begin
    aliquota := FormatFloat('0.00##', NFSe.Servico.Valores.Aliquota / 100);

    aliquota := StringReplace(aliquota, ',', '.', [rfReplaceAll]);
  end
  else
    aliquota := '0';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'AliquotaServicos', 1, 6, 1,
                                                                 aliquota, ''));

  ISSRetido := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                     ['false', 'true'], [stNormal, stRetencao]);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ISSRetido', 1, 5, 1, ISSRetido, ''));

//  xmlNode := GerarCPFCNPJTomador;
//  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipalTomador', 1, 8, 0,
         OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoEstadualTomador', 1, 19, 0,
          OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RazaoSocialTomador', 1, 75, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

//  xmlNode := GerarEnderecoTomador;
//  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EmailTomador', 1, 75, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  if OnlyNumber(NFSe.IntermediarioServico.CpfCnpj) <> '' then
  begin
//    xmlNode := GerarCPFCNPJIntermediario;
//    NFSeNode.AppendChild(xmlNode);

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipalIntermediario', 1, 8, 0,
                 OnlyNumber(NFSe.IntermediarioServico.InscricaoMunicipal), ''));

    sISSRetidoInter := EnumeradoToStr( NFSe.IntermediarioServico.IssRetido,
                                     ['false', 'true'], [stNormal, stRetencao]);

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ISSRetidoIntermediario', 1, 5, 0,
                                                          sISSRetidoInter, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EmailIntermediario', 1, 75, 0,
                                          NFSe.IntermediarioServico.EMail, ''));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Discriminacao', 1, 2000, 1,
                                               NFSe.Servico.Discriminacao, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCargaTributaria', 1, 15, 0,
                                               NFSe.Servico.ValorCargaTributaria, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'PercentualCargaTributaria', 1, 5, 0,
                                   NFSe.Servico.PercentualCargaTributaria, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'FonteCargaTributaria', 1, 10, 0,
                                        NFSe.Servico.FonteCargaTributaria, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodigoCEI', 1, 12, 0,
                                                NFSe.ConstrucaoCivil.nCei, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MatriculaObra', 1, 12, 0,
                                              NFSe.ConstrucaoCivil.nMatri, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroEncapsulamento', 1, 12, 0,
                               NFSe.ConstrucaoCivil.nNumeroEncapsulamento, ''));

  if (NFSe.TipoTributacaoRPS  <> ttTribnoMun) and
     (NFSe.TipoTributacaoRPS  <> ttTribnoMunIsento) then
    NFSeNode.AppendChild(AddNode(tcInt, '#1', 'MunicipioPrestacao', 1, 7, 0,
                                         NFSe.Servico.MunicipioIncidencia, ''));

  Result := True;
  (*
 <xmlProcessamentoNfpse>
 	<bairroTomador>CENTRO</bairroTomador>
 	<baseCalculo>9990</baseCalculo>
 	<baseCalculoSubstituicao>0</baseCalculoSubstituicao>
 	<cfps>9202</cfps>
 	<codigoMunicipioTomador>4211900</codigoMunicipioTomador>
 	<codigoPostalTomador>88020001</codigoPostalTomador>
 	<complementoEnderecoTomador>Complemento</complementoEnderecoTomador>
 	<dadosAdicionais>Casa</dadosAdicionais>
 	<dataEmissao>2017-01-15</dataEmissao>
 	<emailTomador>exemplo@exemplo.com.br;exemplo2@exemplo2.com</emailTomador>
 	<identificacao>001</identificacao>
 	<identificacaoTomador>83930545000124</identificacaoTomador>
 	<inscricaoMunicipalTomador>0000001</inscricaoMunicipalTomador>
 	<itensServico>
 		<itemServico>
 			<aliquota>0.05</aliquota>
 			<baseCalculo>9990</baseCalculo>
 			<cst>0</cst>
 			<descricaoServico>Extração de Pau Brasil</descricaoServico>
 			<idCNAE>8900</idCNAE>
 			<quantidade>999</quantidade>
 			<valorTotal>9990</valorTotal>
 			<valorUnitario>10</valorUnitario>
 		</itemServico>
 	</itensServico>
 	<logradouroTomador>MORRO DOS CAVALOS</logradouroTomador>
 	<nomeMunicipioTomador>Florianópolis</nomeMunicipioTomador>
 	<numeroAEDF>000000</numeroAEDF>
 	<numeroEnderecoTomador>123</numeroEnderecoTomador>
 	<paisTomador>1058</paisTomador>
 	<razaoSocialTomador>TRIBO CARIJOS</razaoSocialTomador>
 	<telefoneTomador>4812345678</telefoneTomador>
 	<ufTomador>SC</ufTomador>
 	<valorISSQN>499.5</valorISSQN>
 	<valorISSQNSubstituicao>0</valorISSQNSubstituicao>
 	<valorTotalServicos>9990</valorTotalServicos>
 	<ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig">***Obrigatório***</ds:Signature>
 </xmlProcessamentoNfpse>
   *)
end;

end.
