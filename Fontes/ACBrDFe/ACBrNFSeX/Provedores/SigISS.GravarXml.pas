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

unit SigISS.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { TNFSeW_SigISS }

  TNFSeW_SigISS = class(TNFSeWClass)
  private
    FpVersao: Integer;
//    FpGerarGrupoDadosPrestador: Boolean;
  protected
    procedure Configuracao; override;

//    function GerarPrestador: TACBrXmlNode;
    function GerarIdentificacaoRPS: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_SigISS103 }

  TNFSeW_SigISS103 = class(TNFSeW_SigISS)
  protected
    procedure Configuracao; override;

    function GerarIdentificacaoRPS: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConversao,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SigISS
//==============================================================================

{ TNFSeW_SigISS }

procedure TNFSeW_SigISS.Configuracao;
begin
  inherited Configuracao;

  FpVersao := 100;
//  FpGerarGrupoDadosPrestador := True;
end;

function TNFSeW_SigISS.GerarIdentificacaoRPS: TACBrXmlNode;
begin
  Result := CreateElement('DescricaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'ccm', 1, 15, 0,
     OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cnpj', 1, 14, 1,
                   OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'senha', 1, 10, 1, Senha, DSC_SENHA));

  Result.AppendChild(AddNode(tcStr, '#2', 'crc', 1, 10, 1,
                                                       NFSe.Prestador.crc, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'crc_estado', 1, 2, 1,
                                                NFSe.Prestador.crc_estado, ''));

  Result.AppendChild(AddNode(tcDe4, '#2', 'aliquota_simples', 1, 15, 0,
                                          NFSE.Servico.Valores.AliquotaSN, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'id_sis_legado', 1, 15, 0,
                                                       NFSe.id_sis_legado, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'servico', 1, 15, 1,
                       OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigo_obra', 1, 15, 0,
                                   NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA));

  Result.AppendChild(AddNode(tcStr, '#1', 'obra_art', 1, 15, 0,
                                            NFSe.ConstrucaoCivil.Art, DSC_ART));

  Result.AppendChild(AddNode(tcStr, '#1', 'situacao', 1, 2, 1,
                            FpAOwner.SituacaoTribToStr(NFSe.SituacaoTrib), ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'valor', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'base', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoNF', 1, 150, 1,
                                               NFSe.Servico.Discriminacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_tipo', 1, 1, 1,
         FpAOwner.TipoPessoaToStr(NFSe.Tomador.IdentificacaoTomador.Tipo), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_cnpj', 1, 15, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_email', 1, 100, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_ie', 1, 15, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_im', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_razao', 1, 100, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_fantasia', 1, 100, 1,
                                                NFSe.Tomador.NomeFantasia, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_endereco', 1, 100, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_numero', 1, 100, 1,
                                           NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_complemento', 1, 50, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_bairro', 1, 100, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_CEP', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_cod_cidade', 1, 7, 1,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_fone', 1, 15, 0,
                                            NFSe.Tomador.Contato.Telefone, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_ramal', 1, 15, 0,
                                            '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_fax', 1, 15, 0,
                                            '', ''));

  if NFSe.IdentificacaoRps.Tipo <> trNFConjugada then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'rps_num', 1, 15, 0,
                                 OnlyNumber(NFSe.IdentificacaoRps.Numero), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'rps_serie', 1, 3, 0,
                                              NFSe.IdentificacaoRps.Serie, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'rps_tipo', 1, 1, 0,
                        FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'rps_dia', 1, 2, 0,
                                FormatDateTime('dd', NFSe.DataEmissaoRps), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'rps_mes', 1, 2, 0,
                                FormatDateTime('MM', NFSe.DataEmissaoRps), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'rps_ano', 1, 4, 0,
                              FormatDateTime('yyyy', NFSe.DataEmissaoRps), ''));
  end;

// <xsd:element name="nfse_substituida" type="xsd:int" minOccurs="0" maxOccurs="1"/>

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_substituido', 1, 15, 1,
                        OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB));

(*
<xsd:element name="obra_alvara_numero" type="xsd:int" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_alvara_ano" type="xsd:int" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_lote" type="xsd:string" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_quadra" type="xsd:string" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_bairro" type="xsd:string" minOccurs="0" maxOccurs="1"/>
*)

  Result.AppendChild(AddNode(tcStr, '#1', 'outro_municipio', 1, 7, 1,
                                 OnlyNumber(NFSe.Servico.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cod_outro_municipio', 1, 7, 1,
                                 OnlyNumber(NFSe.Servico.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'retencao_iss', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'pis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'inss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'irrf', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'csll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  if NFSe.IdentificacaoRps.Tipo = trNFConjugada then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'dia_prest_servico', 1, 2, 0,
                                FormatDateTime('dd', NFSe.DataEmissaoRps), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'mes_prest_servico', 1, 2, 0,
                                FormatDateTime('MM', NFSe.DataEmissaoRps), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'ano_prest_servico', 1, 4, 0,
                              FormatDateTime('yyyy', NFSe.DataEmissaoRps), ''));
  end;
end;

{
function TNFSeW_SigISS.GerarPrestador: TACBrXmlNode;
begin
  Result := CreateElement('DadosPrestador');

  Result.AppendChild(AddNode(tcStr, '#1', 'ccm', 1, 15, 0, Usuario, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cnpj', 1, 14, 1,
                   OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.Cnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'senha', 1, 10, 1, Senha, DSC_SENHA));

  Result.AppendChild(AddNode(tcStr, '#2', 'crc', 1, 10, 0,
                                                     NFSe.Prestador.crc, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'crc_estado', 1, 2, 0,
                                                NFSe.Prestador.crc_estado, ''));

  Result.AppendChild(AddNode(tcDe2, '#2', 'aliquota_simples', 1, 15, 0,
                                          NFSE.Servico.Valores.AliquotaSN, ''));
end;
}
function TNFSeW_SigISS.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;
  Opcoes.DecimalChar := ',';

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('GerarNota');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := StringOfChar('0', 15) +
                    OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie;
  NFSe.InfID.ID := copy(NFSe.InfID.ID, length(NFSe.InfID.ID) - 15 + 1, 15);

{
  if FpGerarGrupoDadosPrestador then
  begin
    xmlNode := GerarPrestador;
    NFSeNode.AppendChild(xmlNode);
  end;
}
  xmlNode := GerarIdentificacaoRPS;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

{ TNFSeW_SigISS103 }

procedure TNFSeW_SigISS103.Configuracao;
begin
  inherited Configuracao;

  FpVersao := 103;
//  FpGerarGrupoDadosPrestador := False;
end;

function TNFSeW_SigISS103.GerarIdentificacaoRPS: TACBrXmlNode;
begin
  Result := CreateElement('DescricaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'ccm', 1, 15, 0,
     OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cnpj', 1, 14, 1,
                   OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cpf', 1, 14, 1,
                                                      OnlyNumber(Usuario), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'senha', 1, 10, 1, Senha, DSC_SENHA));

  Result.AppendChild(AddNode(tcDe2, '#2', 'aliquota', 1, 15, 1,
                                            NFSE.Servico.Valores.Aliquota, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'servico', 1, 15, 1,
                       OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigo_obra', 1, 15, 0,
                                   NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA));

  Result.AppendChild(AddNode(tcStr, '#1', 'obra_art', 1, 15, 0,
                                            NFSe.ConstrucaoCivil.Art, DSC_ART));

  Result.AppendChild(AddNode(tcStr, '#1', 'situacao', 1, 2, 1,
                            FpAOwner.SituacaoTribToStr(NFSe.SituacaoTrib), ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'valor', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'base', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ir', 1, 15, 0,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'pis', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'cofins', 1, 15, 0,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'csll', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'inss', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'retencao_iss', 1, 15, 0,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

{
  Para a cidade de Londrina que no momento somente ela usa a versão 1.03 não
  deve gerar a tag abaixo.
}
//  Result.AppendChild(AddNode(tcStr, '#1', 'incentivo_fiscal', 1, 1, 0,
//   FpAOwner.SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCCULT));

  Result.AppendChild(AddNode(tcStr, '#1', 'cod_municipio_prestacao_servico', 1, 7, 0,
                           OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN));

  Result.AppendChild(AddNode(tcInt, '#1', 'cod_pais_prestacao_servico', 4, 4, 0,
                                           NFSe.Servico.CodigoPais, DSC_CPAIS));

  Result.AppendChild(AddNode(tcInt, '#1', 'cod_municipio_incidencia', 7, 07, 0,
                                NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoNF', 1, 150, 1,
                                               NFSe.Servico.Discriminacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_tipo', 1, 1, 1,
         FpAOwner.TipoPessoaToStr(NFSe.Tomador.IdentificacaoTomador.Tipo), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_cnpj', 1, 15, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_email', 1, 100, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_ie', 1, 15, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_im', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_razao', 1, 100, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_endereco', 1, 100, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_numero', 1, 100, 1,
                                           NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_complemento', 1, 50, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_bairro', 1, 100, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_CEP', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_cod_cidade', 1, 7, 1,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_fone', 1, 15, 0,
                                            NFSe.Tomador.Contato.Telefone, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_ramal', 1, 15, 0,
                                            '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tomador_fax', 1, 15, 0,
                                            '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_num', 1, 15, 0,
                                 OnlyNumber(NFSe.IdentificacaoRps.Numero), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_serie', 1, 3, 0,
                                              NFSe.IdentificacaoRps.Serie, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_tipo', 1, 1, 0,
                        FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_dia', 1, 2, 0,
                                 FormatDateTime('dd',NFSe.DataEmissaoRps), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_mes', 1, 2, 0,
                                 FormatDateTime('MM',NFSe.DataEmissaoRps), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_ano', 1, 4, 0,
                               FormatDateTime('yyyy',NFSe.DataEmissaoRps), ''));

// <xsd:element name="nfse_substituida" type="xsd:int" minOccurs="0" maxOccurs="1"/>

  Result.AppendChild(AddNode(tcStr, '#1', 'rps_substituido', 1, 15, 1,
                        OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB));

(*
<xsd:element name="obra_alvara_numero" type="xsd:int" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_alvara_ano" type="xsd:int" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_lote" type="xsd:string" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_quadra" type="xsd:string" minOccurs="0" maxOccurs="1"/>
<xsd:element name="obra_local_bairro" type="xsd:string" minOccurs="0" maxOccurs="1"/>
*)
end;

function TNFSeW_SigISS103.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;
  Opcoes.DecimalChar := ',';

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('GerarNota');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := StringOfChar('0', 15) +
                    OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie;
  NFSe.InfID.ID := copy(NFSe.InfID.ID, length(NFSe.InfID.ID) - 15 + 1, 15);

{
  if FpGerarGrupoDadosPrestador then
  begin
    xmlNode := GerarPrestador;
    NFSeNode.AppendChild(xmlNode);
  end;
}
  xmlNode := GerarIdentificacaoRPS;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

end.
