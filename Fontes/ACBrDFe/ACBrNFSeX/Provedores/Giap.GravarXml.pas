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

unit Giap.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase,
  ACBrXmlDocument,
  ACBrNFSeXGravarXml,
  ACBrNFSeXGravarXml_ABRASFv1;

type
  { TNFSeW_Giap }

  TNFSeW_Giap = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarDadosPrestador: TACBrXmlNode;
    function GerarDadosServico: TACBrXmlNode;
    function GerarDadosTomador: TACBrXmlNode;
    function GerarDetalheServico: TACBrXmlNode;
    function GerarItem: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_Giap101 }

  TNFSeW_Giap101 = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    procedure CarregarDadosdeConfiguracoes;
    function GerarXmlNfse: Boolean;
    function GerarNfse: TACBrXmlNode;
    function GerarInfNfse: TACBrXmlNode;
    function GerarPrestadorServico: TACBrXmlNode;
    function GerarIdenticacaoPrestador: TACBrXmlNode;
    function GerarEnderecoPrestador: TACBrXmlNode;
    function GerarContatoPrestador: TACBrXmlNode;
    function GerarOrgaoGerador: TACBrXmlNode;
  public
    function GerarXml: Boolean; Override;

  end;

implementation

uses
  ACBrNFSeXConversao,
  ACBrNFSeXConsts,
  ACBrNFSeX,
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Giap
//==============================================================================

{ TNFSeW_Giap }

procedure TNFSeW_Giap.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  if FpAOwner.ConfigGeral.Params.TemParametro('Aliquota4Casas') then
    FormatoAliq := tcDe4;
end;

function TNFSeW_Giap.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.DecimalChar := '.';

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('notaFiscal');

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                   NFSe.IdentificacaoRps.Serie;

  xmlNode := GerarDadosPrestador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDadosServico;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDadosTomador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDetalheServico;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_Giap.GerarDadosPrestador: TACBrXmlNode;
begin
  Result := CreateElement('dadosPrestador');

  Result.AppendChild(AddNode(tcDatVcto, '#1', 'dataEmissao', 1, 21, 1,
                                                      NFSe.DataEmissaoRps, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'im', 1, 11, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'numeroRps', 1, 11, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numeroNota', 1, 11, 0,
                                                              NFSe.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoVerificacao', 1, 11, 0,
                                                   NFSe.CodigoVerificacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'link', 1, 11, 0, NFSe.Link, ''));
end;

function TNFSeW_Giap.GerarDadosServico: TACBrXmlNode;
begin
  Result := CreateElement('dadosServico');

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 25, 1,
                                             NFSe.Servico.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 9, 1,
                                                NFSe.Servico.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cidade', 1, 30, 1,
                                         NFSe.Servico.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 30, 0,
                                        NFSe.Servico.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 50, 1,
                                           NFSe.Servico.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numero', 1, 10, 1,
                                             NFSe.Servico.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'pais', 1, 9, 1,
                                              NFSe.Servico.Endereco.xPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'uf', 1, 2, 1,
                                                 NFSe.Servico.Endereco.UF, ''));
end;

function TNFSeW_Giap.GerarDadosTomador: TACBrXmlNode;
begin
  Result := CreateElement('dadosTomador');

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 25, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 9, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cidade', 1, 50, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'documento', 1, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'email', 1, 14, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'ie', 1, 14, 0,
          OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 50, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'nomeTomador', 1, 120, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numero', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'pais', 1, 9, 1,
                                              NFSe.Tomador.Endereco.xPais, ''));

  if length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'tipoDoc', 1, 1, 1, 'F', ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'tipoDoc', 1, 1, 1, 'J', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'uf', 1, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));
end;

function TNFSeW_Giap.GerarDetalheServico: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('detalheServico');

  Result.AppendChild(AddNode(tcDe2, '#1', 'cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'csll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'deducaoMaterial', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'descontoIncondicional', 1, 15, 1,
                              NFSe.Servico.Valores.DescontoIncondicionado, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'inss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ir', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'issRetido', 1, 1, 1,
         FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), ''));

  xmlNode := GerarItem;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'obs', 1, 4000, 1,
    StringReplace(NFSe.OutrasInformacoes, Opcoes.QuebraLinha,
                      FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll]), ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'pisPasep', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));
end;

function TNFSeW_Giap.GerarItem: TACBrXmlNode;
begin
  Result := CreateElement('item');

  Result.AppendChild(AddNode(FormatoAliq, '#1', 'aliquota', 1, 15, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'cnae', 1, 8, 0,
                                      OnlyNumber(NFSe.Servico.CodigoCnae), ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'codigo', 1, 4, 1,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricao', 1, 4000, 1,
    StringReplace(NFSe.Servico.Discriminacao, Opcoes.QuebraLinha,
                     FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll] ), ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));
end;

{ TNFSeW_Giap101 }

procedure TNFSeW_Giap101.Configuracao;
begin
  inherited Configuracao;

  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  DivAliq100  := True;

  if FpAOwner.ConfigGeral.Params.TemParametro('NaoDividir100') then
    DivAliq100 := False;

  PrefixoPadrao := 'ns4';

  GerarTagTomadorMesmoVazia := True;
end;

function TNFSeW_Giap101.GerarXml: Boolean;
begin
  if NFSe.OptanteSimplesNacional = snSim then
    NrOcorrAliquota := 1;

  if NFSe.tpXML = txmlNFSe then
    Result := GerarXmlNFSe
  else
    Result := inherited GerarXml;
end;

procedure TNFSeW_Giap101.CarregarDadosdeConfiguracoes;
begin
  // Dados do Prestador
  NFSe.Prestador.RazaoSocial := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.RazSocial;
  NFSe.Prestador.NomeFantasia := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.NomeFantasia;
  NFSe.Prestador.Endereco.Endereco := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Endereco;
  NFSe.Prestador.Endereco.Numero := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Numero;
  NFSe.Prestador.Endereco.Complemento := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Complemento;
  NFSe.Prestador.Endereco.Bairro := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Bairro;
  NFSe.Prestador.Endereco.CodigoMunicipio := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.CodigoMunicipio;
  NFSe.Prestador.Endereco.UF := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.UF;
  NFSe.Prestador.Endereco.CEP := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.CEP;
  NFSe.Prestador.Contato.Telefone := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Telefone;
  NFSe.Prestador.Contato.Email := TACBrNFSeX(FpAOwner).Configuracoes.Geral.Emitente.DadosEmitente.Email;
end;

function TNFSeW_Giap101.GerarXmlNfse: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('CompNfse');

//  if FpAOwner.ConfigMsgDados.XmlRps.xmlns <> '' then
//    NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

//  if FormatoDiscriminacao <> fdNenhum then
//    ConsolidarVariosItensServicosEmUmSo;

  xmlNode := GerarNfse;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_Giap101.GerarNfse: TACBrXmlNode;
begin
  Result := CreateElement('Nfse');

  Result.AppendChild(GerarInfNfse);
end;

function TNFSeW_Giap101.GerarInfNfse: TACBrXmlNode;
begin
  Result := CreateElement('InfNfse');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                                                              NFSe.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoVerificacao', 1, 15, 1,
                                                   NFSe.CodigoVerificacao, ''));

  Result.AppendChild(AddNode(FormatoEmissao, '#4', 'DataEmissao', 19, 19, 1,
                                                  NFSe.DataEmissao, DSC_DHEMI));

  Result.AppendChild(GerarIdentificacaoRps);

  Result.AppendChild(AddNode(FormatoEmissao, '#4', 'DataEmissaoRps', 19, 19, 1,
                                                  NFSe.DataEmissao, DSC_DHEMI));

  Result.AppendChild(AddNode(tcStr, '#5', 'NaturezaOperacao', 1, 3, NrOcorrNaturezaOperacao,
                   NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP));

  Result.AppendChild(AddNode(tcStr, '#7', 'OptanteSimplesNacional', 1, 1, NrOcorrOptanteSN,
   FpAOwner.SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN));

  Result.AppendChild(AddNode(tcStr, '#8', 'IncentivadorCultural', 1, 1, NrOcorrIncentCult,
   FpAOwner.SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCCULT));

  Result.AppendChild(AddNode(FormatoEmissao, '#4', 'Competencia', 19, 19, 1,
                                                  NFSe.Competencia, DSC_DHEMI));

  Result.AppendChild(GerarServico);
  Result.AppendChild(GerarPrestadorServico);
  Result.AppendChild(GerarTomador);
  Result.AppendChild(GerarOrgaoGerador);
  Result.AppendChild(GerarConstrucaoCivil);
end;

function TNFSeW_Giap101.GerarPrestadorServico: TACBrXmlNode;
begin
  Result := CreateElement('PrestadorServico');

  Result.AppendChild(GerarIdenticacaoPrestador);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 60, 1,
                                               NFSe.Prestador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'NomeFantasia', 1, 60, 1,
                                              NFSe.Prestador.NomeFantasia, ''));

  Result.AppendChild(GerarEnderecoPrestador);
  Result.AppendChild(GerarContatoPrestador);
end;

function TNFSeW_Giap101.GerarIdenticacaoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoPrestador');

  if NFSe.Prestador.IdentificacaoPrestador.CpfCnpj <> '' then
    Result.AppendChild(GerarCPFCNPJ(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#37', 'InscricaoMunicipal', 1, 15, NrOcorrInscMunTomador,
             NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, DSC_IM));
end;

function TNFSeW_Giap101.GerarEnderecoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#39', 'Endereco', 1, 125, 0,
                                   NFSe.Prestador.Endereco.Endereco, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#40', 'Numero', 1, 10, 0,
                                      NFSe.Prestador.Endereco.Numero, DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#41', 'Complemento', 1, 60, NrOcorrComplTomador,
                                NFSe.Prestador.Endereco.Complemento, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#42', 'Bairro', 1, 60, 0,
                                  NFSe.Prestador.Endereco.Bairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0,
                OnlyNumber(NFSe.Prestador.Endereco.CodigoMunicipio), DSC_CMUN));

  Result.AppendChild(AddNode(tcStr, '#44', 'Uf', 2, 2, 0,
                                           NFSe.Prestador.Endereco.UF, DSC_UF));

  Result.AppendChild(AddNode(tcStr, '#45', 'Cep', 8, 8, 0,
                             OnlyNumber(NFSe.Prestador.Endereco.CEP), DSC_CEP));
end;

function TNFSeW_Giap101.GerarContatoPrestador: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.Prestador.Contato.Telefone <> '') or (NFSe.Prestador.Contato.Email <> '') then
  begin
    Result := CreateElement('Contato');

    Result.AppendChild(AddNode(tcStr, '#46', 'Telefone', 1, 11, NrOcorrFoneTomador,
                        OnlyNumber(NFSe.Prestador.Contato.Telefone), DSC_FONE));

    Result.AppendChild(AddNode(tcStr, '#47', 'Email', 1, 80, NrOcorrEmailTomador,
                                      NFSe.Prestador.Contato.Email, DSC_EMAIL));
  end;
end;

function TNFSeW_Giap101.GerarOrgaoGerador: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.OrgaoGerador.CodigoMunicipio <> '') or (NFSe.OrgaoGerador.Uf <> '') then
  begin
    Result := CreateElement('OrgaoGerador');

    Result.AppendChild(AddNode(tcStr, '#46', 'CodigoMunicipio', 7, 7, 1,
                                  NFSe.OrgaoGerador.CodigoMunicipio, DSC_FONE));

    Result.AppendChild(AddNode(tcStr, '#47', 'Uf', 2, 2, 1,
                                              NFSe.OrgaoGerador.Uf, DSC_EMAIL));
  end;
end;

end.
