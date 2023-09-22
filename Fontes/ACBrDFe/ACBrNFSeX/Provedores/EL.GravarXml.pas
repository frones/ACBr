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

unit EL.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXGravarXml_ABRASFv2,
  ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_EL }

  TNFSeW_EL = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarIdentificacaoRPS: TACBrXmlNode;
    function GerarDadosPrestador: TACBrXmlNode;
    function GerarIdentificaoPrestador: TACBrXmlNode;
    function GerarEnderecoPrestador: TACBrXmlNode;
    function GerarContatoPrestador: TACBrXmlNode;

    function GerarDadosTomador: TACBrXmlNode;
    function GerarIdentificaoTomador: TACBrXmlNode;
    function GerarEnderecoTomador: TACBrXmlNode;
    function GerarContatoTomador: TACBrXmlNode;

    function GerarIntermediarioServico: TACBrXmlNode;
    function GerarServicos: TACBrXmlNode;
    function GerarServico: TACBrXmlNodeArray;
    function GerarValores: TACBrXmlNode;
    function GerarRpsSubstituido: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_EL204 }

  TNFSeW_EL204 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     EL
//==============================================================================

{ TNFSeW_EL }

function TNFSeW_EL.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
  LocPrest: string;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps');

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := StringOfChar('0', 15) +
                    OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie;
  NFSe.InfID.ID := copy(NFSe.InfID.ID, length(NFSe.InfID.ID) - 15 + 1, 15);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Id', 1, 15, 1, NFSe.InfID.ID, ''));

  // Código para identificação do local de prestação do serviço:
  // 1-Fora do município 2-No município
  LocPrest := '2';

  if (NFSe.Prestador.Endereco.CodigoMunicipio <> NFSe.Servico.CodigoMunicipio) then
    LocPrest := '1';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LocalPrestacao', 1, 1, 1,
                                                                 LocPrest, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'IssRetido', 1, 1, 1,
         FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), ''));

  NFSeNode.AppendChild(AddNode(tcDatHor, '#1', 'DataEmissao', 19, 19, 1,
                                                   NFSe.DataEmissao, DSC_DEMI));

  xmlNode := GerarIdentificacaoRPS;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDadosPrestador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDadosTomador;
  NFSeNode.AppendChild(xmlNode);

  if (NFSe.Intermediario.RazaoSocial <> '') or
     (NFSe.Intermediario.Identificacao.CpfCnpj <> '') then
  begin
    xmlNode := GerarIntermediarioServico;
    NFSeNode.AppendChild(xmlNode);
  end;

  xmlNode := GerarServicos;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarValores;
  NFSeNode.AppendChild(xmlNode);

  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    xmlNode := GerarRpsSubstituido;
    NFSeNode.AppendChild(xmlNode);
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Observacao', 1, 255, 0,
                                                   NFSe.OutrasInformacoes, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Status', 1, 1, 1,
                                           StatusRPSToStr(NFSe.StatusRps), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipioPrestacao', 7, 7, 0,
                                 OnlyNumber(NFSe.Servico.CodigoMunicipio), ''));

  Result := True;
end;

procedure TNFSeW_EL.Configuracao;
begin
  inherited Configuracao;

  DivAliq100 := True;
end;

function TNFSeW_EL.GerarContatoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('Contato');

  Result.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 11, 0,
                              OnlyNumber(NFSe.Prestador.Contato.Telefone), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Email', 1, 80, 1,
                                             NFSe.Prestador.Contato.Email, ''));
end;

function TNFSeW_EL.GerarContatoTomador: TACBrXmlNode;
begin
  Result := CreateElement('Contato');

  Result.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 11, 0,
                                OnlyNumber(NFSe.Tomador.Contato.Telefone), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Email', 1, 80, 1,
                                               NFSe.Tomador.Contato.Email, ''));
end;

function TNFSeW_EL.GerarDadosPrestador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('DadosPrestador');

  xmlNode := GerarIdentificaoPrestador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                               NFSe.Prestador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'NomeFantasia', 1, 115, 0,
                                              NFSe.Prestador.NomeFantasia, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'IncentivadorCultural', 1, 1, 1,
                          FpAOwner.SimNaoToStr(NFSe.IncentivadorCultural), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'OptanteSimplesNacional', 1, 1, 1,
                        FpAOwner.SimNaoToStr(NFSe.OptanteSimplesNacional), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'NaturezaOperacao', 1, 1, 1,
                             NaturezaOperacaoToStr(NFSe.NaturezaOperacao), ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'RegimeEspecialTributacao', 1, 1, 1,
    FpAOwner.RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), ''));

  xmlNode := GerarEnderecoPrestador;
  Result.AppendChild(xmlNode);

  xmlNode := GerarContatoPrestador;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_EL.GerarDadosTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('DadosTomador');

  xmlNode := GerarIdentificaoTomador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'NomeFantasia', 1, 115, 0,
                                                NFSe.Tomador.NomeFantasia, ''));

  xmlNode := GerarEnderecoTomador;
  Result.AppendChild(xmlNode);

  xmlNode := GerarContatoTomador;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_EL.GerarEnderecoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroTipo', 1, 125, 0,
                                   NFSe.Prestador.Endereco.TipoLogradouro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 125, 0,
                                         NFSe.Prestador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroNumero', 1, 10, 0,
                                           NFSe.Prestador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroComplemento', 1, 60, 0,
                                      NFSe.Prestador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 60, 0,
                                           NFSe.Prestador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipio', 7, 7, 0,
                      OnlyNumber(NFSe.Prestador.Endereco.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Municipio', 1, 100, 0,
                                       NFSe.Prestador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 2, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cep', 8, 8, 0,
                                  OnlyNumber(NFSe.Prestador.Endereco.CEP), ''));
end;

function TNFSeW_EL.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroTipo', 1, 125, 0,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 125, 0,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroNumero', 1, 10, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'LogradouroComplemento', 1, 60, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 60, 0,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipio', 7, 7, 0,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Municipio', 1, 100, 0,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cep', 8, 8, 0,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));
end;

function TNFSeW_EL.GerarIdentificacaoRPS: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                         OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS));

  Result.AppendChild(AddNode(tcStr, '#1', 'Serie', 1, 5, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

  Result.AppendChild(AddNode(tcStr, '#1', 'Tipo', 1, 1, 1,
               FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS));
end;

function TNFSeW_EL.GerarIdentificaoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoPrestador');

  Result.AppendChild(AddNode(tcStr, '#1', 'CpfCnpj', 11, 14, 1,
                OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  if Length(OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj)) <= 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                       '1', ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                      '2', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));
end;

function TNFSeW_EL.GerarIdentificaoTomador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoTomador');

  Result.AppendChild(AddNode(tcStr, '#1', 'CpfCnpj', 11, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                       '1', ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                      '2', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

//  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoEstadual', 1, 15, 0,
//                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));
end;

function TNFSeW_EL.GerarIntermediarioServico: TACBrXmlNode;
begin
  Result := CreateElement('IntermediarioServico');

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                           NFSe.Intermediario.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CpfCnpj', 14, 14, 1,
                     OnlyNumber(NFSe.Intermediario.Identificacao.CpfCnpj), ''));

  if Length(OnlyNumber(NFSe.Intermediario.Identificacao.CpfCnpj)) <= 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                       '1', ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'IndicacaoCpfCnpj', 1, 1, 1,
                                                                      '2', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                      NFSe.Intermediario.Identificacao.InscricaoMunicipal, ''));
end;

function TNFSeW_EL.GerarRpsSubstituido: TACBrXmlNode;
var
  InfIDSubstituido: string;
begin
  Result := CreateElement('RpsSubstituido');

  InfIDSubstituido := Poem_Zeros(OnlyNumber(NFSe.RpsSubstituido.Numero) +
                                 NFSe.RpsSubstituido.Serie, 15);

  Result.AppendChild(AddNode(tcStr, '#1', 'Id', 1, 15, 1, InfIDSubstituido, ''));
end;

function TNFSeW_EL.GerarServico: TACBrXmlNodeArray;
var
  i: integer;
  xAliquota: Double;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('Servico');

    Result[i].AppendChild(AddNode(tcStr, '#', 'CodigoCnae', 1, 07, 0,
                                   NFSe.Servico.ItemServico[i].CodigoCnae, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'CodigoServico116', 1, 5, 1,
                                    NFSe.Servico.ItemServico[i].CodLCServ, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'CodigoServicoMunicipal', 1, 20, 1,
                                      NFSe.Servico.ItemServico[i].CodServ, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'Quantidade', 1, 5, 1,
                                   NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'Unidade', 1, 20, 1,
                                      NFSe.Servico.ItemServico[i].Unidade, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'Descricao', 1, 255, 1,
                                    NFSe.Servico.ItemServico[i].Descricao, ''));

    xAliquota := NormatizarAliquota(NFSe.Servico.ItemServico[i].Aliquota, DivAliq100);

    Result[i].AppendChild(AddNode(FormatoAliq, '#', 'Aliquota', 1, 5, 1,
                                                                xAliquota, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#', 'ValorServico', 1, 15, 1,
                                   NFSe.Servico.ItemServico[i].ValorTotal, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#', 'ValorIssqn', 1, 15, 1,
                                     NFSe.Servico.ItemServico[i].ValorISS, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'ValorDesconto', 1, 15, 0,
                                NFSe.Servico.ItemServico[i].ValorDeducoes, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'NumeroAlvara', 1, 15, 0,
                                                                       '', ''));
  end;

  if NFSe.Servico.ItemServico.Count > 50 then
    wAlerta('#', 'Servico', '', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TNFSeW_EL.GerarServicos: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('Servicos');

  nodeArray := GerarServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_EL.GerarValores: TACBrXmlNode;
begin
  Result := CreateElement('Valores');

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorServicos', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorDeducoes', 1, 15, 0,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorPis', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorCofins', 1, 15, 0,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorInss', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorIr', 1, 15, 0,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorCsll', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorIss', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorIss, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorOutrasRetencoes', 1, 15, 0,
                                     NFSe.Servico.Valores.OutrasRetencoes, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorLiquidoNfse', 1, 15, 0,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'ValorIssRetido', 1, 15, 0,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'OutrosDescontos', 1, 15, 0,
                                     NFSe.Servico.Valores.OutrosDescontos, ''));
end;

{ TNFSeW_EL204 }

procedure TNFSeW_EL204.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  NrOcorrInformacoesComplemetares := 0;
  NrOcorrCepTomador := 1;
  NrOcorrCodigoPaisTomador := -1;

  TagTomador := 'TomadorServico';

//  GerarIDDeclaracao := False;
//  GerarIDRps := True;
end;

end.
