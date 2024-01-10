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

unit Agili.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils, MaskUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { Provedor com layout próprio }
  { TNFSeW_Agili }

  TNFSeW_Agili = class(TNFSeWClass)
  private
    FpAtividadeEconomica: string;
    FpNrOcorrCodigoCnae: Integer;
    FpNrOcorrItemLei116: Integer;

  protected
    procedure Configuracao; override;

    function FormatarCnae(Codigo: string): string;

    function GerarInfDeclaracaoPrestacaoServico: TACBrXmlNode;
    function GerarIdentificacaoPrestador: TACBrXmlNode;
    function GerarRps: TACBrXmlNode;
    function GerarDadosTomador: TACBrXmlNode;
    function GerarIdentificacaoTomador: TACBrXmlNode;
    function GerarEnderecoExterior: TACBrXmlNode;
    function GerarPais: TACBrXmlNode;
    function GerarEndereco: TACBrXmlNode;
    function GerarMunicipio: TACBrXmlNode;
    function GerarContato: TACBrXmlNode;
    function GerarIntermediario: TACBrXmlNode;
    function GerarIdentificacaoIntermediario: TACBrXmlNode;
    function GerarConstrucaoCivil: TACBrXmlNode;
    function GerarRegimeEspecialTributacao: TACBrXmlNode;
    function GerarResponsavelISSQN: TACBrXmlNode;
    function GerarExigibilidadeISSQN: TACBrXmlNode;
    function GerarMunicipioIncidencia: TACBrXmlNode;
    function GerarListaServico: TACBrXmlNode;
    function GerarDadosServico: TACBrXmlNodeArray;
    function GerarIdentificacaoRps: TACBrXmlNode;
    function GerarDadosProfissionalParceiro(Indice: Integer): TACBrXmlNode;
    function GerarIdentificacaoProfissionalParceiro(Indice: Integer): TACBrXmlNode;

  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Agili
//==============================================================================

{ TNFSeW_Agili }

procedure TNFSeW_Agili.Configuracao;
begin
  inherited Configuracao;

  FpAtividadeEconomica := Trim(FpAOwner.ConfigGeral.Params.ValorParametro('NomeTagAtividadeEconomica'));

  if FpAtividadeEconomica = '' then
    FpAtividadeEconomica := 'CodigoCnaeAtividadeEconomica';

  FpNrOcorrCodigoCnae := 1;
  FpNrOcorrItemLei116 := 1;

  if FpAOwner.ConfigGeral.Params.ParamTemValor('NaoGerarTag', 'CodigoCnae') then
    FpNrOcorrCodigoCnae := -1;

  if FpAOwner.ConfigGeral.Params.ParamTemValor('NaoGerarTag', 'ItemLei116') then
    FpNrOcorrItemLei116 := -1;
end;

function TNFSeW_Agili.FormatarCnae(Codigo: string): string;
begin
  Result := OnlyNumber(Codigo);

  if Length(Result) <> 7 then
    Exit;

  Result := FormatMaskText('99.9.9-9.99;0', Result)
end;

function TNFSeW_Agili.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.xmlRps.xmlns, self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                   NFSe.IdentificacaoRps.Serie;

  xmlNode := GerarInfDeclaracaoPrestacaoServico;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_Agili.GerarConstrucaoCivil: TACBrXmlNode;
begin
  Result := CreateElement('ConstrucaoCivil');

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoObra', 1, 15, 1,
                                          NFSe.ConstrucaoCivil.CodigoObra, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Art', 1, 15, 1,
                                                 NFSe.ConstrucaoCivil.Art, ''));
end;

function TNFSeW_Agili.GerarContato: TACBrXmlNode;
begin
  Result := CreateElement('Contato');

  Result.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 14, 0,
                                OnlyNumber(NFSe.Tomador.Contato.Telefone), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Email', 1, 300, 0,
                                               NFSe.Tomador.Contato.Email, ''));
end;

function TNFSeW_Agili.GerarIdentificacaoProfissionalParceiro(
  Indice: Integer): TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoProfissionalParceiro');

  Result.AppendChild(GerarCPFCNPJ(NFSe.Servico.ItemServico[Indice].DadosProfissionalParceiro.IdentificacaoParceiro.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 100, 1,
   NFSe.Servico.ItemServico[Indice].DadosProfissionalParceiro.IdentificacaoParceiro.InscricaoMunicipal, ''));
end;

function TNFSeW_Agili.GerarDadosProfissionalParceiro(Indice: Integer): TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('DadosProfissionalParceiro');

  xmlNode := GerarIdentificacaoProfissionalParceiro(Indice);
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 120, 1,
   NFSe.Servico.ItemServico[Indice].DadosProfissionalParceiro.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'PercentualProfissionalParceiro', 1, 5, 1,
   NFSe.Servico.ItemServico[Indice].DadosProfissionalParceiro.PercentualProfissionalParceiro, ''));
end;

function TNFSeW_Agili.GerarDadosServico: TACBrXmlNodeArray;
var
  i: integer;
  CodServico: string;
  xmlNode: TACBrXmlNode;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    CodServico := IntToStr(StrToIntDef(OnlyNumber(NFSe.Servico.ItemServico[i].CodServ), 0));

    if Length(CodServico) > 2 then
      Insert('.', CodServico, Length(CodServico) - 2 + 1);

    Result[i] := CreateElement('DadosServico');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'Discriminacao', 1, 2000, 1,
      StringReplace( NFSe.Servico.ItemServico[i].Descricao, ';',
        FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'CodigoCnae', 1, 7, FpNrOcorrCodigoCnae,
                     FormatarCnae(NFSe.Servico.ItemServico[i].CodigoCnae), ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'ItemLei116', 1, 140, FpNrOcorrItemLei116,
                                                               CodServico, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'Quantidade', 1, 17, 1,
                                   NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'ValorServico', 1, 15, 1,
                                NFSe.Servico.ItemServico[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'ValorDesconto', 1, 15, 1,
                       NFSe.Servico.ItemServico[i].DescontoIncondicionado, ''));

    if Trim(NFSe.Servico.ItemServico[i].DadosProfissionalParceiro.IdentificacaoParceiro.CpfCnpj) <> EmptyStr then
    begin
      xmlNode := GerarDadosProfissionalParceiro(i);
      Result[i].AppendChild(xmlNode);
    end;
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#1', 'DadosServico', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_Agili.GerarDadosTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('DadosTomador');

  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    if NFSe.Tomador.Endereco.UF <> 'EX' then
    begin
      xmlNode := GerarIdentificacaoTomador;
      Result.AppendChild(xmlNode);
    end;

    Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

    if NFSe.Tomador.Endereco.UF = 'EX' then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'LocalEndereco', 1, 1, 1, '2', ''));

      xmlNode := GerarEnderecoExterior;
      Result.AppendChild(xmlNode);
    end
    else
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'LocalEndereco', 1, 1, 1, '1', ''));

      xmlNode := GerarEndereco;
      Result.AppendChild(xmlNode);
    end;

    if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
    begin
      xmlNode := GerarContato;
      Result.AppendChild(xmlNode);
    end;

    Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoEstadual', 1, 20, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));
  end;
end;

function TNFSeW_Agili.GerarEndereco: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'TipoLogradouro', 1, 120, 1,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));


  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 120, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Complemento', 1, 300, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 120, 0,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  xmlNode := GerarMunicipio;
  Result.AppendChild(xmlNode);

  xmlNode := GerarPais;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'Cep', 8, 8, 0,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));
end;

function TNFSeW_Agili.GerarEnderecoExterior: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('EnderecoExterior');

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 125, 0,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  xmlNode := GerarPais;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_Agili.GerarExigibilidadeISSQN: TACBrXmlNode;
begin
  Result := CreateElement('ExigibilidadeISSQN');

  Result.AppendChild(AddNode(tcStr, '#1', 'Codigo', 1, 1, 1,
            FpAOwner.ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 30, 0, '', ''));
end;

function TNFSeW_Agili.GerarIdentificacaoIntermediario: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoIntermediario');

  xmlNode := GerarCPFCNPJ(NFSe.Intermediario.Identificacao.CpfCnpj);
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                      NFSe.Intermediario.Identificacao.InscricaoMunicipal, ''));
end;

function TNFSeW_Agili.GerarIdentificacaoPrestador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoPrestador');

  Result.AppendChild(AddNode(tcStr, '#1', 'ChaveDigital', 32, 32, 1,
                                                              ChaveAcesso, ''));

  xmlNode := GerarCPFCNPJ(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj);
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));
end;

function TNFSeW_Agili.GerarIdentificacaoRps: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                         OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS));

  Result.AppendChild(AddNode(tcStr, '#1', 'Serie', 1, 5, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

  Result.AppendChild(AddNode(tcStr, '#1', 'Tipo', 1, 1, 1,
               FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS));
end;

function TNFSeW_Agili.GerarIdentificacaoTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoTomador');

  xmlNode := GerarCPFCNPJ(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));
end;

function TNFSeW_Agili.GerarInfDeclaracaoPrestacaoServico: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('InfDeclaracaoPrestacaoServico');

  xmlNode := GerarIdentificacaoPrestador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'NfseSubstituida', 1, 15, 0,
                                         OnlyNumber(NFSe.NfseSubstituida), ''));

  xmlNode := GerarRps;
  Result.AppendChild(xmlNode);

  xmlNode := GerarDadosTomador;
  Result.AppendChild(xmlNode);

  if (NFSe.Intermediario.RazaoSocial <> '') or
     (NFSe.Intermediario.Identificacao.CpfCnpj <> '') then
  begin
    xmlNode := GerarIntermediario;
    Result.AppendChild(xmlNode);
  end;

  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    xmlNode := GerarConstrucaoCivil;
    Result.AppendChild(xmlNode);
  end;

  if FpAOwner.RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao) <> '' then
  begin
    xmlNode := GerarRegimeEspecialTributacao;
    Result.AppendChild(xmlNode);
  end;

  Result.AppendChild(AddNode(tcStr, '#1', 'OptanteSimplesNacional', 1, 1, 1,
                        FpAOwner.SimNaoToStr(NFSe.OptanteSimplesNacional), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'OptanteMEISimei', 1, 1, 1,
                               FpAOwner.SimNaoToStr(NFSe.OptanteMEISimei), ''));

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    Result.AppendChild(AddNode(tcStr, '#1', 'ISSQNRetido', 1, 1, 1,
                                               FpAOwner.SimNaoToStr(snSim), ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'ISSQNRetido', 1, 1, 1,
                                              FpAOwner.SimNaoToStr(snNao), ''));

  if NFSe.Servico.Valores.IssRetido <> stNormal then
  begin
    xmlNode := GerarResponsavelISSQN;
    Result.AppendChild(xmlNode);
  end;

  if NaoEstaVazio(NFSe.Servico.CodigoTributacaoMunicipio) then
    Result.AppendChild(AddNode(tcStr, '#1', FpAtividadeEconomica, 1, 140, 1,
                                    NFSe.Servico.CodigoTributacaoMunicipio, ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', FpAtividadeEconomica, 1, 140, 1,
                                    FormatarCnae(NFSe.Servico.CodigoCnae), ''));

  xmlNode := GerarExigibilidadeISSQN;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'BeneficioProcesso', 1, 30, 0,
                                              NFSe.Servico.NumeroProcesso, ''));
  if NFSe.OptanteMEISimei = snNao then
  begin
    xmlNode := GerarMunicipioIncidencia;
    Result.AppendChild(xmlNode);
  end;

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorServicos', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorDescontos', 1, 15, 1,
                              NFSe.Servico.Valores.DescontoIncondicionado, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorPis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorCofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorInss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorIrrf', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorCsll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorOutrasRetencoes', 1, 15, 1,
                                     NFSe.Servico.Valores.OutrasRetencoes, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorBaseCalculoISSQN', 1, 15, 0,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaISSQN', 1, 5, 0,
                                            NFSe.Servico.Valores.Aliquota, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorISSQNCalculado', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorIss, ''));

  if (NFSe.OptanteSimplesNacional = snNao) and (NFSe.OptanteMEISimei = snNao) then
    Result.AppendChild(AddNode(tcDe2, '#1', 'ValorISSQNRecolher', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorIss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorDeducaoConstCivil', 1, 15, 1,
                                                                        0, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorLiquido', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Observacao', 1, 4000, 0,
                                                   NFSe.OutrasInformacoes, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Complemento', 1, 3000, 0, '', ''));

  xmlNode := GerarListaServico;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'Versao', 4, 4, 1, '1.00', ''));
end;

function TNFSeW_Agili.GerarIntermediario: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Intermediario');

  xmlNode := GerarIdentificacaoIntermediario;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                           NFSe.Intermediario.RazaoSocial, ''));
end;

function TNFSeW_Agili.GerarListaServico: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := CreateElement('ListaServico');

  nodeArray := GerarDadosServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_Agili.GerarMunicipio: TACBrXmlNode;
begin
  Result := CreateElement('Municipio');

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipioIBGE', 7, 7, 0,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 300, 0,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 2, 2, 0,
                                                 NFSe.Tomador.Endereco.UF, ''));
end;

function TNFSeW_Agili.GerarMunicipioIncidencia: TACBrXmlNode;
begin
  Result := CreateElement('MunicipioIncidencia');

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipioIBGE', 7, 7, 1,
                                         NFSe.Servico.MunicipioIncidencia, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 30, 0, '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 2, 2, 0, '', ''));
end;

function TNFSeW_Agili.GerarPais: TACBrXmlNode;
begin
  Result := CreateElement('Pais');

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoPaisBacen', 4, 4, 1,
                                         NFSe.Tomador.Endereco.CodigoPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 300, 0,
                                              NFSe.Tomador.Endereco.xPais, ''));
end;

function TNFSeW_Agili.GerarRegimeEspecialTributacao: TACBrXmlNode;
begin
  Result := CreateElement('RegimeEspecialTributacao');

  Result.AppendChild(AddNode(tcStr, '#1', 'Codigo', 1, 1, 1,
    FpAOwner.RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 30, 0, '', ''));
end;

function TNFSeW_Agili.GerarResponsavelISSQN: TACBrXmlNode;
begin
  Result := CreateElement('ResponsavelISSQN');

  Result.AppendChild(AddNode(tcStr, '#1', 'Codigo', 1, 1, 1,
      FpAOwner.ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Descricao', 1, 30, 0, '', ''));
end;

function TNFSeW_Agili.GerarRps: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Rps');

  xmlNode := GerarIdentificacaoRps;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcDat, '#1', 'DataEmissao', 10, 10, 1,
                                                  NFSe.DataEmissao, DSC_DHEMI));
end;

end.
