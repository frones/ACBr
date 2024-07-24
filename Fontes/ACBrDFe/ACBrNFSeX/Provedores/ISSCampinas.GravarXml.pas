{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ISSCampinas.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils, synacode,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_ISSCampinas }

  TNFSeW_ISSCampinas = class(TNFSeWClass)
  private
    FPTipoRecolhimento: String;
    FPSituacao: String;
  protected
    function GerarDeducoes: TACBrXmlNode;
    function GerarDeducao: TACBrXmlNodeArray;
    function GerarListaServicos: TACBrXmlNode;
    function GerarItem: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

  TNFSeW_ISSCampinas203 = class(TNFSeW_ABRASFv2)
  protected

  public

  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.Math,
  ACBrDFeUtil,
  ACBrNFSeXConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ISSCampinas
//==============================================================================

{ TNFSeW_ISSCampinas }

function TNFSeW_ISSCampinas.GerarDeducao: TACBrXmlNodeArray;
var
  i: integer;
  sDeducaoPor, sTipoDeducao: String;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.Deducao.Count);

  for i := 0 to NFSe.Servico.Deducao.Count - 1 do
  begin
    Result[i] := CreateElement('Deducao');

    sDeducaoPor := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].DeducaoPor,
                              ['Percentual', 'Valor'], [dpPercentual, dpValor]);

    sTipoDeducao := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].TipoDeducao,
      ['', 'Despesas com Materiais', 'Despesas com Subempreitada',
       'Deducao de Valor', 'Servicos de Veiculacao e Divulgacao'],
      [tdNenhum, tdMateriais, tdSubEmpreitada, tdValor, tdVeiculacao]);

    Result[i].AppendChild(AddNode(tcStr, '#', 'DeducaoPor', 1, 20, 1,
                                                              sDeducaoPor, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'TipoDeducao', 0, 30, 1,
                                                             sTipoDeducao, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'CPFCNPJReferencia', 0, 14, 0,
              OnlyNumber(NFSe.Servico.Deducao.Items[i].CpfCnpjReferencia), ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'NumeroNFReferencia', 0, 10, 0,
                         NFSe.Servico.Deducao.Items[i].NumeroNFReferencia, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'ValorTotalReferencia', 0, 18, 0,
                       NFSe.Servico.Deducao.Items[i].ValorTotalReferencia, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'PercentualDeduzir', 0, 18, 1,
                          NFSe.Servico.Deducao.Items[i].PercentualDeduzir, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'ValorDeduzir', 0, 18, 1,
                               NFSe.Servico.Deducao.Items[i].ValorDeduzir, ''));
  end;

  if NFSe.Servico.Deducao.Count > 10 then
    wAlerta('#', 'Deducao', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_ISSCampinas.GerarDeducoes: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if NFSe.Servico.Deducao.Count > 0 then
  begin
    Result := CreateElement('Deducoes');

    nodeArray := GerarDeducao;
    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
end;

function TNFSeW_ISSCampinas.GerarItem: TACBrXmlNodeArray;
var
  i: integer;
  sTributavel: String;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('Item');

    sTributavel := EnumeradoToStr( NFSe.Servico.ItemServico[i].Tributavel,
                                                    ['S', 'N'], [snSim, snNao]);

    Result[i].AppendChild(AddNode(tcStr, '#', 'DiscriminacaoServico', 1, 80, 1,
                              NFSe.Servico.ItemServico[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#', 'Quantidade', 1, 15, 1,
                             NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#', 'ValorUnitario', 1, 20, 1,
                          NFSe.Servico.ItemServico[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'ValorTotal', 1, 18, 1,
                             NFSe.Servico.ItemServico[i].ValorTotal, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'Tributavel', 1, 1, 0,
                                                              sTributavel, ''));
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#', 'Item', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_ISSCampinas.GerarListaServicos: TACBrXmlNode;
var
  i: Integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if NFSe.Servico.ItemServico.Count > 0 then
  begin
    Result := CreateElement('Itens');

    nodeArray := GerarItem;
    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
end;

function TNFSeW_ISSCampinas.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
  sIEEmit, SerieRPS, NumeroRPS, sDataEmis,
  sTributacao, sSituacaoRPS, sTipoRecolhimento,
  sValorServico, sValorDeducao, sCodAtividade,
  sCPFCNPJTomador, sAssinatura: String;
begin
  Configuracao;

  Opcoes.SuprimirDecimais := True;
  Opcoes.DecimalChar := '.';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSe.InfID.ID := 'Rps_' + NFSe.IdentificacaoRps.Numero;

  NFSeNode := CreateElement('RPS');
  NFSeNode.SetAttribute(FpAOwner.ConfigGeral.Identificador, NFSe.infID.ID);

  FDocument.Root := NFSeNode;

  FPSituacao := EnumeradoToStr( NFSe.StatusRps, ['N','C'], [srNormal, srCancelado]);

  sIEEmit           := Poem_Zeros(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 11);
  SerieRPS          := PadRight( NFSe.IdentificacaoRps.Serie, 5 , ' ');
  NumeroRPS         := Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12);
  sDataEmis         := FormatDateTime('yyyymmdd',NFse.DataEmissaoRps);
  sTributacao       := FpAOwner.TributacaoToStr(NFSe.Servico.Tributacao) + ' ';
  sSituacaoRPS      := FPSituacao;
  sTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido, ['N','S'], [stNormal, stRetencao]);

  sValorServico   := Poem_Zeros( OnlyNumber( FormatFloat('#0.00',
                                (NFSe.Servico.Valores.ValorServicos -
                                 NFSe.Servico.Valores.ValorDeducoes) ) ), 15);
  sValorDeducao   := Poem_Zeros( OnlyNumber( FormatFloat('#0.00',
                                 NFSe.Servico.Valores.ValorDeducoes)), 15);
  sCodAtividade   := PadRight(NFSe.Servico.CodigoCnae, 9, '0');
  sCodAtividade   := Poem_Zeros( OnlyNumber( sCodAtividade ), 10);
  sCPFCNPJTomador := Poem_Zeros( OnlyNumber( NFSe.Tomador.IdentificacaoTomador.CpfCnpj), 14);


  sAssinatura := sIEEmit + SerieRPS + NumeroRPS + sDataEmis + sTributacao +
                 sSituacaoRPS + sTipoRecolhimento + sValorServico +
                 sValorDeducao + sCodAtividade + sCPFCNPJTomador;

  sAssinatura := AsciiToHex(SHA1(AnsiString(sAssinatura)));
  sAssinatura := LowerCase(sAssinatura);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Assinatura', 1, 2000, 1,
                                                              sAssinatura, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipalPrestador', 1, 11, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RazaoSocialPrestador', 1, 120, 1,
                                               NFSe.Prestador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoRPS', 1, 3, 1, 'RPS', ''));

  if NFSe.IdentificacaoRps.Serie = '' then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerieRPS', 1, 2, 1, 'NF', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerieRPS', 1, 2, 1,
                                              NFSe.IdentificacaoRps.Serie, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroRPS', 1, 12, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcDatHor, '#1', 'DataEmissaoRPS', 1, 21, 1,
                                                      NFSe.DataEmissaoRps, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SituacaoRPS', 1, 1, 1,
                                                               FPSituacao, ''));

  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    if NFSe.RpsSubstituido.Serie = '' then
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerieRPSSubstituido', 0, 2, 1,
                                                                      'NF', ''))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerieRPSSubstituido', 0, 2, 1,
                                                NFSe.RpsSubstituido.Serie, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroNFSeSubstituida', 0, 12, 1,
                                               NFSe.RpsSubstituido.Numero, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroRPSSubstituido', 0, 12, 1,
                                               NFSe.RpsSubstituido.Numero, ''));
  end;

  if (NFSe.SeriePrestacao = '') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SeriePrestacao', 1, 2, 1,
                                                                      '99', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SeriePrestacao', 1, 2, 1,
                                                      NFSe.SeriePrestacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipalTomador', 1, 11, 1,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CPFCNPJTomador', 1, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RazaoSocialTomador', 1, 120, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DocTomadorEstrangeiro', 0, 20, 0,
                         NFSe.Tomador.IdentificacaoTomador.DocEstrangeiro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoLogradouroTomador', 0, 10, 1,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LogradouroTomador', 1, 50, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroEnderecoTomador', 1, 9, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ComplementoEnderecoTomador', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoBairroTomador', 0, 10, 1,
                                         NFSe.Tomador.Endereco.TipoBairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'BairroTomador', 1, 50, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CidadeTomador', 1, 10, 0,
   CodIBGEToCodTOM(StrToInt64Def(NFSe.Tomador.Endereco.CodigoMunicipio, 0)), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CidadeTomadorDescricao', 1, 50, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CEPTomador', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EmailTomador', 1, 60, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipalObra', 1, 11, 0,
                            NFSe.ConstrucaoCivil.Endereco.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ServicoObra', 1, 2, 0,
                                          NFSe.ConstrucaoCivil.CodigoObra, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodigoAtividade', 1, 9, 1,
                                PadRight(NFSe.Servico.CodigoCnae, 9, '0'), ''));

  if VersaoNFSe = ve101 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodigoServico', 4, 5, 0,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaAtividade', 1, 11, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  // "A" a receber; "R" retido na Fonte
  FPTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                       ['A','R'], [stNormal, stRetencao]);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoRecolhimento', 1, 1, 1,
                                                       FPTipoRecolhimento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunicipioPrestacao', 1, 10, 1,
          CodIBGEToCodTOM(strtoint64Def(NFSe.Servico.CodigoMunicipio, 0)), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunicipioPrestacaoDescricao', 1, 30, 1,
                                       NFSe.Prestador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Operacao', 1, 1, 1,
                                     OperacaoToStr(NFSe.Servico.Operacao), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Tributacao', 1, 1, 1,
                        FpAOwner.TributacaoToStr(NFSe.Servico.Tributacao), ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorPIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCOFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorINSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorIR', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaPIS', 1, 6, 1,
                                         NFSe.Servico.Valores.AliquotaPIS, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaCOFINS', 1, 6, 1,
                                      NFSe.Servico.Valores.AliquotaCOFINS, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaINSS', 1, 6, 1,
                                        NFSe.Servico.Valores.AliquotaINSS, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaIR', 1, 6, 1,
                                          NFSe.Servico.Valores.AliquotaIR, ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'AliquotaCSLL', 1, 6, 1,
                                        NFSe.Servico.Valores.AliquotaCSLL, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DescricaoRPS', 1, 1500, 1,
                                                   NFSe.OutrasInformacoes, ''));

  if Length(OnlyNumber(NFSe.Prestador.Contato.Telefone)) = 11 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDPrestador', 0, 3, 1,
             LeftStr(OnlyNumber(NFSe.Prestador.Contato.Telefone),3), ''))
  else
    if Length(OnlyNumber(NFSe.Prestador.Contato.Telefone)) = 10 then
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDPrestador', 0, 3, 1,
             LeftStr(OnlyNumber(NFSe.Prestador.Contato.Telefone),2), ''))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDPrestador', 0, 3, 1, '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TelefonePrestador', 0, 8, 1,
           RightStr(OnlyNumber(NFSe.Prestador.Contato.Telefone),8), ''));

  if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDTomador', 0, 3, 1,
             LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),3), ''))
  else
    if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDTomador', 0, 3, 1,
             LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),2), ''))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DDDTomador', 0, 3, 1, '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TelefoneTomador', 0, 8, 1,
           RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),8), ''));

  if (NFSe.StatusRps = srCancelado) then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MotCancelamento', 1, 80, 1,
                                                   NFSE.MotivoCancelamento, ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MotCancelamento', 0, 80, 0,
                                                  NFSE.MotivoCancelamento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CPFCNPJIntermediario', 0, 14, 0,
    OnlyNumber(NFSe.Intermediario.Identificacao.CpfCnpj), ''));

  xmlNode := GerarDeducoes;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarListaServicos;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

end.
