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

unit CTA.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXGravarXml;

type
  { Provedor com layout próprio }
  { TNFSeW_CTA200 }

  TNFSeW_CTA200 = class(TNFSeWClass)
  private
    FpValorTotalDosServicos: Double;
  protected
    function GerarIdentificacaoRps: TACBrXmlNode;
    function GerarServico: TACBrXmlNodeArray;
    function GerarValores: TACBrXmlNode;
    function GerarInformacoes: TACBrXmlNode;
    function GerarValoresRetencoes: TACBrXmlNode;
    function GerarTomador: TACBrXmlNode;
    function GerarEndereco: TACBrXmlNode;

  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConsts, ACBrNFSeXConversao,
  pcnConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     CTA
//==============================================================================

{ TNFSeW_CTA200 }

function TNFSeW_CTA200.GerarIdentificacaoRps: TACBrXmlNode;
var
  TipoTrib: string;
begin
  TipoTrib := FpAOwner.TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS);

  Result := CreateElement('IdentificacaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                         OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS));

  Result.AppendChild(AddNode(tcDatHor, '#2', 'Competencia', 19, 19, 1,
                                                   NFSe.Competencia, DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#3', 'TipodeTributacao', 1, 1, 1,
                                                                 TipoTrib, ''));
end;

function TNFSeW_CTA200.GerarServico: TACBrXmlNodeArray;
var
  i: integer;
  Descricao: string;
  Quantidade, ValorUnitario, ValorTotal: Double;
begin
  FpValorTotalDosServicos := 0;
  Result := nil;
  SetLength(Result, 5 {NFSe.Servico.ItemServico.Count});

  for i := 0 to 4 do
  begin
    Result[i] := CreateElement('Servico' + IntToStr(i+1));

    if i <= NFSe.Servico.ItemServico.Count - 1 then
    begin
      Quantidade := NFSe.Servico.ItemServico.Items[i].Quantidade;
      Descricao := NFSe.Servico.ItemServico.Items[i].Descricao;
      ValorUnitario := NFSe.Servico.ItemServico.Items[i].ValorUnitario;
      ValorTotal := Quantidade * ValorUnitario;
    end
    else
    begin
      Quantidade := 0;
      Descricao := '';
      ValorUnitario := 0;
      ValorTotal := 0;
    end;

    Result[i].AppendChild(AddNode(tcInt, '#1', 'QuantidadeDoItem', 1, 10, 1,
                                                               Quantidade, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'DescritivoDoItem', 1, 60, 1,
                                                                 Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'ValorUnitarioDoItem', 1, 15, 1,
                                                            ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'ValorTotalDoItem', 1, 15, 1,
                                                               ValorTotal, ''));

    FpValorTotalDosServicos := FpValorTotalDosServicos + ValorTotal;
  end;

  if NFSe.Servico.ItemServico.Count > 5 then
    wAlerta('#1', 'Servico', '', ERR_MSG_MAIOR_MAXIMO + '5');
end;

function TNFSeW_CTA200.GerarValores: TACBrXmlNode;
begin
  Result := CreateElement('Valores');

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorTotalDosServicos', 1, 15, 1,
                                                  FpValorTotalDosServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorDeducoes', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'DescontoIncondicionado', 1, 15, 1,
                 NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND));

  Result.AppendChild(AddNode(tcDe2, '#1', 'DescontoCondicionado', 1, 15, 1,
                     NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND));

  Result.AppendChild(AddNode(tcDe2, '#1', 'BaseDeCalculoDoISS', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Aliquota', 1, 5, 1,
                                     NFSe.Servico.Valores.Aliquota, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorIss', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIss, DSC_VISS));
end;

function TNFSeW_CTA200.GerarInformacoes: TACBrXmlNode;
var
  item: string;
begin
  Result := CreateElement('Informacoes');

  Result.AppendChild(AddNode(tcStr, '#1', 'IssRetido', 1, 1, 1,
    FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET));

  Result.AppendChild(AddNode(tcStr, '#1', 'ResponsavelRetencao', 1, 1, 1,
   FpAOwner.ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET));

  item := FormatarItemServico(NFSe.Servico.ItemListaServico, FormatoItemListaServico);

  Result.AppendChild(AddNode(tcStr, '#1', 'ItemListaServico', 1, 8, NrOcorrItemListaServico,
                                                          item, DSC_CLISTSERV));

  Result.AppendChild(AddNode(tcStr, '#1', 'ClassificacaoCNAE', 1, 9, 1,
                                OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE));

  Result.AppendChild(AddNode(tcStr, '#31', 'CodigoTributacaoMunicipio', 1, 20, 1,
                     NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN));
end;

function TNFSeW_CTA200.GerarValoresRetencoes: TACBrXmlNode;
begin
  Result := CreateElement('ValoresRetencoes');

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaPIS', 1, 15, 1,
                                  NFSe.Servico.Valores.AliquotaPis, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorPIS', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorPis, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaCOFINS', 1, 15, 1,
                               NFSe.Servico.Valores.AliquotaCofins, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorCOFINS', 1, 15, 1,
                                NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaCSLL', 1, 15, 1,
                                 NFSe.Servico.Valores.AliquotaCsll, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorCSLL', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorCsll, DSC_VCSLL));

  Result.AppendChild(AddNode(tcDe2, '#1', 'BaseCalculoINSS', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaINSS', 1, 15, 1,
                                 NFSe.Servico.Valores.AliquotaInss, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorINSS', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorInss, DSC_VINSS));

  Result.AppendChild(AddNode(tcDe2, '#1', 'AliquotaIR', 1, 15, 1,
                                   NFSe.Servico.Valores.AliquotaIr, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorIR', 1, 15, 1,
                                        NFSe.Servico.Valores.ValorIr, DSC_VIR));
end;

function TNFSeW_CTA200.GerarTomador: TACBrXmlNode;
var
  tipo: string;
begin
  Result := CreateElement('Tomador');

  Tipo := EnumeradoToStr(NFSe.Tomador.IdentificacaoTomador.Tipo,
                      ['1', '1', '1', '2', ''],
                      [tpPJdoMunicipio, tpPJforaMunicipio, tpPJforaPais,
                       tpPF, tpPFNaoIdentificada]);

  Result.AppendChild(AddNode(tcStr, '#1', 'Tipo', 1, 1, 1, Tipo, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CpfCnpj', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 15, 1,
                 NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoEstadual', 1, 20, 1,
                  NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, DSC_IE));

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 115, 0,
                                          NFSe.Tomador.RazaoSocial, DSC_XNOME));
end;

function TNFSeW_CTA200.GerarEndereco: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'TipoLogradouro', 1, 50, 1,
                             NFSe.Tomador.Endereco.TipoLogradouro, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 125, 1,
                                   NFSe.Tomador.Endereco.Endereco, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 10, 1,
                                      NFSe.Tomador.Endereco.Numero, DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#1', 'Complemento', 1, 60, 1,
                                NFSe.Tomador.Endereco.Complemento, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 60, 1,
                                  NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipio', 7, 7, 1,
                OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 2, 2, 1,
                                           NFSe.Tomador.Endereco.UF, DSC_UF));

  Result.AppendChild(AddNode(tcInt, '#1', 'CodigoPais', 4, 4, 1,
                                NFSe.Tomador.Endereco.CodigoPais, DSC_CPAIS));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cep', 8, 8, 1,
                             OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP));

  Result.AppendChild(AddNode(tcStr, '#1', 'TelefoneContatoTomador', 1, 11, 1,
                          OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE));

  Result.AppendChild(AddNode(tcStr, '#1', 'EmailTomador', 1, 80, 1,
                                        NFSe.Tomador.Contato.Email, DSC_EMAIL));
end;

function TNFSeW_CTA200.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps');

  FDocument.Root := NFSeNode;

  xmlNode := GerarIdentificacaoRps;
  NFSeNode.AppendChild(xmlNode);

  nodeArray := GerarServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      NFSeNode.AppendChild(nodeArray[i]);
    end;
  end;

  xmlNode := GerarValores;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarInformacoes;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarValoresRetencoes;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Observacoes', 1, 1000, 1,
                                                   NFSe.OutrasInformacoes, ''));

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarEndereco;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

end.
