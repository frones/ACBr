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

unit SmarAPD.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeX,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXGravarXml_ABRASFv2,
  ACBrNFSeXConversao;

type
  { TNFSeW_SmarAPD }

  TNFSeW_SmarAPD = class(TNFSeWClass)
  protected
    function Gerartbfatura: TACBrXmlNode;
    function Gerarfatura: TACBrXmlNodeArray;
    function Gerartbservico: TACBrXmlNode;
    function Gerarservico: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_SmarAPD203 }

  TNFSeW_SmarAPD203 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

  { TNFSeW_SmarAPD204 }

  TNFSeW_SmarAPD204 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.DateTime;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SmarAPD
//==============================================================================

{ TNFSeW_SmarAPD }

function TNFSeW_SmarAPD.GerarXml: Boolean;
var
  NFSeNode: TACBrXmlNode;


  function NumeroNFSeInformado: boolean;
  begin
    Result := (StrToIntDef(NFSe.Numero, 0) > 0);
  end;

begin
  Configuracao;

  Opcoes.DecimalChar := ',';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.SaveOptions := [xmlNoDecl, xmlNoEmpty];
  FDocument.Clear();

  NFSeNode := CreateElement('nfd');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeronfd', 1, 12, 1,
                            ifThen(NumeroNFSeInformado, NFSe.Numero, '0'), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'codseriedocumento', 1, 12, 1,
                                              NFSe.IdentificacaoRps.Serie, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'codnaturezaoperacao', 1, 12, 1,
                             NaturezaOperacaoToStr(NFSe.NaturezaOperacao), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'codigocidade', 1, 1, 1, '3', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'inscricaomunicipalemissor', 1, 11, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  NFSeNode.AppendChild(AddNode(tcDatVcto, '#2', 'dataemissao', 1, 21, 1,
                                                         NFSe.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'razaotomador', 1, 120, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  if NFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    NFSeNode.AppendChild(AddNode(tcStr, '#2', 'tppessoa', 1, 1, 1, 'O', ''))
  else
  begin
    if length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11 then
      NFSeNode.AppendChild(AddNode(tcStr, '#2', 'tppessoa', 1, 1, 1, 'F', ''))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#2', 'tppessoa', 1, 1, 1, 'J', ''));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'nomefantasiatomador', 1, 120, 1,
                                                NFSe.Tomador.NomeFantasia, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'enderecotomador', 1, 50, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'numeroendereco', 1, 50, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'cidadetomador', 1, 50, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'estadotomador', 1, 50, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'paistomador', 1, 50, 1,
                                              NFSe.Tomador.Endereco.xPais, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'fonetomador', 1, 50, 1,
                                            NFSe.Tomador.Contato.Telefone, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'faxtomador', 1, 60, 0, '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'ceptomador', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'bairrotomador', 1, 50, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'emailtomador', 1, 50, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'cpfcnpjtomador', 1, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'inscricaoestadualtomador', 1, 14, 1,
          OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'inscricaomunicipaltomador', 1, 14, 0,
         OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'observacao', 1, 50, 1,
                                                   NFSe.OutrasInformacoes, ''));

  if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
    NFSeNode.AppendChild(Gerartbfatura);

  NFSeNode.AppendChild(Gerartbservico);

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'razaotransportadora', 1, 255, 0,
                                           NFSe.Transportadora.xNomeTrans, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'cpfcnpjtransportadora', 1, 20, 0,
                            OnlyNumber(NFSe.Transportadora.xCpfCnpjTrans), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'enderecotransportadora', 1, 255, 0,
                                            NFSe.Transportadora.xEndTrans, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'tipofrete', 1, 1, 1, '2', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'quantidade', 1, 1, 0, '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'especie', 1, 1, 0, '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'pesoliquido', 1, 1, 1, '0', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'pesobruto', 1, 1, 1, '0', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'pis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'csll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'irrf', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'inss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'descdeducoesconstrucao', 1, 500, 0,
                                NFSe.Servico.Valores.JustificativaDeducao, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#2', 'totaldeducoesconstrucao', 1, 15, 0,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  if NFSe.TipoTributacaoRPS = ttTribnoMun then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'tributadonomunicipio', 1, 5, 1,
                                                                   'true', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'tributadonomunicipio', 1, 5, 1,
                                                                  'false', ''));

  if (not NumeroNFSeInformado) then
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#2', 'numerort', 1, 02, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#2', 'codigoseriert', 1, 2, 1,
                                              NFSe.IdentificacaoRps.Serie, ''));

    NFSeNode.AppendChild(AddNode(tcDatVcto, '#2', 'dataemissaort', 1, 21, 1,
                                                      NFSe.DataEmissaoRps, ''));
  end;

  if NFSe.Competencia <> 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#2', 'fatorgerador', 1, 21, 1,
                                 FormatDateBr(NFSe.Competencia, 'MMYYYY'), ''));

  Result := True;
end;

function TNFSeW_SmarAPD.Gerarfatura: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.CondicaoPagamento.Parcelas.Count);

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Result[i] := CreateElement('fatura');

    Result[i].AppendChild(AddNode(tcStr, '#55', 'numfatura', 1, 12, 1,
                         NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, ''));

    Result[i].AppendChild(AddNode(tcDatVcto, '#56', 'vencimentofatura', 1, 12, 1,
                  NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'valorfatura', 1, 12, 1,
                           NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, ''));
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 10 then
    wAlerta('#54', 'fatura', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_SmarAPD.Gerartbfatura: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('tbfatura');

  nodeArray := Gerarfatura;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_SmarAPD.Gerarservico: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('servico');

    Result[i].AppendChild(AddNode(tcDe2, '#55', 'quantidade', 1, 15, 1,
                                   NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcStr, '#56', 'descricao', 1, 255, 1,
                                    NFSe.Servico.ItemServico[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcStr, '#57', 'codatividade', 1, 20, 1,
                                      NFSe.Servico.ItemServico[i].CodServ, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#57', 'valorunitario', 1, 15, 1,
                                NFSe.Servico.ItemServico[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'aliquota', 1, 15, 1,
                                     NFSe.Servico.ItemServico[i].Aliquota, ''));

    if NFSe.NaturezaOperacao = no400 then
      Result[i].AppendChild(AddNode(tcStr, '#57', 'impostoretido', 1, 5, 1,
                                                                       'i', ''))
    else
    begin
      if NFSe.Servico.Valores.IssRetido in [stNormal, stSubstituicao] then
        Result[i].AppendChild(AddNode(tcStr, '#57', 'impostoretido', 1, 5, 1,
                                                                   'false', ''))
      else
        Result[i].AppendChild(AddNode(tcStr, '#57', 'impostoretido', 1, 5, 1,
                                                                   'true', ''));
    end;
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#54', 'servico', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_SmarAPD.Gerartbservico: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('tbservico');

  nodeArray := Gerarservico;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

{ TNFSeW_SmarAPD203 }

procedure TNFSeW_SmarAPD203.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  NrOcorrValTotTrib := 0;
  NrOcorrAliquota := 1;
  NrOcorrCodigoPaisServico := -1;
end;

{ TNFSeW_SmarAPD204 }

procedure TNFSeW_SmarAPD204.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  NrOcorrInformacoesComplemetares := 0;
  NrOcorrValTotTrib := 0;
  NrOcorrCodigoPaisServico := -1;
  NrOcorrCodigoPaisTomador := -1;

  TagTomador := 'TomadorServico';
end;

end.
