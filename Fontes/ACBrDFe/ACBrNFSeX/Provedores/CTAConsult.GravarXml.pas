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

unit CTAConsult.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { Provedor com layout próprio }
  { TNFSeW_CTAConsult }

  TNFSeW_CTAConsult = class(TNFSeWClass)
  protected
    function GerarPrestador: TACBrXmlNode;
    function GerarEnderecoPrestador: TACBrXmlNode;

    function GerarTomador: TACBrXmlNode;
    function GerarEnderecoTomador: TACBrXmlNode;

    function GerarIntermediador: TACBrXmlNode;
    function GerarEnderecoIntermediador: TACBrXmlNode;

    function GerarAtividadeExecutada: TACBrXmlNode;
    function GerarLocalPrestacao: TACBrXmlNode;

    function GerarDeducoes: TACBrXmlNode;
    function GerarDetalhamentoNota: TACBrXmlNode;

    function GerarItensServico: TACBrXmlNode;
    function GerarItem: TACBrXmlNodeArray;

    function GerarTotais: TACBrXmlNode;

    function GerarImpostosFederais: TACBrXmlNode;
    function GerarImposto: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     CTAConsult
//==============================================================================

{ TNFSeW_CTAConsult }

function TNFSeW_CTAConsult.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('nfse');

  FDocument.Root := NFSeNode;

  xmlNode := GerarPrestador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  if NFSe.Intermediario.Identificacao.CpfCnpj <> '' then
  begin
    xmlNode := GerarIntermediador;
    NFSeNode.AppendChild(xmlNode);
  end;

  xmlNode := GerarAtividadeExecutada;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDeducoes;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDetalhamentoNota;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_CTAConsult.GerarPrestador: TACBrXmlNode;
var
  tipoPessoa: string;
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('prestador');

  tipoPessoa := '2';

  if Length(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj) < 14 then
    tipoPessoa := '1';

  Result.AppendChild(AddNode(tcStr, '#1', 'tipoPessoa', 1, 1, 1,
                                                               tipoPessoa, ''));

  if tipoPessoa = '1' then
    Result.AppendChild(AddNode(tcStr, '#1', 'cpf', 11, 11, 1,
                             NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'cnpj', 14, 14, 1,
                            NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'inscricaoMunicipal', 1, 16, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'razaoSocial', 1, 60, 1,
                                               NFSe.Prestador.RazaoSocial, ''));

  xmlNode := GerarEnderecoPrestador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'email', 1, 100, 1,
                                             NFSe.Prestador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'telefoneDdd', 1, 2, 1,
                                               NFSe.Prestador.Contato.DDD, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'telefoneNumero', 1, 10, 1,
                                          NFSe.Prestador.Contato.Telefone, ''));
end;

function TNFSeW_CTAConsult.GerarEnderecoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 60, 1,
    NFSe.Prestador.Endereco.Endereco + ', ' + NFSe.Prestador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 60, 1,
                                      NFSe.Prestador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 60, 1,
                                           NFSe.Prestador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 8, 1,
                                              NFSe.Prestador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoMunipio', 1, 5, 1,
    CodIBGEToCodTOM(StrToIntDef(NFSe.Prestador.Endereco.CodigoMunicipio, 0)), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoMunicipio', 1, 60, 1,
                                       NFSe.Prestador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoEstado', 2, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoEstado', 2, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));
end;

function TNFSeW_CTAConsult.GerarTomador: TACBrXmlNode;
var
  tomadorIdentificado, tipoPessoa: string;
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('tomador');

  tomadorIdentificado := '1';

  if NFSe.Tomador.IdentificacaoTomador.CpfCnpj = '' then
    tomadorIdentificado := '2';

  Result.AppendChild(AddNode(tcStr, '#1', 'tomadorIdentificado', 1, 1, 1,
                                                      tomadorIdentificado, ''));

  if tomadorIdentificado = '1' then
  begin
    tipoPessoa := '2';

    if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) < 14 then
      tipoPessoa := '1';

    Result.AppendChild(AddNode(tcStr, '#1', 'tipoPessoa', 1, 1, 1,
                                                               tipoPessoa, ''));

    if tipoPessoa = '1' then
      Result.AppendChild(AddNode(tcStr, '#1', 'cpf', 11, 11, 1,
                                 NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'cnpj', 14, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'inscricaoMunicipal', 1, 16, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'razaoSocial', 1, 60, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

    xmlNode := GerarEnderecoTomador;
    Result.AppendChild(xmlNode);

    Result.AppendChild(AddNode(tcStr, '#1', 'email', 1, 100, 0,
                                               NFSe.Tomador.Contato.Email, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'telefoneDdd', 1, 2, 0,
                                                 NFSe.Tomador.Contato.DDD, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'telefoneNumero', 1, 10, 0,
                                            NFSe.Tomador.Contato.Telefone, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'apelido', 1, 60, 0,
                                                NFSe.Tomador.NomeFantasia, ''));
  end;
end;

function TNFSeW_CTAConsult.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := CreateElement('endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 60, 1,
     NFSe.Tomador.Endereco.Endereco + ', ' + NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 8, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoMunipio', 1, 5, 1,
    CodIBGEToCodTOM(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0)), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoMunicipio', 1, 60, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoEstado', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoEstado', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));
end;

function TNFSeW_CTAConsult.GerarIntermediador: TACBrXmlNode;
var
  tipoPessoa: string;
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('intermediador');

  tipoPessoa := '2';

  if Length(NFSe.Intermediario.Identificacao.CpfCnpj) < 14 then
    tipoPessoa := '1';

  Result.AppendChild(AddNode(tcStr, '#1', 'tipoPessoa', 1, 1, 1,
                                                               tipoPessoa, ''));

  if tipoPessoa = '1' then
    Result.AppendChild(AddNode(tcStr, '#1', 'cpf', 11, 11, 1,
                                  NFSe.Intermediario.Identificacao.CpfCnpj, ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'cnpj', 14, 14, 1,
                                 NFSe.Intermediario.Identificacao.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'inscricaoMunicipal', 1, 16, 1,
                      NFSe.Intermediario.Identificacao.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'razaoSocial', 1, 60, 1,
                                    NFSe.Intermediario.RazaoSocial, ''));

  xmlNode := GerarEnderecoIntermediador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'email', 1, 100, 1,
                                         NFSe.Intermediario.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'telefoneDdd', 1, 2, 1,
                                           NFSe.Intermediario.Contato.DDD, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'telefoneNumero', 1, 10, 1,
                                      NFSe.Intermediario.Contato.Telefone, ''));
end;

function TNFSeW_CTAConsult.GerarEnderecoIntermediador: TACBrXmlNode;
begin
  Result := CreateElement('endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 60, 1,
     NFSe.Intermediario.Endereco.Endereco + ', ' +
     NFSe.Intermediario.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 60, 1,
                                  NFSe.Intermediario.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 60, 1,
                                       NFSe.Intermediario.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 8, 1,
                                          NFSe.Intermediario.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoMunipio', 1, 5, 1,
    CodIBGEToCodTOM(StrToIntDef(NFSe.Intermediario.Endereco.CodigoMunicipio, 0)), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoMunicipio', 1, 60, 1,
                                   NFSe.Intermediario.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoEstado', 2, 2, 1,
                                           NFSe.Intermediario.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoEstado', 2, 2, 1,
                                           NFSe.Intermediario.Endereco.UF, ''));
end;

function TNFSeW_CTAConsult.GerarAtividadeExecutada: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('atividadeExecutada');

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoServico', 1, 5, 1,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoServico', 1, 100, 1,
                                               NFSe.Servico.Discriminacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoAtividade', 1, 20, 1,
                                                  NFSe.Servico.CodigoCnae, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoAtividade', 1, 100, 1,
                                                   NFSe.Servico.Descricao, ''));

  xmlNode := GerarLocalPrestacao;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'tipoTributacao', 1, 1, 1,
                        FpAOwner.TributacaoToStr(NFSe.Servico.Tributacao), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tipoRecolhimento', 1, 1, 1,
                                                    NFSe.TipoRecolhimento, ''));

  Result.AppendChild(AddNode(tcDe4, '#1', 'aliquota', 1, 7, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

end;

function TNFSeW_CTAConsult.GerarLocalPrestacao: TACBrXmlNode;
begin
  Result := CreateElement('localPrestacao');

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoEstado', 2, 2, 1,
                                                 NFSe.Servico.UFPrestacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoEstado', 2, 2, 1,
                                                 NFSe.Servico.UFPrestacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'codigoMunipio', 1, 5, 1,
            CodIBGEToCodTOM(StrToIntDef(NFSe.Servico.CodigoMunicipio, 0)), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoMunicipio', 1, 60, 1,
                                   NFSe.Servico.MunicipioPrestacaoServico, ''));
end;

function TNFSeW_CTAConsult.GerarDeducoes: TACBrXmlNode;
begin
  Result := CreateElement('deducoes');

  Result.AppendChild(AddNode(tcStr, '#1', 'tipo', 1, 1, 1,
              FpAOwner.TipoDeducaoToStr(NFSe.Servico.Valores.TipoDeducao), ''));
end;

function TNFSeW_CTAConsult.GerarDetalhamentoNota: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('detalhamentoNota');

  Result.AppendChild(AddNode(tcStr, '#1', 'descricaoNota', 1, 256, 1,
                                               NFSe.Servico.Discriminacao, ''));

  xmlNode := GerarItensServico;
  Result.AppendChild(xmlNode);

  xmlNode := GerarTotais;
  Result.AppendChild(xmlNode);

  xmlNode := GerarImpostosFederais;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_CTAConsult.GerarItensServico: TACBrXmlNode;
var
  i : integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := CreateElement('itensServico');

  nodeArray := GerarItem;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_CTAConsult.GerarItem: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('item');
    Result[i].SetAttribute('nItem', IntToStr(i+1));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'tributavel', 1, 5, 1,
       FpAOwner.SimNaoToStr(NFSe.Servico.ItemServico.Items[i].Tributavel), ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'descricao', 1, 60, 1,
                              NFSe.Servico.ItemServico.Items[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcInt, '#1', 'quantidade', 1, 4, 1,
                             NFSe.Servico.ItemServico.Items[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'valorUnitario', 1, 15, 1,
                          NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'valorTotal', 1, 15, 1,
                             NFSe.Servico.ItemServico.Items[i].ValorTotal, ''));
  end;

  if NFSe.Servico.ItemServico.Count > 99 then
    wAlerta('#', 'item', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TNFSeW_CTAConsult.GerarTotais: TACBrXmlNode;
begin
  Result := CreateElement('totais');

  Result.AppendChild(AddNode(tcDe2, '#1', 'valotTotalNota', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valorTotalServico', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valorTotalDeducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valorTotalISS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valorReducaoBC', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));
end;

function TNFSeW_CTAConsult.GerarImpostosFederais: TACBrXmlNode;
var
  i : integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  if NFSe.Servico.Imposto.Count > 0 then
  begin
    Result := CreateElement('impostosFederais');

    nodeArray := GerarImposto;
    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
end;

function TNFSeW_CTAConsult.GerarImposto: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.Imposto.Count);

  for i := 0 to NFSe.Servico.Imposto.Count - 1 do
  begin
    Result[i] := CreateElement('imposto');
    Result[i].SetAttribute('nItem', IntToStr(i+1));

    Result[i].AppendChild(AddNode(tcInt, '#1', 'codigoImposto', 1, 4, 1,
                                     NFSe.Servico.Imposto.Items[i].Codigo, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'descricaoImposto', 1, 60, 1,
                                  NFSe.Servico.Imposto.Items[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'aliquota', 1, 8, 1,
                                   NFSe.Servico.Imposto.Items[i].Aliquota, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'valorImposto', 1, 15, 1,
                                      NFSe.Servico.Imposto.Items[i].Valor, ''));
  end;

  if NFSe.Servico.Imposto.Count > 99 then
    wAlerta('#', 'imposto', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

end.
