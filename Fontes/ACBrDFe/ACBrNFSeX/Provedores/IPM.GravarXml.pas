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

unit IPM.GravarXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  pcnAuxiliar, pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_IPM }

  TNFSeW_IPM = class(TNFSeWClass)
  private
    FpGerarID: Boolean;
  protected
    procedure Configuracao; override;

    function GerarIdentificacaoRPS: TACBrXmlNode;
    function GerarValoresServico: TACBrXmlNode;
    function GerarPrestador: TACBrXmlNode;
    function GerarTomador: TACBrXmlNode;
    function GerarItens: TACBrXmlNode;
    function GerarLista: TACBrXmlNodeArray;
    function GerarCondicaoPagamento: TACBrXmlNode;
    function GerarParcelas: TACBrXmlNode;
    function GerarParcela: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_IPMV110 }

  TNFSeW_IPMV110 = class(TNFSeW_IPM)
  protected

  end;

  { TNFSeW_IPMV120 }

  TNFSeW_IPMV120 = class(TNFSeW_IPM)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrNFSeX;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     IPM
//==============================================================================

{ TNFSeW_IPM }

function TNFSeW_IPM.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  Opcoes.QuebraLinha := FAOwner.ConfigGeral.QuebradeLinha;
  Opcoes.DecimalChar := ',';

  FDocument.Clear();

  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;

  NFSeNode := CreateElement('nfse');

  if FpGerarID then
    NFSeNode.SetAttribute(FAOwner.ConfigGeral.Identificador, NFSe.InfID.ID);

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#2', 'identificador', 1, 80, 0,
    'nfse_' + NFSe.IdentificacaoRps.Numero + '.' + NFSe.IdentificacaoRps.Serie, ''));

  xmlNode := GerarIdentificacaoRPS;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarValoresServico;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarPrestador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarItens;
  NFSeNode.AppendChild(xmlNode);

  if (NFSe.Status = srNormal) and
     (TACBrNFSeX(FAOwner).Configuracoes.Geral.Provedor in [proIPM_110, proIPM_120]) then
  begin
    xmlNode := GerarCondicaoPagamento;
    NFSeNode.AppendChild(xmlNode);
  end;

  Result := True;
end;

procedure TNFSeW_IPM.Configuracao;
begin
  inherited Configuracao;

  FpGerarID := False;
end;

function TNFSeW_IPM.GerarCondicaoPagamento: TACBrXmlNode;
var
  codFp: String;
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('forma_pagamento');

  codFp := EnumeradoToStr(NFSe.CondicaoPagamento.Condicao,
        ['1', '2', '3', '4', '5'],
        [cpAVista, cpAPrazo, cpNaApresentacao, cpCartaoDebito, cpCartaoCredito]);

  Result.AppendChild(AddNode(tcStr, '#1', 'tipo_pagamento', 1, 1, 1, codFp, ''));

  if (NFSe.CondicaoPagamento.QtdParcela > 0) then
  begin
    xmlNode := GerarParcelas;
    Result.AppendChild(xmlNode);
  end;
end;

function TNFSeW_IPM.GerarIdentificacaoRPS: TACBrXmlNode;
begin
  Result :=  nil;

  if( StrToIntDef( NFSe.IdentificacaoRps.Numero, 0 ) > 0 ) then
  begin
    Result := CreateElement('rps');

    Result.AppendChild(AddNode(tcStr, '#1', 'nro_recibo_provisorio', 1, 12, 1,
                                     NFSe.IdentificacaoRps.Numero, DSC_NUMRPS));

    Result.AppendChild(AddNode(tcStr, '#1', 'serie_recibo_provisorio', 1, 2, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

    Result.AppendChild(AddNode(tcStr, '#1', 'data_emissao_recibo_provisorio', 1, 10, 1,
                FormatDateTimeBr(NFSe.DataEmissaoRps, 'dd/mm/yyyy'), DSC_DEMI));

    Result.AppendChild(AddNode(tcStr, '#1', 'hora_emissao_recibo_provisorio', 1, 10, 1,
                  FormatDateTimeBr(NFSe.DataEmissaoRps, 'hh:mm:ss'), DSC_HEMI));
  end;
end;

function TNFSeW_IPM.GerarItens: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('itens');

  nodeArray := GerarLista;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_IPM.GerarLista: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('lista');

    if EstaVazio(NFSe.Servico.CodigoTributacaoMunicipio) then
    begin
      if (NFSe.Prestador.Endereco.CodigoMunicipio <> IntToStr(NFSe.Servico.MunicipioIncidencia)) then
        // Não Tributa no Municipio do prestador
        Result[i].AppendChild(AddNode(tcStr, '#', 'tributa_municipio_prestador', 1, 1, 1,
                                                                       '0', ''))
      else
        // Tributa no Municipio do Prestador
        Result[i].AppendChild(AddNode(tcStr, '#', 'tributa_municipio_prestador', 1, 1, 1,
                                                                      '1', ''));
    end
    else
      Result[i].AppendChild(AddNode(tcStr, '#', 'tributa_municipio_prestador', 1, 1, 1,
                                   NFSe.Servico.CodigoTributacaoMunicipio, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'codigo_local_prestacao_servico', 1, 9, 1,
            CodIBGEToCodTOM(StrToIntDef(NFSe.Servico.CodigoMunicipio, 0)), ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'unidade_codigo', 1, 9, 0,
                   UnidadeToStr(NFSe.Servico.ItemServico[I].TipoUnidade), ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'unidade_quantidade', 1, 15, 0,
                                   NFSe.Servico.ItemServico[I].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'unidade_valor_unitario', 1, 15, 0,
                                NFSe.Servico.ItemServico[I].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'codigo_item_lista_servico', 1, 9, 1,
                 OnlyNumber(NFSe.Servico.ItemServico[I].ItemListaServico), ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'descritivo', 1, 1000, 1,
      IfThen(NFSe.Servico.ItemServico[I].Descricao = '',
       NFSe.Servico.Discriminacao, NFSe.Servico.ItemServico[I].Descricao), ''));

    if NFSe.Servico.ItemServico[I].Aliquota = 0 then
      Result[i].AppendChild(AddNode(tcDe2, '#', 'aliquota_item_lista_servico', 1, 15, 1,
                                             NFSe.Servico.Valores.Aliquota, ''))
    else
      Result[i].AppendChild(AddNode(tcDe2, '#', 'aliquota_item_lista_servico', 1, 15, 1,
                                     NFSe.Servico.ItemServico[I].Aliquota, ''));

    Result[i].AppendChild(AddNode(tcStr, '#', 'situacao_tributaria', 1, 4, 1,
                            NaturezaOperacaoToStr( NFSe.NaturezaOperacao), ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'valor_tributavel', 1, 15, 1,
                                   NFSe.Servico.ItemServico[I].ValorTotal, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'valor_deducao', 1, 15, 0,
                                NFSe.Servico.ItemServico[I].ValorDeducoes, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'valor_issrf', 1, 15, 0,
                                NFSe.Servico.ItemServico[I].ValorIss, DSC_VISS))
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#', 'lista', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_IPM.GerarParcela: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.CondicaoPagamento.Parcelas.Count);

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Result[i] := CreateElement('parcela');

    Result[i].AppendChild(AddNode(tcInt, '#', 'numero', 1, 2, 1,
                         NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#', 'valor', 1, 15, 1,
                           NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, ''));

    Result[i].AppendChild(AddNode(tcDatVcto, '#', 'data_vencimento', 10, 10, 1,
                  NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, ''));
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 10 then
    wAlerta('#', 'parcela', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_IPM.GerarParcelas: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('parcelas');

  nodeArray := GerarParcela;

  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_IPM.GerarPrestador: TACBrXmlNode;
begin
  Result := CreateElement('prestador');

  Result.AppendChild(AddNode(tcStr, '#1', 'cpfcnpj', 11, 14, 1,
             OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.Cnpj), DSC_CNPJ));

  Result.AppendChild(AddNode(tcStr, '#1', 'cidade', 1, 9, 0,
  CodIBGEToCodTOM(StrToIntDef(NFSe.Prestador.Endereco.CodigoMunicipio, 0)), ''));
end;

function TNFSeW_IPM.GerarTomador: TACBrXmlNode;
begin
  Result := CreateElement('tomador');

  if Trim(NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro) <> '' then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'tipo', 1, 1, 1, 'E', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'identificador', 1, 20, 1,
            Trim(NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'estado', 1, 100, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'pais', 1, 100, 1,
                                              NFSe.Tomador.Endereco.xPais, ''));
  end
  else
  begin
    if NFSe.Tomador.IdentificacaoTomador.Tipo in [tpPFNaoIdentificaca, tpPF] then
      Result.AppendChild(AddNode(tcStr, '#1', 'tipo', 1, 1, 1, 'F', ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'tipo', 1, 1, 1, 'J', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'cpfcnpj', 1, 14, 0,
                OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CNPJ));

    Result.AppendChild(AddNode(tcStr, '#1', 'ie', 0, 16, 0,
        OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), DSC_IE));
  end;

  Result.AppendChild(AddNode(tcStr, '#1', 'nome_razao_social', 1, 100, 0,
                                          NFSe.Tomador.RazaoSocial, DSC_XNOME));

  Result.AppendChild(AddNode(tcStr, '#1', 'sobrenome_nome_fantasia', 1, 100, 0,
                                                NFSe.Tomador.NomeFantasia, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'logradouro', 1, 70, 0,
                                     NFSe.Tomador.Endereco.Endereco, DSC_XLGR));

  Result.AppendChild(AddNode(tcStr, '#1', 'email', 1, 100, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'numero_residencia', 1, 8, 0,
                                        NFSe.Tomador.Endereco.Numero, DSC_NRO));

  Result.AppendChild(AddNode(tcStr, '#1', 'complemento', 1, 50, 0,
                                  NFSe.Tomador.Endereco.Complemento, DSC_XCPL));

  Result.AppendChild(AddNode(tcStr, '#1', 'ponto_referencia', 1, 100, 0,
                                                                       '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'bairro', 1, 30, 0,
                                    NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO));

  Result.AppendChild(AddNode(tcStr, '#1', 'cidade', 1, 9, 0,
    CodIBGEToCodTOM(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0)), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 8, 0,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'ddd_fone_comercial', 1, 3, 0,
                                            NFSe.Tomador.Contato.DDD, DSC_DDD));

  Result.AppendChild(AddNode(tcStr, '#1', 'fone_comercial', 1, 9, 0,
                                            NFSe.Tomador.Contato.Telefone, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'ddd_fone_residencial', 1, 3, 0,
                                                                       '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'fone_residencial', 1, 9, 0,
                                                                       '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'ddd_fax', 1, 3, 0,
                                                                       '', ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'fone_fax', 1, 9, 0,
                                                                       '', ''));
{
  if( NFSe.Status <> srCancelado )then
  begin
    if (NFSe.Tomador.Endereco.EnderecoInformado) then
      Result.AppendChild(AddNode(tcStr, '#1', 'endereco_informado', 1, 1, 1,
                                                                       'S', ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'endereco_informado', 1, 1, 1,
                                                                      'N', ''));
  end;
}
end;

function TNFSeW_IPM.GerarValoresServico: TACBrXmlNode;
begin
  Result := CreateElement('nf');

  if NFSe.Status = srCancelado then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'numero', 0, 9, 1,
                                                              NFSe.Numero, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'situacao', 1, 1, 1, 'C', ''));
  end;

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_total', 1, 15, 1,
                             NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_desconto', 1, 15, 0,
                       NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESC));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_ir', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_inss', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_contribuicao_social', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_rps', 1, 15, 0,
                                                                        0, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_pis', 1, 15, 0,
                                      NFSe.Servico.Valores.ValorPis, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#1', 'valor_cofins', 1, 15, 0,
                                NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcStr, '#1', 'observacao', 1, 1000, 0,
                                        NFSe.OutrasInformacoes, DSC_OUTRASINF));
end;

{ TNFSeW_IPMa }

procedure TNFSeW_IPMV120.Configuracao;
begin
  inherited Configuracao;

  FpGerarID := True;
end;

end.
