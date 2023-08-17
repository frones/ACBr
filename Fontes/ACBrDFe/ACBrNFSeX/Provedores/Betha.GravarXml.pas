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

unit Betha.GravarXml;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv1, ACBrNFSeXGravarXml_ABRASFv2,
  ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_Betha }

  TNFSeW_Betha = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    function GerarCondicaoPagamento: TACBrXmlNode; override;
    function GerarParcelas: TACBrXmlNodeArray; override;
  public
    function GerarXml: Boolean; override;
  end;

  { TNFSeW_Betha202 }

  TNFSeW_Betha202 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

    procedure DefinirIDDeclaracao; override;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Betha
//==============================================================================

{ TNFSeW_Betha }

procedure TNFSeW_Betha.Configuracao;
begin
  inherited Configuracao;

  FormatoItemListaServico := filsSemFormatacao;

  NrOcorrOutrasInformacoes := 0;
  NrOcorrValorISSRetido_1 := -1;
  NrOcorrValorISSRetido_2 := 0;
  NrOcorrInscEstTomador := 0;
end;

function TNFSeW_Betha.GerarCondicaoPagamento: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('CondicaoPagamento');

  if (NFSe.CondicaoPagamento.QtdParcela > 0) and
     (NFSe.CondicaoPagamento.Condicao = cpAPrazo) then
  begin
    Result.AppendChild(AddNode(tcStr, '#53', 'Condicao', 1, 15, 1,
         FpAOwner.CondicaoPagToStr(NFSe.CondicaoPagamento.Condicao), DSC_TPAG));

    Result.AppendChild(AddNode(tcInt, '#54', 'QtdParcela', 1, 03, 1,
                                 NFSe.CondicaoPagamento.QtdParcela, DSC_QPARC));

    nodeArray := GerarParcelas;
    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end
  else
    Result.AppendChild(AddNode(tcStr, '#53', 'Condicao', 1, 15, 1,
                                                          'A_VISTA', DSC_TPAG));
end;

function TNFSeW_Betha.GerarParcelas: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.CondicaoPagamento.Parcelas.Count);

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
  begin
    Result[i] := CreateElement('Parcelas');

    Result[i].AppendChild(AddNode(tcStr, '#55', 'Parcela', 1, 03, 1,
                  NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, DSC_NPARC));

    Result[i].AppendChild(AddNode(tcDatVcto, '#56', 'DataVencimento', 10, 10, 1,
           NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, DSC_DVENC));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'Valor', 1, 18, 1,
                    NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, DSC_VPARC));
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 10 then
    wAlerta('#54', 'Parcelas', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_Betha.GerarXml: Boolean;
begin
  if NFSe.OptanteSimplesNacional = snSim then
    NrOcorrAliquota := 1;

  Result := inherited GerarXml;
end;

{ TNFSeW_Betha202 }

procedure TNFSeW_Betha202.Configuracao;
begin
  inherited Configuracao;

  FormatoItemListaServico := filsSemFormatacao;

  NrOcorrCodigoPaisServico := -1;
end;

procedure TNFSeW_Betha202.DefinirIDDeclaracao;
begin
  NFSe.InfID.ID := 'rps' + OnlyNumber(NFSe.IdentificacaoRps.Numero)
end;

function TNFSeW_Betha202.GerarXml: Boolean;
begin
  if NFSe.Servico.Valores.IssRetido <> stNormal then
    NrOcorrRespRetencao := 0   // se tem a retenção a tag deve ser gerada
  else
    NrOcorrRespRetencao := -1; // se não tem a retenção a tag não deve ser gerada

  if NFSe.Tomador.Endereco.CodigoMunicipio = '9999999' then
    NrOcorrCodigoPaisTomador := 1
  else
    NrOcorrCodigoPaisTomador := -1;

  Result := inherited GerarXml;
end;

end.
