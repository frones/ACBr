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

unit SystemPro.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv2, ACBrNFSeXConversao;

type
  { TNFSeW_SystemPro201 }

  TNFSeW_SystemPro201 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

    function GerarServico: TACBrXmlNode; override;
    function GerarServicos: TACBrXmlNodeArray; override;
    function GerarItemValores(i: Integer): TACBrXmlNodeArray; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SystemPro
//==============================================================================

{ TNFSeW_SystemPro201 }

procedure TNFSeW_SystemPro201.Configuracao;
begin
  inherited Configuracao;

  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrAliquota := 1;
  NrOcorrValorTTS := 0;
  NrOcorrQuantDiarias := 0;

  GerarTagRps := False;

  if FpAOwner.ConfigGeral.Params.TemParametro('GerarGrupoRps') then
    GerarTagRps := True;
end;

function TNFSeW_SystemPro201.GerarItemValores(i: Integer): TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, 1);

  Result[0] := CreateElement('Valores');

  Result[0].AppendChild(AddNode(tcDe2, '#13', 'ValorServicos', 1, 15, 1,
                         NFSe.Servico.ItemServico[i].ValorTotal, DSC_VSERVICO));

  Result[0].AppendChild(AddNode(tcDe2, '#14', 'ValorDeducoes', 1, 15, 1,
                     NFSe.Servico.ItemServico[i].ValorDeducoes, DSC_VDEDUCISS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorIss', 1, 15, 0,
                               NFSe.Servico.ItemServico[i].ValorISS, DSC_VISS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorTTS', 1, 15, 0,
                       NFSe.Servico.ItemServico[i].ValorTaxaTurismo, DSC_VTTS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'QuantDiarias', 1, 15, 0,
                          NFSe.Servico.ItemServico[i].QtdeDiaria, DSC_QDIARIA));

  Result[0].AppendChild(AddNode(tcDe2, '#25', 'Aliquota', 1, 05, 0,
                              NFSe.Servico.ItemServico[i].Aliquota, DSC_VALIQ));

  Result[0].AppendChild(AddNode(tcDe2, '#13', 'BaseCalculo', 1, 15, 0,
                                  NFSe.Servico.ItemServico[i].BaseCalculo, ''));

  Result[0].AppendChild(AddNode(tcDe2, '#27', 'DescontoIncondicionado', 1, 15, 0,
          NFSe.Servico.ItemServico[i].DescontoIncondicionado, DSC_VDESCINCOND));

  Result[0].AppendChild(AddNode(tcDe2, '#28', 'DescontoCondicionado  ', 1, 15, 0,
              NFSe.Servico.ItemServico[i].DescontoCondicionado, DSC_VDESCCOND));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorPis', 1, 15, 1,
                               NFSe.Servico.ItemServico[i].ValorPis, DSC_VPIS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorCofins', 1, 15, 1,
                         NFSe.Servico.ItemServico[i].ValorCofins, DSC_VCOFINS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorInss', 1, 15, 1,
                             NFSe.Servico.ItemServico[i].ValorInss, DSC_VINSS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorIr', 1, 15, 1,
                               NFSe.Servico.ItemServico[i].ValorIRRF, DSC_VIR));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorCsll', 1, 15, 1,
                             NFSe.Servico.ItemServico[i].ValorCsll, DSC_VCSLL));
end;

function TNFSeW_SystemPro201.GerarServico: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := nil; //CreateElement('Servico');
{
  if (NFSe.Servico.ItemServico.Count > 0) then
  begin
    nodeArray := GerarServicos;
    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;
  end;
  }
end;

function TNFSeW_SystemPro201.GerarServicos: TACBrXmlNodeArray;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('Servico');

    nodeArray := GerarItemValores(i);
    Result[i].AppendChild(nodeArray[0]);

    Result[i].AppendChild(AddNode(tcStr, '#20', 'IssRetido', 1, 01, NrOcorrIssRetido,
      FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET));

    Result[i].AppendChild(AddNode(tcStr, '#29', 'ItemListaServico', 1, 5, NrOcorrItemListaServico,
                                   NFSe.Servico.ItemListaServico, DSC_CLISTSERV));

    Result[i].AppendChild(AddNode(tcStr, '#30', 'CodigoCnae', 1, 7, NrOcorrCodigoCNAE,
                                  OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE));

    Result[i].AppendChild(AddNode(tcStr, '#31', 'CodigoTributacaoMunicipio', 1, 20, NrOcorrCodTribMun_1,
                       NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN));

    Result[i].AppendChild(AddNode(tcStr, '#32', 'Discriminacao', 1, 2000, NrOcorrDiscriminacao_1,
      StringReplace(NFSe.Servico.ItemServico[i].Descricao, ';', FpAOwner.ConfigGeral.QuebradeLinha,
                                       [rfReplaceAll, rfIgnoreCase]), DSC_DISCR,
                (NFSe.Prestador.Endereco.CodigoMunicipio <> '3304557')));

    Result[i].AppendChild(AddNode(tcStr, '#33', 'CodigoMunicipio', 1, 7, 1,
                             OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN));

    Result[i].AppendChild(AddNode(tcInt, '#34', 'CodigoPais', 4, 4, NrOcorrCodigoPaisServico,
                                            NFSe.Servico.CodigoPais, DSC_CPAIS));

    Result[i].AppendChild(AddNode(tcInt, '#35', 'ExigibilidadeISS',
                               NrMinExigISS, NrMaxExigISS, NrOcorrExigibilidadeISS,
    StrToInt(FpAOwner.ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS)), DSC_INDISS));

    Result[i].AppendChild(AddNode(tcInt, '#36', 'MunicipioIncidencia', 7, 07, 0,
                                  NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI));

    Result[i].AppendChild(AddNode(tcStr, '#37', 'NumeroProcesso', 1, 30, 0,
                                   NFSe.Servico.NumeroProcesso, DSC_NPROCESSO));

    if (NFSe.Servico.Valores.IssRetido <> stNormal) then
      Result[i].AppendChild(AddNode(tcStr, '#21', 'ResponsavelRetencao', 1, 1, 1,
     FpAOwner.ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET));
  end;
end;

end.
