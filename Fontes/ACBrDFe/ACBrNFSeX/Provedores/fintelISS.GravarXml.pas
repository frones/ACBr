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

unit fintelISS.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_fintelISS200 }

  TNFSeW_fintelISS200 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

  { TNFSeW_fintelISS202 }

  TNFSeW_fintelISS202 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

    procedure DefinirIDRps; override;
    function GerarListaServicos: TACBrXmlNode; override;
    function GerarServicos: TACBrXmlNodeArray; override;
    function GerarItemValores(i: Integer): TACBrXmlNodeArray; override;
    function GerarServico: TACBrXmlNode; override;
    function GerarValoresServico: TACBrXmlNode; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConversao,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     fintelISS
//==============================================================================

{ TNFSeW_fintelISS200 }

procedure TNFSeW_fintelISS200.Configuracao;
begin
  inherited Configuracao;

  NrOcorrAliquota := 1;
  NrOcorrCodigoPaisServico := 1;
  NrOcorrDataPagamento := 0;
end;

{ TNFSeW_fintelISS202 }

procedure TNFSeW_fintelISS202.Configuracao;
begin
  inherited Configuracao;

  FormatoEmissao := tcDatHor;
  FormatoCompetencia := tcDatHor;
  TagTomador := 'TomadorServico';
  GerarIDDeclaracao := False;
  GerarIDRps := True;

  NrOcorrAliquota := 1;
  NrOcorrCodigoPaisServico := 1;
  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorISS := 1;

  NrOcorrOutrasInformacoes := 0;
end;

function TNFSeW_fintelISS202.GerarListaServicos: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('ListaServicos');

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
end;

function TNFSeW_fintelISS202.GerarServico: TACBrXmlNode;
begin
  Result := nil
end;

function TNFSeW_fintelISS202.GerarServicos: TACBrXmlNodeArray;
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

    Result[i].AppendChild(AddNode(tcInt, '#36', 'MunicipioIncidencia', 7, 07, NrOcorrMunIncid,
                                  NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI));

    Result[i].AppendChild(AddNode(tcStr, '#37', 'NumeroProcesso', 1, 30, NrOcorrNumProcesso,
                                   NFSe.Servico.NumeroProcesso, DSC_NPROCESSO));
  end;
end;

function TNFSeW_fintelISS202.GerarValoresServico: TACBrXmlNode;
begin
  Result := CreateElement('ValoresServico');

  Result.AppendChild(AddNode(tcDe2, '#15', 'ValorPis', 1, 15, NrOcorrValorPis,
                                      NFSe.Servico.Valores.ValorPis, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#16', 'ValorCofins', 1, 15, NrOcorrValorCofins,
                                NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#17', 'ValorInss', 1, 15, NrOcorrValorInss,
                                    NFSe.Servico.Valores.ValorInss, DSC_VINSS));

  Result.AppendChild(AddNode(tcDe2, '#18', 'ValorIr', 1, 15, NrOcorrValorIr,
                                        NFSe.Servico.Valores.ValorIr, DSC_VIR));

  Result.AppendChild(AddNode(tcDe2, '#19', 'ValorCsll', 1, 15, NrOcorrValorCsll,
                                    NFSe.Servico.Valores.ValorCsll, DSC_VCSLL));

  Result.AppendChild(AddNode(tcDe2, '#21', 'ValorIss', 1, 15, NrOcorrValorISS,
                                      NFSe.Servico.Valores.ValorIss, DSC_VISS));

  Result.AppendChild(AddNode(tcDe2, '#13', 'ValorLiquidoNfse', 1, 15, 1,
                             NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE));

  Result.AppendChild(AddNode(tcDe2, '#13', 'ValorServicos', 1, 15, 1,
                             NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO));
end;

procedure TNFSeW_fintelISS202.DefinirIDRps;
begin
  NFSe.InfID.ID := 'rps' + NFSe.IdentificacaoRps.Numero + NFSe.IdentificacaoRps.Serie;
end;

function TNFSeW_fintelISS202.GerarItemValores(i: Integer): TACBrXmlNodeArray;
begin
  Result := nil;
  SetLength(Result, 1);

  Result[0] := CreateElement('Valores');

  Result[0].AppendChild(AddNode(tcDe2, '#13', 'ValorServicos', 1, 15, 1,
                         NFSe.Servico.ItemServico[i].ValorTotal, DSC_VSERVICO));

  Result[0].AppendChild(AddNode(tcDe2, '#14', 'ValorDeducoes', 1, 15, 1,
                     NFSe.Servico.ItemServico[i].ValorDeducoes, DSC_VDEDUCISS));

  Result[0].AppendChild(AddNode(tcDe2, '#21', 'ValorIss', 1, 15, 1,
                               NFSe.Servico.ItemServico[i].ValorISS, DSC_VISS));

  Result[0].AppendChild(AddNode(FormatoAliq, '#25', 'Aliquota', 1, 05, 1,
                              NFSe.Servico.ItemServico[i].Aliquota, DSC_VALIQ));

  Result[0].AppendChild(AddNode(tcDe2, '#13', 'BaseCalculo', 1, 15, 1,
                                  NFSe.Servico.ItemServico[i].BaseCalculo, ''));
  {
  Result[0].AppendChild(AddNode(tcDe2, '#27', 'DescontoIncondicionado', 1, 15, NrOcorrDescIncond,
          NFSe.Servico.ItemServico[i].DescontoIncondicionado, DSC_VDESCINCOND));

  Result[0].AppendChild(AddNode(tcDe2, '#28', 'DescontoCondicionado  ', 1, 15, NrOcorrDescCond,
              NFSe.Servico.ItemServico[i].DescontoCondicionado, DSC_VDESCCOND));
  }
end;

end.
