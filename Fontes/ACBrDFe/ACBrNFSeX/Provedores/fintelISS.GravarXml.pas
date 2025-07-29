{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit fintelISS.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  IniFiles,
  ACBrXmlBase,
  ACBrXmlDocument,
  ACBrNFSeXGravarXml_ABRASFv2;

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
    function GerarItemServicos: TACBrXmlNodeArray;
    function GerarItemValores(i: Integer): TACBrXmlNodeArray; override;
    function GerarServico: TACBrXmlNode; override;
    function GerarValoresServico: TACBrXmlNode; override;

    procedure GerarINISecaoItemValores(const AINIRec: TMemIniFile); override;
  end;

  TNFSeW_fintelISS204 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;
  end;

implementation

uses
  ACBrDFe.Conversao,
  ACBrUtil.Base,
  ACBrUtil.Strings,
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
    nodeArray := GerarItemServicos;
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

function TNFSeW_fintelISS202.GerarItemServicos: TACBrXmlNodeArray;
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
      StringReplace(NFSe.Servico.ItemServico[i].Descricao, Opcoes.QuebraLinha,
               FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll]), DSC_DISCR));

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

procedure TNFSeW_fintelISS202.GerarINISecaoItemValores(
  const AINIRec: TMemIniFile);
var
  I: Integer;
  sSecao: string;
begin
  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    sSecao:= 'Itens' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'Descricao', ChangeLineBreak(NFSe.Servico.ItemServico[I].Descricao, FpAOwner.ConfigGeral.QuebradeLinha));
    AINIRec.WriteFloat(sSecao, 'ValorTotal', NFSe.Servico.ItemServico[I].ValorTotal);
    AINIRec.WriteFloat(sSecao, 'ValorDeducoes', NFSe.Servico.ItemServico[I].ValorDeducoes);
    AINIRec.WriteFloat(sSecao, 'ValorIss', NFSe.Servico.ItemServico[I].ValorISS);
    AINIRec.WriteFloat(sSecao, 'Aliquota', NFSe.Servico.ItemServico[I].Aliquota);
    AINIRec.WriteFloat(sSecao, 'BaseCalculo', NFSe.Servico.ItemServico[I].BaseCalculo);
  end;
end;

{ TNFSeW_fintelISS204 }

procedure TNFSeW_fintelISS204.Configuracao;
begin
  inherited Configuracao;

  FormatoEmissao := tcDat;
  FormatoCompetencia := tcDat;
  TagTomador := 'TomadorServico';
  GerarIDDeclaracao := True;
  GerarIDRps := False;
end;

end.
