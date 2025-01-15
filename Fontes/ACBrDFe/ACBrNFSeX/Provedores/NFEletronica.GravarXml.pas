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

unit NFEletronica.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlDocument,
  ACBrNFSeXGravarXml_ABRASFv1;

type
  { TNFSeW_NFEletronica }

  TNFSeW_NFEletronica = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    procedure DefinirIDRps; override;

    function GerarValores: TACBrXmlNode; override;
    function GerarServico: TACBrXmlNode; override;
    function GerarEnderecoTomador: TACBrXmlNode; override;
    function GerarTomador: TACBrXmlNode; override;
    function Gerar_CondicaoPagamento: TACBrXmlNode;
    function GerarIdentificacaoTomador: TACBrXmlNode; override;
    function GerarCPFCNPJTomador(const CPFCNPJ: string): TACBrXmlNode;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrUtil.Strings,
  ACBrNFSeXConversao,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     NFEletronica
//==============================================================================

{ TNFSeW_NFEletronica }

procedure TNFSeW_NFEletronica.Configuracao;
begin
  inherited Configuracao;

  DivAliq100  := True;

  if FpAOwner.ConfigGeral.Params.TemParametro('NaoDividir100') then
    DivAliq100 := False;

  FormatoEmissao := tcDat;

  FormatoItemListaServico := filsComFormatacaoSemZeroEsquerda;

  NrOcorrOutrasInformacoes := 1;
  NrOcorrInscMunTomador := 1;
  NrOcorrInscEstTomador := 1;
  NrOcorrFoneTomador := 1;
  NrOcorrEmailTomador := 1;
end;

procedure TNFSeW_NFEletronica.DefinirIDRps;
begin
  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                    NFSe.IdentificacaoRps.Serie;
end;

function TNFSeW_NFEletronica.GerarServico: TACBrXmlNode;
var
  item: string;
begin
  Result := CreateElement('Servico');

  Result.AppendChild(GerarValores);

  item := FormatarItemServico(NFSe.Servico.ItemListaServico, FormatoItemListaServico);

  Result.AppendChild(AddNode(tcStr, '#29', 'ItemListaServico', 1, 5, 1,
                                                          item, DSC_CLISTSERV));

  Result.AppendChild(AddNode(tcStr, '#30', 'CodigoCnae', 1, 7, 1,
                                OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE));

  Result.AppendChild(AddNode(tcStr, '#32', 'Discriminacao', 1, 4000, 1,
    StringReplace(NFSe.Servico.Discriminacao, Opcoes.QuebraLinha,
               FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll]), DSC_DISCR));

  Result.AppendChild(AddNode(tcStr, '#33', 'CodigoMunicipio', 1, 7, 1,
                           OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN));
end;

function TNFSeW_NFEletronica.GerarTomador: TACBrXmlNode;
begin
  Result := inherited GerarTomador;

  Result.AppendChild(Gerar_CondicaoPagamento);
end;

function TNFSeW_NFEletronica.GerarValores: TACBrXmlNode;
var
  Aliquota: Double;
begin
  Result := CreateElement('Valores');

  Result.AppendChild(AddNode(tcDe2, '#13', 'ValorServicos', 1, 15, 1,
                             NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO));

  Result.AppendChild(AddNode(tcDe2, '#20', 'IssRetido', 1, 15, 1,
                           NFSe.Servico.Valores.ValorIssRetido, DSC_INDISSRET));

  Result.AppendChild(AddNode(tcDe2, '#18', 'ValorIR', 1, 15, 1,
                                        NFSe.Servico.Valores.ValorIr, DSC_VIR));

  Result.AppendChild(AddNode(tcDe2, '#15', 'ValorPis', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorPis, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#16', 'ValorCofins', 1, 15, 1,
                                NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#19', 'ValorCsll', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorCsll, DSC_VCSLL));

  Result.AppendChild(AddNode(tcDe2, '#17', 'ValorINSS', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorInss, DSC_VINSS));

  Result.AppendChild(AddNode(tcDe2, '#21', 'ValorIss', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIss, DSC_VISS));

  Result.AppendChild(AddNode(tcDe2, '#24', 'BaseCalculo', 1, 15, 1,
                                 NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS));

  Aliquota := NormatizarAliquota(NFSe.Servico.Valores.Aliquota, DivAliq100);

  Result.AppendChild(AddNode(FormatoAliq, '#25', 'Aliquota', 1, 5, 1,
                                                          Aliquota, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#26', 'ValorLiquidoNfse', 1, 15, 1,
                             NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE));
end;

function TNFSeW_NFEletronica.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.Tomador.Endereco.Endereco <> '') or (NFSe.Tomador.Endereco.Numero <> '') or
     (NFSe.Tomador.Endereco.Bairro <> '') or (NFSe.Tomador.Endereco.CodigoMunicipio <> '') or
     (NFSe.Tomador.Endereco.UF <> '') or (NFSe.Tomador.Endereco.CEP <> '') then
  begin
    Result := CreateElement('Endereco');

    Result.AppendChild(AddNode(tcStr, '#39', 'Logradouro', 1, 60, 1,
                                     NFSe.Tomador.Endereco.Endereco, DSC_XLGR));

    Result.AppendChild(AddNode(tcStr, '#40', 'Numero', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Numero, DSC_NRO));

    Result.AppendChild(AddNode(tcStr, '#42', 'Bairro', 1, 60, 1,
                                    NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO));

    Result.AppendChild(AddNode(tcStr, '#43', 'CodigoMunicipio', 7, 7, 1,
                  OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN));

    Result.AppendChild(AddNode(tcStr, '#44', 'Uf', 2, 2, 1,
                                             NFSe.Tomador.Endereco.UF, DSC_UF));

    Result.AppendChild(AddNode(tcStr, '#45', 'Cep', 8, 8, 1,
                               OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP));
  end;
end;

function TNFSeW_NFEletronica.GerarCPFCNPJTomador(
  const CPFCNPJ: string): TACBrXmlNode;
var
  aDoc: string;
begin
  aDoc := OnlyNumber(CPFCNPJ);

  Result := CreateElement('CpfCnpj');

  Result.AppendChild(AddNode(tcStr, '#34', 'Cnpj', 11, 14, 1, aDoc, DSC_CNPJ));
end;

function TNFSeW_NFEletronica.GerarIdentificacaoTomador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoTomador');

  if NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
    Result.AppendChild(GerarCPFCNPJTomador(NFSe.Tomador.IdentificacaoTomador.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#37', 'InscricaoMunicipal', 1, 15, NrOcorrInscMunTomador,
                 NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM));

  Result.AppendChild(AddNode(tcStr, '#38', 'InscricaoEstadual', 1, 20, NrocorrInscEstTomador,
                  NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, DSC_IE));
end;

function TNFSeW_NFEletronica.Gerar_CondicaoPagamento: TACBrXmlNode;
begin
  Result := CreateElement('CondicaoPagamento');

  Result.AppendChild(AddNode(tcDat, '#56', 'DataVencimento', 10, 10, 1,
                             NFSe.CondicaoPagamento.DataVencimento, DSC_DVENC));

  Result.AppendChild(AddNode(tcStr, '#45', 'InstrucaoPagamento', 1, 1000, 1,
                       NFSe.CondicaoPagamento.InstrucaoPagamento, DSC_INSTPAG));

  Result.AppendChild(AddNode(tcInt, '#45', 'CodigoVencimento', 3, 3, 1,
                         NFSe.CondicaoPagamento.CodigoVencimento, DSC_CODVENV));
end;

end.
