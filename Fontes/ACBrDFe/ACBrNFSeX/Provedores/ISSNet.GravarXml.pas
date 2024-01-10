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

unit ISSNet.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros,
  ACBrNFSeXGravarXml_ABRASFv1, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_ISSNet }

  TNFSeW_ISSNet = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    function GerarPrestador: TACBrXmlNode; override;
    function GerarCodigoMunicipioUF: TACBrXmlNodeArray; override;
    function GerarServicoCodigoMunicipio: TACBrXmlNode; override;
  end;

  { TNFSeW_ISSNet204 }

  TNFSeW_ISSNet204 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  public
    function GerarXml: Boolean; Override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ISSNet
//==============================================================================

{ TNFSeW_ISSNet }

procedure TNFSeW_ISSNet.Configuracao;
begin
  inherited Configuracao;

  FormatoItemListaServico := filsSemFormatacaoSemZeroEsquerda;

  if FpAOwner.ConfigGeral.Params.TemParametro('NaoFormatarItemServico') then
    FormatoItemListaServico := filsNaoSeAplica;

  DivAliq100 := True;

  if FpAOwner.ConfigGeral.Params.TemParametro('NaoDividir100') then
    DivAliq100 := False;

  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrAliquota := 1;

  PrefixoPadrao := 'tc';
end;

function TNFSeW_ISSNet.GerarPrestador: TACBrXmlNode;
begin
  Result := CreateElement('Prestador');

  Result.AppendChild(GerarCPFCNPJ(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#35', 'InscricaoMunicipal', 1, 15, 0,
             NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, DSC_IM));
end;

function TNFSeW_ISSNet.GerarCodigoMunicipioUF: TACBrXmlNodeArray;
begin
  SetLength(Result, 2);

  Result[0] := AddNode(tcStr, '#43', 'Cidade', 7, 7, 0,
                  OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);

  Result[1] := AddNode(tcStr, '#44', 'Estado', 2, 2, 0,
                                             NFSe.Tomador.Endereco.UF, DSC_UF);
end;

function TNFSeW_ISSNet.GerarServicoCodigoMunicipio: TACBrXmlNode;
begin
  Result := AddNode(tcStr, '#33', 'MunicipioPrestacaoServico', 1, 7, 1,
                            OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);
end;

{ TNFSeW_ISSNet204 }

procedure TNFSeW_ISSNet204.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  NrOcorrCodTribMun_1 := 0;
  NrOcorrCodigoNBS := 0;
  NrOcorrInformacoesComplemetares := 0;

  NrOcorrDiscriminacao_2 := 1;
  NrOcorrCodigoMunic_2 := 1;

  NrOcorrDiscriminacao_1 := -1;
  NrOcorrCodigoMunic_1 := -1;
  NrOcorrCodigoPaisServico := -1;
  NrOcorrCodigoPaisTomador := -1;

  TagTomador := 'TomadorServico';
end;

function TNFSeW_ISSNet204.GerarXml: Boolean;
begin
  if (NFSe.Tomador.Endereco.CodigoMunicipio = '9999999') or
     (NFSe.Tomador.Endereco.UF = 'EX') then
    NrOcorrCodigoPaisServico := 1;

  if (NFSe.OptanteSimplesNacional = snSim) or
     (NFSe.RegimeEspecialTributacao = retMicroempresarioIndividual) then
  begin
    if FpAOwner.ConfigGeral.Params.TemParametro('TagAliquotaObrigSN') then
      NrOcorrAliquota := 1;

    if FpAOwner.ConfigGeral.Params.TemParametro('TagValorISSObrigSN') then
      NrOcorrValorIss := 1;
  end;

  Result := inherited GerarXml;
end;

end.
