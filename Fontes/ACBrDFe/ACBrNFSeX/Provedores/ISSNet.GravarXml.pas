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
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml_ABRASFv1;

type
  { TNFSeW_ISSNet }

  TNFSeW_ISSNet = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    function GerarPrestador: TACBrXmlNode; override;
    function GerarCodigoMunicipioUF: TACBrXmlNodeArray; override;
    function GerarServicoCodigoMunicipio: TACBrXmlNode; override;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ISSNet
//==============================================================================

{ TNFSeW_ISSNet }

procedure TNFSeW_ISSNet.Configuracao;
begin
  inherited Configuracao;

  FormatoItemListaServico := filsSemFormatacaoSemZeroEsquerda;

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

end.
