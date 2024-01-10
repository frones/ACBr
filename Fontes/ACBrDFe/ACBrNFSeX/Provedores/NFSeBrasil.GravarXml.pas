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

unit NFSeBrasil.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv1;

type
  { TNFSeW_NFSeBrasil }

  TNFSeW_NFSeBrasil = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    procedure DefinirIDRps; override;

    function GerarServico: TACBrXmlNode; override;
    function GerarTomador: TACBrXmlNode; override;
    function GerarEnderecoTomador: TACBrXmlNode; override;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrNFSeXConsts,
  ACBrNFSeXConversao,
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     NFSeBrasil
//==============================================================================

{ TNFSeW_NFSeBrasil }

procedure TNFSeW_NFSeBrasil.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  NrOcorrComplTomador := 1;
  NrOcorrFoneTomador := 1;
  NrOcorrEmailTomador := 1;
  NrOcorrAliquota := 1;
  NrOcorrValorDeducoes := 1;
  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrOutrasRet := 1;
  NrOcorrDescIncond := 1;
  NrOcorrDescCond := 1;

  NrOcorrBaseCalc := -1;
  NrOcorrValLiq := -1;
  NrOcorrOptanteSN := -1;
  NrOcorrIncentCult := -1;

  NrOcorrValorISSRetido_1 := -1;
  NrOcorrValorISSRetido_2 := -1;
end;

procedure TNFSeW_NFSeBrasil.DefinirIDRps;
begin
  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;
end;

function TNFSeW_NFSeBrasil.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.Tomador.Endereco.Endereco <> '') or (NFSe.Tomador.Endereco.Numero <> '') or
     (NFSe.Tomador.Endereco.Bairro <> '') or (NFSe.Tomador.Endereco.CodigoMunicipio <> '') or
     (NFSe.Tomador.Endereco.UF <> '') or (NFSe.Tomador.Endereco.CEP <> '') then
  begin
    Result := CreateElement('Endereco');

    Result.AppendChild(AddNode(tcStr, '#39', 'Endereco', 1, 125, 0,
                                     NFSe.Tomador.Endereco.Endereco, DSC_XLGR));

    Result.AppendChild(AddNode(tcStr, '#40', 'Numero', 1, 10, 0,
                                        NFSe.Tomador.Endereco.Numero, DSC_NRO));

    Result.AppendChild(AddNode(tcStr, '#41', 'Complemento', 1, 60, NrOcorrComplTomador,
                                  NFSe.Tomador.Endereco.Complemento, DSC_XCPL));

    Result.AppendChild(AddNode(tcStr, '#42', 'Bairro', 1, 60, 0,
                                    NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO));

    Result.AppendChild(AddNode(tcStr, '#43', 'CodigoMunicipio', 1, 7, 0,
                              NFSe.Tomador.Endereco.CodigoMunicipio, DSC_CMUN));

    Result.AppendChild(AddNode(tcStr, '#44', 'Uf', 1, 2, 0,
                                             NFSe.Tomador.Endereco.UF, DSC_UF));

    Result.AppendChild(AddNode(tcStr, '#45', 'Cep', 8, 8, 0,
                               OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP));
  end;
end;

function TNFSeW_NFSeBrasil.GerarServico: TACBrXmlNode;
begin
  Result := CreateElement('Servico');

  Result.AppendChild(GerarValores);

//  Result.AppendChild(AddNode(tcStr, '#29', 'ItemListaServico', 1, 5, 0,
//                                 NFSe.Servico.ItemListaServico, DSC_CLISTSERV));

  Result.AppendChild(AddNode(tcStr, '#30', 'CodigoTributacaoMunicipio', 1, 20, 0,
                     NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN));

  Result.AppendChild(AddNode(tcStr, '#31', 'CodigoCnae', 1, 7, 1,
                                OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE));

  Result.AppendChild(AddNode(tcStr, '#32', 'Discriminacao', 1, 2000, 1,
    StringReplace(NFSe.Servico.Discriminacao, ';', FpAOwner.ConfigGeral.QuebradeLinha,
                                       [rfReplaceAll, rfIgnoreCase]), DSC_DISCR,
                (NFSe.Prestador.Endereco.CodigoMunicipio <> '3304557')));

  Result.AppendChild(GerarServicoCodigoMunicipio);
end;

function TNFSeW_NFSeBrasil.GerarTomador: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    Result := CreateElement('Tomador');

    if ((NFSe.Tomador.Endereco.UF <> 'EX') and
        ((NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
         (NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal <> ''))) then
    begin
      Result.AppendChild(GerarIdentificacaoTomador);
    end;

    // Envio de nome com o Caracter '&'
    NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial,
                                          '&amp;', '&amp;amp;', [rfReplaceAll]);

    Result.AppendChild(AddNode(tcStr, '#38', 'RazaoSocial', 1, 115, 0,
                                          NFSe.Tomador.RazaoSocial, DSC_XNOME));

    Result.AppendChild(GerarEnderecoTomador);

    Result.AppendChild(AddNode(tcStr, '#46', 'Email', 1, 80, NrOcorrEmailTomador,
                                        NFSe.Tomador.Contato.Email, DSC_EMAIL));

    Result.AppendChild(AddNode(tcStr, '#47', 'Telefone', 1, 11, NrOcorrFoneTomador,
                          OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE));
  end;
end;

function TNFSeW_NFSeBrasil.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  ListaDeAlertas.Clear;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps');

  FDocument.Root := NFSeNode;

  xmlNode := GerarInfRps;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

end.
