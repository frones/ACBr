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

unit ISSCambe.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXConsts, ACBrNFSeXGravarXml;

type
  { TNFSeW_ISSCambe }

  TNFSeW_ISSCambe = class(TNFSeWClass)
  private

  protected
    procedure Configuracao; override;

    function GerarRPS: TACBrXmlNode;
    function GerarDadosPrestador: TACBrXmlNode;
    function GerarDadosTomador: TACBrXmlNode;
    function GerarDemaisDados: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//    ISSCambe
//==============================================================================

{ TNFSeW_ISSCambe }

procedure TNFSeW_ISSCambe.Configuracao;
const
  CRLF = #$D#$A;
begin
  inherited Configuracao;

  Opcoes.QuebraLinha    := CRLF;
  Opcoes.RetirarEspacos := False;
  FormatoAliq := tcDe4;
  DivAliq100  := False;
end;

function TNFSeW_ISSCambe.GerarDadosPrestador: TACBrXmlNode;
begin
   Result := CreateElement('dadosPrestador');

   Result.AppendChild(AddNode(tcStr, '#1', 'prestadorCMC', 1, 9, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));
end;

function TNFSeW_ISSCambe.GerarDadosTomador: TACBrXmlNode;
begin
   Result := CreateElement('dadosTomador');

   Result.AppendChild(AddNode(tcStr, '#1', 'tipoTomador', 1, 1, 1,
         FpAOwner.TipoPessoaToStr(NFSe.Tomador.IdentificacaoTomador.Tipo), ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorCPFCNPJ', 1, 14, 0,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorInscricaoEstadual', 1,15, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorCMC', 1, 9, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorRazaoSocial', 1, 60, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorLogradouro', 1, 40, 0,
                                           NFSe.Tomador.Endereco.Endereco, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorNumero', 1, 6, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorComplemento', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorBairro', 1, 40, 0,
                                             NFSe.Tomador.Endereco.Bairro, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorCEP', 1, 8, 0,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorMunicipio', 1, 9, 0,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorUF', 1, 2, 0,
                                                 NFSe.Tomador.Endereco.UF, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tomadorEmail', 1, 50, 0,
                                               NFSe.Tomador.Contato.Email, ''));

   Result.AppendChild(AddNode(tcInt, '#1', 'tomadorPais', 1, 5, 0,
                                         NFSe.Tomador.Endereco.CodigoPais, ''));
end;

function TNFSeW_ISSCambe.GerarDemaisDados: TACBrXmlNode;
var
  OptanteSimplesNacional: string;
  Aliquota: Double;
begin
   Result := CreateElement('demaisDados');

   Result.AppendChild(AddNode(tcStr, '#1', 'servicoISS', 1, 9, 1,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'codigoObra', 1, 15, 0,
                                          NFSe.ConstrucaoCivil.CodigoObra, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'ART', 1, 20, 0,
                                                 NFSe.ConstrucaoCivil.Art, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'incentivoFiscal', 1, 5, 1,
                                                                  'false', ''));

   OptanteSimplesNacional := EnumeradoToStr(NFSe.OptanteSimplesNacional,
                                             ['false', 'true'], [snNao, snSim]);

   Result.AppendChild(AddNode(tcStr, '#1', 'optanteSimplesNacional', 1, 5, 1,
                                                   OptanteSimplesNacional, ''));

   Aliquota := NormatizarAliquota(NFSe.Servico.Valores.Aliquota, DivAliq100);

   Result.AppendChild(AddNode(FormatoAliq, '#1', 'aliquota', 1, 6, 1,
                                                                 Aliquota, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'ISSDevido', 1, 1, 1,
                            FpAOwner.SituacaoTribToStr(NFSe.SituacaoTrib), ''));

   Result.AppendChild(AddNode(tcInt, '#1', 'paisPrestacao', 1, 5, 0,
                                                  NFSe.Servico.CodigoPais, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'municipioPrestacao', 1, 9, 0,
                                             NFSe.Servico.CodigoMunicipio, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'servicoValor', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorBaseCalculo', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'tipoDeducao', 1, 1, 1, '1', ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorDeducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorIR', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorPIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorCOFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorCSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

   Result.AppendChild(AddNode(tcDe2, '#1', 'valorINSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

   Result.AppendChild(AddNode(tcStr, '#1', 'servicoDiscriminacao', 1, 1400, 0,
                                               NFSe.Servico.Discriminacao, ''));
end;


function TNFSeW_ISSCambe.GerarRPS: TACBrXmlNode;
begin
   Result := CreateElement('RPS');

   Result.AppendChild(AddNode(tcStr, '#1', 'RPSNumero', 1, 10, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

   Result.AppendChild(AddNode(tcDat, '#1', 'RPSDataEmissao', 1, 10, 1,
                                                         NFse.DataEmissao, ''));
end;

function TNFSeW_ISSCambe.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('declaracaoPrestacaoServico');

  FDocument.Root := NFSeNode;

  if NFSe.IdentificacaoRps.Numero <> '' then
  begin
    xmlNode := GerarRPS;
    NFSeNode.AppendChild(xmlNode);
  end;

  xmlNode := GerarDadosPrestador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarDadosTomador;
  NFSeNode.AppendChild(xmlNode);


  xmlNode := GerarDemaisDados;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

end.
