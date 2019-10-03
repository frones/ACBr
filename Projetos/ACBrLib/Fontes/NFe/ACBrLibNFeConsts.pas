{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeConsts;

interface

uses
  Classes, SysUtils;

const
  CSessaoDANFE = 'DANFE';
  CChaveTipoDANFE = 'TipoDANFE';
  CChaveImprimeTotalLiquido = 'ImprimeTotalLiquido';
  CChavevTribFed = 'vTribFed';
  CChavevTribEst = 'vTribEst';
  CChavevTribMun = 'vTribMun';
  CChaveFonteTributos = 'FonteTributos';
  CChaveChaveTributos = 'ChaveTributos';
  CChaveImprimeTributos = 'ImprimeTributos';
  CChaveExibeTotalTributosItem = 'ExibeTotalTributosItem';
  CChaveImprimeCodigoEan = 'ImprimeCodigoEan';
  CChaveImprimeEmUmaLinha = 'ImprimeEmUmaLinha';
  CChaveExibeInforAdicProduto = 'ExibeInforAdicProduto';
  CChaveQuebraLinhaEmDetalhamentos = 'QuebraLinhaEmDetalhamentos';

  CSessaoDANFENFE = 'DANFENFe';
  CChaveFormularioContinuo = 'FormularioContinuo';
  CChaveImprimeValor = 'ImprimeValor';
  CChaveImprimeDescPorPercentual = 'ImprimeDescPorPercentual';
  CChaveImprimeDetalhamentoEspecifico = 'ImprimeDetalhamentoEspecifico';
  CChavePosCanhoto = 'PosCanhoto';
  CChaveExibeResumoCanhoto = 'ExibeResumoCanhoto';
  CChaveTextoResumoCanhoto = 'TextoResumoCanhoto';
  CChaveExibeCampoFatura = 'ExibeCampoFatura';
  CChaveExibeDadosISSQN = 'ExibeDadosISSQN';
  CChaveExibeDadosDocReferenciados = 'ExibeDadosDocReferenciados';
  CChaveDetVeiculos = 'DetVeiculos';
  CChaveDetMedicamentos = 'DetMedicamentos';
  CChaveDetArmamentos = 'DetArmamentos';
  CChaveDetCombustiveis = 'DetCombustiveis';
  CChaveTributosPercentual = 'TributosPercentual';
  CChaveTributosPercentualPersonalizado = 'TributosPercentualPersonalizado';
  CChaveMarcadAgua = 'MarcadAgua';
  CChaveLarguraCodProd = 'LarguraCodProd';
  CChaveExibeEAN = 'ExibeEAN';
  CChaveAltLinhaComun = 'AltLinhaComun';
  CChaveEspacoEntreProdutos = 'EspacoEntreProdutos';
  CChaveAlternaCoresProdutos = 'AlternaCoresProdutos';
  CChaveCorDestaqueProdutos = 'CorDestaqueProdutos';
  CChaveTamanhoLogoHeight = 'TamanhoLogoHeight';
  CChaveTamanhoLogoWidth = 'TamanhoLogoWidth';
  CChaveRecuoEndereco = 'RecuoEndereco';
  CChaveRecuoEmpresa = 'RecuoEmpresa';
  CChaveLogoemCima = 'LogoemCima';
  CChaveRecuoLogo = 'RecuoLogo';
  CChaveFonteNome = 'Fonte.Nome';
  CChaveFonteNegrito = 'Fonte.Negrito';
  CChaveFonteTamanhoFonteRazaoSocial = 'Fonte.TamanhoFonteRazaoSocial';
  CChaveFonteTamanhoFonteEndereco = 'Fonte.TamanhoFonteEndereco';
  CChaveFonteTamanhoFonteDemaisCampos = 'Fonte.TamanhoFonteDemaisCampos';

  CSessaoDANFENFCE = 'DANFENFCe';
  CChaveTipoRelatorioEvento = 'TipoRelatorioEvento';
  CChaveLarguraBobina = 'LarguraBobina';
  CChaveImprimeDescAcrescItem = 'ImprimeDescAcrescItem';
  CChaveImprimeItens = 'ImprimeItens';
  CChaveViaConsumidor = 'ViaConsumidor';
  CChavevTroco = 'vTroco';
  CChaveImprimeQRCodeLateral = 'ImprimeQRCodeLateral';
  CChaveImprimeLogoLateral = 'ImprimeLogoLateral';
  CChaveEspacoFinal = 'EspacoFinal';

  CSessaoRespStatus = 'Status';
  CSessaoRespInutilizacao = 'Inutilizacao';
  CSessaoRespConsulta = 'Consulta';
  CSessaoRespEnvio = 'Envio';
  CSessaoRespCancelamento = 'Cancelamento';
  CSessaoRespConsultaCadastro = 'ConsultaCadastro';
  CSessaoRespEvento = 'Evento';
  CSessaoRespConsultaInfCan = 'InfCan';

  ErrValidacaoNFe = -11;
  ErrChaveNFe = -12;
  ErrAssinarNFe = -13;
  ErrConsulta = -14;
  ErrCNPJ = -15;
  ErrRetorno = -16;
  ErrEnvio = -17;
  ErrEnvioEvento = -18;

Resourcestring
  SInfNFeCarregadas = '%d NFe(s) Carregada(s)';
  SInfEventosCarregados = '%d Evento(s) Carregado(s)';

  SErrChaveInvalida = 'Chave % inválida.';
  SErrCNPJInvalido = 'CNPJ % inválido.';
  SErrCNPJCPFInvalido = 'CNPJ/CPF % inválido.';

function SetRetornoNFeCarregadas(const NumNFe: Integer): Integer;
function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;

implementation
uses
  ACBrLibComum;

function SetRetornoNFeCarregadas(const NumNFe: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumNFe,} Format(SInfNFeCarregadas, [NumNFe]));
end;

function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumNFe,} Format(SInfEventosCarregados, [NumEventos]));
end;

end.

