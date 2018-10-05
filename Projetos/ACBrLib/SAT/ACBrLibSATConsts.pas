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

unit ACBrLibSATConsts;

interface

uses
  Classes, SysUtils;

const
  CLibSATNome = 'ACBrLibSAT';
  CLibSATVersao = '0.0.1';

  CSessaoSAT = 'SAT';
  CChaveNomeDLL = 'NomeDLL';
  CChaveCodigoDeAtivacao = 'CodigoDeAtivacao';
  CChaveSignAC = 'SignAC';
  CChaveValidarNumero = 'ValidarNumeroSessaoResposta';
  CChaveNumeroTentativas = 'NumeroTentativasValidarSessao';

  CSessaoSATConfig = 'SATConfig';
  CChaveVersaoDadosEnt = 'infCFe_versaoDadosEnt';
  CChaveIdeCNPJ = 'ide_CNPJ';
  CChaveIdeNumeroCaixa = 'ide_numeroCaixa';
  CChaveIdeTpAmb = 'ide_tpAmb';
  CChaveEmitCNPJ = 'emit_CNPJ';
  CChaveEmitIE = 'emit_IE';
  CChaveEmitIM = 'emit_IM';
  CChaveEmitcRegTrib = 'emit_cRegTrib';
  CChaveEmitcRegTribISSQN = 'emit_cRegTribISSQN';
  CChaveEmitIndRatISSQN = 'emit_indRatISSQN';
  CChaveEhUTF8 = 'EhUTF8';
  CChavePaginaDeCodigo = 'PaginaDeCodigo';
  CChaveArqSchema = 'ArqSchema';
  CChaveXmlSignLib = 'XmlSignLib';

  CSessaoSATConfigArquivos = 'SATConfigArquivos';
  CChaveSalvarCFe = 'SalvarCFe';
  CChaveSalvarCFeCanc = 'SalvarCFeCanc';
  CChaveSalvarEnvio = 'SalvarEnvio';
  CChaveSepararPorCNPJ = 'SepararPorCNPJ';
  CChaveSepararPorModelo = 'SepararPorModelo';
  CChaveSepararPorAno = 'SepararPorAno';
  CChaveSepararPorMes = 'SepararPorMes';
  CChaveSepararPorDia = 'SepararPorDia';
  CChavePastaCFeVenda = 'PastaCFeVenda';
  CChavePastaCFeCancelamento = 'PastaCFeCancelamento';
  CChavePastaEnvio = 'PastaEnvio';
  CChavePrefixoArqCFe = 'PrefixoArqCFe';
  CChavePrefixoArqCFeCanc = 'PrefixoArqCFeCanc';

  CSessaoSATRede = 'SATRede';
  CChaveTipoInter = 'tipoInter';
  CChaveSSID = 'SSID';
  CChaveSeg = 'seg';
  CChaveCodigo = 'codigo';
  CChaveTipoLan = 'tipoLan';
  CChaveLanIP = 'lanIP';
  CChaveLanMask = 'lanMask';
  CChaveLanGW = 'lanGW';
  CChaveLanDNS1 = 'lanDNS1';
  CChaveLanDNS2 = 'lanDNS2';
  CChaveUsuario = 'usuario';
  CChaveSenha = 'senha';
  CChaveProxy = 'proxy';
  CChaveProxyIp = 'proxy_ip';
  CChaveProxyPorta = 'proxy_porta';
  CChaveProxyUser = 'proxy_user';
  CChaveProxySenha = 'proxy_senha';

  CSessaoExtrato = 'Extrato';
  CChaveTipo = 'Tipo';
  CChaveMask_qCom = 'Mask_qCom';
  CChaveMask_vUnCom = 'Mask_vUnCom';
  CChaveImprimeQRCode = 'ImprimeQRCode';
  CChaveImprimeMsgOlhoNoImposto = 'ImprimeMsgOlhoNoImposto';
  CChaveImprimeCPFNaoInformado = 'ImprimeCPFNaoInformado';
  CChavePictureLogo = 'PictureLogo';
  CChaveMostrarPreview = 'MostrarPreview';
  CChaveMostrarSetup = 'MostrarSetup';
  CChaveNumCopias = 'NumCopias';
  CChaveNomeArquivo = 'NomeArquivo';
  CChaveSoftwareHouse = 'SoftwareHouse';
  CChaveSite = 'Site';
  CChaveFiltro = 'Filtro';
  CChaveMsgAppQRCode = 'MsgAppQRCode';
  CChaveImprimeEmUmaLinha = 'ImprimeEmUmaLinha';
  CChaveImprimeDescAcrescItem = 'ImprimeDescAcrescItem';
  CChaveUsaCodigoEanImpressao = 'UsaCodigoEanImpressao';

  CChaveLarguraBobina = 'LarguraBobina';
  CChaveMargensTopo = 'MargensTopo';
  CChaveMargensEsquerda = 'MargensEsquerda';
  CChaveMargensFundo = 'MargensFundo';
  CChaveMargensDireita = 'MargensDireita';
  CChaveEspacoFinal = 'EspacoFinal';
  CChaveLogoWidth = 'LogoWidth';
  CChaveLogoHeigth = 'LogoHeigth';
  CChaveLogoStretch = 'LogoStretch';
  CChaveLogoAutoSize = 'LogoAutoSize';
  CChaveLogoCenter = 'LogoCenter';
  CChaveLogoVisible = 'LogoVisible';
  CChavePrinterName = 'PrinterName';
  CChaveImprimeChaveEmUmaLinha = 'ImprimeChaveEmUmaLinha';

  CSessaoCFe = 'CFE';
  CSessaoENVIO = 'ENVIO';
  CSessaoCFeCancelado = 'CANCELAMENTO';
  CSessaoTESTEFIMAFIM = 'TESTEFIMAFIM';
  CSessaoStatusSAT = 'STATUSSAT';


implementation


end.

