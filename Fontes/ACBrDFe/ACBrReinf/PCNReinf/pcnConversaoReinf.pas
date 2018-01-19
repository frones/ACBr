{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}

unit pcnConversaoReinf;

{$I ACBr.inc}

interface

uses
  SysUtils, Classes;

type

  tpTpInsc                = (tiCNPJ = 1, tiCPF = 2, tiCNO = 4);

  TpTpInscProp            = (tpCNPJ, tpCPF);

  TLayReinf = (orLayENVIO, orLayConsulta);

  TTypeOperacao         = (toInclusao, toAlteracao, toExclusao);

  TpTpAmb                 = (taProducao = 1, taProducaoRestritaDadosReais = 2, taProducaoRestritaDadosFicticios = 3);

  tpSimNao                = (tpSim, tpNao);

  TpProcEmi               = (peAplicEmpregador = 1, peAplicGoverno = 2);

  TpIndCoop               = (icNaoecooperativa, icCooperativadeTrabalho, icCooperativadeProducao, icOutrasCooperativas );

  tpTpProc                = (tpAdministrativo, tpJudicial);

  tpIndSusp               = (siLiminarMandadoSeguranca = 1,
                             siAntecipacaoTutela = 4,
                             siLiminarMedidaCautelar = 5,
                             siSentencaMandadoSegurancaFavoravelContribuinte = 8,
                             siSentencaAcaoOrdinariaFavContribuinteConfirmadaPeloTRF = 9,
                             siAcordaoTRFFavoravelContribuinte = 10,
                             siAcordaoSTJRecursoEspecialFavoravelContribuinte = 11,
                             siAcordaoSTFRecursoExtraordinarioFavoravelContribuinte = 12,
                             siSentenca1instanciaNaoTransitadaJulgadoEfeitoSusp = 13,
                             siDecisaoDefinitivaAFavorDoContribuinte = 90,
                             siSemSuspensaoDaExigibilidade = 92);


  TindSitPJ               = (spNormal, spExtincao, spFusao, spCisao, spIncorporacao);

  TTypeAutoria            = (taContribuinte = 1, taOutraEntidade = 2);

  TIndRetificacao         = (trOriginal = 1, trRetificacao = 2);

  TpindObra               = (ioNaoeObraDeConstrucaoCivil = 0, ioObradeConstrucaoCivilTotal = 1, ioObradeConstrucaoCivilParcial = 2);

  TpindCPRB               = (icNaoContribuintePrevidenciariaReceitaBruta = 0, icContribuintePrevidenciaReceitaBruta = 1);

  TtpProcRetPrinc         = (tprAdministrativoTomador = 1, tprJudicialTomador = 2, tprJudicialPrestador = 3);


  TReinfSchema            =   (
                             rsevtInfoContri, // R-1000 - Informações do Empregador/Contribuinte
                             rsevtTabProcesso,      // R-1070 - Tabela de Processos Administrativos/Judiciais
                             rsevtServTom, // R-2010 - Retenção Contribuição Previdenciária - Serviços Tomados
                             rsevtServPrest, // R - 2020
                             rsevtCPRB, // R-2060
                             rsevtFechaEvPer, // R-2099
                             rsevtReabreEvPer, // R-2098
                             rsevtExclusao // R-9000
                             );

const
  PrefixVersao = '-v';
  TReinfSchemaStr : array[0..7] of string = ('evtInfoContribuinte','evtTabProcesso', 'evtTomadorServicos', 'evtPrestadorServicos',
    'evtInfoCPRB', 'evtFechamento', 'evtReabreEvPer', 'evtExclusao');

implementation

end.
