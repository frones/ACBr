{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 23/08/2016: Italo Jurisato Junior
|*  - Criado uma Unit especifica para as constantes usadas pelo componente
|*    ACBrNFSe
*******************************************************************************}

{$I ACBr.inc}

unit pnfsConsts;

interface

uses
  SysUtils;

const
  DSC_NUMRPS = 'Numero do RPS';
  DSC_SERIERPS = 'Serie do RPS';
  DSC_TIPORPS = 'Tipo do RPS';
  DSC_NUMRPSSUB = 'Numero do RPS Substituido';
  DSC_SERIERPSSUB = 'Serie do RPS Substituido';
  DSC_TIPORPSSUB = 'Tipo do RPS Substituido';
  DSC_VSERVICO = 'Valor do Serviço';
  DSC_OUTRASRETENCOES = 'Valor de Outras Retenções';
  DSC_VNFSE = 'Valor Liquido da NFS-e';
  DSC_DISCR = 'Discriminação do Serviço';
  DSC_VUNIT = 'Valor Unitário';
  DSC_MUNINCI = 'Municipio de Incidencia';
  DSC_INDRESPRET = 'Indicador de Responsável pela Retenção';
  DSC_COBRA = 'Código da Obra';
  DSC_ART = 'Arte';
  DSC_QPARC = 'Quantidade de Parcelas';
  DSC_NPARC = 'Numero da Parcela';
  DSC_VPARC = 'Valor da Parcela';
  DSC_INDNATOP = 'Indicador de Natureza de Operação';
  DSC_INDOPSN = 'Indicador do Optante pelo Simples Nacional';
  DSC_INDINCCULT = 'Indicador de Incentivador Cultural';
  DSC_INDSTATUS = 'Indicador de Status';
  DSC_OUTRASINF = 'Outras Informações';
  DSC_SENHA = 'Senha';
  DSC_FRASESECRETA = 'Frase Secreta';
  DSC_USUARIO = 'Usuario';
  DSC_ASSINATURA = 'Assinatura';
  DSC_DDD = 'DDD';
  DSC_TPTELEFONE = 'Tipo Telefone';
  DSC_VTOTREC = 'Valor Total Recebido';

implementation

end.

