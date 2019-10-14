{******************************************************************************}
{ Projeto: Componente ACBrONE                                                  }
{  Operador Nacional dos Estados - ONE                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
|* 14/10/2019: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnONEConsts;

interface

uses
  SysUtils;

const
  NAME_SPACE_ONE  = 'xmlns="http://www.portalfiscal.inf.br/one"';
  DSC_verAplic = 'Versão do Aplicativo';
  DSC_tpMan = 'Tipo de Manutenção';
  DSC_CNPJOper = 'CNPJ do Operador';
  DSC_cEQP = 'Código do Equipamento';
  DSC_xEQP = 'Descrição do Equipamento';
  DSC_cUF = 'Código IBGE da UF';
  DSC_tpSentido = 'Tipo de Sentido';
  DSC_Latitude = 'Latitude';
  DSC_Longitude = 'Longitude';
  DSC_tpEQP = 'Tipo de Equipamento';
  DSC_tpTransm = 'Tipo de Transmissão';
  DSC_Placa = 'Placa';
  DSC_tpVeiculo = 'Tipo de Veiculo';
  DSC_Velocidade = 'Velocidade do Veiculo';
  DSC_foto = 'Foto';
  DSC_IndicadorConfianca = 'Indicador de Confiança do Equipamento';
  DSC_PesoBrutoTotal = 'Peso Bruto Total';
  DSC_NroEixos = 'Numero de Eixos do Veiculo';
  DSC_tpDist = 'Tipo de Distribuição';
  DSC_NSUFin = 'NSU Final';

implementation

end.

