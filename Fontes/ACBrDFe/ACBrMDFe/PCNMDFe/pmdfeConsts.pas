{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
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
|* 17/08/2016: Italo Jurisato Junior
|*  - Criado uma Unit especifica para as constantes usadas pelo componente
|*    ACBrMDFe
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeConsts;

interface

uses
  SysUtils;

const
  DSC_NMDF        = 'Número do Manifesto';
  DSC_CMDF        = 'Código numérico que compõe a Chave de Acesso';
  DSC_TPEMIT      = 'Tipo do Emitente';
  DSC_TPTRANSP    = 'Tipo de Transportador';
  DSC_CMUNCARREGA = 'Código do Município de Carregamento';
  DSC_XMUNCARREGA = 'Nome do Município de Carregamento';
  DSC_SEGCODBARRA = 'Segundo código de barras';
  DSC_NCT         = 'Número do CT';
  DSC_SUBSERIE    = 'Subsérie do CT';
  DSC_PIN         = 'PIN SUFRAMA';
  DSC_QCTE        = 'Quantidade total de CTe relacionados no Manifesto';
  DSC_QCT         = 'Quantidade total de CT relacionados no Manifesto';
  DSC_QNFE        = 'Quantidade total de NFe relacionados no Manifesto';
  DSC_QNF         = 'Quantidade total de NF relacionados no Manifesto';
  DSC_QCARGA      = 'Peso Bruto Total da Carga / Mercadoria Transportada';
  DSC_DHINIVIAGEM = 'Data e Hora previstas de Inicio da Viagem';
  DSC_INDREENTREGA = 'Indicador de reentrega';
  DSC_QTDTOTAL    = 'Quantidade Total';
  DSC_QTDPARCIAL  = 'Quantidade Parcial';


  // Rodoviário
  DSC_CIOT        = 'Código Identificador da Operação de Transporte';
  DSC_CINTV       = 'Código interno do veículo';
  DSC_TARA        = 'Tara em KG';
  DSC_CAPKG       = 'Capacidade em KG';
  DSC_CAPM3       = 'Capacidade em m3';
  DSC_CNPJFORN    = 'CNPJ da empresa fornecedora do Vale-Pedágio';
  DSC_CNPJPG      = 'CNPJ do responsável pelo pagamento do Vale-Pedágio';
  DSC_NCOMPRA     = 'Número do comprovante de compra';
  DSC_CODAGPORTO  = 'Código de Agendamento no Porto';

  // Aéreo
  DSC_NAC         = 'Marca da Nacionalidade da Aeronave';
  DSC_MATR        = 'Marca da Matricula da Aeronave';
  DSC_NVOO        = 'Número do Vôo';
  DSC_CAEREMB     = 'Aeródromo de Embarque';
  DSC_CAERDES     = 'Aeródromo de Destino';
  DSC_DVOO        = 'Data do Vôo';

  // Aquaviário
  DSC_CNPJAGENAV  = 'CNPJ da Agência de Navegação';
  DSC_TPEMB       = 'Tipo de Embarcação';
  DSC_CEMBAR      = 'Código da Embarcação';
  DSC_XEMBAR      = 'Nome da Embarcação';
  DSC_NVIAG       = 'Número da Viagem';
  DSC_CPRTEMB     = 'Código do Porto de Embarque';
  DSC_CPRTDEST    = 'Código do Porto de Destino';
  DSC_PRTTRANS    = 'Porto de Transbordo';
  DSC_CTERMCARREG = 'Código do Terminal de Carregamento';
  DSC_XTERMCARREG = 'Nome do Terminal de Carregamento';
  DSC_CTERMDESCAR = 'Código do Terminal de Descarregamento';
  DSC_XTERMDESCAR = 'Nome do Terminal de Descarregamento';
  DSC_CEMBCOMB    = 'Código da Embarcação do comboio';
  DSC_XBALSA      = 'Identificador da Balsa';
  
  // Ferroviário
  DSC_XPREF       = 'Prefixo do Trem';
  DSC_DHTREM      = 'Data e Hora de liberação do Trem na origem';
  DSC_XORI        = 'Origem do Trem';
  DSC_XDEST       = 'Destino do Trem';
  DSC_QVAG        = 'Quantidade de vagões carregados';
  DSC_NVAG        = 'Número de Identificação do vagão';
  DSC_NSEQ        = 'Sequência do vagão na composição';
  DSC_TU          = 'Tonelada Útil';

  DSC_INDCANALVERDE = 'Indicador de Canal Verde';
  DSC_INDCARREGAPOSTERIOR = 'Indicador de Carregamento Posterior';
  DSC_INFQRCODMDFE  = 'Texto com o QR-Code impresso no DAMDFE';

implementation

end.

