{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
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
|*    ACBrCTe
*******************************************************************************}

{$I ACBr.inc}

unit pcteConsts;

interface

uses
  SysUtils;

const
  xRazao = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

  DSC_TPCTe    = 'Tipo do Conhecimento';
  DSC_CMUNEMI  = 'Código do Município onde o CT-e está sendo emitido';
  DSC_ORIGCALC = 'Município origem para efeito de cálculo do frete';
  DSC_DESTCALC = 'Município destino para efeito de cálculo do frete';
  DSC_XOBS     = 'Observações Gerais';
  DSC_TOMA     = 'Tomador do Serviço';
  DSC_INFNF    = 'Informações da Nota Fiscal';
  DSC_INFNFE   = 'Informações da Nota Fiscal Eletrônica';
  DSC_NDOC     = 'Número da Nota Fiscal';
  DSC_PESO     = 'Peso';
  DSC_TPDOC    = 'Tipo de documento originário';
  DSC_OUTROS   = 'Descrição';
  DSC_VTPREST  = 'Valor total da prestação do serviço';
  DSC_VREC     = 'Valor a Receber';
  DSC_XNOMEC   = 'Nome do Componente';
  DSC_VCOMP    = 'Valor do Componente';
  DSC_VCRED    = 'Valor do Crédito outorgado/presumido';
  DSC_VMERC    = 'Valor da mercadoria para efeito de averbação';

  DSC_DPREV    = 'Data prevista de entrega';
  DSC_CINT     = 'Código interno do emitente';
  DSC_INFOUTRO = 'informações dos demais documentos';
  DSC_TPSERV   = 'Tipo do Serviço';
  DSC_RETIRA   = 'Recebedor retira na Filial?';
  DSC_PRED     = 'Produto predominante';
  DSC_OUTCAT   = 'Outras características da carga';
  DSC_VTMERC   = 'Valor total da mercadoria';
  DSC_INFQ     = 'Informações de quantidades da Carga';
  DSC_TPMED    = 'Tipo da Medida';
  DSC_QTD      = 'Quantidade';
  DSC_DRET     = 'Detalhes da Retirada';

  DSC_XCARACAD  = 'Caracteristica adicional do transporte';
  DSC_XCARACSET = 'Caracteristica adicional do serviço';
  DSC_XEMI      = 'Funcionário emissor do CTe';
  DSC_XORIG     = 'Sigla ou código interno de Origem';
  DSC_XPASS     = 'Sigla ou código interno da Passagem';
  DSC_XDEST     = 'Sigla ou código interno do Destino';
  DSC_XROTA     = 'Código da Rota de Entrega';
  DSC_TPPER     = 'Tipo de data/período programado para entrega';
  DSC_DPROG     = 'Data programada';
  DSC_DINI      = 'Data inicial';
  DSC_DFIM      = 'Data final';
  DSC_TPHOR     = 'Tipo de hora para entrega';
  DSC_HPROG     = 'Hora programada';
  DSC_HINI      = 'Hora inicial';
  DSC_HFIM      = 'Hora final';
  DSC_NROMA     = 'Numero do Romaneio';
  DSC_NPED      = 'Numero do Pedido';
  DSC_VUNITV    = 'Valor Unitário do Veículo';
  DSC_VFRETEV   = 'Frete Unitário';
  DSC_CIOT      = 'Código Identificador da Operação de Transporte';
  DSC_NOCC      = 'Número da Ordem de Coleta';
  DSC_NCOMPRA   = 'Número do Comprovante de Compra';
  DSC_CINTV     = 'Código interno do Veículo';
  DSC_TARA      = 'Tara em KG';
  DSC_CAPKG     = 'Capacidade em KG';
  DSC_CAPM3     = 'Capacidade em M3';
  DSC_INDSN     = 'Indicador de Simples Nacional';
  DSC_NMINU     = 'Número da Minuta';
  DSC_NOCA      = 'Número Operacional do Conhecimento Aéreo';
  DSC_XLAGEMI   = 'Identificação do Emissor';
  DSC_IDT       = 'Identificação Interna do Tomador';
  DSC_CL        = 'Classe';
  DSC_CTAR      = 'Código da Tarifa';
  DSC_VTAR      = 'Valor da Tarifa';
  DSC_XDIME     = 'Dimensão';
  DSC_CINFMANU  = 'Informações de Manuseio';
  DSC_CIMP      = 'Carga Especial';
  DSC_VPREST    = 'Valor da prestação BC do AFRMM';
  DSC_NBOOKING  = 'Número do Booking';
  DSC_NCTRL     = 'Número de Controle';
  DSC_XNAVIO    = 'Identificação do Navio';
  DSC_XBALSA    = 'Identificador da Balsa';
  DSC_NVIAG     = 'Número da Viagem';
  DSC_DIREC     = 'Direção';
  DSC_PRTEMB    = 'Porto de Embarque';
  DSC_PRTTRANS  = 'Porto de Transbordo';
  DSC_PRTDEST   = 'Porto de Destino';
  DSC_TPTRAF    = 'Tipo de Tráfego';
  DSC_RESPFAT   = 'Responsável pelo Faturamento';
  DSC_FERREMI   = 'Ferrovia Emitente do CTe';
  DSC_FLUXO     = 'Fluxo Ferroviário';
  DSC_IDTREM    = 'Identificação do Trem';
  DSC_CINTF     = 'Código interno da Ferrovia envolvida';
  DSC_CAPTO     = 'Capacidade em Toneladas';
  DSC_TPVAG     = 'Tipo de Vagão';
  DSC_PESOR     = 'Peso Real em Toneladas';
  DSC_PESOBC    = 'Peso Base de Calculo de Frete em Toneladas';
  DSC_COTM      = 'Número do Certificado do Operador de Transporte Multimodal';
  DSC_INDNEG    = 'Indicador Negociável';

  DSC_PONTOFULGOR  = 'Ponto de Fulgor';
  DSC_QTOTEMB      = 'Quantidade Total de Embalagens';
  DSC_UNIAP        = 'Unidade de Medida';

  DSC_NCONT            = 'Identificação do Container';
  DSC_DETCONT          = 'Detalhamento dos Containers';
  DSC_INDGLOBALIZADO   = 'Indicador de CT-e Globalizado';
  DSC_INDIETOMA        = 'Indicador de I.E. do tomador';
  DSC_PASS             = 'Passagens';
  DSC_XDESCSERV        = 'Descrição do Serviço';
  DSC_QCARGA           = 'Quantidade de Carga';
  DSC_IDDOCANTPAP      = 'Documentos de transporte anterior de Papel';
  DSC_IDDOCANTELE      = 'Documentos de transporte anterior eletrônicos';
  DSC_IDDOCANT         = 'Documentos de transporte anterior';
  DSC_EMIDOCANT        = 'Emissor do documento anterior';
  DSC_OCC              = 'Ordens de Coleta associados';
  DSC_VEIC             = 'Veiculos';
  DSC_TAF              = 'Termo de Autorização de Fretamento';
  DSC_NROREGESTADUAL   = 'Número do Registro Estadual';
  DSC_PERI             = 'Produtos Perigosos';
  DSC_VEICNOVOS        = 'Veículos Novos';
  DSC_INDALTERATOMA    = 'Indicador de alteração de Tomador';
  DSC_CHCTEMULTIMODAL  = 'Chave do CT-e Multimodal';
  DSC_INFCTEMULTIMODAL = 'Informações do CT-e Multimodal';
  DSC_TPFRETAMENTO     = 'Tipo de Fretamento';
  DSC_DHVIAGEM         = 'Data / Hora da Viagem';

implementation

end.

