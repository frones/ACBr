{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrBPeConsts;

interface

uses
  SysUtils;

const
  NAME_SPACE_BPE = 'xmlns="http://www.portalfiscal.inf.br/bpe"';

  DSC_CHBPE = 'Chave do Bilhete de Passagem Eletrônico';
  DSC_INFQRCODEBPE = 'QR-Code do BP-e';
  DSC_BOARDPASSBPE = 'Boarding Pass do BP-e';
  DSC_MODALBPE = 'Modal do BP-e';
  DSC_TPBPE = 'Tipo de BP-e';
  DSC_INDPRESBPE = 'Indicador de Presença';
  DSC_UFINIBPE = 'UF de Inicio';
  DSC_CMUNINIBPE = 'Código do Municipio de Inicio';
  DSC_UFFIMBPE = 'UF de Fim';
  DSC_CMUNFIMBPE = 'Código do Municipio de Fim';
  DSC_CRTBPE = 'Código do Regime Tributário';
  DSC_TAR = 'Termo de Autorização de Serviço Regular';
  DSC_IDESTRBPE = 'Indicador de Comprador Estrangeiro';
  DSC_TPSUB = 'Tipo de Substituição';
  DSC_CLOCORIG = 'Código do Local de Origem';
  DSC_XLOCORIG = 'Descrição do Local de Origem';
  DSC_CLOCDEST = 'Código do Local de Destino';
  DSC_XLOCDEST = 'Descrição do Local de Destino';
  DSC_DHEMB = 'Data e Hora de Embarque';
  DSC_DHVALIDADE = 'Data e Hora de Validade';
  DSC_XNOMEPASS = 'Nome do Passageiro';
  DSC_TPDOC = 'Tipo de Documento';
  DSC_NDOC = 'Numero do Documento';
  DSC_DNASC = 'Data de Nascimento';
  DSC_CPERCURSO = 'Código do Percurso';
  DSC_XPERCURSO = 'Descrição do Percurso';
  DSC_TPVIAGEM = 'Tipo de Viagem';
  DSC_TPSERVICO = 'Tipo de Serviço';
  DSC_TPACOMODACAO = 'Tipo de Acomodação';
  DSC_TPTRECHO = 'Tipo de Trecho';
  DSC_DHVIAGEM = 'Data e Hora da Viagem';
  DSC_DHCONEXAO = 'Data e Hora da Conexão';
  DSC_INFVIAGEM = 'Informações da Viagem';
  DSC_PREFIXO = 'Prefixo da Linha';
  DSC_POLTRONA = 'Numero da Poltrona / Assento / Cabine';
  DSC_PLATAFORMA = 'Plataforma / Carro / Barco de Embarque';
  DSC_TPVEICULO = 'Tipo de Veículo Transportado';
  DSC_SITVEICULO = 'Situação do Veículo Transportado';
  DSC_VBP = 'Valor do Bilhete de Passagem';
  DSC_VDESCONTO = 'Valor do Desconto';
  DSC_VPGTO = 'Valor Pago';
  DSC_VTROCO = 'Valor do Troco';
  DSC_TPDESCONTO = 'Tipo de Desconto / Beneficio';
  DSC_XDESCONTO = 'Descrição do Tipo de Desconto / Beneficio Concedido';
  DSC_TPCOMP = 'Tipo de Componente';
  DSC_VCOMP = 'Valor do Componente';
  DSC_COMP = 'Componente do Valor do Bilhete';
  DSC_VCRED = 'Valor do Crédito Outorgado / Presumido';
  DSC_VTOTTRIB = 'Valor Aproximado dos Tributos';
  DSC_VBCUFFIM = 'Valor da BC';
  DSC_PFCPUFFIM = 'Percentual para Fundo ao Combate a Pobreza';
  DSC_PICMSUFFIM = 'Percentual do ICMS';
  DSC_PICMSINTER = 'Percentual do ICMS Interno';
  DSC_PICMSINTERPART = 'Percentual do ICMS Interno Parte';
  DSC_VFCPUFFIM = 'Valor do Fundo ao Combate a Pobreza';
  DSC_VICMSUFFIM = 'Valor do ICMS da UF de Fim';
  DSC_VICMSUFINI = 'Valor do ICMS da UF de Inicio';
  DSC_XPAG = 'Descição da forma de pagamento';
  DSC_XBAND = 'Descrição do tipo de bandeira';
  DSC_NSUTRANS = 'Numero Sequencial Unico da Transação';
  DSC_NSUHOST = 'Numero Sequencial Unico da Host';
  DSC_NPARCELAS = 'Numero de Parcelas';
  DSC_CDESCONTO = 'Código do Desconto / Beneficio Concedido';
  DSC_INFADCARD = 'Informações Adicionais do Cartão de Crédito';
  DSC_XDOC = 'Descrição do Documento';
  DSC_NDOCPAG = 'Numero do Documento para Pagamento';
  DSC_UFINIVIAGEM ='UF de inicio de Viagem';
  DSC_UFFIMVIAGEM = 'UF de fim de Viagem';
  DSC_NCONTINICIO = 'Contador início da viagem';
  DSC_NCONTFIM = 'Contador fim da viagem';
  DSC_QPASS = 'Quantidade de Passagens da viagem';
  DSC_QCOMP = 'Quantidade do componente';
  DSC_DCOMPET = 'Data de Competencia';

  DSC_IEST = 'Inscrição Estadual do Substituto tributário';
  DSC_CNAE = 'Classificação Nacional de Atividades Econômicas';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_VICMS = 'Valor do ICMS';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_VICMSDESON = 'Valor do ICMS Desoneração';
  DSC_CBENEF = 'Código de Benefício Fiscal na UF aplicado';

  DSC_TPAG = 'Forma de Pagamento';
  DSC_VPAG = 'Valor do Pagamento';
  DSC_TPINTEGRA = 'Tipo de Integração para pagamento';
  DSC_TBAND = 'Bandeira da Operadora de Cartão';
  DSC_CAUT = 'Número da Autorização';

  DSC_PLACA = 'Placa do Veículo';

implementation

end.

