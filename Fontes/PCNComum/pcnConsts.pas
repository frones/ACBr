{******************************************************************************}
{ Projeto: Componente ACBrDFe                                                  }
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
|*  - Criado uma Unit especifica para as constantes usadas pelos componentes
|*    que geram DF-e
*******************************************************************************}

{$I ACBr.inc}

unit pcnConsts;

interface

uses
  SysUtils;

const
  DSC_CHAVE = 'Chave do DFe';
  DSC_ULTNSU = 'Último NSU recebido pela Empresa';
  DSC_NSU = 'NSU específico';
  DSC_NNF = 'Número do Documento Fiscal';
  DSC_NATOP = 'Descrição da Natureza da Operação';
  DSC_INDPAG = 'Indicador da forma de pagamento';
  DSC_TPIMP = 'Formato de Impressão do Documento Auxiliar';
  DSC_TPEMIS = 'Forma de Emissão do DF-e';
  DSC_PROCEMI = 'Processo de emissão do DF-e';
  DSC_VERPROC = 'Versão do Processo de emissão do DF-e';
  DSC_DHCONT = 'Data e Hora de entrada em contingencia';
  DSC_XJUSTCONT = 'Justificativa de entrada em contingencia';
  DSC_IEST = 'Inscrição Estadual do Substituto tributário';
  DSC_XFANT = 'Nome de Fantasia';
  DSC_FONE = 'Telefone';
  DSC_EMAIL = 'Endereço de Email';
  DSC_CMUN = 'Código do Município';
  DSC_CPAIS = 'Código do País';
  DSC_XPAIS = 'Nome do País';
  DSC_OBSCONT = 'Observações de interesse do contribuite';
  DSC_ISUF = 'Inscrição na SUFRAMA';
  DSC_INFADFISCO = 'Informações adicionais de interesse do Fisco';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_VIR = 'Valor do IR';
  DSC_VINSS = 'Valor do INSS';
  DSC_VCSLL = 'Valor do CSLL';
  DSC_VNF = 'Valor Total do DF-e';
  DSC_VBCICMS = 'BC do ICMS';
  DSC_VBCST = 'Valor da BC do ICMS ST';
  DSC_VST = 'Valor TOTAL Icms substituição Tributária';
  DSC_NLACRE = 'Número dos Lacres';
  DSC_REFNFE = 'Chave de acesso das NF-e referenciadas';
  DSC_TPNF = 'Tipo do Documento Fiscal';
  DSC_RENAVAM = 'RENAVAM';
  DSC_PLACA = 'Placa do Veículo';
  DSC_TPVEIC = 'Tipo de Veículo';
  DSC_VAFRMM = 'Valor da AFRMM';
  DSC_VFRETE = 'Valor Total do Frete';
  DSC_VAGAO = 'Identificação do vagão';
  DSC_CHASSI = 'Número do chassi';
  DSC_CCOR = 'Cor do Veículo';
  DSC_XCOR = 'Descrição da Cor';
  DSC_CMOD = 'Modelo do Veículo';
  DSC_NFAT = 'Número da fatura';
  DSC_VORIG = 'Valor Original da Fatura';
  DSC_VLIQ = 'Valor Líquido da Fatura';
  DSC_NDUP = 'Número da duplicata';
  DSC_DVENC = 'Data de vencimento';
  DSC_VDUP = 'Valor da duplicata';
  DSC_XSERV = 'Descrição do serviço';
  DSC_ANO = 'Ano';
  DSC_NNFINI = 'Numero inicial';
  DSC_NNFFIN = 'Numero final';
  DSC_XJUST = 'Justificativa';
  DSC_NREC = 'Numero do recibo';
  DSC_NSERIE = 'Número de série';
  DSC_NPROT = 'Numero do protocolo';
  DSC_CNAE = 'Classificação Nacional de Atividades Econômicas';
  DSC_QTDE = 'Quantidade';
  DSC_QTDEITENS = 'Quantidade de Itens na lista';
  DSC_INDISS = 'Indicador da Exigibilidade do ISS';
  DSC_VDESCINCOND = 'Valor Desconto Incondicionado';
  DSC_VDESCCOND = 'Valor Desconto Condicionado';
  DSC_INDISSRET = 'Indicador de ISS Retido';
  DSC_VISSRET = 'Valor Retenção ISS';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_TPAG = 'Forma de Pagamento';
  DSC_INDINCENTIVO = 'Indicador de Incentivo Fiscal';

  DSC_MODAL  = 'Tipo de Modal';
  DSC_RNTRC  = 'Registro Nacional de Transportadores Rodoviários de Carga';
  DSC_TPPROP = 'Tipo de Proprietário';
  DSC_TPROD  = 'Tipo de Rodado';
  DSC_TPCAR  = 'Tipo de Carroceria';
  DSC_REFCTE = 'Chave de acesso do CT-e referenciado';
  DSC_QTDRAT = 'Quantidade Rateada';
  DSC_VDOC   = 'Valor do documento';
  DSC_CUNID  = 'Código da unidade de medida';
  DSC_LACR   = 'Grupo de lacres';
  DSC_CHCTE  = 'Chave do CTe';
  DSC_LOTA   = 'Indicador de lotação';
  DSC_VVALEPED  = 'Valor do Vale-Pedagio';

  DSC_RESPSEG  = 'Responsável pelo Seguro';
  DSC_XSEG     = 'Nome da Seguradora';
  DSC_NAPOL    = 'Número da Apólice';
  DSC_NAVER    = 'Número da Averbação';
  DSC_INFSEG   = 'Informações de seguro da carga';
  DSC_UFPER    = 'Sigla da UF do percurso do veículo';

  DSC_NONU      = 'Número ONU/UN';
  DSC_XNOMEAE   = 'Nome apropriado para embarque do produto';
  DSC_XCLARISCO = 'Classe e Risco secundário';
  DSC_GREMB     = 'Grupo de Embalagem';
  DSC_QTOTPROD  = 'Quantidade total por produto';
  DSC_QVOLTIPO  = 'Quantidade e tipo de volumes';

  DSC_TPUNIDTRANSP = 'Tipo de Unidade de Transporte';
  DSC_IDUNIDTRANSP = 'Identificação da Unidade de Transporte';
  DSC_TPUNIDCARGA  = 'Tipo de Unidade de Carga';
  DSC_IDUNIDCARGA  = 'Identificação da Unidade de Carga';

  DSC_TPNAV     = 'Tipo de Navegação';
  DSC_IRIN      = 'Irin do navio sempre deverá ser informado';

  DSC_AUTXML   = 'Autorizados para download do XML do DF-e';
  DSC_XCONTATO = 'Nome do Contato';

  DSC_DigestValue = 'Digest Value';
  DSC_SignatureValue = 'Signature Value';
  DSC_X509Certificate = 'X509 Certificate';

  DSC_VPAG = 'Valor do Pagamento';
  DSC_TPINTEGRA = 'Tipo de Integração para pagamento';
  DSC_TBAND = 'Bandeira da Operadora de Cartão';
  DSC_CAUT = 'Número da Autorização';

  DSC_IDCSRT = 'Identificador CSRT - Código de Segurança do Responsável Técnico';
  DSC_HASHCSRT = 'Hash do CSRT - Código de Segurança do Responsável Técnico';

implementation

end.

