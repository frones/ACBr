{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDFeConsts;

interface

const
  CODIGO_BRASIL = 1058;
  CMUN_EXTERIOR = 9999999;
  XML_V01 = '?xml version="1.0"?';
  ENCODING_UTF8 = '?xml version="1.0" encoding="UTF-8"?';
  ENCODING_UTF8_STD = '?xml version="1.0" encoding="UTF-8" standalone="no"?';
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

resourcestring
  // Descrição de Mensagens de Erro
  ERR_MSG_MAIOR = 'Tamanho maior que o máximo permitido';
  ERR_MSG_MENOR = 'Tamanho menor que o mínimo permitido';
  ERR_MSG_VAZIO = 'Nenhum valor informado';
  ERR_MSG_INVALIDO = 'Conteúdo inválido';
  ERR_MSG_MAXIMO_DECIMAIS = 'Numero máximo de casas decimais permitidas';
  ERR_MSG_GERAR_CHAVE = 'Erro ao gerar a chave da DFe!';
  ERR_MSG_FINAL_MENOR_INICIAL = 'O numero final não pode ser menor que o inicial';
  ERR_MSG_ARQUIVO_NAO_ENCONTRADO = 'Arquivo não encontrado';
  ERR_MSG_SOMENTE_UM = 'Somente um campo deve ser preenchido';
  ERR_MSG_MAIOR_MAXIMO = 'Número de ocorrências maior que o máximo permitido - Máximo ';
  ERR_MSG_MENOR_MINIMO = 'Número de ocorrências menor que o mínimo permitido - Mínimo ';

  // Descrição de Documentos
  DSC_CNPJ = 'CNPJ(MF)';
  DSC_CPF = 'CPF';
  DSC_IE = 'Inscrição Estadual';
  DSC_IM = 'Inscrição Municipal';

  // Descrição de Documentos Fiscais
  DSC_MOD = 'Modelo do Documento Fiscal';
  DSC_NDF = 'Numero do Documento Fiscal';
  DSC_SERIE = 'Série do Documento Fiscal';
  DSC_CDF = 'Código do Documento Fiscal';
  DSC_CDV = 'Digito Verificador';
  DSC_VDF = 'Valor Total do DF-e';

  // Descrição de Identificação do Documento Fiscal
  DSC_TPAMB = 'Identificação do Ambiente';
  DSC_CUF = 'Código IBGE da UF';
  DSC_DHEMI = 'Data e Hora de Emissão';
  DSC_DEMI = 'Data de emissão';
  DSC_HEMI = 'Hora de emissão';
  DSC_TPEMIS = 'Tipo de Emissão';
  DSC_TPIMP = 'Formato de Impressão do Documento Auxiliar';
  DSC_PROCEMI = 'Processo de emissão do DF-e';
  DSC_VERPROC = 'Versão do processo de emissão';
  DSC_DHCONT = 'Data e Hora da entrada em contingência';
  DSC_XJUSTCONT = 'Justificativa da entrada em contingência';
  DSC_QRCODNFCOM = 'Texto com o QR-Code para consulta';
  DSC_CHAVE = 'Chave do DFe';
  DSC_CHAVESEGURANCA = 'Chave de segurança';
  DSC_ANO = 'Ano';
  DSC_NNFINI = 'Numero inicial';
  DSC_NNFFIN = 'Numero final';
  DSC_XJUST = 'Justificativa';

  // Descrição de Dados da Pessoa Física ou Jurídica
  DSC_CRT = 'Código de Regime Tributário';
  DSC_XNOME = 'Razão social ou Nome';
  DSC_XFANT = 'Nome fantasia';
  DSC_XLGR = 'Logradouro';
  DSC_NRO = 'Número do Logradouro';
  DSC_XCPL = 'Complemento';
  DSC_XBAIRRO = 'Bairro';
  DSC_CMUN = 'Código IBGE do município';
  DSC_XMUN = 'Nome do município';
  DSC_CEP = 'CEP';
  DSC_UF = 'Sigla da UF';
  DSC_CPAIS = 'Código do País';
  DSC_XPAIS = 'Nome do País';
  DSC_FONE = 'Telefone';
  DSC_EMAIL = 'Endereço de E-mail';

  // Descrição de Informações do Contribuinte/Fisco
  DSC_XOBS = 'Observação';
  DSC_OBSCONT = 'Observações de interesse do Contribuite';
  DSC_INFCPL = 'Informações complementares de interesse do Contribuinte';
  DSC_OBSFISCO = 'Observações de interesse do Fisco';
  DSC_INFADFISCO = 'Informações adicionais de interesse do Fisco';
  DSC_XCAMPO = 'Identificação do Campo';
  DSC_XTEXTO = 'Conteúdo do Campo';

  // Descrição de Informações do Autorizado
  DSC_AUTXML = 'Autorizados para download do XML do DF-e';

  // Descrição de Informações do Certificado Digital
  DSC_DigestValue = 'Digest Value';
  DSC_SignatureValue = 'Signature Value';
  DSC_X509Certificate = 'X509 Certificate';

  // Descrição de Dados do Responsável Técnico
  DSC_XCONTATO = 'Nome da pessoa a ser contatada';
  DSC_IDCSRT = 'Identificador do código de segurança do responsável técnico';
  DSC_HASHCSRT = 'Hash do token do código de segurança do responsável técnico';

  // Descrição de Tributos
  DSC_CFOP = 'CFOP';
  DSC_CST = 'Código da situação tributária ';

  // Descrição de dados do DistribuicaoDFe
  DSC_ULTNSU = 'Último NSU recebido pela Empresa';
  DSC_NSU = 'NSU específico';

implementation

end.

