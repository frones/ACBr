{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior                                }
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

unit ACBrBoletoPcnConsts;

interface

uses
  SysUtils;

const
  DSC_USUARIO_SERVICO = 'Autenticação: nome do Usuário Serviço WebService';
  DSC_AUTENTICACAO = 'Autenticação do usuário';
  DSC_KEYUSER = 'Key User Código Chave Usuário';
  DSC_VERSAODF = 'Versão do Serviço WebService';
  DSC_TIPO_SERVICO = 'Tipo do Serviço WebServico';
  DSC_SISTEMA_ORIGEM = 'Código Sistema de Origem WebService';
  DSC_AGENCIA = 'Número da Agência';
  DSC_DATA_HORA = 'Data Hora de Envio Remessa';
  DSC_CODIGO_CEDENTE = 'Código do Cedente';
  DSC_CONVENIO = 'Número do Convenio';
  DSC_CARTEIRA = 'Número da Carteira';
  DSC_VARIACAO_CARTEIRA = 'Número da variação de Carteira';
  DSC_MODALIDADE = 'Modalidade do Título';
  DSC_CODIGO_MODALIDADE = 'Codigo Modalidade Titulo';
  DSC_TIPO_HIBRIDO = 'Tipo Híbrido';
  DSC_NOSSO_NUMERO = 'Nosso Número';
  DSC_NUMERO_DOCUMENTO = 'Número do Documento';
  DSC_DATA_VENCIMENTO = 'Vencimento Título';
  DSC_VALOR_DOCUMENTO = 'Valor do Título';
  DSC_TIPO_ESPECIE = 'Tipo Especie';
  DSC_ACEITE = 'Aceite';
  DSC_DATA_DOCUMENTO = 'Data Documento';
  DSC_VALOR_ABATIMENTO = 'Valor Abatimento';
  DSC_VALOR_IOF = 'Valor IOF';
  DSC_MOEDA = 'Código Moeda';
  DSC_CODIGO_MORA_JUROS = 'Código Mora Juros';
  DSC_DATA_MORA_JUROS = 'Data Mora Juros';
  DSC_VALOR_MORA_JUROS = 'Valor Mora Juros';
  DSC_CODIGO_NEGATIVACAO = 'Código Negativação';
  DSC_DIAS_PROTESTO = 'Número Dias Protesto';
  DSC_NOME_SACADO = 'Nome do Sacado';
  DSC_LOGRADOURO = 'Logradouro do Sacado';
  DSC_BAIRRO = 'Bairro do Sacado';
  DSC_CIDADE = 'Cidade do Sacado';
  DSC_UF = 'UF do Sacado';
  DSC_CEP = 'CEP do Sacado';
  DSC_NUMERO_SACADO = 'Número do Endereço do Sacado';
  DSC_FONE = 'Fone do Sacado';
  DSC_NOME_AVALISTA = 'Nome Avalista';
  DSC_DATA_MULTA = 'Data Multa';
  DSC_PERCENTUAL_MULTA = 'Percentual Multa';
  DSC_TIPO_DESCONTO = 'Tipo Desconto';
  DSC_DATA_DESCONTO = 'Data Desconto';
  DSC_VALOR_DESCONTO = 'Valor Desconto';
  DSC_DATA_DESCONTO2 = 'Data Desconto2';
  DSC_VALOR_DESCONTO2 = 'Valor Desconto2';
  DSC_MENSAGEM = 'Mensagem';
  DSC_INSTRUCAO1 = 'Instrução 1';
  DSC_INSTRUCAO2 = 'Instrução 2';
  DSC_INSTRUCAO3 = 'Instrução 3';
  DSC_QTDE_PAGAMENTO_PARCIAL = 'Qtde Pagamento Parcial';
  DSC_TIPO_PAGAMENTO = 'Tipo de Pagamento';
  DSC_VALOR_MIN_PAGAMENTO = 'Valor Min Pagamento';
  DSC_VALOR_MAX_PAGAMENTO = 'Valor Max Pagamento';
  DSC_PERCENTUAL_MIN_PAGAMENTO = 'Percentual Min Pagamento';
  DSC_PERCENTUAL_MAX_PAGAMENTO = 'Percentual Max Pagamento';
  DSC_CANAL_SOLICITACAO = 'Canal de Solicitação do Serviço';
  DSC_DATA_LIMITE_PAGAMENTO = 'Data limite para pagamento';

implementation

end.
