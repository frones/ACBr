{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXConsts;

interface

uses
  SysUtils;

resourcestring
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
  DSC_CODQRT = 'Codigo Interno do Quarto';
  DSC_QTDHOSPDS = 'Quantidade de Hospedes';
  DSC_CHECKIN = 'Chechin';
  DSC_QTDDIAR = 'Quantidade de Diarias';
  DSC_VDIAR = 'Valor da Diaria';
  DSC_INSCMUN = 'Inscrição Municipal';
  DSC_GENERICOSTITULO = 'Título do campo livre';
  DESC_GENERICOSDESCRICAO = 'Conteúdo do campo livre.';
  DSC_VTTS = 'Valor Taxa Turismo';
  DSC_QDiaria = 'Quantidade Diaria';

  // Códigos e Descrições das mensagens
  Cod001 = 'X001';
  Desc001 = 'Serviço não implementado pelo Provedor.';
  Cod002 = 'X002';
  Desc002 = 'Nenhum RPS adicionado ao componente.';
  Cod003 = 'X003';
  Desc003 = 'Conjunto de RPS transmitidos (máximo de xxx RPS) excedido. Quantidade atual: yyy';
  Cod004 = 'X004';
  Desc004 = 'Nenhum Evento adicionado ao componente';
  Cod005 = 'X005';
  Desc005 = 'Conjunto de RPS transmitidos (mínimo de xxx RPS). Quantidade atual: yyy';

  Cod101 = 'X101';
  Desc101 = 'Número do Protocolo não informado.';
  Cod102 = 'X102';
  Desc102 = 'Número do RPS não informado.';
  Cod103 = 'X103';
  Desc103 = 'Série do Rps não informada.';
  Cod104 = 'X104';
  Desc104 = 'Tipo do Rps não informado.';
  Cod105 = 'X105';
  Desc105 = 'Número Inicial da NFSe não informado.';
  Cod106 = 'X106';
  Desc106 = 'Número Final da NFSe não informado.';
  Cod107 = 'X107';
  Desc107 = 'Pedido de Cancelamento não informado.';
  Cod108 = 'X108';
  Desc108 = 'Número da NFSe não informado.';
  Cod109 = 'X109';
  Desc109 = 'Código de Cancelamento não informado.';
  Cod110 = 'X110';
  Desc110 = 'Motivo do Cancelamento não informado.';
  Cod111 = 'X111';
  Desc111 =	'Número do Lote não informado.';
  Cod112 = 'X112';
  Desc112 = 'Série da NFSe não informada.';
  Cod113 = 'X113';
  Desc113 = 'Valor da NFSe não informado.';
  Cod114 = 'X114';
  Desc114	= 'Tipo da NFSe não informado.';
  Cod115 = 'X115';
  Desc115	= 'Data Inicial não informada.';
  Cod116 = 'X116';
  Desc116 =	'Data Final não informada.';
  Cod117 = 'X117';
  Desc117 = 'Código de Verificação/Validação não informado.';
  Cod118 = 'X118';
  Desc118	= 'Chave da NFSe não informada.';
  Cod119 = 'X119';
  Desc119	= 'Emitente.WSUser não informado.';
  Cod120 = 'X120';
  Desc120	= 'Emitente.WSSenha não informada.';
  Cod121 = 'X121';
  Desc121	= 'Cadastro Econômico não informado.';
  Cod122 = 'X122';
  Desc122 = 'Data Emissão da NFSe não informada.';
  Cod123 = 'X123';
  Desc123 = 'Código do Serviço não informado.';
  Cod124 = 'X124';
  Desc124	= 'Emitente.WSChaveAcesso não informada.';
  Cod125 = 'X125';
  Desc125	= 'Emitente.WSChaveAutoriz não informada.';
  Cod126 = 'X126';
  Desc126	= 'Chave da DPS não informada.';
  Cod127 = 'X127';
  Desc127	= 'CNPJ do Tomador não informado.';
  Cod128 = 'X128';
  Desc128	= 'NSU não informado.';
  Cod129 = 'X129';
  Desc129	= 'Emitente.InscMun não informada.';
  Cod130 = 'X130';
  Desc130	= 'Emitente.CNPJ não informado.';
  Cod131 = 'X131';
  Desc131 =	'Data de Competencia não informada.';
  Cod132 = 'X132';
  Desc132 =	'Página de retorno da consulta não informada.';
  Cod133 = 'X133';
  Desc133	= 'Geral.CNPJPrefeitura não informado.';

  Cod201 = 'X201';
  Desc201 = 'WebService retornou um XML vazio.';
  Cod202 = 'X202';
  Desc202 = 'Lista de NFSe não encontrada! (ListaNfse)';
  Cod203 = 'X203';
  Desc203 = 'Não foi retornado nenhuma NFSe.';
  Cod204 = 'X204';
  Desc204 = 'Confirmação do Cancelamento não encontrada.';
  Cod205 = 'X205';
  Desc205 = 'Retorno da Substituição não encontrado.';
  Cod206 = 'X206';
  Desc206 = 'Nfse Substituida não encontrada.';
  Cod207 = 'X207';
  Desc207 = 'Nfse Substituidora não encontrada.';
  Cod208 = 'X208';
  Desc208	= 'Não foi retornado nenhum Rps.';
  Cod209 = 'X209';
  Desc209 = 'Retorno do Cancelamento não encontrado.';
  Cod210 = 'X210';
  Desc210 = 'Nfse do Cancelamento não encontrada.';
  Cod211 = 'X211';
  Desc211 = 'Não foi retornado nenhum Evento.';
  Cod212 = 'X212';
  Desc212 = 'Não foi retornado nenhum JSON.';
  Cod213 = 'X213';
  Desc213 = 'Não foi retornado nenhum Token.';

  Cod800 = 'X800';
  Desc800 = 'Erro de Validação: ';
  Cod801 = 'X801';
  Desc801 = 'Erro ao Assinar: ';

  Cod999 = 'X999';
  Desc999 = 'Erro de Conexão: ';

implementation

end.

