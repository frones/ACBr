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

unit ACBrNFComConsts;

interface

uses
  SysUtils;

const
  CODIGO_BRASIL = 1058;
  CMUN_EXTERIOR = 9999999;
  ENCODING_UTF8 = '?xml version="1.0" encoding="UTF-8"?';
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

resourcestring
  NAME_SPACE_NFCom = 'xmlns="http://www.portalfiscal.inf.br/nfcom"';

  // Descrição de Mensagens de Erro - Futuramente vai para a unit ACBrDFeConsts
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

  // Descrição de Identificação do Documento Fiscal - Futuramente vai para a unit ACBrDFeConsts
  DSC_TPAMB = 'Identificação do Ambiente';
  DSC_CUF = 'Código IBGE da UF';
  DSC_DHEMI = 'Data e Hora de Emissão';
  DSC_DEMI = 'Data de emissão';
  DSC_HEMI = 'Hora de emissão';
  DSC_TPEMIS = 'Tipo de Emissão';
  DSC_VERPROC = 'Versão do processo de emissão';
  DSC_DHCONT = 'Data e Hora da entrada em contingência';
  DSC_XJUSTCONT = 'Justificativa da entrada em contingência';
  DSC_QRCODNFCOM = 'Texto com o QR-Code para consulta';
  DSC_CHAVESEGURANCA = 'Chave de segurança';

  // Descrição de Documentos Fiscais - Futuramente vai para a unit ACBrDFeConsts
  DSC_MOD = 'Modelo do Documento Fiscal';
  DSC_NDF = 'Numero do Documento Fiscal';
  DSC_SERIE = 'Série do Documento Fiscal';
  DSC_CDF = 'Código do Documento Fiscal';
  DSC_CDV = 'Digito Verificador';

  // Descrição de Dados da Pessoa Física ou Jurídica - Futuramente vai para a unit ACBrDFeConsts
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

  // Descrição de Documentos - Futuramente vai para a unit ACBrDFeConsts
  DSC_CNPJ = 'CNPJ(MF)';
  DSC_CPF = 'CPF';
  DSC_IE = 'Inscrição Estadual';
  DSC_IM = 'Inscrição Municipal';

  // Descrição de Informações do Contribuinte/Fisco - Futuramente vai para a unit ACBrDFeConsts
  DSC_XOBS = 'Observação';
  DSC_INFADFISCO = 'Informações adicionais de interesse do Fisco';
  DSC_INFCPL = 'Informações complementares de interesse do Contribuinte';

  // Descrição de Dados do Responsável Técnico - Futuramente vai para a unit ACBrDFeConsts
  DSC_XCONTATO = 'Nome da pessoa a ser contatada';
  DSC_IDCSRT = 'Identificador do código de segurança do responsável técnico';
  DSC_HASHCSRT = 'Hash do token do código de segurança do responsável técnico';
  DSC_CFOP = 'CFOP';
  DSC_VITEM = 'Valor unitário do item ';
  DSC_VDESC = 'Valor do Desconto';
  DSC_VOUTRO = 'Outras despesas acessórias ';
  DSC_VPROD = 'Valor total do item';
  DSC_CST = 'Classificação Tributária';
  DSC_VBC = 'Valor da Base de Calculo';
  DSC_PICMS = 'Alíquota do ICMS';
  DSC_VICMS = 'Valor do ICMS';
  DSC_PFCP = 'Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_VFCP = 'Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_PREDBC = 'Percentual de redução da Base de Calculo';
  DSC_VICMSDESON = 'Valor do ICMS de desoneração';
  DSC_CBENEF = 'Código de Benefício Fiscal na UF aplicado ao item';
  DSC_VBCUFDEST = 'Valor da BC do ICMS na UF de destino';
  DSC_PFCPUFDEST = 'Percentual do ICMS relativo ao Fundo de Combate à pobreza (FCP) na UF de destino';
  DSC_PICMSUFDEST = 'Alíquota interna da UF de destino';
  DSC_VFCPUFDEST = 'Valor do ICMS relativo ao Fundo de Combate á Pobreza (FCP) da UF de destino';
  DSC_VICMSUFDEST = 'Valor do ICMS de partilha para a UF de destino';
  DSC_VICMSUFEMI = 'Valor do ICMS de partilha para a UF de emissão';
  DSC_PPIS = 'Alíquota do PIS';
  DSC_VPIS = 'Valor do PIS';
  DSC_PCOFINS = 'Alíquota do COFINS';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_VNF = 'Valor Total do Documento';
  DSC_DVENC = 'Data de vencimento';

  DSC_NSITEAUTORIZ = 'Numero do Site de Autorização';
  DSC_FINNFCom = 'Finalidade de emissão da NFCom';
  DSC_TPFAT = 'Tipo de Faturamento da NFCom';
  DSC_IEUFDEST = 'Inscrição Estadual Virtual do emitente na UF de Destino da partilha (IE Virtual)';
  DSC_IDESTR = 'Idenditicação do destinatário outros';
  DSC_INDIEDEST = 'Indicador da IE do Destinatário';
  DSC_ICODASSINANTE = 'Código único de Identificação do assinante';
  DSC_TPASSINANTE = 'Tipo de assinante';
  DSC_TPSERVUTIL = 'Tipo de serviço utilizado';
  DSC_NCONTRATO = 'Número do Contrato do assinante';
  DSC_DCONTRATOINI = 'Data de início do contrato';
  DSC_DCONTRATOFIM = 'Data de término do contrato';
  DSC_NROTERMPRINC = 'Número do Terminal Principal do serviço contratado';
  DSC_CHNFCOM = 'Chave de acesso da NFCom emitida pela Operadora Local';
  DSC_CMUNFG = 'Código do município de ocorrência do fato gerador';
  DSC_NROTERMADIC = 'Número dos Terminais adicionais do serviço contratado';
  DSC_MOTSUB = 'Motivo da substituição';
  DSC_COMPETEMIS = 'Ano e mês da emissão da NF (AAAAMM)';
  DSC_HASH115 = 'Hash do registro no arquivo do convênio 115';
  DSC_CPROD = 'Código do produto ou serviço.';
  DSC_XPROD = 'Descrição do produto ou serviço';
  DSC_CCLASS = 'Código de classificação';
  DSC_UMED = 'Unidade Básica de Medida';
  DSC_QFATURADA = 'Quantidade Faturada';
  DSC_DEXPIRACAO = 'Data de expiração de crédito';
  DSC_INFADPROD = 'Informações Adicionais do produto';
  DSC_VRETPIS = 'Valor do PIS retido';
  DSC_VRETCOFINS = 'Valor do COFNS retido';
  DSC_VRETCSLL = 'Valor da CSLL retida';
  DSC_VBCIRRF = 'Base de cálculo do IRRF';
  DSC_VIRRF = 'Valor do IRRF retido';
  DSC_TPPROC = 'Tipo de Processo';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_VFUNTTEL = 'Valor do FUNTTEL';
  DSC_VFUST = 'Valor do FUST';
  DSC_COMPETFAT = 'Ano e mês referência do faturamento (AAAAMM)';
  DSC_DPERUSOINI = 'Período de uso inicial';
  DSC_DPERUSOFIM = 'Período de uso final';
  DSC_CODBARRAS = 'Linha digitável do código de barras';
  DSC_CODDEBAUTO = 'Código de autorização débito em conta';
  DSC_CODBANCO = 'Número do banco para débito em conta';
  DSC_CODAGENCIA = 'Número da agência bancária para débito em conta';
  DSC_URLQRCODEPIX = 'URL do QRCode do PIX que será apresentado na fatura';
  DSC_PFUST = 'Alíquota do FUST (em percentual)';
  DSC_PFUNTTEL = 'Alíquota do FUNTTEL (em percentual)';
  DSC_TPRESSARC = 'Tipo de Ressarcimento';
  DSC_DREF = 'Data de referência';
  DSC_NPROTRECLAMA = 'Número do protocolo de reclamação';
  DSC_QTDSALDOPTS = 'Saldo de pontos do cliente na data de referência';
  DSC_DREFSALDOPTS= 'Data de aferição do saldo de pontos';
  DSC_QTDPTSRESG = 'Qtd de pontos resgatados na data de referência';
  DSC_DREFRESGPTS = 'Data de resgate dos pontos';

implementation

end.

