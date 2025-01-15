{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
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
                                  
// Documentação:
// https://apoio.developers.bb.com.br/referency/post/61cdac823948cb0012557c8f

{$I ACBr.inc}

unit ACBrSchemasPagamentosAPIBB;

interface

uses
  Classes, SysUtils,
  ACBrAPIBase, ACBrJSON,
  ACBrUtil.Base,
  ACBrUtil.Strings;

type

  TACBrPagamentosBBEstado = (
    pgeNenhum,
    pgeAgendado,       // Agendado - Pagamento aguardando a data para efetivação do crédito
    pgeCancelado,      // Cancelado - Pagamento cancelado pelo Cliente Conveniado antes da data do crédito
    pgeConsistente,    // Consistente - Dados recebidos pelo Banco sem ocorrências quanto ao formato. Aguardando validação dos dados para liberação/efetivação dos pagamentos
    pgeDevolvido,      // Devolvido - Pagamento efetuado e posteriormente recusado pelo recebedor. O valor é devolvida para a Conta corrente onde ocorreu o débito da requisição
    pgeInconsistente,  // Inconsistente - Dados recebidos pelo Banco com ocorrências quanto ao formato. A situação será alterada para rejeitado
    pgePago,           // Pago - Pagamento efetuado;
    pgePendente,       // Pendente - Falta autorização para o débito do pagamento na conta do cliente conveniado
    pgeRejeitado,      // Rejeitado - Dados do pagamento não passaram na validações físicas e/ou lógicas. Ex: agência e conta não existem, conta não pertence ao CPF informado
    pgeVencido        // Vencido - Pagamento não efetuado na data indicada por falta de saldo ou falta de autorização para débito do pagamento na conta do cliente conveniado
  );

  TACBrPagamentosBBLoteEstado = (
    pleNenhum,
    pleConsistente,        // 1 - Consistente
    pleInconsistente,      // 2 - Inconsistente
    plePendente,           // 3 - Pendente
    pleAgendado,           // 4 - Agendado
    plePago,               // 5 - Pago
    pleRejeitado,          // 6 - Rejeitado
    pleDevolvido,          // 7 - Devolvido
    pleCancelado,          // 8 - Cancelado
    pleDebitado,           // 9 - Debitado
    pleBloqueado,          // 10 - Bloqueado
    pleAguardandoDebito    // 11 - Aguardando Débito
  );

  TACBrPagamentosBBTipoBeneficiario = (
    ptbNenhum,
    ptbCPF,
    ptbCNPJ
  );

  TACBrPagamentosBBTipoConta = (
    ptcNenhum,
    ptcContaCorrente,   // 1 - Conta Corrente
    ptcContaPagamento,  // 2 - Conta Pagamento
    ptcContaPoupanca    // 3 - Conta Poupança
  );

  TACBrPagamenosBBTransferenciaErro = (
    pteNenhum,
    pteAgenciaZerada,                    // 1 - Agência de crédito está zerada. Informe o nº da Agência de Crédito
    pteAgenciaNaoNumerica,               // 2 - Conta de crédito informada não é numérica. Informe apenas números
    pteDVContaNaoInformado,              // 3 - Dígito da conta de crédito não informado. Informe o DV da conta de crédito
    pteCPFNaoNumerico,                   // 4 - CPF informado não é numérico. Informe apenas números
    pteCNPJNaoNumerico,                  // 5 - CNPJ informado não é numérico. Informe apenas números
    pteDataNaoInformada,                 // 6 - Data do pagamento não informada. Informe a data do pagamento
    pteDataInvalida,                     // 7 - Data do pagamento inválida. Verifique o dado informado
    pteValorNaoNumerico,                 // 8 - Valor do pagamento informado não é númerico. Informe apenas números
    pteValorZerado,                      // 9 - Valor do pagamento está zerado. Informe o valor do pagamento
    pteCompensacaoISPBNaoInformados,     // 10 - Ambos os campos Número Compensação e Número ISPB não foram informados. Informe um dos campos
    pteCompensacaoISPBInformados,        // 11 - Ambos os campos Número Compensação e Número ISPB foram informados. Informe apenas um dos campos
    pteDOCTEDNaoInformados,              // 12 - Ambos os campos Finalidade DOC e Finalidade TED não foram informados. Informe um dos campos
    pteDOCTEDInformados,                 // 13 - Ambos os campos Finalidade DOC e Finalidade TED foram informados. Informe apenas um dos campos
    pteNumDepositoJudicialNaoInformado,  // 14 - Número de depósito judicial não informado. Informe o número do depósito judicial
    pteDVContaInvalido,                  // 15 - Digito da conta de crédito inválido. Verifique o dado informado
    pteCPFCNPJInformados,                // 16 - Ambos os campos CPF e CNPJ foram informados. Informe apenas um dos campos. Caso informado os 2 campos, nas consultas será exibido apenas os dados do CPF
    pteCPFCNPJNaoInformaos,              // 17 - Ambos os campos CPF e CNPJ não foram informados. Informe um dos campos
    pteCPFInvalido,                      // 18 - Dígito do CPF inválido. Verifique o dado informado
    pteCNPJInvalido,                     // 19 - Dígito do CNPJ inválido. Verifique o dado informado
    pteAgenciaContaIguais,               // 20 - Agência e conta de crédito estão iguais às de débito. Opção não permitida
    pteNumCompensacaoInvalido,           // 21 - Número Compensação inválido. Verifique o dado informado
    pteISPBDiferenteDeZeros,             // 22 - Número ISPB diferente de zeros. Não informe o nº ISPB
    pteContaCreditoNaoInformada,         // 23 - Conta de crédito não informada. Informe o número da conta de crédito
    pteCPFNaoInformado,                  // 24 - CPF não informado. Informe o nº do CPF
    pteCNPJNaoInformado,                 // 25 - CNPJ foi informado. Não informe CNPJ
    pteContaCreditoInformada,            // 26 - Conta de crédito foi informada. Não informe Conta de crédito
    pteDVCreditoInformado,               // 27 - Dígito da conta de crédito foi informado. Não informe dígito da conta de crédito
    pteFinalidadeDOCInformada,           // 28 - Finalidade do DOC foi informada. Não informe finalidade do DOC
    pteFinalidadeTEDInformada,           // 29 - Finalidade da TED foi informada. Não informe finalidade da TED
    pteNumDepositoJudicialInformado,     // 30 - Número Depósito Judicial informado. Não informe finalidade Depósito Judicial
    pteDocumentoCreditoNaoNumerico,      // 31 - Número do documento de crédito informado não é numérico. Informe apenas números
    pteDocumentoDebitoNaoNumerico,       // 32 - Número do documento de débito não é numérico. Informe apenas números
    pteCPFNaoEncontrado,                 // 33 - CPF não encontrado na base da receita federal. Verifique o dado informado
    pteCNPJNaoEncontrado,                // 34 - CNPJ não encontrado na base da receita federal. Verifique o dado informado
    pteContaPoupancaNaoPermitida,        // 35 - Conta poupança não permitido para "Pagamento ao Fornecedor". Para creditar em conta poupança utilize o recurso para efetivação de "Pagamentos Diversos"
    pteCOMPEDeveSer1,                    // 36 - Código COMPE deve ser igual a 1
    pteISPBDeveSer0,                     // 37 - Código ISPB deve ser igual a 0
    pteCodBarrasNaoNumerio,              // 38 - Código de barras não é numérico. Informe apenas números
    pteCodBarrasIgualZeros,              // 39 - Código de barras igual a zeros. Informe apenas números
    pteNumInscricaoNaoNumerico,          // 40 - Número de inscrição do pagador não é numérico. Informe apenas números
    pteInscricaoBeneficiarioNaoNumerico, // 41 - Número de inscrição do beneficiário não é numérico. Informe apenas números
    pteInscricaoAvalistaNaoNumerico,     // 42 - Número de inscrição do avalista não é numérico. Informe apenas números
    pteDVCPFPagadorInvalido,             // 43 - Digito do CPF para o pagador inválido. Verifique o dado informado
    pteDVCPFBeneficiarioInvalido,        // 44 - Digito do CPF para o beneficiário inválido. Verifique o dado informado
    pteDVCPFAvalistaInvalido,            // 45 - Digito do CPF para o avalista inválido. Verifique o dado informado
    pteDVCNPJPagadorInvalido,            // 46 - Digito do CNPJ para o pagador inválido. Verifique o dado informado
    pteDVCNPJBeneficiarioInvalido,       // 47 - Digito do CNPJ para o beneficiário inválido. Verifique o dado informado
    pteDVCNPJAvalistaInvalido,           // 48 - Digito do CNPJ para o avalista inválido.Verifique o dado informado
    pteDataVencimentoInvalida,           // 49 - Data do vencimento inválida. Verifique o dado informado
    pteValorNominalNaoNumerico,          // 50 - Valor nominal não é numérico. Informe apenas números
    pteValorDescontoNaoNumerico,         // 51 - Valor de desconto não é numérico. Informe apenas números
    pteValorMoraNaoNumerico,             // 52 - Valor de mora não é numérico. Informe apenas números
    pteDataPagamentoMenorAtual,          // 53 - Data do pagamento deve ser maior ou igual ao dia atual
    pteDocDebitoNaoInformado,            // 54 - Número do documento de débito não informado. Informe o nº do doc de débito
    pteDataVencimentoNaoInformada,       // 55 - Data do vencimento não informada. Informe a data de vencimento
    pteNomeBeneficiarioNaoInformado,     // 56 - Nome do beneficiário não informado. Informe o nome do beneficiário
    pteInscricaoBeneficiarioNaoInformada,// 57 - Número de inscrição do beneficiário não informado. Informe o CPF ou o CNPJ do beneficiário
    pteContaPagamentoInformada,          // 58 - Conta pagamento foi informada. Não informe conta pagamento
    pteContaCreditoPagamentoInformada,   // 59 - Ambos os campos conta de crédito e conta pagamento foram informados. Informe apenas um dos campos
    pteConsultarBancoErro,               // 99 - Consultar o Banco para detalhar o erro
    pteInsuficienciaFundos,              // 200 - Insuficiência de Fundos - Débito Não Efetuado
    pteCreditoDebitoCancelado,           // 201 - Crédito ou Débito Cancelado pelo Pagador
    pteDebitoAutorizado,                 // 202 - Débito Autorizado pela Agência - Efetuado
    pteControleInvalido,                 // 203 - Controle Inválido. Verificar campos 01, 02 e 03 do header ou segmento A, B, C, J, J52, N, O ou W do Arquivo CNAB240.
    pteTipoOperacaoInvalido,             // 204 - Tipo de Operação Inválido. Verificar campo 04.1 do header de lote. Valor default = "C"
    pteTipoServicoInvalido,              // 205 - Tipo de Serviço Inválido. Utilize 20 para Pagamento a Fornecedores, 30 Pagamento de Salários ou 98 Pagamentos Diversos no header de Lote, campo 05.1, do CNAB240
    pteFormaLancamentoInvalida,          // 206 - Forma de Lançamento Inválida. Para crédito em Poupança utilize Pagamentos Diversos. Para crédito em Conta Pagamento utilize Pagamentos Diversos ou Pagamento a Fornecedores. Para Pagamento de salário a conta de crédito deve ser do BB.
    pteTipoNumeroInscricaoInvalido,      // 207 - Tipo/Número de Inscrição Inválido. CPF ou CNPJ inválido. Verifique dados informados.
    pteCodigoConvenioInvalido,           // 208 - Código de Convênio Inválido. Verifique dados informados.
    pteAgenciaContaCorrenteDVInvalido,   // 209 - Agência/Conta Corrente/DV Inválido. Verifique dados informados.
    pteNumeroSequencialRegistroInvalido, // 210 - Nº Seqüencial do Registro no Lote Inválido. Verifique dado informado.
    pteCodigoSegmentoDetalheInvalido,    // 211 - Código de Segmento de Detalhe Inválido. Verifique dado informado.
    pteLancamentoInconsistente,          // 212 - Lançamento inconsistente, rejeitado na prévia. Corrigir os dados do lançamento e enviar novo pagamento.
    pteNumeroCompeBancoCreditoInvalido,  // 213 - Nº Compe do Banco para crédito Inválido. Verifique dado informado.
    pteNumeroISPBInvalido,               // 214 - Nº do ISPB Banco, Instituição de Pagamento para crédito Inválido. Verifique dado informado.
    pteAgenciaMantenedoraInvalida,       // 215 - Agência Mantenedora da Conta Corrente do Favorecido Inválida. Verifique dado informado.
    pteContaCorrenteDVInvalido,          // 216 - Conta Corrente/DV/Conta de Pagamento do Favorecido Inválido. Verifique dado informado.
    pteNomeFavorecidoNaoInformado,       // 217 - Nome do Favorecido não Informado. Informe o nome do favorecido.
    pteDataLancamentoInvalida,           // 218 - Data Lançamento Inválido. Verifique dado informado.
    pteTipoQuantidadeMoedaInvalida,      // 219 - Tipo/Quantidade da Moeda Inválido. Verifique dado informado.
    pteValorLancamentoInvalido,          // 220 - Valor do Lançamento Inválido. Verifique dado informado.
    pteAvisoFavorecidoIdentificacaoInvalida, // 221 - Aviso ao Favorecido - Identificação Inválida.
    pteTipoNumeroInscricaoFavorecidoInvalido, // 222 - Tipo/Número de Inscrição do Favorecido Inválido CPF ou CNPJ do favorecido inválido. Arquivo: Verifique o campo 07.3B - registro detalhe do segmento B.
    pteLogradouroFavorecidoNaoInformado, // 223 - Logradouro do Favorecido não Informado. Informe o logradouro do favorecido.
    pteNumeroLocalFavorecidoNaoInformado,// 224 - Nº do Local do Favorecido não Informado. Informe o nº do local do favorecido.
    pteCidadeFavorecidoNaoInformada,     // 225 - Cidade do Favorecido não Informada. Informe a cidade do favorecido.
    pteCEPFavorecidoInvalido,            // 226 - CEP/Complemento do Favorecido Inválido. Verifique dado informado.
    pteSiglaEstadoFavorecidoInvalida,    // 227 - Sigla do Estado do Favorecido Inválida. Verifique dado informado.
    pteNumeroBancoCreditoInvalido,       // 228 - Nº do Banco para crédito Inválido. Verifique dado informado.
    pteCodigoNomeAgenciaDepositariaNaoInformado, // 229 - Código/Nome da Agência Depositária não Informado. Informe o dado solicitado.
    pteSeuNumeroInvalido,                // 230 - Seu Número Inválido. Verifique dado informado.
    pteNossoNumeroInvalido,              // 231 - Nosso Número Inválido. Verifique dado informado.
    pteInclusaoEfetuadaSucesso,          // 232 - Inclusão Efetuada com Sucesso
    pteAlteracaoEfetuadaSucesso,         // 233 - Alteração Efetuada com Sucesso
    pteExclusaoEfetuadaSucesso,          // 234 - Exclusão Efetuada com Sucesso
    pteAgenciaContaImpedidaLegalmente,   // 235 - Agência/Conta Impedida Legalmente
    pteEmpresaNaoPagouSalario,           // 236 - Empresa não pagou salário Conta de crédito só aceita pagamento de salário.
    pteFalecimentoMutuario,              // 237 - Falecimento do mutuário.
    pteEmpresaNaoEnviouRemessaMutuario,  // 238 - Empresa não enviou remessa do mutuário
    pteEmpresaNaoEnviouRemessaVencimento,// 239 - Empresa não enviou remessa no vencimento
    pteValorParcelaInvalida,             // 240 - Valor da parcela inválida. Verifique dado informado.
    pteIdentificacaoContratoInvalida,    // 241 - Identificação do contrato inválida. Verifique dado informado.
    pteOperacaoConsignacaoIncluidaSucesso, // 242 - Operação de Consignação Incluída com Sucesso
    pteOperacaoConsignacaoAlteradaSucesso, // 243 - Operação de Consignação Alterada com Sucesso
    pteOperacaoConsignacaoExcluidaSucesso, // 244 - Operação de Consignação Excluída com Sucesso
    pteOperacaoConsignacaoLiquidadaSucesso, // 245 - Operação de Consignação Liquidada com Sucesso
    pteReativacaoEfetuadaSucesso,        // 246 - Reativação Efetuada com Sucesso
    pteSuspensaoEfetuadaSucesso,         // 247 - Suspensão Efetuada com Sucesso
    pteCodigoBarrasBancoInvalido,        // 248 - Código de Barras - Código do Banco Inválido.
    pteCodigoBarrasMoedaInvalido,        // 249 - Código de Barras - Código da Moeda Inválido
    pteCodigoBarrasDigitoVerificadorInvalido, // 250 - Código de Barras - Dígito Verificador Geral Inválido
    pteCodigoBarrasValorTituloInvalido,  // 251 - Código de Barras - Valor do Título Inválido
    pteCodigoBarrasCampoLivreInvalido,   // 252 - Código de Barras - Campo Livre Inválido
    pteValorDocumentoInvalido,           // 253 - Valor do Documento Inválido. Verifique dado informado.
    pteValorAbatimentoInvalido,          // 254 - Valor do Abatimento Inválido. Verifique dado informado.
    pteValorDescontoInvalido,            // 255 - Valor do Desconto Inválido. Verifique dado informado.
    pteValorMoraInvalido,                // 256 - Valor de Mora Inválido. Verifique dado informado.
    pteValorMultaInvalido,               // 257 - Valor da Multa Inválido. Verifique dado informado.
    pteValorIRInvalido,                  // 258 - Valor do IR Inválido. Verifique dado informado.
    pteValorISSInvalido,                 // 259 - Valor do ISS Inválido. Verifique dado informado.
    pteValorIOFInvalido,                 // 260 - Valor do IOF Inválido. Verifique dado informado.
    pteValorOutrasDeducoesInvalido,      // 261 - Valor de Outras Deduções Inválido. Verifique dado informado.
    pteValorOutrosAcrescimosInvalido,    // 262 - Valor de Outros Acréscimos Inválido. Verifique dado informado.
    pteValorINSSInvalido,                // 263 - Valor do INSS Inválido. Verifique dado informado.
    pteLoteNaoAceito,                    // 264 - Lote Não Aceito. Reenvie os documentos.
    pteInscricaoEmpresaInvalidaContrato, // 265 - Inscrição da Empresa Inválida para o Contrato
    pteConvenioEmpresaInexistenteContrato, // 266 - Convênio com a Empresa Inexistente/Inválido para o Contrato
    pteAgenciaContaCorrenteEmpresaInexistenteContrato, // 267 - Agência/Conta Corrente da Empresa Inexistente/Inválido para o Contrato. Verifique dado informado.
    pteTipoServicoInvalidoContrato,      // 268 - Tipo de Serviço Inválido para o Contrato. Para contrato de Pagamentos, utilize 20 para Pagamento a Fornecedores, 30 Pagamento de Salários ou 98 Pagamentos Diversos no header de Lote, campo 05.1, do CNAB240
    pteContaCorrenteSaldoInsuficiente,   // 269 - Conta Corrente da Empresa com Saldo Insuficiente.
    pteLoteServicoForaSequencia,         // 270 - Lote de Serviço Fora de Seqüência
    pteLoteServicoInvalido,              // 271 - Lote de Serviço Inválido
    pteArquivoNaoAceito,                 // 272 - Arquivo não aceito
    pteTipoRegistroInvalido,             // 273 - Tipo de Registro Inválido
    pteCodigoRemessaRetornoInvalido,     // 274 - Código Remessa / Retorno Inválido
    pteVersaoLayoutInvalida,             // 275 - Versão de layout inválida
    pteMutuarioNaoIdentificado,          // 276 - Mutuário não identificado
    pteTipoBeneficioNaoPermiteEmprestimo, // 277 - Tipo do beneficio não permite empréstimo
    pteBeneficioCessadoSuspenso,         // 278 - Beneficio cessado/suspenso
    pteBeneficioPossuiRepresentanteLegal, // 279 - Beneficio possui representante legal
    pteBeneficioTipoPA,                  // 280 - Beneficio é do tipo PA (Pensão alimentícia)
    pteQuantidadeContratosExcedida,      // 281 - Quantidade de contratos permitida excedida
    pteBeneficioNaoPertenceBanco,        // 282 - Beneficio não pertence ao Banco informado
    pteInicioDescontoUltrapassado,       // 283 - Início do desconto informado já ultrapassado
    pteNumeroParcelaInvalida,            // 284 - Número da parcela inválida. Verifique dado informado.
    pteQuantidadeParcelaInvalida,        // 285 - Quantidade de parcela inválida. Verifique dado informado.
    pteMargemConsignavelExcedidaPrazo,   // 286 - Margem consignável excedida para o mutuário dentro do prazo do contrato. Verifique suas margens disponíveis.
    pteEmprestimoJaCadastrado,           // 287 - Empréstimo já cadastrado
    pteEmprestimoInexistente,            // 288 - Empréstimo inexistente
    pteEmprestimoJaEncerrado,            // 289 - Empréstimo já encerrado
    pteArquivoSemTrailer,                // 290 - Arquivo sem trailer
    pteMutuarioSemCreditoCompetencia,    // 291 - Mutuário sem crédito na competência
    pteNaoDescontadoOutrosMotivos,       // 292 - Não descontado – outros motivos
    pteRetornoCreditoNaoPago,            // 293 - Retorno de Crédito não pago
    pteCancelamentoEmprestimoRetroativo, // 294 - Cancelamento de empréstimo retroativo
    pteOutrosMotivosGlosa,               // 295 - Outros Motivos de Glosa
    pteMargemConsignavelExcedidaAcimaPrazo, // 296 - Margem consignável excedida para o mutuário acima do prazo do contrato
    pteMutuarioDesligadoEmpregador,      // 297 - Mutuário desligado do empregador. Pagamento não permitido.
    pteMutuarioAfastadoLicenca,          // 298 - Mutuário afastado por licença. Pagamento não permitido.
    ptePrimeiroNomeMutuarioDiferente,    // 299 - Primeiro nome do mutuário diferente do primeiro nome do movimento do censo ou diferente da base de Titular do Benefício. Verificar necessidade de ajustes.
    pteBeneficioSuspensoCessadoAPS,      // 300 - Benefício suspenso/cessado pela APS ou Sisobi
    pteBeneficioSuspensoDependenciaCalculo, // 301 - Benefício suspenso por dependência de cálculo
    pteBeneficioSuspensoCessadoInspetoria, // 302 - Benefício suspenso/cessado pela inspetoria/auditoria
    pteBeneficioBloqueadoEmprestimoBeneficiario, // 303 - Benefício bloqueado para empréstimo pelo beneficiário
    pteBeneficioBloqueadoEmprestimoTBM,  // 304 - Benefício bloqueado para empréstimo por TBM
    pteBeneficioFaseConcessaoPA,         // 305 - Benefício está em fase de concessão de PA ou desdobramento.
    pteBeneficioCessadoObito,            // 306 - Benefício cessado por óbito.
    pteBeneficioCessadoFraude,           // 307 - Benefício cessado por fraude.
    pteBeneficioCessadoOutroBeneficio,   // 308 - Benefício cessado por concessão de outro benefício.
    pteBeneficioCessadoEstatutario,      // 309 - Benefício cessado: estatutário transferido para órgão de origem.
    pteEmprestimoSuspensoAPS,            // 310 - Empréstimo suspenso pela APS.
    pteEmprestimoCanceladoBanco,         // 311 - Empréstimo cancelado pelo banco.
    pteCreditoTransformadoPAB,           // 312 - Crédito transformado em PAB.
    pteTerminoConsignacaoAlterado,       // 313 - Término da consignação foi alterado.
    pteFimEmprestimoPeriodoSuspensao,    // 314 - Fim do empréstimo ocorreu durante período de suspensão ou concessão.
    pteEmprestimoSuspensoBanco,          // 315 - Empréstimo suspenso pelo banco.
    pteNaoAverbacaoContratoQuantidadeParcelas, // 316 - Não averbação de contrato – quantidade de parcelas/competências informadas ultrapassou a data limite da extinção de cota do dependente titular de benefícios
    pteLoteNaoAceitoTotaisDiferenca,     // 317 - Lote Não Aceito - Totais do Lote com Diferença
    pteTituloNaoEncontrado,              // 318 - Título Não Encontrado
    pteIdentificadorRegistroOpcionalInvalido, // 319 - Identificador Registro Opcional Inválido. Verifique dado informado.
    pteCodigoPadraoInvalido,             // 320 - Código Padrão Inválido. Verifique dado informado.
    pteCodigoOcorrenciaInvalido,         // 321 - Código de Ocorrência Inválido. Verifique dado informado.
    pteComplementoOcorrenciaInvalido,    // 322 - Complemento de Ocorrência Inválido. Verifique dado informado.
    pteAlegacaoJaInformada,              // 323 - Alegação já Informada
    pteAgenciaContaFavorecidoSubstituida, // 324 - Agência / Conta do Favorecido Substituída. Verifique dado informado.
    pteDivergenciaNomeBeneficiario,      // 325 - Divergência entre o primeiro e último nome do beneficiário versus primeiro e último nome na Receita Federal. Verificar com beneficiário necessidade de ajustes.
    pteConfirmacaoAntecipacaoValor,      // 326 - Confirmação de Antecipação de Valor
    pteAntecipacaoParcialValor,          // 327 - Antecipação parcial de valor
    pteBoletoBloqueadoBase,              // 328 - Boleto bloqueado na base. Não passível de pagamento.
    pteSistemaContingenciaBoletoValorMaior, // 329 - Sistema em contingência – Boleto valor maior que referência. Consulte o beneficiário ou tente efetuar o pagamento mais tarde.
    pteSistemaContingenciaBoletoVencido, // 330 - Sistema em contingência – Boleto vencido. Consulte o beneficiário ou tente efetuar o pagamento mais tarde.
    pteSistemaContingenciaBoletoIndexado, // 331 - Sistema em contingência – Boleto indexado. Consulte o beneficiário ou tente efetuar o pagamento mais tarde.
    pteBeneficiarioDivergente,           // 332 - Beneficiário divergente. Verifique dado informado.
    pteLimitePagamentosParciaisExcedido, // 333 - Limite de pagamentos parciais do boleto excedido. Consulte o Beneficiário do boleto.
    pteBoletoJaLiquidado,                // 334 - Boleto já liquidado. Não passível de pagamento.
    pteConsultarBancoDetalharErro        // 999 - Consultar o Banco para detalhar o erro.
  );

  TACBrPagamentosBBFormaIdentificacao = (
    pfiNenhum,
    pfiTelefone,        // 1 igual a Chave Pix tipo Telefone
    pfiEmail,           // 2 igual a Chave Pix tipo Email
    pfiCPFCNPJ,         // 3 igual a Chave Pix tipo CPF/CNPJ
    pfiChaveAleatoria,  // 4 igual a Chave Aleatória
    pfiDadosBancarios   // 5 igual a Dados Bancários
  );

  TACBrPagamentosBBEstadoRequisicao = (
    perNenhum,
    perDadosConsistentes,              // 1 - Requisição com todos os lançamentos com dados consistentes
    perDadosInconsistentesParcial,     // 2 - Requisição com ao menos um dos lançamentos com dados inconsistentes
    perDadosInconsistentesTotal,       // 3 - Requisição com todos os lançamentos com dados inconsistentes
    perPendenteAcaoConveniado,         // 4 - Requisição pendente de ação pelo Conveniado - falta autorizar o pagamento
    perEmProcessamentoBanco,           // 5 - Requisição em processamento pelo Banco
    perProcessada,                     // 6 - Requisição Processada
    perRejeitada,                      // 7 - Requisição Rejeitada
    perPreparandoRemessaNaoLiberada,   // 8 - Preparando remessa não liberada
    perLiberadaViaAPI,                 // 9 - Requisição liberada via API
    perPreparandoRemessaLiberada       // 10 - Preparando remessa liberada
  );

  TACBrPagamentosBBTipoPagamento = (
    ppgNenhum,
    ppgPagamentoFornecedores,  // 126 - Pagamento de fornecedores
    ppgPagamentoSalario,       // 127 - Pagamento de salário
    ppgPagamentoDiverso        // 128 - Pagamento diverso
  );

  TACBrPagamentosBBTipoCredito = (
    pcrNenhum,
    pcrCreditoContaCorrente,           // 1 - Crédito em Conta Corrente
    pcrDOCTED,                         // 3 - DOC ou TED
    pcrCreditoContaPoupanca,           // 5 - Crédito em Conta Poupança
    pcrLiquidacaoGuiaCodigoBarra,      // 13 - Liquidação de Guia com Código de Barra
    pcrLiquidacaoGuiaSemCodigoBarra,   // 21 - Liquidação de Guia sem Código de Barra
    pcrLiquidacaoBoletoBB,             // 30 - Liquidação de Boleto do Banco do Brasil
    pcrLiquidacaoBoletoOutrosBancos,   // 31 - Liquidação de Boleto de Outros Bancos
    pcrDepositoJudicial                // 71 - Depósito Judicial
  );

  TACBrPagamentosBBCodigoDevolucao = (
    pcdNenhum,
    pcdAgenciaCreditoZerada,                          // 1 - Agência de crédito está zerada. Informe o nº da Agência de Crédito
    pcdContaCreditoNaoNumerica,                       // 2 - Conta de crédito informada não é numérica. Informe apenas números
    pcdDigitoContaCreditoNaoInformado,                // 3 - Dígito da conta de crédito não informado. Informe o DV da conta de crédito
    pcdCPFInformadoNaoNumerico,                       // 4 - CPF informado não é numérico. Informe apenas números
    pcdCNPJInformadoNaoNumerico,                      // 5 - CNPJ informado não é numérico. Informe apenas números
    pcdDataPagamentoNaoInformada,                     // 6 - Data do pagamento não informada. Informe a data do pagamento
    pcdDataPagamentoInvalida,                         // 7 - Data do pagamento inválida. Verifique o dado informado
    pcdValorPagamentoNaoNumerico,                     // 8 - Valor do pagamento informado não é númerico. Informe apenas números
    pcdValorPagamentoZerado,                          // 9 - Valor do pagamento está zerado. Informe o valor do pagamento
    pcdNumeroCompensacaoISPBNaoInformados,            // 10 - Ambos os campos Número Compensação e Número ISPB não foram informados. Informe um dos campos
    pcdNumeroCompensacaoISPBInformados,               // 11 - Ambos os campos Número Compensação e Número ISPB foram informados. Informe apenas um dos campos
    pcdFinalidadeDOCTEDNaoInformados,                 // 12 - Ambos os campos Finalidade DOC e Finalidade TED não foram informados. Informe um dos campos
    pcdFinalidadeDOCTEDInformados,                    // 13 - Ambos os campos Finalidade DOC e Finalidade TED foram informados. Informe apenas um dos campos
    pcdNumeroDepositoJudicialNaoInformado,            // 14 - Número de depósito judicial não informado. Informe o número do depósito judicial
    pcdDigitoContaCreditoInvalido,                    // 15 - Digito da conta de crédito inválido. Verifique o dado informado
    pcdCPFECNPJInformados,                            // 16 - Ambos os campos CPF e CNPJ foram informados. Informe apenas um dos campos. Caso informado os 2 campos, nas consultas será exibido apenas os dados do CPF
    pcdCPFECNPJNaoInformados,                         // 17 - Ambos os campos CPF e CNPJ não foram informados. Informe um dos campos
    pcdDigitoCPFNaoInformado,                         // 18 - Dígito do CPF inválido. Verifique o dado informado
    pcdDigitoCNPJInvalido,                            // 19 - Dígito do CNPJ inválido. Verifique o dado informado
    pcdAgenciaContaCreditoIguaisDebito,               // 20 - Agência e conta de crédito estão iguais às de débito. Opção não permitida
    pcdNumeroCompensacaoInvalido,                     // 21 - Número Compensação inválido. Verifique o dado informado
    pcdNumeroISPBDiferenteZeros,                      // 22 - Número ISPB diferente de zeros. Não informe o nº ISPB
    pcdContaCreditoNaoInformada,                      // 23 - Conta de crédito não informada. Informe o número da conta de crédito
    pcdCPFNaoInformado,                               // 24 - CPF não informado. Informe o nº do CPF
    pcdCNPJInformado,                                 // 25 - CNPJ foi informado. Não informe CNPJ
    pcdContaCreditoInformada,                         // 26 - Conta de crédito foi informada. Não informe Conta de crédito
    pcdDigitoContaCreditoInformado,                   // 27 - Dígito da conta de crédito foi informado. Não informe dígito da conta de crédito
    pcdFinalidadeDOCInformada,                        // 28 - Finalidade do DOC foi informada. Não informe finalidade do DOC
    pcdFinalidadeTEDInformada,                        // 29 - Finalidade da TED foi informada. Não informe finalidade da TED
    pcdNumeroDepositoJudicialInformado,               // 30 - Número Depósito Judicial informado. Não informe finalidade Depósito Judicial
    pcdNumeroDocumentoCreditoNaoNumerico,             // 31 - Número do documento de crédito informado não é numérico. Informe apenas números
    pcdNumeroDocumentoDebitoNaoNumerico,              // 32 - Número do documento de débito não é numérico. Informe apenas números
    pcdCPFNaoEncontradoReceitaFederal,                // 33 - CPF não encontrado na base da receita federal. Verifique o dado informado
    pcdCNPJNaoEncontradoReceitaFederal,               // 34 - CNPJ não encontrado na base da receita federal. Verifique o dado informado
    pcdContaPoupancaNaoPermitidaFornecedor,           // 35 - Conta poupança não permitido para "Pagamento ao Fornecedor". Para creditar em conta poupança utilize o recurso para efetivação de "Pagamentos Diversos"
    pcdCodigoCOMPEIgualUm,                            // 36 - Código COMPE deve ser igual a 1
    pcdCodigoISPBIgualZero,                           // 37 - Código ISPB deve ser igual a 0
    pcdCodigoBarrasNaoNumerico,                       // 38 - Código de barras não é numérico. Informe apenas números
    pcdCodigoBarrasZeros,                             // 39 - Código de barras igual a zeros. Informe apenas números
    pcdNumeroInscricaoPagadorNaoNumerico,             // 40 - Número de inscrição do pagador não é numérico. Informe apenas números
    pcdNumeroInscricaoBeneficiarioNaoNumerico,        // 41 - Número de inscrição do beneficiário não é numérico. Informe apenas números
    pcdNumeroInscricaoAvalistaNaoNumerico,            // 42 - Número de inscrição do avalista não é numérico. Informe apenas números
    pcdDigitoCPFPagadorInvalido,                      // 43 - Digito do CPF para o pagador inválido. Verifique o dado informado
    pcdDigitoCPFBeneficiarioInvalido,                 // 44 - Digito do CPF para o beneficiário inválido. Verifique o dado informado
    pcdDigitoCPFAvalistaInvalido,                     // 45 - Digito do CPF para o avalista inválido. Verifique o dado informado
    pcdDigitoCNPJPagadorInvalido,                     // 46 - Digito do CNPJ para o pagador inválido. Verifique o dado informado
    pcdDigitoCNPJBeneficiarioInvalido,                // 47 - Digito do CNPJ para o beneficiário inválido. Verifique o dado informado
    pcdDigitoCNPJAvalistaInvalido,                    // 48 - Digito do CNPJ para o avalista inválido. Verifique o dado informado
    pcdDataVencimentoInvalida,                        // 49 - Data do vencimento inválida. Verifique o dado informado
    pcdValorNominalNaoNumerico,                       // 50 - Valor nominal não é numérico. Informe apenas números
    pcdValorDescontoNaoNumerico,                      // 51 - Valor de desconto não é numérico. Informe apenas números
    pcdValorMoraNaoNumerico,                          // 52 - Valor de mora não é numérico. Informe apenas números
    pcdDataPagamentoMaiorIgualAtual,                  // 53 - Data do pagamento deve ser maior ou igual ao dia atual
    pcdNumeroDocumentoDebitoNaoInformado,             // 54 - Número do documento de débito não informado. Informe o nº do doc de débito
    pcdDataVencimentoNaoInformada,                    // 55 - Data do vencimento não informada. Informe a data de vencimento
    pcdNomeBeneficiarioNaoInformado,                  // 56 - Nome do beneficiário não informado. Informe o nome do beneficiário
    pcdNumeroInscricaoBeneficiarioNaoInformado,       // 57 - Número de inscrição do beneficiário não informado. Informe o CPF ou o CNPJ do beneficiário
    pcdContaPagamentoInformada,                       // 58 - Conta pagamento foi informada. Não informe conta pagamento
    pcdContaCreditoContaPagamentoInformados,          // 59 - Ambos os campos conta de crédito e conta pagamento foram informados. Informe apenas um dos campos
    pcdTransacaoCanceladaCliente,                     // 60 - Transação cancelada pelo cliente
    pcdCodigoReceitaTributoNaoInformado,              // 61 - Código da Receita do Tributo não informado
    pcdTipoIdentificacaoContribuinteNaoInformado,     // 62 - Tipo de Identificação do Contribuinte não informado
    pcdNumeroIdentificacaoContribuinteNaoInformado,   // 63 - Nº de Identificação do Contribuinte não informado
    pcdNumeroIdentificacaoContribuinteNaoNumerico,    // 64 - Nº de Identificação do Contribuinte não numérico
    pcdCodigoIdentificacaoTributoNaoInformado,        // 65 - Código de Identificação do Tributo não informado
    pcdPeriodoApuracaoNaoInformado,                   // 66 - Período de apuração não informado
    pcdNumeroReferenciaNaoInformado,                  // 67 - Numero de Referência não informado
    pcdValorPrincipalNaoNumerico,                     // 68 - Valor Principal não é numérico
    pcdValorPrincipalNaoInformado,                    // 69 - Valor Principal não informado
    pcdValorMultaNaoNumerico,                         // 70 - Valor da Multa não é numérico
    pcdValorJurosEncargosNaoNumerico,                 // 71 - Valor dos Juros/Encargos não é numérico
    pcdDataVencimentoNaoInformada2,                   // 72 - Data de Vencimento não informada
    pcdMesAnoCompetenciaNaoInformados,                // 73 - Mês e ano de competência não informados
    pcdValorPrevistoPagamentoINSSNaoNumerico,         // 74 - Valor previsto do pagamento do INSS não é numérico
    pcdValorPrevistoPagamentoINSSNaoInformado,        // 75 - Valor previsto do pagamento do INSS não informado
    pcdValorOutrasEntidadesNaoNumerico,               // 76 - Valor de Outras Entidades não é numérico
    pcdValorAtualizacaoMonetariaNaoNumerico,          // 77 - Valor de Atualização Monetária não é numérico
    pcdPeriodoApuracaoInvalido,                       // 79 - Período de apuração inválido
    pcdContaCreditoInvalida,                          // 80 - Conta de crédito inválida. Informe o numero sem o 45 do início
    pcdContaNaoPertenceFuncionario,                   // 81 - A conta informada não pertence ao funcionário
    pcdPagamentoPermitidoApenasPessoasFisicas,        // 82 - Pagamento permitido apenas para pessoas físicas
    pcdAgenciaContaIncorretos,                        // 83 - Agência e Conta incorretos
    pcdContaNaoAtiva,                                 // 84 - A conta informada não está ativa
    pcdContaNaoPermiteCreditoSalario,                 // 85 - Conta não permite crédito de salário. Informe outra conta
    pcdAgenciaCreditoContaPagamentoInformados,        // 86 - Ambos os campos agência de crédito e conta pagamento foram informados
    pcdMesCompetenciaInvalido,                        // 90 - Mês de competência inválido
    pcdValorOutrasDeducoesInvalido,                   // 91 - Valor de outras deduções inválido
    pcdValorOutrosAcrescimosInvalido,                 // 92 - Valor de outros acréscimos inválido
    pcdCodigoFormaIdentificacaoClienteNaoInformado,   // 93 - Código da forma de identificação do cliente não foi informado
    pcdDDDPixNaoInformado,                            // 94 - DDD do cliente do Pix não foi informado
    pcdTelefonePixNaoInformado,                       // 95 - Telefone do Cliente do Pix não foi informado
    pcdEmailPixNaoInformado,                          // 96 - Email do cliente do Pix não foi informado
    pcdChaveAleatoriaPixNaoInformada,                 // 97 - Chave Aleatória do Cliente do Pix não foi informado
    pcdCodigoTipoContaPixNaoInformado,                // 98 - Código de tipo de conta do Cliente do Pix não foi informado
    pcdConsultarBancoDetalharErro,                    // 99 - Consultar o Banco para detalhar o erro
    pcdEmailInvalido,                                 // 100 - E-mail inválido
    pcdEmailPixCaractereEspecial,                     // 101 - Email do cliente do PIX não deve conter caractere especial
    pcdTelefoneInvalido,                              // 102 - Telefone Inválido
    pcdDDDInvalido,                                   // 103 - DDD inválido
    pcdEmailTamanhoMaior77,                           // 104 - E-mail com tamanho maior que 77 caracteres
    pcdInsuficienciaFundosDebitoNaoEfetuado,          // 200 - Insuficiência de Fundos - Débito Não Efetuado
    pcdCreditoDebitoCanceladoPagador,                 // 201 - Crédito ou Débito Cancelado pelo Pagador
    pcdDebitoAutorizadoAgenciaEfetuado,               // 202 - Débito Autorizado pela Agência - Efetuado
    pcdControleInvalido,                              // 203 - Controle Inválido. Verificar campos 01, 02 e 03 do header ou segmento A, B, C, J, J52, N, O ou W do Arquivo CNAB240
    pcdTipoOperacaoInvalido,                          // 204 - Tipo de Operação Inválido. Verificar campo 04.1 do header de lote. Valor default = "C"
    pcdTipoServicoInvalido,                           // 205 - Tipo de Serviço Inválido. Utilize 20 para Pagamento a Fornecedores, 30 Pagamento de Salários ou 98 Pagamentos Diversos no header de Lote, campo 05.1, do CNAB240
    pcdFormaLancamentoInvalida,                       // 206 - Forma de Lançamento Inválida. Para crédito em Poupança utilize Pagamentos Diversos. Para crédito em Conta Pagamento utilize Pagamentos Diversos ou Pagamento a Fornecedores. Para Pagamento de salário a conta de crédito deve ser do BB
    pcdTipoNumeroInscricaoInvalido,                   // 207 - Tipo/Número de Inscrição Inválido. CPF ou CNPJ inválido. Verifique dados informados
    pcdCodigoConvenioInvalido,                        // 208 - Código de Convênio Inválido. Verifique dados informados
    pcdAgenciaContaCorrenteDVInvalido,                // 209 - Agência/Conta Corrente/DV Inválido. Verifique dados informados
    pcdNumeroSequencialRegistroLoteInvalido,          // 210 - Nº Seqüencial do Registro no Lote Inválido. Verifique dado informado
    pcdCodigoSegmentoDetalheInvalido,                 // 211 - Código de Segmento de Detalhe Inválido. Verifique dado informado
    pcdLancamentoInconsistenteRejeitadoPrevia,        // 212 - Lançamento inconsistente, rejeitado na prévia. Corrigir os dados do lançamento e enviar novo pagamento
    pcdNumeroCompeBancoCreditoInvalido,               // 213 - Nº Compe do Banco para crédito Inválido. Verifique dado informado
    pcdNumeroISPBInvalido,                            // 214 - Nº do ISPB Banco, Instituição de Pagamento para crédito Inválido. Verifique dado informado
    pcdAgenciaMantenedoraContaCorrenteFavorecidoInvalida, // 215 - Agência Mantenedora da Conta Corrente do Favorecido Inválida. Verifique dado informado
    pcdContaCorrenteDVContaPagamentoFavorecidoInvalido, // 216 - Conta Corrente/DV/Conta de Pagamento do Favorecido Inválido. Verifique dado informado
    pcdNomeFavorecidoNaoInformado,                    // 217 - Nome do Favorecido não Informado. Informe o nome do favorecido
    pcdDataLancamentoInvalida,                        // 218 - Data Lançamento Inválido. Verifique dado informado
    pcdTipoQuantidadeMoedaInvalido,                   // 219 - Tipo/Quantidade da Moeda Inválido. Verifique dado informado
    pcdValorLancamentoInvalido,                       // 220 - Valor do Lançamento Inválido. Verifique dado informado
    pcdAvisoFavorecidoIdentificacaoInvalida,          // 221 - Aviso ao Favorecido - Identificação Inválida
    pcdTipoNumeroInscricaoFavorecidoInvalido,         // 222 - Tipo/Número de Inscrição do Favorecido Inválido CPF ou CNPJ do favorecido inválido. Arquivo: Verifique o campo 07.3B - registro detalhe do segmento B
    pcdLogradouroFavorecidoNaoInformado,              // 223 - Logradouro do Favorecido não Informado. Informe o logradouro do favorecido
    pcdNumeroLocalFavorecidoNaoInformado,             // 224 - Nº do Local do Favorecido não Informado. Informe o nº do local do favorecido
    pcdCidadeFavorecidoNaoInformada,                  // 225 - Cidade do Favorecido não Informada. Informe a cidade do favorecido
    pcdCEPFavorecidoInvalido,                         // 226 - CEP/Complemento do Favorecido Inválido. Verifique dado informado
    pcdSiglaEstadoFavorecidoInvalida,                 // 227 - Sigla do Estado do Favorecido Inválida. Verifique dado informado
    pcdNumeroBancoCreditoInvalido,                    // 228 - Nº do Banco para crédito Inválido. Verifique dado informado
    pcdCodigoNomeAgenciaDepositariaNaoInformado,      // 229 - Código/Nome da Agência Depositária não Informado. Informe o dado solicitado
    pcdSeuNumeroInvalido,                             // 230 - Seu Número Inválido. Verifique dado informado
    pcdNossoNumeroInvalido,                           // 231 - Nosso Número Inválido. Verifique dado informado
    pcdInclusaoEfetuadaSucesso,                       // 232 - Inclusão Efetuada com Sucesso
    pcdAlteracaoEfetuadaSucesso,                      // 233 - Alteração Efetuada com Sucesso
    pcdExclusaoEfetuadaSucesso,                       // 234 - Exclusão Efetuada com Sucesso
    pcdAgenciaContaImpedidaLegalmente,                // 235 - Agência/Conta Impedida Legalmente
    pcdEmpresaNaoPagouSalario,                        // 236 - Empresa não pagou salário Conta de crédito só aceita pagamento de salário
    pcdFalecimentoMutuario,                           // 237 - Falecimento do mutuário
    pcdEmpresaNaoEnviouRemessaMutuario,               // 238 - Empresa não enviou remessa do mutuário
    pcdEmpresaNaoEnviouRemessaVencimento,             // 239 - Empresa não enviou remessa no vencimento
    pcdValorParcelaInvalida,                          // 240 - Valor da parcela inválida. Verifique dado informado
    pcdIdentificacaoContratoInvalida,                 // 241 - Identificação do contrato inválida. Verifique dado informado
    pcdOperacaoConsignacaoIncluidaSucesso,            // 242 - Operação de Consignação Incluída com Sucesso
    pcdOperacaoConsignacaoAlteradaSucesso,            // 243 - Operação de Consignação Alterada com Sucesso
    pcdOperacaoConsignacaoExcluidaSucesso,            // 244 - Operação de Consignação Excluída com Sucesso
    pcdOperacaoConsignacaoLiquidadaSucesso,           // 245 - Operação de Consignação Liquidada com Sucesso
    pcdReativacaoEfetuadaSucesso,                     // 246 - Reativação Efetuada com Sucesso
    pcdSuspensaoEfetuadaSucesso,                      // 247 - Suspensão Efetuada com Sucesso
    pcdCodigoBarrasCodigoBancoInvalido,               // 248 - Código de Barras - Código do Banco Inválido
    pcdCodigoBarrasCodigoMoedaInvalido,               // 249 - Código de Barras - Código da Moeda Inválido
    pcdCodigoBarrasDigitoVerificadorGeralInvalido,    // 250 - Código de Barras - Dígito Verificador Geral Inválido
    pcdCodigoBarrasValorTituloInvalido,               // 251 - Código de Barras - Valor do Título Inválido
    pcdCodigoBarrasCampoLivreInvalido,                // 252 - Código de Barras - Campo Livre Inválido
    pcdValorDocumentoInvalido,                        // 253 - Valor do Documento Inválido. Verifique dado informado
    pcdValorAbatimentoInvalido,                       // 254 - Valor do Abatimento Inválido. Verifique dado informado
    pcdValorDescontoInvalido,                         // 255 - Valor do Desconto Inválido. Verifique dado informado
    pcdValorMoraInvalido,                             // 256 - Valor de Mora Inválido. Verifique dado informado
    pcdValorMultaInvalido,                            // 257 - Valor da Multa Inválido. Verifique dado informado
    pcdValorIRInvalido,                               // 258 - Valor do IR Inválido. Verifique dado informado
    pcdValorISSInvalido,                              // 259 - Valor do ISS Inválido. Verifique dado informado
    pcdValorIOFInvalido,                              // 260 - Valor do IOF Inválido. Verifique dado informado
    pcdValorOutrasDeducoesInvalido2,                  // 261 - Valor de Outras Deduções Inválido. Verifique dado informado
    pcdValorOutrosAcrescimosInvalido2,                // 262 - Valor de Outros Acréscimos Inválido. Verifique dado informado
    pcdValorINSSInvalido,                             // 263 - Valor do INSS Inválido. Verifique dado informado
    pcdLoteNaoAceito,                                 // 264 - Lote Não Aceito. Reenvie os documentos
    pcdInscricaoEmpresaInvalidaContrato,              // 265 - Inscrição da Empresa Inválida para o Contrato
    pcdConvenioEmpresaInexistenteContrato,            // 266 - Convênio com a Empresa Inexistente/Inválido para o Contrato
    pcdAgenciaContaCorrenteEmpresaInexistenteContrato,// 267 - Agência/Conta Corrente da Empresa Inexistente/Inválido para o Contrato. Verifique dado informado
    pcdTipoServicoInvalidoContrato,                   // 268 - Tipo de Serviço Inválido para o Contrato. Para contrato de Pagamentos, utilize 20 para Pagamento a Fornecedores, 30 Pagamento de Salários ou 98 Pagamentos Diversos no header de Lote, campo 05.1, do CNAB240
    pcdContaCorrenteEmpresaSaldoInsuficiente,         // 269 - Conta Corrente da Empresa com Saldo Insuficiente
    pcdLoteServicoForaSequencia,                      // 270 - Lote de Serviço Fora de Seqüência
    pcdLoteServicoInvalido,                           // 271 - Lote de Serviço Inválido
    pcdArquivoNaoAceito,                              // 272 - Arquivo não aceito
    pcdTipoRegistroInvalido,                          // 273 - Tipo de Registro Inválido
    pcdCodigoRemessaRetornoInvalido,                  // 274 - Código Remessa / Retorno Inválido
    pcdVersaoLayoutInvalida,                          // 275 - Versão de layout inválida
    pcdMutuarioNaoIdentificado,                       // 276 - Mutuário não identificado
    pcdTipoBeneficioNaoPermiteEmprestimo,             // 277 - Tipo do beneficio não permite empréstimo
    pcdBeneficioCessadoSuspenso,                      // 278 - Beneficio cessado/suspenso
    pcdBeneficioPossuiRepresentanteLegal,             // 279 - Beneficio possui representante legal
    pcdBeneficioTipoPA,                               // 280 - Beneficio é do tipo PA (Pensão alimentícia)
    pcdQuantidadeContratosExcedida,                   // 281 - Quantidade de contratos permitida excedida
    pcdBeneficioNaoPertenceBanco,                     // 282 - Beneficio não pertence ao Banco informado
    pcdInicioDescontoUltrapassado,                    // 283 - Início do desconto informado já ultrapassado
    pcdNumeroParcelaInvalida,                         // 284 - Número da parcela inválida. Verifique dado informado
    pcdQuantidadeParcelaInvalida,                     // 285 - Quantidade de parcela inválida. Verifique dado informado
    pcdMargemConsignavelExcedidaPrazo,                // 286 - Margem consignável excedida para o mutuário dentro do prazo do contrato. Verifique suas margens disponíveis
    pcdEmprestimoJaCadastrado,                        // 287 - Empréstimo já cadastrado
    pcdEmprestimoInexistente,                         // 288 - Empréstimo inexistente
    pcdEmprestimoJaEncerrado,                         // 289 - Empréstimo já encerrado
    pcdArquivoSemTrailer,                             // 290 - Arquivo sem trailer
    pcdMutuarioSemCreditoCompetencia,                 // 291 - Mutuário sem crédito na competência
    pcdNaoDescontadoOutrosMotivos,                    // 292 - Não descontado – outros motivos
    pcdRetornoCreditoNaoPago,                         // 293 - Retorno de Crédito não pago
    pcdCancelamentoEmprestimoRetroativo,              // 294 - Cancelamento de empréstimo retroativo
    pcdOutrosMotivosGlosa,                            // 295 - Outros Motivos de Glosa
    pcdMargemConsignavelExcedidaAcimaPrazo,           // 296 - Margem consignável excedida para o mutuário acima do prazo do contrato
    pcdMutuarioDesligadoEmpregador,                   // 297 - Mutuário desligado do empregador. Pagamento não permitido
    pcdMutuarioAfastadoLicenca,                       // 298 - Mutuário afastado por licença. Pagamento não permitido
    pcdPrimeiroNomeMutuarioDiferente,                 // 299 - Primeiro nome do mutuário diferente do primeiro nome do movimento do censo ou diferente da base de Titular do Benefício. Verificar necessidade de ajustes
    pcdBeneficioSuspensoCessadoAPS,                   // 300 - Benefício suspenso/cessado pela APS ou Sisobi
    pcdBeneficioSuspensoDependenciaCalculo,           // 301 - Benefício suspenso por dependência de cálculo
    pcdBeneficioSuspensoCessadoInspetoria,            // 302 - Benefício suspenso/cessado pela inspetoria/auditoria
    pcdBeneficioBloqueadoEmprestimoBeneficiario,      // 303 - Benefício bloqueado para empréstimo pelo beneficiário
    pcdBeneficioBloqueadoEmprestimoTBM,               // 304 - Benefício bloqueado para empréstimo por TBM
    pcdBeneficioFaseConcessaoPA,                      // 305 - Benefício está em fase de concessão de PA ou desdobramento
    pcdBeneficioCessadoObito,                         // 306 - Benefício cessado por óbito
    pcdBeneficioCessadoFraude,                        // 307 - Benefício cessado por fraude
    pcdBeneficioCessadoOutroBeneficio,                // 308 - Benefício cessado por concessão de outro benefício
    pcdBeneficioCessadoEstatutario,                   // 309 - Benefício cessado: estatutário transferido para órgão de origem
    pcdEmprestimoSuspensoAPS,                         // 310 - Empréstimo suspenso pela APS
    pcdEmprestimoCanceladoBanco,                      // 311 - Empréstimo cancelado pelo banco
    pcdCreditoTransformadoPAB,                        // 312 - Crédito transformado em PAB
    pcdTerminoConsignacaoAlterado,                    // 313 - Término da consignação foi alterado
    pcdFimEmprestimoPeriodoSuspensao,                 // 314 - Fim do empréstimo ocorreu durante período de suspensão ou concessão
    pcdEmprestimoSuspensoBanco,                       // 315 - Empréstimo suspenso pelo banco
    pcdNaoAverbacaoContratoQuantidadeParcelas,        // 316 - Não averbação de contrato – quantidade de parcelas/competências informadas ultrapassou a data limite da extinção de cota do dependente titular de benefícios
    pcdLoteNaoAceitoTotaisDiferenca,                  // 317 - Lote Não Aceito - Totais do Lote com Diferença
    pcdTituloNaoEncontrado,                           // 318 - Título Não Encontrado
    pcdIdentificadorRegistroOpcionalInvalido,         // 319 - Identificador Registro Opcional Inválido. Verifique dado informado
    pcdCodigoPadraoInvalido,                          // 320 - Código Padrão Inválido. Verifique dado informado
    pcdCodigoOcorrenciaInvalido,                      // 321 - Código de Ocorrência Inválido. Verifique dado informado
    pcdComplementoOcorrenciaInvalido,                 // 322 - Complemento de Ocorrência Inválido. Verifique dado informado
    pcdAlegacaoJaInformada,                           // 323 - Alegação já Informada
    pcdAgenciaContaFavorecidoSubstituida,             // 324 - Agência / Conta do Favorecido Substituída. Verifique dado informado
    pcdDivergenciaNomeBeneficiario,                   // 325 - Divergência entre o primeiro e último nome do beneficiário versus primeiro e último nome na Receita Federal. Verificar com beneficiário necessidade de ajustes
    pcdConfirmacaoAntecipacaoValor,                   // 326 - Confirmação de Antecipação de Valor
    pcdAntecipacaoParcialValor,                       // 327 - Antecipação parcial de valor
    pcdBoletoBloqueadoBase,                           // 328 - Boleto bloqueado na base. Não passível de pagamento
    pcdSistemaContingenciaBoletoValorMaior,           // 329 - Sistema em contingência – Boleto valor maior que referência. Consulte o beneficiário ou tente efetuar o pagamento mais tarde
    pcdSistemaContingenciaBoletoVencido,              // 330 - Sistema em contingência – Boleto vencido. Consulte o beneficiário ou tente efetuar o pagamento mais tarde
    pcdSistemaContingenciaBoletoIndexado,             // 331 - Sistema em contingência – Boleto indexado. Consulte o beneficiário ou tente efetuar o pagamento mais tarde
    pcdBeneficiarioDivergente,                        // 332 - Beneficiário divergente. Verifique dado informado
    pcdLimitePagamentosParciaisExcedido,              // 333 - Limite de pagamentos parciais do boleto excedido. Consulte o Beneficiário do boleto
    pcdBoletoJaLiquidado,                             // 334 - Boleto já liquidado. Não passível de pagamento
    pcdConsultarBancoDetalharErro2                    // 999 - Consultar o Banco para detalhar o erro
  );

  TACBrPagamentosBBVinculadosErros = (
    pveNenhum,
    pveAgenciaZerada,                             // 1 - Agência zerada. Campo obrigatório.
    pveContaCreditoIncorreta,                     // 2 - Preenchimento da Conta de crédito incorreto. Campo deve ser numérico.
    pveDVCreditoEmBranco,                         // 3 - DV da conta de crédito em branco. Campo obrigatório quando a conta é informada.
    pveCPFInvalido,                               // 4 - Preenchimento do CPF inválido. Campo deve ser numérico.
    pveCNPJInvalido,                              // 5 - Preenchimento do CNPJ inválido. Campo deve ser numérico.
    pveDataPagamentoZerada,                       // 6 - Data de pagamento zerada. Campo obrigatório.
    pveDataPagamentoInvalida,                     // 7 - Data de pagamento inválida.
    pveValorIncorreto,                            // 8 - Preenchimento do valor incorreto. Campo deve ser numérico.
    pveValorZerado,                               // 9 - Valor zerado. Campo obrigatório.
    pveFinalidadeDOCeTEDNaoInformadas,            // 12 - Finalidade DOC e Finalidade TED não foram informadas. Preencha uma das duas para transferências para outros bancos.
    pveFinalidadesDOCeTEDInformadas,              // 13 - Informadas finalidades para DOC e para TED. Informe apenas uma das duas.
    pveDVCreditoInvalido,                         // 15 - DV da conta de crédito inválido.
    pveCPFECNPJInformados,                        // 16 - Ambos os identificadores do cliente CPF e CNPJ foram informados. Obrigatório informar apenas um dos identificadores do beneficiário.
    pveCPFECNPJZerados,                           // 17 - Ambos os identificadores do cliente CPF e CNPJ zerados. Obrigatório informar um dos identificadores do beneficiário.
    pveDVCPFInvalido,                             // 18 - DV do CPF inválido.
    pveDVCNPJInvalido,                            // 19 - DV do CNPJ inválido.
    pveContaCreditoIgualDebito,                   // 20 - Conta de crédito igual à conta de débito.
    pvePagamentoSalarioBB,                        // 21 - Pagamento de salário só é permitido para Banco do Brasil. Número do banco do beneficiário deve ser zero ou 001 (BB).
    pvePagamentoSalarioISPBZerado,                // 22 - Pagamento de salário só é permitido para Banco do Brasil. Código ISPB deve ser zerado.
    pveContaCreditoObrigatoria,                   // 23 - Conta de crédito deve ser informada para todos os tipos de pagamento, exceto Depósito Judicial.
    pveCPFouCNPJObrigatorioDepositoJudicial,      // 24 - CPF ou CNPJ obrigatório para Depósitos Judiciais.
    pveContaCreditoNaoInformadaDepositoJudicial,  // 26 - Número da conta de crédito não deve ser informado para Depósitos Judiciais.
    pveDVCreditoNaoInformadoDepositoJudicial,     // 27 - DV da conta de crédito não deve ser informado para Depósitos Judiciais.
    pveFinalidadeDOCPreenchida,                   // 28 - Código de finalidade DOC só deve ser preenchido para transferências para outros bancos.
    pveFinalidadeTEDPreenchida,                   // 29 - Código de finalidade TED só deve ser preenchido para transferências para outros bancos.
    pveDepositoJudicialDOCouTED                   // 30 - Número do Depósito Judicial preenchido para DOC ou TED. Não é permitido envio de depósito judicial para outros bancos.
  );

  TACBrPagamentosBBTipoContribuinte = (
    pctNenhum,
    pctCNPJ,          // 1 - CNPJ
    pctCPF,           // 2 - CPF
    pctNITPISPASEP,   // 3 - NIT/PIS/PASEP
    pctCEI,           // 4 - CEI
    pctNB,            // 6 - NB
    pctNumeroTitulo,  // 7 - N° Título
    pctDEBCAD,        // 8 - DEBCAD
    pctReferencia     // 9 - Referência
  );

  { TACBrPagamentosBBErro }

  TACBrPagamentosBBErro = class(TACBrAPISchema)
  private
    fcodigo: String;
    fmensagem: String;
    focorrencia: String;
    fversao: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBErro);

    property codigo: String read fcodigo write fcodigo;
    property versao: String read fversao write fversao;
    property mensagem: String read fmensagem write fmensagem;
    property ocorrencia: String read focorrencia write focorrencia;
  end;

  { TACBrPagamentosBBErros }

  TACBrPagamentosBBErros = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBErro;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBErro);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBErro): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBErro);
    function New: TACBrPagamentosBBErro;
    property Items[aIndex: Integer]: TACBrPagamentosBBErro read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLancamentoErroObject }

  TACBrPagamentosBBLancamentoErroObject = class(TACBrAPISchema)
  private
    fErro: Integer;
  public
    property Erro: Integer read fErro write fErro;
  end;

  { TACBrPagamentosBBLancamentoErros }

  TACBrPagamentosBBLancamentoErros = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoErroObject;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoErroObject);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBLancamentoErroObject): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBLancamentoErroObject);
    function New: TACBrPagamentosBBLancamentoErroObject;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoErroObject read GetItem write SetItem; default;
  end;

  { TACBrPagamenosBBTransferenciaErroObject }

  TACBrPagamenosBBTransferenciaErroObject = class(TACBrAPISchema)
  private
    fErro: TACBrPagamenosBBTransferenciaErro;
  public
    property Erro: TACBrPagamenosBBTransferenciaErro read fErro write fErro;
  end;

  { TACBrPagamenosBBTransferenciaErros }

  TACBrPagamenosBBTransferenciaErros = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamenosBBTransferenciaErroObject;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamenosBBTransferenciaErroObject);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamenosBBTransferenciaErroObject): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamenosBBTransferenciaErroObject);
    function New: TACBrPagamenosBBTransferenciaErroObject;
    property Items[aIndex: Integer]: TACBrPagamenosBBTransferenciaErroObject read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBErroOAuthAttribute }

  TACBrPagamentosBBErroOAuthAttribute = class(TACBrAPISchema)
  private
    ferror: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBErroOAuthAttribute);
    property error: String read ferror write ferror;
  end;

  { TACBrPagamentosBBErroOAuthAttributes }

  TACBrPagamentosBBErroOAuthAttributes = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBErroOAuthAttribute;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBErroOAuthAttribute);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBErroOAuthAttribute): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBErroOAuthAttribute);
    function New: TACBrPagamentosBBErroOAuthAttribute;
    property Items[aIndex: Integer]: TACBrPagamentosBBErroOAuthAttribute read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBErroOAuth }

  TACBrPagamentosBBErroOAuth = class(TACBrAPISchema)
  private
    fattributes: TACBrPagamentosBBErroOAuthAttributes;
    ferror: String;
    fmessage: String;
    fstatusCode: Integer;
    function Getattributes: TACBrPagamentosBBErroOAuthAttributes;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBErroOAuth);

    property statusCode: Integer read fstatusCode write fstatusCode;
    property error: String read ferror write ferror;
    property message: String read fmessage write fmessage;
    property attributes: TACBrPagamentosBBErroOAuthAttributes read Getattributes write fattributes;
  end;

  { TACBrPagamentosBBDevolucaoBase }

  TACBrPagamentosBBDevolucaoBase = class(TACBrAPISchema)
  private
    fcodigoMotivo: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBDevolucaoBase);

    property codigoMotivo: Integer read fcodigoMotivo write fcodigoMotivo;
  end;

  { TACBrPagamentosBBDevolucaoListaBase }

  TACBrPagamentosBBDevolucaoListaBase = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBDevolucaoBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBDevolucaoBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBDevolucaoBase): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBDevolucaoBase);
    function New: TACBrPagamentosBBDevolucaoBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBDevolucaoBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBDevolucao }

  TACBrPagamentosBBDevolucao = class(TACBrAPISchema)
  private
    fcodigoMotivo: Double;
    fdataDevolucao: TDateTime;
    fvalorDevolucao: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBDevolucao);

    property codigoMotivo: Double read fcodigoMotivo write fcodigoMotivo;
    property dataDevolucao: TDateTime read fdataDevolucao write fdataDevolucao;
    property valorDevolucao: Double read fvalorDevolucao write fvalorDevolucao;
  end;

  { TACBrPagamentosBBDevolucaoLista }

  TACBrPagamentosBBDevolucaoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBDevolucao;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBDevolucao);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBDevolucao): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBDevolucao);
    function New: TACBrPagamentosBBDevolucao;
    property Items[aIndex: Integer]: TACBrPagamentosBBDevolucao read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBOcorrenciaBase }

  TACBrPagamentosBBOcorrenciaBase = class(TACBrAPISchema)
  private
    fcodigo: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBOcorrenciaBase);

    property codigo: Integer read fcodigo write fcodigo;
  end;

  { TACBrPagamentosBBOcorrenciaLista }

  TACBrPagamentosBBOcorrenciaLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBOcorrenciaBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBOcorrenciaBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBOcorrenciaBase): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBOcorrenciaBase);
    function New: TACBrPagamentosBBOcorrenciaBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBOcorrenciaBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaBase }

  TACBrPagamentosBBConsultaRespostaBase = class(TACBrAPISchema)
  private
    fagenciaDebito: Integer;
    fcodigoAutenticacaoPagamento: String;
    fcontaCorrenteDebito: Integer;
    fdataPagamento: TDateTime;
    fdigitoVerificadorContaCorrenteDebito: String;
    fdocumentoDebito: Integer;
    festadoPagamento: TACBrPagamentosBBEstado;
    ffimCartaoCredito: Integer;
    fid: Integer;
    finicioCartaoCredito: Integer;
    fvalorPagamento: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaBase);

    property id: Integer read fid write fid;
    property estadoPagamento: TACBrPagamentosBBEstado read festadoPagamento write festadoPagamento;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property inicioCartaoCredito: Integer read finicioCartaoCredito write finicioCartaoCredito;
    property fimCartaoCredito: Integer read ffimCartaoCredito write ffimCartaoCredito;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property documentoDebito: Integer read fdocumentoDebito write fdocumentoDebito;
    property codigoAutenticacaoPagamento: String read fcodigoAutenticacaoPagamento write fcodigoAutenticacaoPagamento;
  end;

  { TACBrPagamentosBBPagamento }

  TACBrPagamentosBBPagamento = class(TACBrAPISchema)
  private
    fagenciaCredito: Integer;
    fagenciaDebito: Integer;
    fcodigoBarrasBoleto: String;
    fcodigoDevolucao: TACBrPagamentosBBCodigoDevolucao;
    fcompeCredito: Integer;
    fcontaCorrenteDebito: Integer;
    fcontaCredito: Integer;
    fcontaPagamentoCredito: String;
    fcpfCnpj: Int64;
    fdataDevolucao: Integer;
    fdataPagamento: TDateTime;
    fdddTelefone: Integer;
    fdescricaoPagamentoInstantaneo: String;
    fdigitoVerificadorContaCorrenteDebito: String;
    fdigitoVerificadorContaCredito: String;
    fdocumentoDebito: Integer;
    femail: String;
    ffimCartao: Integer;
    fformaIdentificacao: TACBrPagamentosBBFormaIdentificacao;
    fidentificacaoAleatoria: String;
    fidentificadorPagamento: Integer;
    finicioCartao: Integer;
    fispbCredito: Integer;
    fnome: String;
    fnumeroRequisicao: Integer;
    fsequenciaDevolucao: Integer;
    ftelefone: Integer;
    ftipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
    ftipoConta: TACBrPagamentosBBTipoConta;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    ftipoPagamento: TACBrPagamentosBBTipoPagamento;
    fvalorBoleto: Double;
    fvalorDevolucao: Double;
    fvalorPagamento: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamento);

    property identificadorPagamento: Integer read fidentificadorPagamento write fidentificadorPagamento;
    property tipoPagamento: TACBrPagamentosBBTipoPagamento read ftipoPagamento write ftipoPagamento;
    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property compeCredito: Integer read fcompeCredito write fcompeCredito;
    property ispbCredito: Integer read fispbCredito write fispbCredito;
    property agenciaCredito: Integer read fagenciaCredito write fagenciaCredito;
    property contaCredito: Integer read fcontaCredito write fcontaCredito;
    property digitoVerificadorContaCredito: String read fdigitoVerificadorContaCredito write fdigitoVerificadorContaCredito;
    property contaPagamentoCredito: String read fcontaPagamentoCredito write fcontaPagamentoCredito;
    property tipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read ftipoBeneficiario write ftipoBeneficiario;
    property cpfCnpj: Int64 read fcpfCnpj write fcpfCnpj;
    property nome: String read fnome write fnome;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property codigoBarrasBoleto: String read fcodigoBarrasBoleto write fcodigoBarrasBoleto;
    property valorBoleto: Double read fvalorBoleto write fvalorBoleto;
    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property inicioCartao: Integer read finicioCartao write finicioCartao;
    property fimCartao: Integer read ffimCartao write ffimCartao;
    property documentoDebito: Integer read fdocumentoDebito write fdocumentoDebito;
    property dataDevolucao: Integer read fdataDevolucao write fdataDevolucao;
    property valorDevolucao: Double read fvalorDevolucao write fvalorDevolucao;
    property codigoDevolucao: TACBrPagamentosBBCodigoDevolucao read fcodigoDevolucao write fcodigoDevolucao;
    property sequenciaDevolucao: Integer read fsequenciaDevolucao write fsequenciaDevolucao;
    property tipoConta: TACBrPagamentosBBTipoConta read ftipoConta write ftipoConta;
    property descricaoPagamentoInstantaneo: String read fdescricaoPagamentoInstantaneo write fdescricaoPagamentoInstantaneo;
    property formaIdentificacao: TACBrPagamentosBBFormaIdentificacao read fformaIdentificacao write fformaIdentificacao;
    property dddTelefone: Integer read fdddTelefone write fdddTelefone;
    property telefone: Integer read ftelefone write ftelefone;
    property email: String read femail write femail;
    property identificacaoAleatoria: String read fidentificacaoAleatoria write fidentificacaoAleatoria;
  end;

  { TACBrPagamentosBBPagamentos }

  TACBrPagamentosBBPagamentos = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPagamento;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamento);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBPagamento): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBPagamento);
    function New: TACBrPagamentosBBPagamento;
    property Items[aIndex: Integer]: TACBrPagamentosBBPagamento read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBPagamentosResposta }

  TACBrPagamentosBBPagamentosResposta = class(TACBrAPISchema)
  private
    findice: Integer;
    fpagamentos: TACBrPagamentosBBPagamentos;
    fquantidadeRegistros: Integer;
    fquantidadeTotalRegistros: Integer;
    function Getpagamentos: TACBrPagamentosBBPagamentos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamentosResposta);

    property indice: Integer read findice write findice;
    property quantidadeTotalRegistros: Integer read fquantidadeTotalRegistros write fquantidadeTotalRegistros;
    property quantidadeRegistros: Integer read fquantidadeRegistros write fquantidadeRegistros;
    property pagamentos: TACBrPagamentosBBPagamentos read Getpagamentos write fpagamentos;
  end;

  { TACBrPagamentoBBLotePagamentosRequisicao }

  TACBrPagamentoBBLotePagamentosRequisicao = class(TACBrAPISchema)
  private
    fnumeroRequisicao: Integer;
    fcodigoContrato: Integer;
    fnumeroAgenciaDebito: Integer;
    fnumeroContaCorrenteDebito: Integer;
    fdigitoVerificadorContaCorrenteDebito: String;

  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentoBBLotePagamentosRequisicao);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property codigoContrato: Integer read fcodigoContrato write fcodigoContrato;
    property numeroAgenciaDebito: Integer read fnumeroAgenciaDebito write fnumeroAgenciaDebito;
    property numeroContaCorrenteDebito: Integer read fnumeroContaCorrenteDebito write fnumeroContaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
  end;

  { TACBrPagamentosBBLiberarPagamentosRequisicao }

  TACBrPagamentosBBLiberarPagamentosRequisicao = class(TACBrAPISchema)
  private
    findicadorFloat: String;
    fnumeroRequisicao: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLiberarPagamentosRequisicao);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property indicadorFloat: String read findicadorFloat write findicadorFloat;
  end;

  { TACBrPagamentosBBPagamentoCancelar }

  TACBrPagamentosBBPagamentoCancelar = class(TACBrAPISchema)
  private
    fcodigoPagamento: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamentoCancelar);

    property codigoPagamento: String read fcodigoPagamento write fcodigoPagamento;
  end;

  { TACBrPagamentosBBPagamentoCancelarLista }

  TACBrPagamentosBBPagamentoCancelarLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPagamentoCancelar;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamentoCancelar);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBPagamentoCancelar): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBPagamentoCancelar);
    function New: TACBrPagamentosBBPagamentoCancelar;
    property Items[aIndex: Integer]: TACBrPagamentosBBPagamentoCancelar read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBCancelarLotePagamentos }

  TACBrPagamentosBBCancelarLotePagamentos = class(TACBrAPISchema)
  private
    fagenciaDebito: Int64;
    fcontaCorrenteDebito: Int64;
    fdigitoVerificadorContaCorrente: String;
    flistaPagamentos: TACBrPagamentosBBPagamentoCancelarLista;
    fnumeroContratoPagamento: Int64;
    function GetlistaPagamentos: TACBrPagamentosBBPagamentoCancelarLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBCancelarLotePagamentos);

    property agenciaDebito: Int64 read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Int64 read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property numeroContratoPagamento: Int64 read fnumeroContratoPagamento write fnumeroContratoPagamento;
    property listaPagamentos: TACBrPagamentosBBPagamentoCancelarLista read GetlistaPagamentos write flistaPagamentos;
  end;

  { TACBrPagamentosBBLotePagamento }

  TACBrPagamentosBBLotePagamento = class(TACBrAPISchema)
  private
    fcpfCnpjBeneficiario: Int64;
    fdataPagamento: Integer;
    fdescricaoPagamento: String;
    ferros: TACBrPagamentosBBLancamentoErros;
    festadoPagamento: TACBrPagamentosBBLoteEstado;
    fidentificadorPagamento: Int64;
    fnomeBeneficiario: String;
    ftipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    fvalorPagamento: Double;
    function Geterros: TACBrPagamentosBBLancamentoErros;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamento);

    property identificadorPagamento: Int64 read fidentificadorPagamento write fidentificadorPagamento;
    property dataPagamento: Integer read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property tipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read ftipoBeneficiario write ftipoBeneficiario;
    property cpfCnpjBeneficiario: Int64 read fcpfCnpjBeneficiario write fcpfCnpjBeneficiario;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property estadoPagamento: TACBrPagamentosBBLoteEstado read festadoPagamento write festadoPagamento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property erros: TACBrPagamentosBBLancamentoErros read Geterros write ferros;
  end;

  { TACBrPagamentosBBLotePagamentos }

  TACBrPagamentosBBLotePagamentos = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLotePagamento;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLotePagamento);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBLotePagamento): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBLotePagamento);
    function New: TACBrPagamentosBBLotePagamento;
    property Items[aIndex: Integer]: TACBrPagamentosBBLotePagamento read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLotePagamentosResposta }

  TACBrPagamentosBBLotePagamentosResposta = class(TACBrAPISchema)
  private
    fdataRequisicao: TDateTime;
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    findice: Int64;
    fpagamentos: TACBrPagamentosBBLotePagamentos;
    fquantidadePagamentos: Int64;
    ftipoPagamento: Integer;
    fvalorPagamentos: Double;
    function Getpagamentos: TACBrPagamentosBBLotePagamentos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosResposta);

    property indice: Int64 read findice write findice;
    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property tipoPagamento: Integer read ftipoPagamento write ftipoPagamento;
    property dataRequisicao: TDateTime read fdataRequisicao write fdataRequisicao;
    property quantidadePagamentos: Int64 read fquantidadePagamentos write fquantidadePagamentos;
    property valorPagamentos: Double read fvalorPagamentos write fvalorPagamentos;
    property pagamentos: TACBrPagamentosBBLotePagamentos read Getpagamentos write fpagamentos;
  end;

  { TACBrPagamentosBBMovimento }

  TACBrPagamentosBBMovimento = class(TACBrAPISchema)
  private
    fcodigoAutenticacaoPagamento: String;
    fcodigoDoTipoDePessoa: TACBrPagamentosBBTipoBeneficiario;
    fcodigoFormaCredito: Integer;
    fcodigoIdentificadorDoPagamento: Int64;
    fcodigoTipoPagamento: TACBrPagamentosBBTipoPagamento;
    fdataDebito: TDateTime;
    fdataPagamento: TDateTime;
    fnomeDoFavorecido: String;
    fnumeroCPFouCNPJ: Int64;
    fnumeroDocumentoCredito: Int64;
    fnumeroDocumentoDebito: Int64;
    fnumeroRequisicaoPagamento: Integer;
    ftextoEstadoPagamento: String;
    fvalorPagamento: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBMovimento);

    property numeroRequisicaoPagamento: Integer read fnumeroRequisicaoPagamento write fnumeroRequisicaoPagamento;
    property textoEstadoPagamento: String read ftextoEstadoPagamento write ftextoEstadoPagamento;
    property codigoIdentificadorDoPagamento: Int64 read fcodigoIdentificadorDoPagamento write fcodigoIdentificadorDoPagamento;
    property nomeDoFavorecido: String read fnomeDoFavorecido write fnomeDoFavorecido;
    property codigoDoTipoDePessoa: TACBrPagamentosBBTipoBeneficiario read fcodigoDoTipoDePessoa write fcodigoDoTipoDePessoa;
    property numeroCPFouCNPJ: Int64 read fnumeroCPFouCNPJ write fnumeroCPFouCNPJ;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Int64 read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property numeroDocumentoCredito: Int64 read fnumeroDocumentoCredito write fnumeroDocumentoCredito;
    property codigoFormaCredito: Integer read fcodigoFormaCredito write fcodigoFormaCredito;
    property codigoAutenticacaoPagamento: String read fcodigoAutenticacaoPagamento write fcodigoAutenticacaoPagamento;
    property dataDebito: TDateTime read fdataDebito write fdataDebito;
    property codigoTipoPagamento: TACBrPagamentosBBTipoPagamento read fcodigoTipoPagamento write fcodigoTipoPagamento;
  end;

  { TACBrPagamentosBBMovimentoLista }

  TACBrPagamentosBBMovimentoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBMovimento;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBMovimento);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBMovimento): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBMovimento);
    function New: TACBrPagamentosBBMovimento;
    property Items[aIndex: Integer]: TACBrPagamentosBBMovimento read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLancamentosPeriodoResposta }

  TACBrPagamentosBBLancamentosPeriodoResposta = class(TACBrAPISchema)
  private
    flistaMovimento: TACBrPagamentosBBMovimentoLista;
    fnumeroDaposicaoDePesquisa: Integer;
    fquantidadeOcorrenciasTabeladas: Integer;
    fquantidadeOcorrenciasTotal: Integer;
    function GetlistaMovimento: TACBrPagamentosBBMovimentoLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentosPeriodoResposta);

    property numeroDaposicaoDePesquisa: Integer read fnumeroDaposicaoDePesquisa write fnumeroDaposicaoDePesquisa;
    property quantidadeOcorrenciasTotal: Integer read fquantidadeOcorrenciasTotal write fquantidadeOcorrenciasTotal;
    property quantidadeOcorrenciasTabeladas: Integer read fquantidadeOcorrenciasTabeladas write fquantidadeOcorrenciasTabeladas;
    property listaMovimento: TACBrPagamentosBBMovimentoLista read GetlistaMovimento write flistaMovimento;
  end;

  { TACBrPagamentosBBTransferencia }

  TACBrPagamentosBBTransferencia = class(TACBrAPISchema)
  private
    fagenciaDebito: Integer;
    fcontaCorrenteDebito: Integer;
    fdataRequisicao: TDateTime;
    fdigitoVerificadorContaCorrente: String;
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    fidentificacaoRequisitante: String;
    fnumeroRequisicao: Integer;
    fquantidadeTransferencias: Integer;
    fquantidadeTransferenciasValidas: Integer;
    ftipoPagamento: TACBrPagamentosBBTipoPagamento;
    ftotalTransferencias: Double;
    ftotalTransferenciasValidas: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferencia);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property dataRequisicao: TDateTime read fdataRequisicao write fdataRequisicao;
    property tipoPagamento: TACBrPagamentosBBTipoPagamento read ftipoPagamento write ftipoPagamento;
    property identificacaoRequisitante: String read fidentificacaoRequisitante write fidentificacaoRequisitante;
    property quantidadeTransferencias: Integer read fquantidadeTransferencias write fquantidadeTransferencias;
    property totalTransferencias: Double read ftotalTransferencias write ftotalTransferencias;
    property quantidadeTransferenciasValidas: Integer read fquantidadeTransferenciasValidas write fquantidadeTransferenciasValidas;
    property totalTransferenciasValidas: Double read ftotalTransferenciasValidas write ftotalTransferenciasValidas;
  end;

  { TACBrPagamentosBBTransferencias }

  TACBrPagamentosBBTransferencias = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferencia;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferencia);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferencia): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferencia);
    function New: TACBrPagamentosBBTransferencia;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferencia read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLoteTransferenciasResposta }

  TACBrPagamentosBBLoteTransferenciasResposta = class(TACBrAPISchema)
  private
    findice: Integer;
    ftransferencias: TACBrPagamentosBBTransferencias;
    function Gettransferencias: TACBrPagamentosBBTransferencias;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLoteTransferenciasResposta);

    property indice: Integer read findice write findice;
    property transferencias: TACBrPagamentosBBTransferencias read Gettransferencias write ftransferencias;
  end;

  { TACBrPagamentosBBTransferenciaRequisicao }

  TACBrPagamentosBBTransferenciaRequisicao = class(TACBrAPISchema)
  private
    fagenciaCredito: Int64;
    fcnpjBeneficiario: Int64;
    fcodigoFinalidadeDOC: Int64;
    fcodigoFinalidadeTED: Int64;
    fcontaCorrenteCredito: Int64;
    fcontaPagamentoCredito: String;
    fcpfBeneficiario: Int64;
    fdataTransferencia: TDateTime;
    fdescricaoTransferencia: String;
    fdigitoVerificadorContaCorrente: String;
    fdocumentoCredito: Int64;
    fdocumentoDebito: Int64;
    fnumeroCOMPE: Int64;
    fnumeroDepositoJudicial: String;
    fnumeroISPB: Int64;
    fvalorTransferencia: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaRequisicao);

    property numeroCOMPE: Int64 read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Int64 read fnumeroISPB write fnumeroISPB;
    property agenciaCredito: Int64 read fagenciaCredito write fagenciaCredito;
    property contaCorrenteCredito: Int64 read fcontaCorrenteCredito write fcontaCorrenteCredito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property contaPagamentoCredito: String read fcontaPagamentoCredito write fcontaPagamentoCredito;
    property cpfBeneficiario: Int64 read fcpfBeneficiario write fcpfBeneficiario;
    property cnpjBeneficiario: Int64 read fcnpjBeneficiario write fcnpjBeneficiario;
    property dataTransferencia: TDateTime read fdataTransferencia write fdataTransferencia;
    property valorTransferencia: Double read fvalorTransferencia write fvalorTransferencia;
    property documentoDebito: Int64 read fdocumentoDebito write fdocumentoDebito;
    property documentoCredito: Int64 read fdocumentoCredito write fdocumentoCredito;
    property codigoFinalidadeDOC: Int64 read fcodigoFinalidadeDOC write fcodigoFinalidadeDOC;
    property codigoFinalidadeTED: Int64 read fcodigoFinalidadeTED write fcodigoFinalidadeTED;
    property numeroDepositoJudicial: String read fnumeroDepositoJudicial write fnumeroDepositoJudicial;
    property descricaoTransferencia: String read fdescricaoTransferencia write fdescricaoTransferencia;
  end;

  { TACBrPagamentosBBTransferenciasRequisicao }

  TACBrPagamentosBBTransferenciasRequisicao = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaRequisicao;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaRequisicao);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferenciaRequisicao): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaRequisicao);
    function New: TACBrPagamentosBBTransferenciaRequisicao;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferenciaRequisicao read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLoteTransferenciasRequisicao }

  TACBrPagamentosBBLoteTransferenciasRequisicao = class(TACBrAPISchema)
  private
    fagenciaDebito: Int64;
    fcontaCorrenteDebito: Int64;
    fdigitoVerificadorContaCorrente: String;
    flistaTransferencias: TACBrPagamentosBBTransferenciasRequisicao;
    fnumeroContratoPagamento: Int64;
    fnumeroRequisicao: Int64;
    ftipoPagamento: TACBrPagamentosBBTipoPagamento;
    function GetlistaTransferencias: TACBrPagamentosBBTransferenciasRequisicao;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLoteTransferenciasRequisicao);

    property numeroRequisicao: Int64 read fnumeroRequisicao write fnumeroRequisicao;
    property numeroContratoPagamento: Int64 read fnumeroContratoPagamento write fnumeroContratoPagamento;
    property agenciaDebito: Int64 read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Int64 read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property tipoPagamento: TACBrPagamentosBBTipoPagamento read ftipoPagamento write ftipoPagamento;
    property listaTransferencias: TACBrPagamentosBBTransferenciasRequisicao read GetlistaTransferencias write flistaTransferencias;
  end;

  { TACBrPagamentosBBTransferenciaRequisicaoResposta }

  TACBrPagamentosBBTransferenciaRequisicaoResposta = class(TACBrAPISchema)
  private
    ferros: TACBrPagamentosBBLancamentoErros;
    fidentificadorTransferencia: Integer;
    findicadorAceite: String;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    function Geterros: TACBrPagamentosBBLancamentoErros;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaRequisicaoResposta);

    property identificadorTransferencia: Integer read fidentificadorTransferencia write fidentificadorTransferencia;
    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property indicadorAceite: String read findicadorAceite write findicadorAceite;
    property erros: TACBrPagamentosBBLancamentoErros read Geterros write ferros;
  end;

  { TACBrPagamentosBBTransferenciasRequisicaoResposta }

  TACBrPagamentosBBTransferenciasRequisicaoResposta = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaRequisicaoResposta;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaRequisicaoResposta);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferenciaRequisicaoResposta): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaRequisicaoResposta);
    function New: TACBrPagamentosBBTransferenciaRequisicaoResposta;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferenciaRequisicaoResposta read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLoteTransferenciasRequisicaoResposta }

  TACBrPagamentosBBLoteTransferenciasRequisicaoResposta = class(TACBrAPISchema)
  private
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    fquantidadeTransferencias: Integer;
    fquantidadeTransferenciasValidas: Integer;
    ftransferencias: TACBrPagamentosBBTransferenciasRequisicaoResposta;
    fvalorTransferencias: Double;
    fvalorTransferenciasValidas: Double;
    function Gettransferencias: TACBrPagamentosBBTransferenciasRequisicaoResposta;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLoteTransferenciasRequisicaoResposta);

    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property quantidadeTransferencias: Integer read fquantidadeTransferencias write fquantidadeTransferencias;
    property valorTransferencias: Double read fvalorTransferencias write fvalorTransferencias;
    property quantidadeTransferenciasValidas: Integer read fquantidadeTransferenciasValidas write fquantidadeTransferenciasValidas;
    property valorTransferenciasValidas: Double read fvalorTransferenciasValidas write fvalorTransferenciasValidas;
    property transferencias: TACBrPagamentosBBTransferenciasRequisicaoResposta read Gettransferencias write ftransferencias;
  end;

  { TACBrPagamentosBBTransferenciaPagamentoEspecifico }

  TACBrPagamentosBBTransferenciaPagamentoEspecifico = class(TACBrAPISchema)
  private
    fagenciaCredito: Integer;
    fcontaCorrenteCredito: Integer;
    fcpfCnpjBeneficiario: Integer;
    fdigitoVerificadorContaCorrente: String;
    fdocumentoCredito: Integer;
    fnomeBeneficiario: String;
    fnumeroCOMPE: Integer;
    fnumeroContaCredito: String;
    fnumeroISPB: Integer;
    ftexto: String;
    ftipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPagamentoEspecifico);

    property numeroCOMPE: Integer read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Integer read fnumeroISPB write fnumeroISPB;
    property agenciaCredito: Integer read fagenciaCredito write fagenciaCredito;
    property contaCorrenteCredito: Integer read fcontaCorrenteCredito write fcontaCorrenteCredito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property numeroContaCredito: String read fnumeroContaCredito write fnumeroContaCredito;
    property tipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read ftipoBeneficiario write ftipoBeneficiario;
    property cpfCnpjBeneficiario: Integer read fcpfCnpjBeneficiario write fcpfCnpjBeneficiario;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property documentoCredito: Integer read fdocumentoCredito write fdocumentoCredito;
    property texto: String read ftexto write ftexto;
  end;

  { TACBrPagamentosBBTransferenciaPagamentoEspecificoLista }

  TACBrPagamentosBBTransferenciaPagamentoEspecificoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPagamentoEspecifico;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPagamentoEspecifico);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferenciaPagamentoEspecifico): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaPagamentoEspecifico);
    function New: TACBrPagamentosBBTransferenciaPagamentoEspecifico;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferenciaPagamentoEspecifico read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta }

  TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta = class(TACBrPagamentosBBConsultaRespostaBase)
  private
    fcodigoFinalidadeDOC: String;
    fcodigoFinalidadeTED: String;
    flistaDevolucao: TACBrPagamentosBBDevolucaoLista;
    flistaPagamentos: TACBrPagamentosBBTransferenciaPagamentoEspecificoLista;
    fnumeroDepositoJudicial: String;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    function GetlistaDevolucao: TACBrPagamentosBBDevolucaoLista;
    function GetlistaPagamentos: TACBrPagamentosBBTransferenciaPagamentoEspecificoLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta);

    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property numeroDepositoJudicial: String read fnumeroDepositoJudicial write fnumeroDepositoJudicial;
    property codigoFinalidadeDOC: String read fcodigoFinalidadeDOC write fcodigoFinalidadeDOC;
    property codigoFinalidadeTED: String read fcodigoFinalidadeTED write fcodigoFinalidadeTED;
    property listaPagamentos: TACBrPagamentosBBTransferenciaPagamentoEspecificoLista read GetlistaPagamentos write flistaPagamentos;
    property listaDevolucao: TACBrPagamentosBBDevolucaoLista read GetlistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBSolicitacaoLotePagamento }

  TACBrPagamentosBBSolicitacaoLotePagamento = class(TACBrAPISchema)
  private
    fagenciaCredito: Int64;
    fcnpjBeneficiario: Int64;
    fcodigoFinalidadeDOC: Integer;
    fcodigoFinalidadeTED: Integer;
    fcontaCorrenteCredito: Int64;
    fcontaPagamentoCredito: String;
    fcpfBeneficiario: Int64;
    fdataPagamento: TDateTime;
    fdescricaoPagamento: String;
    fdigitoVerificadorContaCorrente: String;
    fdocumentoCredito: Int64;
    fdocumentoDebito: Int64;
    ferros: TACBrPagamentosBBLancamentoErros;
    fidentificadorPagamento: Int64;
    findicadorAceite: String;
    fnumeroCOMPE: Int64;
    fnumeroDepositoJudicial: String;
    fnumeroISPB: Int64;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    fvalorPagamento: Double;
    function Geterros: TACBrPagamentosBBLancamentoErros;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBSolicitacaoLotePagamento);

    property identificadorPagamento: Int64 read fidentificadorPagamento write fidentificadorPagamento;
    property numeroCOMPE: Int64 read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Int64 read fnumeroISPB write fnumeroISPB;
    property agenciaCredito: Int64 read fagenciaCredito write fagenciaCredito;
    property contaCorrenteCredito: Int64 read fcontaCorrenteCredito write fcontaCorrenteCredito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property contaPagamentoCredito: String read fcontaPagamentoCredito write fcontaPagamentoCredito;
    property cpfBeneficiario: Int64 read fcpfBeneficiario write fcpfBeneficiario;
    property cnpjBeneficiario: Int64 read fcnpjBeneficiario write fcnpjBeneficiario;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property documentoDebito: Int64 read fdocumentoDebito write fdocumentoDebito;
    property documentoCredito: Int64 read fdocumentoCredito write fdocumentoCredito;
    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property codigoFinalidadeDOC: Integer read fcodigoFinalidadeDOC write fcodigoFinalidadeDOC;
    property codigoFinalidadeTED: Integer read fcodigoFinalidadeTED write fcodigoFinalidadeTED;
    property numeroDepositoJudicial: String read fnumeroDepositoJudicial write fnumeroDepositoJudicial;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property indicadorAceite: String read findicadorAceite write findicadorAceite;
    property erros: TACBrPagamentosBBLancamentoErros read Geterros write ferros;
  end;

  { TACBrPagamentosBBSolicitacaoLotePagamentos }

  TACBrPagamentosBBSolicitacaoLotePagamentos = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBSolicitacaoLotePagamento;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBSolicitacaoLotePagamento);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBSolicitacaoLotePagamento): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBSolicitacaoLotePagamento);
    function New: TACBrPagamentosBBSolicitacaoLotePagamento;
    property Items[aIndex: Integer]: TACBrPagamentosBBSolicitacaoLotePagamento read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBSolicitacaoLotePagamentosResposta }

  TACBrPagamentosBBSolicitacaoLotePagamentosResposta = class(TACBrAPISchema)
  private
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    fpagamentos: TACBrPagamentosBBSolicitacaoLotePagamentos;
    fquantidadePagamentos: Int64;
    fquantidadePagamentosValidos: Int64;
    fvalorPagamentos: Double;
    fvalorPagamentosValidos: Double;
    function Getpagamentos: TACBrPagamentosBBSolicitacaoLotePagamentos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBSolicitacaoLotePagamentosResposta);

    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property quantidadePagamentos: Int64 read fquantidadePagamentos write fquantidadePagamentos;
    property valorPagamentos: Double read fvalorPagamentos write fvalorPagamentos;
    property quantidadePagamentosValidos: Int64 read fquantidadePagamentosValidos write fquantidadePagamentosValidos;
    property valorPagamentosValidos: Double read fvalorPagamentosValidos write fvalorPagamentosValidos;
    property pagamentos: TACBrPagamentosBBSolicitacaoLotePagamentos read Getpagamentos write fpagamentos;
  end;

  { TACBrPagamentosBBAlteracaoData }

  TACBrPagamentosBBAlteracaoData = class(TACBrAPISchema)
  private
    fcodigoProduto: Integer;
    fdataNovoPagamento: TDateTime;
    fdataOriginalPagamento: TDateTime;
    fdigitoVerificadorContaCorrenteDebito: String;
    fnumeroAgenciaDebito: Integer;
    fnumeroContaCorrenteDebito: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBAlteracaoData);

    property numeroAgenciaDebito: Integer read fnumeroAgenciaDebito write fnumeroAgenciaDebito;
    property numeroContaCorrenteDebito: Integer read fnumeroContaCorrenteDebito write fnumeroContaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property codigoProduto: Integer read fcodigoProduto write fcodigoProduto;
    property dataOriginalPagamento: TDateTime read fdataOriginalPagamento write fdataOriginalPagamento;
    property dataNovoPagamento: TDateTime read fdataNovoPagamento write fdataNovoPagamento;
  end;

  { TACBrPagamentosBBAlteracaoDataRetorno }

  TACBrPagamentosBBAlteracaoDataRetorno = class(TACBrAPISchema)
  private
    fquantidadeLancamentoAlterado: Integer;
    fquantidadeLancamentoOriginal: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBAlteracaoDataRetorno);

    property quantidadeLancamentoOriginal: Integer read fquantidadeLancamentoOriginal write fquantidadeLancamentoOriginal;
    property quantidadeLancamentoAlterado: Integer read fquantidadeLancamentoAlterado write fquantidadeLancamentoAlterado;
  end;

  { TACBrPagamentosBBBeneficiariosTransferencia }

  TACBrPagamentosBBBeneficiariosTransferencia = class(TACBrAPISchema)
  private
    fagenciaCredito: Integer;
    fagenciaDebito: Integer;
    fcodigoAutenticacaoPagamento: String;
    fcodigoFinalidadeDOC: String;
    fcodigoFinalidadeTED: String;
    fcontaCorrenteCredito: Integer;
    fcontaCorrenteDebito: Integer;
    fcontaPagamentoCredito: String;
    fcpfCnpjBeneficiario: Int64;
    fdataTransferencia: TDateTime;
    fdescricaoTransferencia: String;
    fdigitoVerificadorContaCorrente: String;
    fdigitoVerificadorContaCorrenteDebito: String;
    fdocumentoDebito: Integer;
    festadoPagamento: String;
    ffimCartao: Integer;
    fformaTransmissao: Integer;
    fidentificador: Int64;
    finicioCartao: Integer;
    fnomeBeneficiario: String;
    fnumeroArquivoPagamento: Integer;
    fnumeroCOMPE: Integer;
    fnumeroDepositoJudicial: String;
    fnumeroISPB: Integer;
    fnumeroRequisicao: Integer;
    ftipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
    ftipoCredito: TACBrPagamentosBBTipoCredito;
    ftipoPagamento: Integer;
    fvalorTransferencia: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBBeneficiariosTransferencia);

    property identificador: Int64 read fidentificador write fidentificador;
    property estadoPagamento: String read festadoPagamento write festadoPagamento;
    property tipoPagamento: Integer read ftipoPagamento write ftipoPagamento;
    property tipoCredito: TACBrPagamentosBBTipoCredito read ftipoCredito write ftipoCredito;
    property dataTransferencia: TDateTime read fdataTransferencia write fdataTransferencia;
    property valorTransferencia: Double read fvalorTransferencia write fvalorTransferencia;
    property documentoDebito: Integer read fdocumentoDebito write fdocumentoDebito;
    property numeroCOMPE: Integer read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Integer read fnumeroISPB write fnumeroISPB;
    property agenciaCredito: Integer read fagenciaCredito write fagenciaCredito;
    property contaCorrenteCredito: Integer read fcontaCorrenteCredito write fcontaCorrenteCredito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property contaPagamentoCredito: String read fcontaPagamentoCredito write fcontaPagamentoCredito;
    property tipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read ftipoBeneficiario write ftipoBeneficiario;
    property cpfCnpjBeneficiario: Int64 read fcpfCnpjBeneficiario write fcpfCnpjBeneficiario;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property codigoAutenticacaoPagamento: String read fcodigoAutenticacaoPagamento write fcodigoAutenticacaoPagamento;
    property codigoFinalidadeDOC: String read fcodigoFinalidadeDOC write fcodigoFinalidadeDOC;
    property codigoFinalidadeTED: String read fcodigoFinalidadeTED write fcodigoFinalidadeTED;
    property numeroDepositoJudicial: String read fnumeroDepositoJudicial write fnumeroDepositoJudicial;
    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property numeroArquivoPagamento: Integer read fnumeroArquivoPagamento write fnumeroArquivoPagamento;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property inicioCartao: Integer read finicioCartao write finicioCartao;
    property fimCartao: Integer read ffimCartao write ffimCartao;
    property descricaoTransferencia: String read fdescricaoTransferencia write fdescricaoTransferencia;
    property formaTransmissao: Integer read fformaTransmissao write fformaTransmissao;
  end;

  { TACBrPagamentosBBBeneficiariosTransferencias }

  TACBrPagamentosBBBeneficiariosTransferencias = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBBeneficiariosTransferencia;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBBeneficiariosTransferencia);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBBeneficiariosTransferencia): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBBeneficiariosTransferencia);
    function New: TACBrPagamentosBBBeneficiariosTransferencia;
    property Items[aIndex: Integer]: TACBrPagamentosBBBeneficiariosTransferencia read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBBeneficiariosTransferenciaResposta }

  TACBrPagamentosBBBeneficiariosTransferenciaResposta = class(TACBrAPISchema)
  private
    findice: Integer;
    fquantidadeTotalTransferencias: Integer;
    ftransferencias: TACBrPagamentosBBBeneficiariosTransferencias;
    function Gettransferencias: TACBrPagamentosBBBeneficiariosTransferencias;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBBeneficiariosTransferenciaResposta);

    property indice: Integer read findice write findice;
    property quantidadeTotalTransferencias: Integer read fquantidadeTotalTransferencias write fquantidadeTotalTransferencias;
    property transferencias: TACBrPagamentosBBBeneficiariosTransferencias read Gettransferencias write ftransferencias;
  end;

  function PagamentosBBTipoContaToInteger(const aValue: TACBrPagamentosBBTipoConta): Integer;
  function IntegerToPagamentosBBTipoConta(const aValue: Integer): TACBrPagamentosBBTipoConta;
  function PagamentosBBTipoCreditoToInteger(const aValue: TACBrPagamentosBBTipoCredito): Integer;
  function IntegerToPagamentosBBTipoCredito(const aValue: Integer): TACBrPagamentosBBTipoCredito;
  function PagamentosBBTipoPagamentoToInteger(const aValue: TACBrPagamentosBBTipoPagamento): Integer;
  function IntegerToPagamentosBBTipoPagamento(const aValue: Integer): TACBrPagamentosBBTipoPagamento;
  function PagamentosBBTipoBeneficiarioToInteger(const aValue: TACBrPagamentosBBTipoBeneficiario): Integer;
  function IntegerToPagamentosBBTipoBeneficiario(const aValue: Integer): TACBrPagamentosBBTipoBeneficiario;
  function PagamentosBBFormaIdentificacaoToInteger(const aValue: TACBrPagamentosBBFormaIdentificacao): Integer;
  function IntegerToPagamentosBBFormaIdentificacao(const aValue: Integer): TACBrPagamentosBBFormaIdentificacao;
  function PagamentosBBEstadoRequisicaoToInteger(const aValue: TACBrPagamentosBBEstadoRequisicao): Integer;
  function IntegerToPagamentosBBEstadoRequisicao(const aValue: Integer): TACBrPagamentosBBEstadoRequisicao;
  function PagamentosBBVinculadosErrosToInteger(const aValue: TACBrPagamentosBBVinculadosErros): Integer;
  function IntegerToPagamentosBBVinculadosErros(const aValue: Integer): TACBrPagamentosBBVinculadosErros;
  function PagamentosBBTipoContribuinteToInteger(const aValue: TACBrPagamentosBBTipoContribuinte): Integer;
  function IntegerToPagamentosBBTipoContribuinte(const aValue: Integer): TACBrPagamentosBBTipoContribuinte;
  function PagamentosBBCodigoDevolucaoToInteger(const aValue: TACBrPagamentosBBCodigoDevolucao): Integer;
  function IntegerToPagamentosBBCodigoDevolucao(const aValue: Integer): TACBrPagamentosBBCodigoDevolucao;
  function PagamentosBBTransferenciaErroToInteger(const aValue: TACBrPagamenosBBTransferenciaErro): Integer;
  function IntegerToPagamentosBBTransferenciaErro(const aValue: Integer): TACBrPagamenosBBTransferenciaErro;
  function PagamentosBBEstadoToInteger(const aValue: TACBrPagamentosBBEstado): Integer;
  function IntegerToPagamentosBBEstado(const aValue: Integer): TACBrPagamentosBBEstado;
  function PagamentosBBLoteEstadoToInteger(const aValue: TACBrPagamentosBBLoteEstado): Integer;
  function IntegerToPagamentosBBLoteEstado(const aValue: Integer): TACBrPagamentosBBLoteEstado;

implementation

function PagamentosBBTipoContaToInteger(const aValue: TACBrPagamentosBBTipoConta): Integer;
begin
  Result := 0;
  case aValue of
    ptcContaCorrente: Result := 1;
    ptcContaPagamento: Result := 2;
    ptcContaPoupanca: Result := 3;
  end;
end;

function IntegerToPagamentosBBTipoConta(const aValue: Integer): TACBrPagamentosBBTipoConta;
begin 
  Result := ptcNenhum;
  case aValue of
    1: Result := ptcContaCorrente;
    2: Result := ptcContaPagamento;
    3: Result := ptcContaPoupanca;
  end;
end;

function PagamentosBBTipoCreditoToInteger(const aValue: TACBrPagamentosBBTipoCredito): Integer;
begin
  Result := 0;
  case aValue of
    pcrCreditoContaCorrente: Result := 1;
    pcrDOCTED: Result := 3;
    pcrCreditoContaPoupanca: Result := 5;
    pcrLiquidacaoGuiaCodigoBarra: Result := 13;
    pcrLiquidacaoGuiaSemCodigoBarra: Result := 21;
    pcrLiquidacaoBoletoBB: Result := 30;
    pcrLiquidacaoBoletoOutrosBancos: Result := 31;
    pcrDepositoJudicial: Result := 71;
  end;
end;

function IntegerToPagamentosBBTipoCredito(const aValue: Integer): TACBrPagamentosBBTipoCredito;
begin
  Result := pcrNenhum;
  case aValue of
    1: Result := pcrCreditoContaCorrente;
    3: Result := pcrDOCTED;
    5: Result := pcrCreditoContaPoupanca;
    13: Result := pcrLiquidacaoGuiaCodigoBarra;
    21: Result := pcrLiquidacaoGuiaSemCodigoBarra;
    30: Result := pcrLiquidacaoBoletoBB;
    31: Result := pcrLiquidacaoBoletoOutrosBancos;
    71: Result := pcrDepositoJudicial;
  end;
end;

function PagamentosBBTipoPagamentoToInteger(const aValue: TACBrPagamentosBBTipoPagamento): Integer;
begin
  Result := 0;
  case aValue of
    ppgPagamentoFornecedores: Result := 126;
    ppgPagamentoSalario: Result := 127;
    ppgPagamentoDiverso: Result := 128;
  end;
end;

function IntegerToPagamentosBBTipoPagamento(const aValue: Integer): TACBrPagamentosBBTipoPagamento;
begin
  Result := ppgNenhum;
  case aValue of
    126: Result := ppgPagamentoFornecedores;
    127: Result := ppgPagamentoSalario;
    128: Result := ppgPagamentoDiverso;
  end;
end;

function PagamentosBBTipoBeneficiarioToInteger(const aValue: TACBrPagamentosBBTipoBeneficiario): Integer;
begin
  Result := 0;
  case aValue of
    ptbCPF: Result := 1;
    ptbCNPJ: Result := 2;
  end;
end;

function IntegerToPagamentosBBTipoBeneficiario(const aValue: Integer): TACBrPagamentosBBTipoBeneficiario;
begin
  Result := ptbNenhum;
  case aValue of
    1: Result := ptbCPF;
    2: Result := ptbCNPJ;
  end;
end;

function PagamentosBBFormaIdentificacaoToInteger(const aValue: TACBrPagamentosBBFormaIdentificacao): Integer;
begin 
  Result := 0;
  case aValue of
    pfiTelefone: Result := 1;
    pfiEmail: Result := 2;
    pfiCPFCNPJ: Result := 3;
    pfiChaveAleatoria: Result := 4;
    pfiDadosBancarios: Result := 5;
  end;
end;

function IntegerToPagamentosBBFormaIdentificacao(const aValue: Integer): TACBrPagamentosBBFormaIdentificacao;
begin 
  Result := pfiNenhum;
  case aValue of
    1: Result := pfiTelefone;
    2: Result := pfiEmail;
    3: Result := pfiCPFCNPJ;
    4: Result := pfiChaveAleatoria;
    5: Result := pfiDadosBancarios;
  end;
end;

function PagamentosBBEstadoRequisicaoToInteger(const aValue: TACBrPagamentosBBEstadoRequisicao): Integer;
begin
  Result := 0;
  case aValue of
    perDadosConsistentes: Result := 1;
    perDadosInconsistentesParcial: Result := 2;
    perDadosInconsistentesTotal: Result := 3;
    perPendenteAcaoConveniado: Result := 4;
    perEmProcessamentoBanco: Result := 5;
    perProcessada: Result := 6;
    perRejeitada: Result := 7;
    perPreparandoRemessaNaoLiberada: Result := 8;
    perLiberadaViaAPI: Result := 9;
    perPreparandoRemessaLiberada: Result := 10;
  end;
end;

function IntegerToPagamentosBBEstadoRequisicao(const aValue: Integer): TACBrPagamentosBBEstadoRequisicao;
begin
  Result := perNenhum;
  case aValue of
    1: Result := perDadosConsistentes;
    2: Result := perDadosInconsistentesParcial;
    3: Result := perDadosInconsistentesTotal;
    4: Result := perPendenteAcaoConveniado;
    5: Result := perEmProcessamentoBanco;
    6: Result := perProcessada;
    7: Result := perRejeitada;
    8: Result := perPreparandoRemessaNaoLiberada;
    9: Result := perLiberadaViaAPI;
    10: Result := perPreparandoRemessaLiberada;
  end;
end;

function PagamentosBBVinculadosErrosToInteger(const aValue: TACBrPagamentosBBVinculadosErros): Integer;
begin
  Result := 0;
  case aValue of
    pveAgenciaZerada: Result := 1;
    pveContaCreditoIncorreta: Result := 2;
    pveDVCreditoEmBranco: Result := 3;
    pveCPFInvalido: Result := 4;
    pveCNPJInvalido: Result := 5;
    pveDataPagamentoZerada: Result := 6;
    pveDataPagamentoInvalida: Result := 7;
    pveValorIncorreto: Result := 8;
    pveValorZerado: Result := 9;
    pveFinalidadeDOCeTEDNaoInformadas: Result := 12;
    pveFinalidadesDOCeTEDInformadas: Result := 13;
    pveDVCreditoInvalido: Result := 15;
    pveCPFECNPJInformados: Result := 16;
    pveCPFECNPJZerados: Result := 17;
    pveDVCPFInvalido: Result := 18;
    pveDVCNPJInvalido: Result := 19;
    pveContaCreditoIgualDebito: Result := 20;
    pvePagamentoSalarioBB: Result := 21;
    pvePagamentoSalarioISPBZerado: Result := 22;
    pveContaCreditoObrigatoria: Result := 23;
    pveCPFouCNPJObrigatorioDepositoJudicial: Result := 24;
    pveContaCreditoNaoInformadaDepositoJudicial: Result := 26;
    pveDVCreditoNaoInformadoDepositoJudicial: Result := 27;
    pveFinalidadeDOCPreenchida: Result := 28;
    pveFinalidadeTEDPreenchida: Result := 29;
    pveDepositoJudicialDOCouTED: Result := 30;
  end;
end;

function IntegerToPagamentosBBVinculadosErros(const aValue: Integer): TACBrPagamentosBBVinculadosErros;
begin
  Result := pveNenhum;
  case aValue of
    1: Result := pveAgenciaZerada;
    2: Result := pveContaCreditoIncorreta;
    3: Result := pveDVCreditoEmBranco;
    4: Result := pveCPFInvalido;
    5: Result := pveCNPJInvalido;
    6: Result := pveDataPagamentoZerada;
    7: Result := pveDataPagamentoInvalida;
    8: Result := pveValorIncorreto;
    9: Result := pveValorZerado;
    12: Result := pveFinalidadeDOCeTEDNaoInformadas;
    13: Result := pveFinalidadesDOCeTEDInformadas;
    15: Result := pveDVCreditoInvalido;
    16: Result := pveCPFECNPJInformados;
    17: Result := pveCPFECNPJZerados;
    18: Result := pveDVCPFInvalido;
    19: Result := pveDVCNPJInvalido;
    20: Result := pveContaCreditoIgualDebito;
    21: Result := pvePagamentoSalarioBB;
    22: Result := pvePagamentoSalarioISPBZerado;
    23: Result := pveContaCreditoObrigatoria;
    24: Result := pveCPFouCNPJObrigatorioDepositoJudicial;
    26: Result := pveContaCreditoNaoInformadaDepositoJudicial;
    27: Result := pveDVCreditoNaoInformadoDepositoJudicial;
    28: Result := pveFinalidadeDOCPreenchida;
    29: Result := pveFinalidadeTEDPreenchida;
    30: Result := pveDepositoJudicialDOCouTED;
  end;
end;

function PagamentosBBTipoContribuinteToInteger(const aValue: TACBrPagamentosBBTipoContribuinte): Integer;
begin
  Result := 0;
  case aValue of
    pctCNPJ: Result := 1;
    pctCPF: Result := 2;
    pctNITPISPASEP: Result := 3;
    pctCEI: Result := 4;
    pctNB: Result := 6;
    pctNumeroTitulo: Result := 7;
    pctDEBCAD: Result := 8;
    pctReferencia: Result := 9;
  end;
end;

function IntegerToPagamentosBBTipoContribuinte(const aValue: Integer): TACBrPagamentosBBTipoContribuinte;
begin 
  Result := pctNenhum;
  case aValue of
    1: Result := pctCNPJ;
    2: Result := pctCPF;
    3: Result := pctNITPISPASEP;
    4: Result := pctCEI;
    6: Result := pctNB;
    7: Result := pctNumeroTitulo;
    8: Result := pctDEBCAD;
    9: Result := pctReferencia;
  end;
end;

function PagamentosBBCodigoDevolucaoToInteger(const aValue: TACBrPagamentosBBCodigoDevolucao): Integer;
begin
  Result := 0;
  case aValue of
    pcdAgenciaCreditoZerada: Result := 1;
    pcdContaCreditoNaoNumerica: Result := 2;
    pcdDigitoContaCreditoNaoInformado: Result := 3;
    pcdCPFInformadoNaoNumerico: Result := 4;
    pcdCNPJInformadoNaoNumerico: Result := 5;
    pcdDataPagamentoNaoInformada: Result := 6;
    pcdDataPagamentoInvalida: Result := 7;
    pcdValorPagamentoNaoNumerico: Result := 8;
    pcdValorPagamentoZerado: Result := 9;
    pcdNumeroCompensacaoISPBNaoInformados: Result := 10;
    pcdNumeroCompensacaoISPBInformados: Result := 11;
    pcdFinalidadeDOCTEDNaoInformados: Result := 12;
    pcdFinalidadeDOCTEDInformados: Result := 13;
    pcdNumeroDepositoJudicialNaoInformado: Result := 14;
    pcdDigitoContaCreditoInvalido: Result := 15;
    pcdCPFECNPJInformados: Result := 16;
    pcdCPFECNPJNaoInformados: Result := 17;
    pcdDigitoCPFNaoInformado: Result := 18;
    pcdDigitoCNPJInvalido: Result := 19;
    pcdAgenciaContaCreditoIguaisDebito: Result := 20;
    pcdNumeroCompensacaoInvalido: Result := 21;
    pcdNumeroISPBDiferenteZeros: Result := 22;
    pcdContaCreditoNaoInformada: Result := 23;
    pcdCPFNaoInformado: Result := 24;
    pcdCNPJInformado: Result := 25;
    pcdContaCreditoInformada: Result := 26;
    pcdDigitoContaCreditoInformado: Result := 27;
    pcdFinalidadeDOCInformada: Result := 28;
    pcdFinalidadeTEDInformada: Result := 29;
    pcdNumeroDepositoJudicialInformado: Result := 30;
    pcdNumeroDocumentoCreditoNaoNumerico: Result := 31;
    pcdNumeroDocumentoDebitoNaoNumerico: Result := 32;
    pcdCPFNaoEncontradoReceitaFederal: Result := 33;
    pcdCNPJNaoEncontradoReceitaFederal: Result := 34;
    pcdContaPoupancaNaoPermitidaFornecedor: Result := 35;
    pcdCodigoCOMPEIgualUm: Result := 36;
    pcdCodigoISPBIgualZero: Result := 37;
    pcdCodigoBarrasNaoNumerico: Result := 38;
    pcdCodigoBarrasZeros: Result := 39;
    pcdNumeroInscricaoPagadorNaoNumerico: Result := 40;
    pcdNumeroInscricaoBeneficiarioNaoNumerico: Result := 41;
    pcdNumeroInscricaoAvalistaNaoNumerico: Result := 42;
    pcdDigitoCPFPagadorInvalido: Result := 43;
    pcdDigitoCPFBeneficiarioInvalido: Result := 44;
    pcdDigitoCPFAvalistaInvalido: Result := 45;
    pcdDigitoCNPJPagadorInvalido: Result := 46;
    pcdDigitoCNPJBeneficiarioInvalido: Result := 47;
    pcdDigitoCNPJAvalistaInvalido: Result := 48;
    pcdDataVencimentoInvalida: Result := 49;
    pcdValorNominalNaoNumerico: Result := 50;
    pcdValorDescontoNaoNumerico: Result := 51;
    pcdValorMoraNaoNumerico: Result := 52;
    pcdDataPagamentoMaiorIgualAtual: Result := 53;
    pcdNumeroDocumentoDebitoNaoInformado: Result := 54;
    pcdDataVencimentoNaoInformada: Result := 55;
    pcdNomeBeneficiarioNaoInformado: Result := 56;
    pcdNumeroInscricaoBeneficiarioNaoInformado: Result := 57;
    pcdContaPagamentoInformada: Result := 58;
    pcdContaCreditoContaPagamentoInformados: Result := 59;
    pcdTransacaoCanceladaCliente: Result := 60;
    pcdCodigoReceitaTributoNaoInformado: Result := 61;
    pcdTipoIdentificacaoContribuinteNaoInformado: Result := 62;
    pcdNumeroIdentificacaoContribuinteNaoInformado: Result := 63;
    pcdNumeroIdentificacaoContribuinteNaoNumerico: Result := 64;
    pcdCodigoIdentificacaoTributoNaoInformado: Result := 65;
    pcdPeriodoApuracaoNaoInformado: Result := 66;
    pcdNumeroReferenciaNaoInformado: Result := 67;
    pcdValorPrincipalNaoNumerico: Result := 68;
    pcdValorPrincipalNaoInformado: Result := 69;
    pcdValorMultaNaoNumerico: Result := 70;
    pcdValorJurosEncargosNaoNumerico: Result := 71;
    pcdDataVencimentoNaoInformada2: Result := 72;
    pcdMesAnoCompetenciaNaoInformados: Result := 73;
    pcdValorPrevistoPagamentoINSSNaoNumerico: Result := 74;
    pcdValorPrevistoPagamentoINSSNaoInformado: Result := 75;
    pcdValorOutrasEntidadesNaoNumerico: Result := 76;
    pcdValorAtualizacaoMonetariaNaoNumerico: Result := 77;
    pcdPeriodoApuracaoInvalido: Result := 79;
    pcdContaCreditoInvalida: Result := 80;
    pcdContaNaoPertenceFuncionario: Result := 81;
    pcdPagamentoPermitidoApenasPessoasFisicas: Result := 82;
    pcdAgenciaContaIncorretos: Result := 83;
    pcdContaNaoAtiva: Result := 84;
    pcdContaNaoPermiteCreditoSalario: Result := 85;
    pcdAgenciaCreditoContaPagamentoInformados: Result := 86;
    pcdMesCompetenciaInvalido: Result := 90;
    pcdValorOutrasDeducoesInvalido: Result := 91;
    pcdValorOutrosAcrescimosInvalido: Result := 92;
    pcdCodigoFormaIdentificacaoClienteNaoInformado: Result := 93;
    pcdDDDPixNaoInformado: Result := 94;
    pcdTelefonePixNaoInformado: Result := 95;
    pcdEmailPixNaoInformado: Result := 96;
    pcdChaveAleatoriaPixNaoInformada: Result := 97;
    pcdCodigoTipoContaPixNaoInformado: Result := 98;
    pcdConsultarBancoDetalharErro: Result := 99;
    pcdEmailInvalido: Result := 100;
    pcdEmailPixCaractereEspecial: Result := 101;
    pcdTelefoneInvalido: Result := 102;
    pcdDDDInvalido: Result := 103;
    pcdEmailTamanhoMaior77: Result := 104;
    pcdInsuficienciaFundosDebitoNaoEfetuado: Result := 200;
    pcdCreditoDebitoCanceladoPagador: Result := 201;
    pcdDebitoAutorizadoAgenciaEfetuado: Result := 202;
    pcdControleInvalido: Result := 203;
    pcdTipoOperacaoInvalido: Result := 204;
    pcdTipoServicoInvalido: Result := 205;
    pcdFormaLancamentoInvalida: Result := 206;
    pcdTipoNumeroInscricaoInvalido: Result := 207;
    pcdCodigoConvenioInvalido: Result := 208;
    pcdAgenciaContaCorrenteDVInvalido: Result := 209;
    pcdNumeroSequencialRegistroLoteInvalido: Result := 210;
    pcdCodigoSegmentoDetalheInvalido: Result := 211;
    pcdLancamentoInconsistenteRejeitadoPrevia: Result := 212;
    pcdNumeroCompeBancoCreditoInvalido: Result := 213;
    pcdNumeroISPBInvalido: Result := 214;
    pcdAgenciaMantenedoraContaCorrenteFavorecidoInvalida: Result := 215;
    pcdContaCorrenteDVContaPagamentoFavorecidoInvalido: Result := 216;
    pcdNomeFavorecidoNaoInformado: Result := 217;
    pcdDataLancamentoInvalida: Result := 218;
    pcdTipoQuantidadeMoedaInvalido: Result := 219;
    pcdValorLancamentoInvalido: Result := 220;
    pcdAvisoFavorecidoIdentificacaoInvalida: Result := 221;
    pcdTipoNumeroInscricaoFavorecidoInvalido: Result := 222;
    pcdLogradouroFavorecidoNaoInformado: Result := 223;
    pcdNumeroLocalFavorecidoNaoInformado: Result := 224;
    pcdCidadeFavorecidoNaoInformada: Result := 225;
    pcdCEPFavorecidoInvalido: Result := 226;
    pcdSiglaEstadoFavorecidoInvalida: Result := 227;
    pcdNumeroBancoCreditoInvalido: Result := 228;
    pcdCodigoNomeAgenciaDepositariaNaoInformado: Result := 229;
    pcdSeuNumeroInvalido: Result := 230;
    pcdNossoNumeroInvalido: Result := 231;
    pcdInclusaoEfetuadaSucesso: Result := 232;
    pcdAlteracaoEfetuadaSucesso: Result := 233;
    pcdExclusaoEfetuadaSucesso: Result := 234;
    pcdAgenciaContaImpedidaLegalmente: Result := 235;
    pcdEmpresaNaoPagouSalario: Result := 236;
    pcdFalecimentoMutuario: Result := 237;
    pcdEmpresaNaoEnviouRemessaMutuario: Result := 238;
    pcdEmpresaNaoEnviouRemessaVencimento: Result := 239;
    pcdValorParcelaInvalida: Result := 240;
    pcdIdentificacaoContratoInvalida: Result := 241;
    pcdOperacaoConsignacaoIncluidaSucesso: Result := 242;
    pcdOperacaoConsignacaoAlteradaSucesso: Result := 243;
    pcdOperacaoConsignacaoExcluidaSucesso: Result := 244;
    pcdOperacaoConsignacaoLiquidadaSucesso: Result := 245;
    pcdReativacaoEfetuadaSucesso: Result := 246;
    pcdSuspensaoEfetuadaSucesso: Result := 247;
    pcdCodigoBarrasCodigoBancoInvalido: Result := 248;
    pcdCodigoBarrasCodigoMoedaInvalido: Result := 249;
    pcdCodigoBarrasDigitoVerificadorGeralInvalido: Result := 250;
    pcdCodigoBarrasValorTituloInvalido: Result := 251;
    pcdCodigoBarrasCampoLivreInvalido: Result := 252;
    pcdValorDocumentoInvalido: Result := 253;
    pcdValorAbatimentoInvalido: Result := 254;
    pcdValorDescontoInvalido: Result := 255;
    pcdValorMoraInvalido: Result := 256;
    pcdValorMultaInvalido: Result := 257;
    pcdValorIRInvalido: Result := 258;
    pcdValorISSInvalido: Result := 259;
    pcdValorIOFInvalido: Result := 260;
    pcdValorOutrasDeducoesInvalido2: Result := 261;
    pcdValorOutrosAcrescimosInvalido2: Result := 262;
    pcdValorINSSInvalido: Result := 263;
    pcdLoteNaoAceito: Result := 264;
    pcdInscricaoEmpresaInvalidaContrato: Result := 265;
    pcdConvenioEmpresaInexistenteContrato: Result := 266;
    pcdAgenciaContaCorrenteEmpresaInexistenteContrato: Result := 267;
    pcdTipoServicoInvalidoContrato: Result := 268;
    pcdContaCorrenteEmpresaSaldoInsuficiente: Result := 269;
    pcdLoteServicoForaSequencia: Result := 270;
    pcdLoteServicoInvalido: Result := 271;
    pcdArquivoNaoAceito: Result := 272;
    pcdTipoRegistroInvalido: Result := 273;
    pcdCodigoRemessaRetornoInvalido: Result := 274;
    pcdVersaoLayoutInvalida: Result := 275;
    pcdMutuarioNaoIdentificado: Result := 276;
    pcdTipoBeneficioNaoPermiteEmprestimo: Result := 277;
    pcdBeneficioCessadoSuspenso: Result := 278;
    pcdBeneficioPossuiRepresentanteLegal: Result := 279;
    pcdBeneficioTipoPA: Result := 280;
    pcdQuantidadeContratosExcedida: Result := 281;
    pcdBeneficioNaoPertenceBanco: Result := 282;
    pcdInicioDescontoUltrapassado: Result := 283;
    pcdNumeroParcelaInvalida: Result := 284;
    pcdQuantidadeParcelaInvalida: Result := 285;
    pcdMargemConsignavelExcedidaPrazo: Result := 286;
    pcdEmprestimoJaCadastrado: Result := 287;
    pcdEmprestimoInexistente: Result := 288;
    pcdEmprestimoJaEncerrado: Result := 289;
    pcdArquivoSemTrailer: Result := 290;
    pcdMutuarioSemCreditoCompetencia: Result := 291;
    pcdNaoDescontadoOutrosMotivos: Result := 292;
    pcdRetornoCreditoNaoPago: Result := 293;
    pcdCancelamentoEmprestimoRetroativo: Result := 294;
    pcdOutrosMotivosGlosa: Result := 295;
    pcdMargemConsignavelExcedidaAcimaPrazo: Result := 296;
    pcdMutuarioDesligadoEmpregador: Result := 297;
    pcdMutuarioAfastadoLicenca: Result := 298;
    pcdPrimeiroNomeMutuarioDiferente: Result := 299;
    pcdBeneficioSuspensoCessadoAPS: Result := 300;
    pcdBeneficioSuspensoDependenciaCalculo: Result := 301;
    pcdBeneficioSuspensoCessadoInspetoria: Result := 302;
    pcdBeneficioBloqueadoEmprestimoBeneficiario: Result := 303;
    pcdBeneficioBloqueadoEmprestimoTBM: Result := 304;
    pcdBeneficioFaseConcessaoPA: Result := 305;
    pcdBeneficioCessadoObito: Result := 306;
    pcdBeneficioCessadoFraude: Result := 307;
    pcdBeneficioCessadoOutroBeneficio: Result := 308;
    pcdBeneficioCessadoEstatutario: Result := 309;
    pcdEmprestimoSuspensoAPS: Result := 310;
    pcdEmprestimoCanceladoBanco: Result := 311;
    pcdCreditoTransformadoPAB: Result := 312;
    pcdTerminoConsignacaoAlterado: Result := 313;
    pcdFimEmprestimoPeriodoSuspensao: Result := 314;
    pcdEmprestimoSuspensoBanco: Result := 315;
    pcdNaoAverbacaoContratoQuantidadeParcelas: Result := 316;
    pcdLoteNaoAceitoTotaisDiferenca: Result := 317;
    pcdTituloNaoEncontrado: Result := 318;
    pcdIdentificadorRegistroOpcionalInvalido: Result := 319;
    pcdCodigoPadraoInvalido: Result := 320;
    pcdCodigoOcorrenciaInvalido: Result := 321;
    pcdComplementoOcorrenciaInvalido: Result := 322;
    pcdAlegacaoJaInformada: Result := 323;
    pcdAgenciaContaFavorecidoSubstituida: Result := 324;
    pcdDivergenciaNomeBeneficiario: Result := 325;
    pcdConfirmacaoAntecipacaoValor: Result := 326;
    pcdAntecipacaoParcialValor: Result := 327;
    pcdBoletoBloqueadoBase: Result := 328;
    pcdSistemaContingenciaBoletoValorMaior: Result := 329;
    pcdSistemaContingenciaBoletoVencido: Result := 330;
    pcdSistemaContingenciaBoletoIndexado: Result := 331;
    pcdBeneficiarioDivergente: Result := 332;
    pcdLimitePagamentosParciaisExcedido: Result := 333;
    pcdBoletoJaLiquidado: Result := 334;
    pcdConsultarBancoDetalharErro2: Result := 999;
  end;
end;

function IntegerToPagamentosBBCodigoDevolucao(const aValue: Integer): TACBrPagamentosBBCodigoDevolucao;
begin 
  Result := pcdNenhum;
  case aValue of
    1: Result := pcdAgenciaCreditoZerada;
    2: Result := pcdContaCreditoNaoNumerica;
    3: Result := pcdDigitoContaCreditoNaoInformado;
    4: Result := pcdCPFInformadoNaoNumerico;
    5: Result := pcdCNPJInformadoNaoNumerico;
    6: Result := pcdDataPagamentoNaoInformada;
    7: Result := pcdDataPagamentoInvalida;
    8: Result := pcdValorPagamentoNaoNumerico;
    9: Result := pcdValorPagamentoZerado;
    10: Result := pcdNumeroCompensacaoISPBNaoInformados;
    11: Result := pcdNumeroCompensacaoISPBInformados;
    12: Result := pcdFinalidadeDOCTEDNaoInformados;
    13: Result := pcdFinalidadeDOCTEDInformados;
    14: Result := pcdNumeroDepositoJudicialNaoInformado;
    15: Result := pcdDigitoContaCreditoInvalido;
    16: Result := pcdCPFECNPJInformados;
    17: Result := pcdCPFECNPJNaoInformados;
    18: Result := pcdDigitoCPFNaoInformado;
    19: Result := pcdDigitoCNPJInvalido;
    20: Result := pcdAgenciaContaCreditoIguaisDebito;
    21: Result := pcdNumeroCompensacaoInvalido;
    22: Result := pcdNumeroISPBDiferenteZeros;
    23: Result := pcdContaCreditoNaoInformada;
    24: Result := pcdCPFNaoInformado;
    25: Result := pcdCNPJInformado;
    26: Result := pcdContaCreditoInformada;
    27: Result := pcdDigitoContaCreditoInformado;
    28: Result := pcdFinalidadeDOCInformada;
    29: Result := pcdFinalidadeTEDInformada;
    30: Result := pcdNumeroDepositoJudicialInformado;
    31: Result := pcdNumeroDocumentoCreditoNaoNumerico;
    32: Result := pcdNumeroDocumentoDebitoNaoNumerico;
    33: Result := pcdCPFNaoEncontradoReceitaFederal;
    34: Result := pcdCNPJNaoEncontradoReceitaFederal;
    35: Result := pcdContaPoupancaNaoPermitidaFornecedor;
    36: Result := pcdCodigoCOMPEIgualUm;
    37: Result := pcdCodigoISPBIgualZero;
    38: Result := pcdCodigoBarrasNaoNumerico;
    39: Result := pcdCodigoBarrasZeros;
    40: Result := pcdNumeroInscricaoPagadorNaoNumerico;
    41: Result := pcdNumeroInscricaoBeneficiarioNaoNumerico;
    42: Result := pcdNumeroInscricaoAvalistaNaoNumerico;
    43: Result := pcdDigitoCPFPagadorInvalido;
    44: Result := pcdDigitoCPFBeneficiarioInvalido;
    45: Result := pcdDigitoCPFAvalistaInvalido;
    46: Result := pcdDigitoCNPJPagadorInvalido;
    47: Result := pcdDigitoCNPJBeneficiarioInvalido;
    48: Result := pcdDigitoCNPJAvalistaInvalido;
    49: Result := pcdDataVencimentoInvalida;
    50: Result := pcdValorNominalNaoNumerico;
    51: Result := pcdValorDescontoNaoNumerico;
    52: Result := pcdValorMoraNaoNumerico;
    53: Result := pcdDataPagamentoMaiorIgualAtual;
    54: Result := pcdNumeroDocumentoDebitoNaoInformado;
    55: Result := pcdDataVencimentoNaoInformada;
    56: Result := pcdNomeBeneficiarioNaoInformado;
    57: Result := pcdNumeroInscricaoBeneficiarioNaoInformado;
    58: Result := pcdContaPagamentoInformada;
    59: Result := pcdContaCreditoContaPagamentoInformados;
    60: Result := pcdTransacaoCanceladaCliente;
    61: Result := pcdCodigoReceitaTributoNaoInformado;
    62: Result := pcdTipoIdentificacaoContribuinteNaoInformado;
    63: Result := pcdNumeroIdentificacaoContribuinteNaoInformado;
    64: Result := pcdNumeroIdentificacaoContribuinteNaoNumerico;
    65: Result := pcdCodigoIdentificacaoTributoNaoInformado;
    66: Result := pcdPeriodoApuracaoNaoInformado;
    67: Result := pcdNumeroReferenciaNaoInformado;
    68: Result := pcdValorPrincipalNaoNumerico;
    69: Result := pcdValorPrincipalNaoInformado;
    70: Result := pcdValorMultaNaoNumerico;
    71: Result := pcdValorJurosEncargosNaoNumerico;
    72: Result := pcdDataVencimentoNaoInformada2;
    73: Result := pcdMesAnoCompetenciaNaoInformados;
    74: Result := pcdValorPrevistoPagamentoINSSNaoNumerico;
    75: Result := pcdValorPrevistoPagamentoINSSNaoInformado;
    76: Result := pcdValorOutrasEntidadesNaoNumerico;
    77: Result := pcdValorAtualizacaoMonetariaNaoNumerico;
    79: Result := pcdPeriodoApuracaoInvalido;
    80: Result := pcdContaCreditoInvalida;
    81: Result := pcdContaNaoPertenceFuncionario;
    82: Result := pcdPagamentoPermitidoApenasPessoasFisicas;
    83: Result := pcdAgenciaContaIncorretos;
    84: Result := pcdContaNaoAtiva;
    85: Result := pcdContaNaoPermiteCreditoSalario;
    86: Result := pcdAgenciaCreditoContaPagamentoInformados;
    90: Result := pcdMesCompetenciaInvalido;
    91: Result := pcdValorOutrasDeducoesInvalido;
    92: Result := pcdValorOutrosAcrescimosInvalido;
    93: Result := pcdCodigoFormaIdentificacaoClienteNaoInformado;
    94: Result := pcdDDDPixNaoInformado;
    95: Result := pcdTelefonePixNaoInformado;
    96: Result := pcdEmailPixNaoInformado;
    97: Result := pcdChaveAleatoriaPixNaoInformada;
    98: Result := pcdCodigoTipoContaPixNaoInformado;
    99: Result := pcdConsultarBancoDetalharErro;
    100: Result := pcdEmailInvalido;
    101: Result := pcdEmailPixCaractereEspecial;
    102: Result := pcdTelefoneInvalido;
    103: Result := pcdDDDInvalido;
    104: Result := pcdEmailTamanhoMaior77;
    200: Result := pcdInsuficienciaFundosDebitoNaoEfetuado;
    201: Result := pcdCreditoDebitoCanceladoPagador;
    202: Result := pcdDebitoAutorizadoAgenciaEfetuado;
    203: Result := pcdControleInvalido;
    204: Result := pcdTipoOperacaoInvalido;
    205: Result := pcdTipoServicoInvalido;
    206: Result := pcdFormaLancamentoInvalida;
    207: Result := pcdTipoNumeroInscricaoInvalido;
    208: Result := pcdCodigoConvenioInvalido;
    209: Result := pcdAgenciaContaCorrenteDVInvalido;
    210: Result := pcdNumeroSequencialRegistroLoteInvalido;
    211: Result := pcdCodigoSegmentoDetalheInvalido;
    212: Result := pcdLancamentoInconsistenteRejeitadoPrevia;
    213: Result := pcdNumeroCompeBancoCreditoInvalido;
    214: Result := pcdNumeroISPBInvalido;
    215: Result := pcdAgenciaMantenedoraContaCorrenteFavorecidoInvalida;
    216: Result := pcdContaCorrenteDVContaPagamentoFavorecidoInvalido;
    217: Result := pcdNomeFavorecidoNaoInformado;
    218: Result := pcdDataLancamentoInvalida;
    219: Result := pcdTipoQuantidadeMoedaInvalido;
    220: Result := pcdValorLancamentoInvalido;
    221: Result := pcdAvisoFavorecidoIdentificacaoInvalida;
    222: Result := pcdTipoNumeroInscricaoFavorecidoInvalido;
    223: Result := pcdLogradouroFavorecidoNaoInformado;
    224: Result := pcdNumeroLocalFavorecidoNaoInformado;
    225: Result := pcdCidadeFavorecidoNaoInformada;
    226: Result := pcdCEPFavorecidoInvalido;
    227: Result := pcdSiglaEstadoFavorecidoInvalida;
    228: Result := pcdNumeroBancoCreditoInvalido;
    229: Result := pcdCodigoNomeAgenciaDepositariaNaoInformado;
    230: Result := pcdSeuNumeroInvalido;
    231: Result := pcdNossoNumeroInvalido;
    232: Result := pcdInclusaoEfetuadaSucesso;
    233: Result := pcdAlteracaoEfetuadaSucesso;
    234: Result := pcdExclusaoEfetuadaSucesso;
    235: Result := pcdAgenciaContaImpedidaLegalmente;
    236: Result := pcdEmpresaNaoPagouSalario;
    237: Result := pcdFalecimentoMutuario;
    238: Result := pcdEmpresaNaoEnviouRemessaMutuario;
    239: Result := pcdEmpresaNaoEnviouRemessaVencimento;
    240: Result := pcdValorParcelaInvalida;
    241: Result := pcdIdentificacaoContratoInvalida;
    242: Result := pcdOperacaoConsignacaoIncluidaSucesso;
    243: Result := pcdOperacaoConsignacaoAlteradaSucesso;
    244: Result := pcdOperacaoConsignacaoExcluidaSucesso;
    245: Result := pcdOperacaoConsignacaoLiquidadaSucesso;
    246: Result := pcdReativacaoEfetuadaSucesso;
    247: Result := pcdSuspensaoEfetuadaSucesso;
    248: Result := pcdCodigoBarrasCodigoBancoInvalido;
    249: Result := pcdCodigoBarrasCodigoMoedaInvalido;
    250: Result := pcdCodigoBarrasDigitoVerificadorGeralInvalido;
    251: Result := pcdCodigoBarrasValorTituloInvalido;
    252: Result := pcdCodigoBarrasCampoLivreInvalido;
    253: Result := pcdValorDocumentoInvalido;
    254: Result := pcdValorAbatimentoInvalido;
    255: Result := pcdValorDescontoInvalido;
    256: Result := pcdValorMoraInvalido;
    257: Result := pcdValorMultaInvalido;
    258: Result := pcdValorIRInvalido;
    259: Result := pcdValorISSInvalido;
    260: Result := pcdValorIOFInvalido;
    261: Result := pcdValorOutrasDeducoesInvalido2;
    262: Result := pcdValorOutrosAcrescimosInvalido2;
    263: Result := pcdValorINSSInvalido;
    264: Result := pcdLoteNaoAceito;
    265: Result := pcdInscricaoEmpresaInvalidaContrato;
    266: Result := pcdConvenioEmpresaInexistenteContrato;
    267: Result := pcdAgenciaContaCorrenteEmpresaInexistenteContrato;
    268: Result := pcdTipoServicoInvalidoContrato;
    269: Result := pcdContaCorrenteEmpresaSaldoInsuficiente;
    270: Result := pcdLoteServicoForaSequencia;
    271: Result := pcdLoteServicoInvalido;
    272: Result := pcdArquivoNaoAceito;
    273: Result := pcdTipoRegistroInvalido;
    274: Result := pcdCodigoRemessaRetornoInvalido;
    275: Result := pcdVersaoLayoutInvalida;
    276: Result := pcdMutuarioNaoIdentificado;
    277: Result := pcdTipoBeneficioNaoPermiteEmprestimo;
    278: Result := pcdBeneficioCessadoSuspenso;
    279: Result := pcdBeneficioPossuiRepresentanteLegal;
    280: Result := pcdBeneficioTipoPA;
    281: Result := pcdQuantidadeContratosExcedida;
    282: Result := pcdBeneficioNaoPertenceBanco;
    283: Result := pcdInicioDescontoUltrapassado;
    284: Result := pcdNumeroParcelaInvalida;
    285: Result := pcdQuantidadeParcelaInvalida;
    286: Result := pcdMargemConsignavelExcedidaPrazo;
    287: Result := pcdEmprestimoJaCadastrado;
    288: Result := pcdEmprestimoInexistente;
    289: Result := pcdEmprestimoJaEncerrado;
    290: Result := pcdArquivoSemTrailer;
    291: Result := pcdMutuarioSemCreditoCompetencia;
    292: Result := pcdNaoDescontadoOutrosMotivos;
    293: Result := pcdRetornoCreditoNaoPago;
    294: Result := pcdCancelamentoEmprestimoRetroativo;
    295: Result := pcdOutrosMotivosGlosa;
    296: Result := pcdMargemConsignavelExcedidaAcimaPrazo;
    297: Result := pcdMutuarioDesligadoEmpregador;
    298: Result := pcdMutuarioAfastadoLicenca;
    299: Result := pcdPrimeiroNomeMutuarioDiferente;
    300: Result := pcdBeneficioSuspensoCessadoAPS;
    301: Result := pcdBeneficioSuspensoDependenciaCalculo;
    302: Result := pcdBeneficioSuspensoCessadoInspetoria;
    303: Result := pcdBeneficioBloqueadoEmprestimoBeneficiario;
    304: Result := pcdBeneficioBloqueadoEmprestimoTBM;
    305: Result := pcdBeneficioFaseConcessaoPA;
    306: Result := pcdBeneficioCessadoObito;
    307: Result := pcdBeneficioCessadoFraude;
    308: Result := pcdBeneficioCessadoOutroBeneficio;
    309: Result := pcdBeneficioCessadoEstatutario;
    310: Result := pcdEmprestimoSuspensoAPS;
    311: Result := pcdEmprestimoCanceladoBanco;
    312: Result := pcdCreditoTransformadoPAB;
    313: Result := pcdTerminoConsignacaoAlterado;
    314: Result := pcdFimEmprestimoPeriodoSuspensao;
    315: Result := pcdEmprestimoSuspensoBanco;
    316: Result := pcdNaoAverbacaoContratoQuantidadeParcelas;
    317: Result := pcdLoteNaoAceitoTotaisDiferenca;
    318: Result := pcdTituloNaoEncontrado;
    319: Result := pcdIdentificadorRegistroOpcionalInvalido;
    320: Result := pcdCodigoPadraoInvalido;
    321: Result := pcdCodigoOcorrenciaInvalido;
    322: Result := pcdComplementoOcorrenciaInvalido;
    323: Result := pcdAlegacaoJaInformada;
    324: Result := pcdAgenciaContaFavorecidoSubstituida;
    325: Result := pcdDivergenciaNomeBeneficiario;
    326: Result := pcdConfirmacaoAntecipacaoValor;
    327: Result := pcdAntecipacaoParcialValor;
    328: Result := pcdBoletoBloqueadoBase;
    329: Result := pcdSistemaContingenciaBoletoValorMaior;
    330: Result := pcdSistemaContingenciaBoletoVencido;
    331: Result := pcdSistemaContingenciaBoletoIndexado;
    332: Result := pcdBeneficiarioDivergente;
    333: Result := pcdLimitePagamentosParciaisExcedido;
    334: Result := pcdBoletoJaLiquidado;
    999: Result := pcdConsultarBancoDetalharErro2;
  end;
end;

function PagamentosBBTransferenciaErroToInteger(const aValue: TACBrPagamenosBBTransferenciaErro): Integer;
begin
  Result := 0;
  case aValue of
    pteAgenciaZerada: Result := 1;
    pteAgenciaNaoNumerica: Result := 2;
    pteDVContaNaoInformado: Result := 3;
    pteCPFNaoNumerico: Result := 4;
    pteCNPJNaoNumerico: Result := 5;
    pteDataNaoInformada: Result := 6;
    pteDataInvalida: Result := 7;
    pteValorNaoNumerico: Result := 8;
    pteValorZerado: Result := 9;
    pteCompensacaoISPBNaoInformados: Result := 10;
    pteCompensacaoISPBInformados: Result := 11;
    pteDOCTEDNaoInformados: Result := 12;
    pteDOCTEDInformados: Result := 13;
    pteNumDepositoJudicialNaoInformado: Result := 14;
    pteDVContaInvalido: Result := 15;
    pteCPFCNPJInformados: Result := 16;
    pteCPFCNPJNaoInformaos: Result := 17;
    pteCPFInvalido: Result := 18;
    pteCNPJInvalido: Result := 19;
    pteAgenciaContaIguais: Result := 20;
    pteNumCompensacaoInvalido: Result := 21;
    pteISPBDiferenteDeZeros: Result := 22;
    pteContaCreditoNaoInformada: Result := 23;
    pteCPFNaoInformado: Result := 24;
    pteCNPJNaoInformado: Result := 25;
    pteContaCreditoInformada: Result := 26;
    pteDVCreditoInformado: Result := 27;
    pteFinalidadeDOCInformada: Result := 28;
    pteFinalidadeTEDInformada: Result := 29;
    pteNumDepositoJudicialInformado: Result := 30;
    pteDocumentoCreditoNaoNumerico: Result := 31;
    pteDocumentoDebitoNaoNumerico: Result := 32;
    pteCPFNaoEncontrado: Result := 33;
    pteCNPJNaoEncontrado: Result := 34;
    pteContaPoupancaNaoPermitida: Result := 35;
    pteCOMPEDeveSer1: Result := 36;
    pteISPBDeveSer0: Result := 37;
    pteCodBarrasNaoNumerio: Result := 38;
    pteCodBarrasIgualZeros: Result := 39;
    pteNumInscricaoNaoNumerico: Result := 40;
    pteInscricaoBeneficiarioNaoNumerico: Result := 41;
    pteInscricaoAvalistaNaoNumerico: Result := 42;
    pteDVCPFPagadorInvalido: Result := 43;
    pteDVCPFBeneficiarioInvalido: Result := 44;
    pteDVCPFAvalistaInvalido: Result := 45;
    pteDVCNPJPagadorInvalido: Result := 46;
    pteDVCNPJBeneficiarioInvalido: Result := 47;
    pteDVCNPJAvalistaInvalido: Result := 48;
    pteDataVencimentoInvalida: Result := 49;
    pteValorNominalNaoNumerico: Result := 50;
    pteValorDescontoNaoNumerico: Result := 51;
    pteValorMoraNaoNumerico: Result := 52;
    pteDataPagamentoMenorAtual: Result := 53;
    pteDocDebitoNaoInformado: Result := 54;
    pteDataVencimentoNaoInformada: Result := 55;
    pteNomeBeneficiarioNaoInformado: Result := 56;
    pteInscricaoBeneficiarioNaoInformada: Result := 57;
    pteContaPagamentoInformada: Result := 58;
    pteContaCreditoPagamentoInformada: Result := 59;
    pteConsultarBancoErro: Result := 99;
    pteInsuficienciaFundos: Result := 200;
    pteCreditoDebitoCancelado: Result := 201;
    pteDebitoAutorizado: Result := 202;
    pteControleInvalido: Result := 203;
    pteTipoOperacaoInvalido: Result := 204;
    pteTipoServicoInvalido: Result := 205;
    pteFormaLancamentoInvalida: Result := 206;
    pteTipoNumeroInscricaoInvalido: Result := 207;
    pteCodigoConvenioInvalido: Result := 208;
    pteAgenciaContaCorrenteDVInvalido: Result := 209;
    pteNumeroSequencialRegistroInvalido: Result := 210;
    pteCodigoSegmentoDetalheInvalido: Result := 211;
    pteLancamentoInconsistente: Result := 212;
    pteNumeroCompeBancoCreditoInvalido: Result := 213;
    pteNumeroISPBInvalido: Result := 214;
    pteAgenciaMantenedoraInvalida: Result := 215;
    pteContaCorrenteDVInvalido: Result := 216;
    pteNomeFavorecidoNaoInformado: Result := 217;
    pteDataLancamentoInvalida: Result := 218;
    pteTipoQuantidadeMoedaInvalida: Result := 219;
    pteValorLancamentoInvalido: Result := 220;
    pteAvisoFavorecidoIdentificacaoInvalida: Result := 221;
    pteTipoNumeroInscricaoFavorecidoInvalido: Result := 222;
    pteLogradouroFavorecidoNaoInformado: Result := 223;
    pteNumeroLocalFavorecidoNaoInformado: Result := 224;
    pteCidadeFavorecidoNaoInformada: Result := 225;
    pteCEPFavorecidoInvalido: Result := 226;
    pteSiglaEstadoFavorecidoInvalida: Result := 227;
    pteNumeroBancoCreditoInvalido: Result := 228;
    pteCodigoNomeAgenciaDepositariaNaoInformado: Result := 229;
    pteSeuNumeroInvalido: Result := 230;
    pteNossoNumeroInvalido: Result := 231;
    pteInclusaoEfetuadaSucesso: Result := 232;
    pteAlteracaoEfetuadaSucesso: Result := 233;
    pteExclusaoEfetuadaSucesso: Result := 234;
    pteAgenciaContaImpedidaLegalmente: Result := 235;
    pteEmpresaNaoPagouSalario: Result := 236;
    pteFalecimentoMutuario: Result := 237;
    pteEmpresaNaoEnviouRemessaMutuario: Result := 238;
    pteEmpresaNaoEnviouRemessaVencimento: Result := 239;
    pteValorParcelaInvalida: Result := 240;
    pteIdentificacaoContratoInvalida: Result := 241;
    pteOperacaoConsignacaoIncluidaSucesso: Result := 242;
    pteOperacaoConsignacaoAlteradaSucesso: Result := 243;
    pteOperacaoConsignacaoExcluidaSucesso: Result := 244;
    pteOperacaoConsignacaoLiquidadaSucesso: Result := 245;
    pteReativacaoEfetuadaSucesso: Result := 246;
    pteSuspensaoEfetuadaSucesso: Result := 247;
    pteCodigoBarrasBancoInvalido: Result := 248;
    pteCodigoBarrasMoedaInvalido: Result := 249;
    pteCodigoBarrasDigitoVerificadorInvalido: Result := 250;
    pteCodigoBarrasValorTituloInvalido: Result := 251;
    pteCodigoBarrasCampoLivreInvalido: Result := 252;
    pteValorDocumentoInvalido: Result := 253;
    pteValorAbatimentoInvalido: Result := 254;
    pteValorDescontoInvalido: Result := 255;
    pteValorMoraInvalido: Result := 256;
    pteValorMultaInvalido: Result := 257;
    pteValorIRInvalido: Result := 258;
    pteValorISSInvalido: Result := 259;
    pteValorIOFInvalido: Result := 260;
    pteValorOutrasDeducoesInvalido: Result := 261;
    pteValorOutrosAcrescimosInvalido: Result := 262;
    pteValorINSSInvalido: Result := 263;
    pteLoteNaoAceito: Result := 264;
    pteInscricaoEmpresaInvalidaContrato: Result := 265;
    pteConvenioEmpresaInexistenteContrato: Result := 266;
    pteAgenciaContaCorrenteEmpresaInexistenteContrato: Result := 267;
    pteTipoServicoInvalidoContrato: Result := 268;
    pteContaCorrenteSaldoInsuficiente: Result := 269;
    pteLoteServicoForaSequencia: Result := 270;
    pteLoteServicoInvalido: Result := 271;
    pteArquivoNaoAceito: Result := 272;
    pteTipoRegistroInvalido: Result := 273;
    pteCodigoRemessaRetornoInvalido: Result := 274;
    pteVersaoLayoutInvalida: Result := 275;
    pteMutuarioNaoIdentificado: Result := 276;
    pteTipoBeneficioNaoPermiteEmprestimo: Result := 277;
    pteBeneficioCessadoSuspenso: Result := 278;
    pteBeneficioPossuiRepresentanteLegal: Result := 279;
    pteBeneficioTipoPA: Result := 280;
    pteQuantidadeContratosExcedida: Result := 281;
    pteBeneficioNaoPertenceBanco: Result := 282;
    pteInicioDescontoUltrapassado: Result := 283;
    pteNumeroParcelaInvalida: Result := 284;
    pteQuantidadeParcelaInvalida: Result := 285;
    pteMargemConsignavelExcedidaPrazo: Result := 286;
    pteEmprestimoJaCadastrado: Result := 287;
    pteEmprestimoInexistente: Result := 288;
    pteEmprestimoJaEncerrado: Result := 289;
    pteArquivoSemTrailer: Result := 290;
    pteMutuarioSemCreditoCompetencia: Result := 291;
    pteNaoDescontadoOutrosMotivos: Result := 292;
    pteRetornoCreditoNaoPago: Result := 293;
    pteCancelamentoEmprestimoRetroativo: Result := 294;
    pteOutrosMotivosGlosa: Result := 295;
    pteMargemConsignavelExcedidaAcimaPrazo: Result := 296;
    pteMutuarioDesligadoEmpregador: Result := 297;
    pteMutuarioAfastadoLicenca: Result := 298;
    ptePrimeiroNomeMutuarioDiferente: Result := 299;
    pteBeneficioSuspensoCessadoAPS: Result := 300;
    pteBeneficioSuspensoDependenciaCalculo: Result := 301;
    pteBeneficioSuspensoCessadoInspetoria: Result := 302;
    pteBeneficioBloqueadoEmprestimoBeneficiario: Result := 303;
    pteBeneficioBloqueadoEmprestimoTBM: Result := 304;
    pteBeneficioFaseConcessaoPA: Result := 305;
    pteBeneficioCessadoObito: Result := 306;
    pteBeneficioCessadoFraude: Result := 307;
    pteBeneficioCessadoOutroBeneficio: Result := 308;
    pteBeneficioCessadoEstatutario: Result := 309;
    pteEmprestimoSuspensoAPS: Result := 310;
    pteEmprestimoCanceladoBanco: Result := 311;
    pteCreditoTransformadoPAB: Result := 312;
    pteTerminoConsignacaoAlterado: Result := 313;
    pteFimEmprestimoPeriodoSuspensao: Result := 314;
    pteEmprestimoSuspensoBanco: Result := 315;
    pteNaoAverbacaoContratoQuantidadeParcelas: Result := 316;
    pteLoteNaoAceitoTotaisDiferenca: Result := 317;
    pteTituloNaoEncontrado: Result := 318;
    pteIdentificadorRegistroOpcionalInvalido: Result := 319;
    pteCodigoPadraoInvalido: Result := 320;
    pteCodigoOcorrenciaInvalido: Result := 321;
    pteComplementoOcorrenciaInvalido: Result := 322;
    pteAlegacaoJaInformada: Result := 323;
    pteAgenciaContaFavorecidoSubstituida: Result := 324;
    pteDivergenciaNomeBeneficiario: Result := 325;
    pteConfirmacaoAntecipacaoValor: Result := 326;
    pteAntecipacaoParcialValor: Result := 327;
    pteBoletoBloqueadoBase: Result := 328;
    pteSistemaContingenciaBoletoValorMaior: Result := 329;
    pteSistemaContingenciaBoletoVencido: Result := 330;
    pteSistemaContingenciaBoletoIndexado: Result := 331;
    pteBeneficiarioDivergente: Result := 332;
    pteLimitePagamentosParciaisExcedido: Result := 333;
    pteBoletoJaLiquidado: Result := 334;
    pteConsultarBancoDetalharErro: Result := 999;
  end;
end;

function IntegerToPagamentosBBTransferenciaErro(const aValue: Integer): TACBrPagamenosBBTransferenciaErro;
begin
  Result := pteNenhum;
  case aValue of
    1: Result := pteAgenciaZerada;
    2: Result := pteAgenciaNaoNumerica;
    3: Result := pteDVContaNaoInformado;
    4: Result := pteCPFNaoNumerico;
    5: Result := pteCNPJNaoNumerico;
    6: Result := pteDataNaoInformada;
    7: Result := pteDataInvalida;
    8: Result := pteValorNaoNumerico;
    9: Result := pteValorZerado;
    10: Result := pteCompensacaoISPBNaoInformados;
    11: Result := pteCompensacaoISPBInformados;
    12: Result := pteDOCTEDNaoInformados;
    13: Result := pteDOCTEDInformados;
    14: Result := pteNumDepositoJudicialNaoInformado;
    15: Result := pteDVContaInvalido;
    16: Result := pteCPFCNPJInformados;
    17: Result := pteCPFCNPJNaoInformaos;
    18: Result := pteCPFInvalido;
    19: Result := pteCNPJInvalido;
    20: Result := pteAgenciaContaIguais;
    21: Result := pteNumCompensacaoInvalido;
    22: Result := pteISPBDiferenteDeZeros;
    23: Result := pteContaCreditoNaoInformada;
    24: Result := pteCPFNaoInformado;
    25: Result := pteCNPJNaoInformado;
    26: Result := pteContaCreditoInformada;
    27: Result := pteDVCreditoInformado;
    28: Result := pteFinalidadeDOCInformada;
    29: Result := pteFinalidadeTEDInformada;
    30: Result := pteNumDepositoJudicialInformado;
    31: Result := pteDocumentoCreditoNaoNumerico;
    32: Result := pteDocumentoDebitoNaoNumerico;
    33: Result := pteCPFNaoEncontrado;
    34: Result := pteCNPJNaoEncontrado;
    35: Result := pteContaPoupancaNaoPermitida;
    36: Result := pteCOMPEDeveSer1;
    37: Result := pteISPBDeveSer0;
    38: Result := pteCodBarrasNaoNumerio;
    39: Result := pteCodBarrasIgualZeros;
    40: Result := pteNumInscricaoNaoNumerico;
    41: Result := pteInscricaoBeneficiarioNaoNumerico;
    42: Result := pteInscricaoAvalistaNaoNumerico;
    43: Result := pteDVCPFPagadorInvalido;
    44: Result := pteDVCPFBeneficiarioInvalido;
    45: Result := pteDVCPFAvalistaInvalido;
    46: Result := pteDVCNPJPagadorInvalido;
    47: Result := pteDVCNPJBeneficiarioInvalido;
    48: Result := pteDVCNPJAvalistaInvalido;
    49: Result := pteDataVencimentoInvalida;
    50: Result := pteValorNominalNaoNumerico;
    51: Result := pteValorDescontoNaoNumerico;
    52: Result := pteValorMoraNaoNumerico;
    53: Result := pteDataPagamentoMenorAtual;
    54: Result := pteDocDebitoNaoInformado;
    55: Result := pteDataVencimentoNaoInformada;
    56: Result := pteNomeBeneficiarioNaoInformado;
    57: Result := pteInscricaoBeneficiarioNaoInformada;
    58: Result := pteContaPagamentoInformada;
    59: Result := pteContaCreditoPagamentoInformada;
    99: Result := pteConsultarBancoErro;
    200: Result := pteInsuficienciaFundos;
    201: Result := pteCreditoDebitoCancelado;
    202: Result := pteDebitoAutorizado;
    203: Result := pteControleInvalido;
    204: Result := pteTipoOperacaoInvalido;
    205: Result := pteTipoServicoInvalido;
    206: Result := pteFormaLancamentoInvalida;
    207: Result := pteTipoNumeroInscricaoInvalido;
    208: Result := pteCodigoConvenioInvalido;
    209: Result := pteAgenciaContaCorrenteDVInvalido;
    210: Result := pteNumeroSequencialRegistroInvalido;
    211: Result := pteCodigoSegmentoDetalheInvalido;
    212: Result := pteLancamentoInconsistente;
    213: Result := pteNumeroCompeBancoCreditoInvalido;
    214: Result := pteNumeroISPBInvalido;
    215: Result := pteAgenciaMantenedoraInvalida;
    216: Result := pteContaCorrenteDVInvalido;
    217: Result := pteNomeFavorecidoNaoInformado;
    218: Result := pteDataLancamentoInvalida;
    219: Result := pteTipoQuantidadeMoedaInvalida;
    220: Result := pteValorLancamentoInvalido;
    221: Result := pteAvisoFavorecidoIdentificacaoInvalida;
    222: Result := pteTipoNumeroInscricaoFavorecidoInvalido;
    223: Result := pteLogradouroFavorecidoNaoInformado;
    224: Result := pteNumeroLocalFavorecidoNaoInformado;
    225: Result := pteCidadeFavorecidoNaoInformada;
    226: Result := pteCEPFavorecidoInvalido;
    227: Result := pteSiglaEstadoFavorecidoInvalida;
    228: Result := pteNumeroBancoCreditoInvalido;
    229: Result := pteCodigoNomeAgenciaDepositariaNaoInformado;
    230: Result := pteSeuNumeroInvalido;
    231: Result := pteNossoNumeroInvalido;
    232: Result := pteInclusaoEfetuadaSucesso;
    233: Result := pteAlteracaoEfetuadaSucesso;
    234: Result := pteExclusaoEfetuadaSucesso;
    235: Result := pteAgenciaContaImpedidaLegalmente;
    236: Result := pteEmpresaNaoPagouSalario;
    237: Result := pteFalecimentoMutuario;
    238: Result := pteEmpresaNaoEnviouRemessaMutuario;
    239: Result := pteEmpresaNaoEnviouRemessaVencimento;
    240: Result := pteValorParcelaInvalida;
    241: Result := pteIdentificacaoContratoInvalida;
    242: Result := pteOperacaoConsignacaoIncluidaSucesso;
    243: Result := pteOperacaoConsignacaoAlteradaSucesso;
    244: Result := pteOperacaoConsignacaoExcluidaSucesso;
    245: Result := pteOperacaoConsignacaoLiquidadaSucesso;
    246: Result := pteReativacaoEfetuadaSucesso;
    247: Result := pteSuspensaoEfetuadaSucesso;
    248: Result := pteCodigoBarrasBancoInvalido;
    249: Result := pteCodigoBarrasMoedaInvalido;
    250: Result := pteCodigoBarrasDigitoVerificadorInvalido;
    251: Result := pteCodigoBarrasValorTituloInvalido;
    252: Result := pteCodigoBarrasCampoLivreInvalido;
    253: Result := pteValorDocumentoInvalido;
    254: Result := pteValorAbatimentoInvalido;
    255: Result := pteValorDescontoInvalido;
    256: Result := pteValorMoraInvalido;
    257: Result := pteValorMultaInvalido;
    258: Result := pteValorIRInvalido;
    259: Result := pteValorISSInvalido;
    260: Result := pteValorIOFInvalido;
    261: Result := pteValorOutrasDeducoesInvalido;
    262: Result := pteValorOutrosAcrescimosInvalido;
    263: Result := pteValorINSSInvalido;
    264: Result := pteLoteNaoAceito;
    265: Result := pteInscricaoEmpresaInvalidaContrato;
    266: Result := pteConvenioEmpresaInexistenteContrato;
    267: Result := pteAgenciaContaCorrenteEmpresaInexistenteContrato;
    268: Result := pteTipoServicoInvalidoContrato;
    269: Result := pteContaCorrenteSaldoInsuficiente;
    270: Result := pteLoteServicoForaSequencia;
    271: Result := pteLoteServicoInvalido;
    272: Result := pteArquivoNaoAceito;
    273: Result := pteTipoRegistroInvalido;
    274: Result := pteCodigoRemessaRetornoInvalido;
    275: Result := pteVersaoLayoutInvalida;
    276: Result := pteMutuarioNaoIdentificado;
    277: Result := pteTipoBeneficioNaoPermiteEmprestimo;
    278: Result := pteBeneficioCessadoSuspenso;
    279: Result := pteBeneficioPossuiRepresentanteLegal;
    280: Result := pteBeneficioTipoPA;
    281: Result := pteQuantidadeContratosExcedida;
    282: Result := pteBeneficioNaoPertenceBanco;
    283: Result := pteInicioDescontoUltrapassado;
    284: Result := pteNumeroParcelaInvalida;
    285: Result := pteQuantidadeParcelaInvalida;
    286: Result := pteMargemConsignavelExcedidaPrazo;
    287: Result := pteEmprestimoJaCadastrado;
    288: Result := pteEmprestimoInexistente;
    289: Result := pteEmprestimoJaEncerrado;
    290: Result := pteArquivoSemTrailer;
    291: Result := pteMutuarioSemCreditoCompetencia;
    292: Result := pteNaoDescontadoOutrosMotivos;
    293: Result := pteRetornoCreditoNaoPago;
    294: Result := pteCancelamentoEmprestimoRetroativo;
    295: Result := pteOutrosMotivosGlosa;
    296: Result := pteMargemConsignavelExcedidaAcimaPrazo;
    297: Result := pteMutuarioDesligadoEmpregador;
    298: Result := pteMutuarioAfastadoLicenca;
    299: Result := ptePrimeiroNomeMutuarioDiferente;
    300: Result := pteBeneficioSuspensoCessadoAPS;
    301: Result := pteBeneficioSuspensoDependenciaCalculo;
    302: Result := pteBeneficioSuspensoCessadoInspetoria;
    303: Result := pteBeneficioBloqueadoEmprestimoBeneficiario;
    304: Result := pteBeneficioBloqueadoEmprestimoTBM;
    305: Result := pteBeneficioFaseConcessaoPA;
    306: Result := pteBeneficioCessadoObito;
    307: Result := pteBeneficioCessadoFraude;
    308: Result := pteBeneficioCessadoOutroBeneficio;
    309: Result := pteBeneficioCessadoEstatutario;
    310: Result := pteEmprestimoSuspensoAPS;
    311: Result := pteEmprestimoCanceladoBanco;
    312: Result := pteCreditoTransformadoPAB;
    313: Result := pteTerminoConsignacaoAlterado;
    314: Result := pteFimEmprestimoPeriodoSuspensao;
    315: Result := pteEmprestimoSuspensoBanco;
    316: Result := pteNaoAverbacaoContratoQuantidadeParcelas;
    317: Result := pteLoteNaoAceitoTotaisDiferenca;
    318: Result := pteTituloNaoEncontrado;
    319: Result := pteIdentificadorRegistroOpcionalInvalido;
    320: Result := pteCodigoPadraoInvalido;
    321: Result := pteCodigoOcorrenciaInvalido;
    322: Result := pteComplementoOcorrenciaInvalido;
    323: Result := pteAlegacaoJaInformada;
    324: Result := pteAgenciaContaFavorecidoSubstituida;
    325: Result := pteDivergenciaNomeBeneficiario;
    326: Result := pteConfirmacaoAntecipacaoValor;
    327: Result := pteAntecipacaoParcialValor;
    328: Result := pteBoletoBloqueadoBase;
    329: Result := pteSistemaContingenciaBoletoValorMaior;
    330: Result := pteSistemaContingenciaBoletoVencido;
    331: Result := pteSistemaContingenciaBoletoIndexado;
    332: Result := pteBeneficiarioDivergente;
    333: Result := pteLimitePagamentosParciaisExcedido;
    334: Result := pteBoletoJaLiquidado;
    999: Result := pteConsultarBancoDetalharErro;
  end;
end;

function PagamentosBBEstadoToInteger(const aValue: TACBrPagamentosBBEstado): Integer;
begin
  Result := 0;
  case aValue of
    pgeAgendado: Result := 1;
    pgeCancelado: Result := 2;
    pgeConsistente: Result := 3;
    pgeDevolvido: Result := 4;
    pgeInconsistente: Result := 5;
    pgePago: Result := 6;
    pgePendente: Result := 7;
    pgeRejeitado: Result := 8;
    pgeVencido: Result := 9;
  end;
end;

function IntegerToPagamentosBBEstado(const aValue: Integer): TACBrPagamentosBBEstado;
begin 
  Result := pgeNenhum;
  case aValue of
    1: Result := pgeAgendado;
    2: Result := pgeCancelado;
    3: Result := pgeConsistente;
    4: Result := pgeDevolvido;
    5: Result := pgeInconsistente;
    6: Result := pgePago;
    7: Result := pgePendente;
    8: Result := pgeRejeitado;
    9: Result := pgeVencido;
  end;
end;

function PagamentosBBLoteEstadoToInteger(const aValue: TACBrPagamentosBBLoteEstado): Integer;
begin 
  Result := 0;
  case aValue of
    pleConsistente: Result := 1;
    pleInconsistente: Result := 2;
    plePendente: Result := 3;
    pleAgendado: Result := 4;
    plePago: Result := 5;
    pleRejeitado: Result := 6;
    pleDevolvido: Result := 7;
    pleCancelado: Result := 8;
    pleDebitado: Result := 9;
    pleBloqueado: Result := 10;
    pleAguardandoDebito: Result := 11;
  end;
end;

function IntegerToPagamentosBBLoteEstado(const aValue: Integer): TACBrPagamentosBBLoteEstado;
begin
  Result := pleNenhum;
  case aValue of
    1: Result := pleConsistente;
    2: Result := pleInconsistente;
    3: Result := plePendente;
    4: Result := pleAgendado;
    5: Result := plePago;
    6: Result := pleRejeitado;
    7: Result := pleDevolvido;
    8: Result := pleCancelado;
    9: Result := pleDebitado;
    10: Result := pleBloqueado;
    11: Result := pleAguardandoDebito;
  end;
end;

{ TACBrPagamentosBBBeneficiariosTransferencias }

function TACBrPagamentosBBBeneficiariosTransferencias.GetItem(aIndex: Integer): TACBrPagamentosBBBeneficiariosTransferencia;
begin
  Result := TACBrPagamentosBBBeneficiariosTransferencia(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBBeneficiariosTransferencias.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBBeneficiariosTransferencia);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBBeneficiariosTransferencias.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBBeneficiariosTransferencias.Add(aItem: TACBrPagamentosBBBeneficiariosTransferencia): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBBeneficiariosTransferencias.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBBeneficiariosTransferencia);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBBeneficiariosTransferencias.New: TACBrPagamentosBBBeneficiariosTransferencia;
begin
  Result := TACBrPagamentosBBBeneficiariosTransferencia.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBSolicitacaoLotePagamentos }

function TACBrPagamentosBBSolicitacaoLotePagamentos.GetItem(aIndex: Integer): TACBrPagamentosBBSolicitacaoLotePagamento;
begin
  Result := TACBrPagamentosBBSolicitacaoLotePagamento(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentos.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBSolicitacaoLotePagamento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBSolicitacaoLotePagamentos.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBSolicitacaoLotePagamentos.Add(aItem: TACBrPagamentosBBSolicitacaoLotePagamento): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentos.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBSolicitacaoLotePagamento);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBSolicitacaoLotePagamentos.New: TACBrPagamentosBBSolicitacaoLotePagamento;
begin
  Result := TACBrPagamentosBBSolicitacaoLotePagamento.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferenciaPagamentoEspecificoLista }

function TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPagamentoEspecifico;
begin
  Result := TACBrPagamentosBBTransferenciaPagamentoEspecifico(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPagamentoEspecifico);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.Add(aItem: TACBrPagamentosBBTransferenciaPagamentoEspecifico): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.Insert(
  aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaPagamentoEspecifico);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.New: TACBrPagamentosBBTransferenciaPagamentoEspecifico;
begin
  Result := TACBrPagamentosBBTransferenciaPagamentoEspecifico.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferenciasRequisicaoResposta }

function TACBrPagamentosBBTransferenciasRequisicaoResposta.GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaRequisicaoResposta;
begin
  Result := TACBrPagamentosBBTransferenciaRequisicaoResposta(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferenciasRequisicaoResposta.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaRequisicaoResposta);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferenciasRequisicaoResposta.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferenciasRequisicaoResposta.Add(aItem: TACBrPagamentosBBTransferenciaRequisicaoResposta): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferenciasRequisicaoResposta.Insert(
  aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaRequisicaoResposta);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferenciasRequisicaoResposta.New: TACBrPagamentosBBTransferenciaRequisicaoResposta;
begin
  Result := TACBrPagamentosBBTransferenciaRequisicaoResposta.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferenciasRequisicao }

function TACBrPagamentosBBTransferenciasRequisicao.GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaRequisicao;
begin
  Result := TACBrPagamentosBBTransferenciaRequisicao(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferenciasRequisicao.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaRequisicao);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferenciasRequisicao.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferenciasRequisicao.Add(aItem: TACBrPagamentosBBTransferenciaRequisicao): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferenciasRequisicao.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBTransferenciaRequisicao);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferenciasRequisicao.New: TACBrPagamentosBBTransferenciaRequisicao;
begin
  Result := TACBrPagamentosBBTransferenciaRequisicao.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferencias }

function TACBrPagamentosBBTransferencias.GetItem(aIndex: Integer): TACBrPagamentosBBTransferencia;
begin
  Result := TACBrPagamentosBBTransferencia(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferencias.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferencia);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferencias.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferencias.Add(aItem: TACBrPagamentosBBTransferencia): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferencias.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBTransferencia);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferencias.New: TACBrPagamentosBBTransferencia;
begin
  Result := TACBrPagamentosBBTransferencia.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBMovimentoLista }

function TACBrPagamentosBBMovimentoLista.GetItem(aIndex: Integer): TACBrPagamentosBBMovimento;
begin
  Result := TACBrPagamentosBBMovimento(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBMovimentoLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBMovimento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBMovimentoLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBMovimentoLista.Add(aItem: TACBrPagamentosBBMovimento): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBMovimentoLista.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBMovimento);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBMovimentoLista.New: TACBrPagamentosBBMovimento;
begin
  Result := TACBrPagamentosBBMovimento.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBLotePagamentos }

function TACBrPagamentosBBLotePagamentos.GetItem(aIndex: Integer): TACBrPagamentosBBLotePagamento;
begin
  Result := TACBrPagamentosBBLotePagamento(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBLotePagamentos.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLotePagamento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBLotePagamentos.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBLotePagamentos.Add(aItem: TACBrPagamentosBBLotePagamento): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBLotePagamentos.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBLotePagamento);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBLotePagamentos.New: TACBrPagamentosBBLotePagamento;
begin
  Result := TACBrPagamentosBBLotePagamento.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBPagamentoCancelarLista }

function TACBrPagamentosBBPagamentoCancelarLista.GetItem(aIndex: Integer): TACBrPagamentosBBPagamentoCancelar;
begin
  Result := TACBrPagamentosBBPagamentoCancelar(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBPagamentoCancelarLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamentoCancelar);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBPagamentoCancelarLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBPagamentoCancelarLista.Add(aItem: TACBrPagamentosBBPagamentoCancelar): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBPagamentoCancelarLista.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBPagamentoCancelar);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBPagamentoCancelarLista.New: TACBrPagamentosBBPagamentoCancelar;
begin
  Result := TACBrPagamentosBBPagamentoCancelar.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBPagamentos }

function TACBrPagamentosBBPagamentos.GetItem(aIndex: Integer): TACBrPagamentosBBPagamento;
begin
  Result := TACBrPagamentosBBPagamento(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBPagamentos.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBPagamentos.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBPagamentos.Add(aItem: TACBrPagamentosBBPagamento): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBPagamentos.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBPagamento);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBPagamentos.New: TACBrPagamentosBBPagamento;
begin
  Result := TACBrPagamentosBBPagamento.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBOcorrenciaLista }

function TACBrPagamentosBBOcorrenciaLista.GetItem(aIndex: Integer): TACBrPagamentosBBOcorrenciaBase;
begin
  Result := TACBrPagamentosBBOcorrenciaBase(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBOcorrenciaLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBOcorrenciaBase);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBOcorrenciaLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBOcorrenciaLista.Add(aItem: TACBrPagamentosBBOcorrenciaBase): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBOcorrenciaLista.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBOcorrenciaBase);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBOcorrenciaLista.New: TACBrPagamentosBBOcorrenciaBase;
begin
  Result := TACBrPagamentosBBOcorrenciaBase.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBDevolucaoLista }

function TACBrPagamentosBBDevolucaoLista.GetItem(aIndex: Integer): TACBrPagamentosBBDevolucao;
begin
  Result := TACBrPagamentosBBDevolucao(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBDevolucaoLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBDevolucao);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBDevolucaoLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBDevolucaoLista.Add(aItem: TACBrPagamentosBBDevolucao): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBDevolucaoLista.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBDevolucao);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBDevolucaoLista.New: TACBrPagamentosBBDevolucao;
begin
  Result := TACBrPagamentosBBDevolucao.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBDevolucaoListaBase }

function TACBrPagamentosBBDevolucaoListaBase.GetItem(aIndex: Integer): TACBrPagamentosBBDevolucaoBase;
begin
  Result := TACBrPagamentosBBDevolucaoBase(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBDevolucaoListaBase.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBDevolucaoBase);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBDevolucaoListaBase.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBDevolucaoListaBase.Add(aItem: TACBrPagamentosBBDevolucaoBase): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBDevolucaoListaBase.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBDevolucaoBase);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBDevolucaoListaBase.New: TACBrPagamentosBBDevolucaoBase;
begin
  Result := TACBrPagamentosBBDevolucaoBase.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBErroOAuthAttributes }

function TACBrPagamentosBBErroOAuthAttributes.GetItem(aIndex: Integer): TACBrPagamentosBBErroOAuthAttribute;
begin
  Result := TACBrPagamentosBBErroOAuthAttribute(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBErroOAuthAttributes.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBErroOAuthAttribute);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBErroOAuthAttributes.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBErroOAuthAttributes.Add(aItem: TACBrPagamentosBBErroOAuthAttribute): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBErroOAuthAttributes.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBErroOAuthAttribute);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBErroOAuthAttributes.New: TACBrPagamentosBBErroOAuthAttribute;
begin
  Result := TACBrPagamentosBBErroOAuthAttribute.Create;
  Self.Add(Result);
end;

{ TACBrPagamenosBBTransferenciaErros }

function TACBrPagamenosBBTransferenciaErros.GetItem(aIndex: Integer): TACBrPagamenosBBTransferenciaErroObject;
begin
  Result := TACBrPagamenosBBTransferenciaErroObject(inherited Items[aIndex]);
end;

procedure TACBrPagamenosBBTransferenciaErros.SetItem(aIndex: Integer; aValue: TACBrPagamenosBBTransferenciaErroObject);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamenosBBTransferenciaErros.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamenosBBTransferenciaErros.Add(
  aItem: TACBrPagamenosBBTransferenciaErroObject): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamenosBBTransferenciaErros.Insert(aIndex: Integer;
  aItem: TACBrPagamenosBBTransferenciaErroObject);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamenosBBTransferenciaErros.New: TACBrPagamenosBBTransferenciaErroObject;
begin
  Result := TACBrPagamenosBBTransferenciaErroObject.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBLancamentoErros }

function TACBrPagamentosBBLancamentoErros.GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoErroObject;
begin
  Result := TACBrPagamentosBBLancamentoErroObject(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBLancamentoErros.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoErroObject);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBLancamentoErros.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBLancamentoErros.Add(aItem: TACBrPagamentosBBLancamentoErroObject): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBLancamentoErros.Insert(aIndex: Integer; aItem: TACBrPagamentosBBLancamentoErroObject);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBLancamentoErros.New: TACBrPagamentosBBLancamentoErroObject;
begin
  Result := TACBrPagamentosBBLancamentoErroObject.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBErros }

function TACBrPagamentosBBErros.GetItem(aIndex: Integer): TACBrPagamentosBBErro;
begin
  Result := TACBrPagamentosBBErro(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBErros.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBErro);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBErros.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBErros.Add(aItem: TACBrPagamentosBBErro): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBErros.Insert(aIndex: Integer; aItem: TACBrPagamentosBBErro);
begin  
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBErros.New: TACBrPagamentosBBErro;
begin
  Result := TACBrPagamentosBBErro.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBBeneficiariosTransferenciaResposta }

function TACBrPagamentosBBBeneficiariosTransferenciaResposta.Gettransferencias: TACBrPagamentosBBBeneficiariosTransferencias;
begin
  if (not Assigned(ftransferencias)) then
    ftransferencias := TACBrPagamentosBBBeneficiariosTransferencias.Create('transferencias');
  Result := ftransferencias;
end;

procedure TACBrPagamentosBBBeneficiariosTransferenciaResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBBeneficiariosTransferenciaResposta) then
    Assign(TACBrPagamentosBBBeneficiariosTransferenciaResposta(ASource));
end;

procedure TACBrPagamentosBBBeneficiariosTransferenciaResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('indice', findice)
    .AddPair('quantidadeTotalTransferencias', fquantidadeTotalTransferencias);

  if Assigned(ftransferencias) then
    ftransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBBeneficiariosTransferenciaResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('indice', findice)
    .Value('quantidadeTotalTransferencias', fquantidadeTotalTransferencias);

  if Assigned(ftransferencias) then
    ftransferencias.ReadFromJSon(aJSon);
end;

procedure TACBrPagamentosBBBeneficiariosTransferenciaResposta.Clear;
begin
  findice := 0;
  fquantidadeTotalTransferencias := 0;

  if Assigned(ftransferencias) then
    ftransferencias.Clear;
end;

function TACBrPagamentosBBBeneficiariosTransferenciaResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(findice) and
    EstaZerado(fquantidadeTotalTransferencias);

  if Assigned(ftransferencias) then
    Result := Result and ftransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBBeneficiariosTransferenciaResposta.Assign(
  aSource: TACBrPagamentosBBBeneficiariosTransferenciaResposta);
begin
  findice := ASource.indice;
  fquantidadeTotalTransferencias := ASource.quantidadeTotalTransferencias;
  transferencias.Assign(ASource.transferencias);
end;

{ TACBrPagamentosBBBeneficiariosTransferencia }

procedure TACBrPagamentosBBBeneficiariosTransferencia.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBBeneficiariosTransferencia) then
    Assign(TACBrPagamentosBBBeneficiariosTransferencia(ASource));
end;

procedure TACBrPagamentosBBBeneficiariosTransferencia.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificador', fidentificador)
    .AddPair('estadoPagamento', festadoPagamento)
    .AddPair('tipoPagamento', ftipoPagamento)
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('dataTransferencia', DateToStr(fdataTransferencia))
    .AddPair('valorTransferencia', fvalorTransferencia)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCorrenteCredito', fcontaCorrenteCredito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('contaPagamentoCredito', fcontaPagamentoCredito)
    .AddPair('tipoBeneficiario', PagamentosBBTipoBeneficiarioToInteger(ftipoBeneficiario))
    .AddPair('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento)
    .AddPair('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .AddPair('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .AddPair('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('numeroArquivoPagamento', fnumeroArquivoPagamento)
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .AddPair('inicioCartao', finicioCartao)
    .AddPair('fimCartao', ffimCartao)
    .AddPair('descricaoTransferencia', fdescricaoTransferencia)
    .AddPair('formaTransmissao', fformaTransmissao);
end;

procedure TACBrPagamentosBBBeneficiariosTransferencia.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('identificador', fidentificador)
    .Value('estadoPagamento', festadoPagamento)
    .Value('tipoPagamento', ftipoPagamento)
    .Value('tipoCredito',  i1)
    .Value('dataTransferencia', fdataTransferencia)
    .Value('valorTransferencia', fvalorTransferencia)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCorrenteCredito', fcontaCorrenteCredito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('contaPagamentoCredito', fcontaPagamentoCredito)
    .Value('tipoBeneficiario', i2)
    .Value('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento)
    .Value('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .Value('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .Value('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('numeroArquivoPagamento', fnumeroArquivoPagamento)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .Value('inicioCartao', finicioCartao)
    .Value('fimCartao', ffimCartao)
    .Value('descricaoTransferencia', fdescricaoTransferencia)
    .Value('formaTransmissao', fformaTransmissao);

  if NaoEstaZerado(i1) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i1);

  if NaoEstaZerado(i2) then
    ftipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i2);
end;

procedure TACBrPagamentosBBBeneficiariosTransferencia.Clear;
begin
  fidentificador := 0;
  festadoPagamento := EmptyStr;
  ftipoPagamento := 0;
  ftipoCredito := pcrNenhum;
  fdataTransferencia := 0;
  fvalorTransferencia := 0;
  fdocumentoDebito := 0;
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  fagenciaCredito := 0;
  fcontaCorrenteCredito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fcontaPagamentoCredito := EmptyStr;
  ftipoBeneficiario := ptbNenhum;
  fcpfCnpjBeneficiario := 0;
  fnomeBeneficiario := EmptyStr;
  fcodigoAutenticacaoPagamento := EmptyStr;
  fcodigoFinalidadeDOC := EmptyStr;
  fcodigoFinalidadeTED := EmptyStr;
  fnumeroDepositoJudicial := EmptyStr;
  fnumeroRequisicao := 0;
  fnumeroArquivoPagamento := 0;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := EmptyStr;
  finicioCartao := 0;
  ffimCartao := 0;
  fdescricaoTransferencia := EmptyStr;
  fformaTransmissao := 0;
end;

function TACBrPagamentosBBBeneficiariosTransferencia.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificador) and
    EstaVazio(festadoPagamento) and
    EstaZerado(ftipoPagamento) and
    EstaZerado(Ord(ftipoCredito)) and
    EstaZerado(fdataTransferencia) and
    EstaZerado(fvalorTransferencia) and
    EstaZerado(fdocumentoDebito) and
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCorrenteCredito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaVazio(fcontaPagamentoCredito) and
    EstaZerado(Ord(ftipoBeneficiario)) and
    EstaZerado(fcpfCnpjBeneficiario) and
    EstaVazio(fnomeBeneficiario) and
    EstaVazio(fcodigoAutenticacaoPagamento) and
    EstaVazio(fcodigoFinalidadeDOC) and
    EstaVazio(fcodigoFinalidadeTED) and
    EstaVazio(fnumeroDepositoJudicial) and
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fnumeroArquivoPagamento) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito) and
    EstaZerado(finicioCartao) and
    EstaZerado(ffimCartao) and
    EstaVazio(fdescricaoTransferencia) and
    EstaZerado(fformaTransmissao);
end;

procedure TACBrPagamentosBBBeneficiariosTransferencia.Assign(aSource: TACBrPagamentosBBBeneficiariosTransferencia);
begin
  fidentificador := ASource.identificador;
  festadoPagamento := ASource.estadoPagamento;
  ftipoPagamento := ASource.tipoPagamento;
  ftipoCredito := ASource.tipoCredito;
  fdataTransferencia := ASource.dataTransferencia;
  fvalorTransferencia := ASource.valorTransferencia;
  fdocumentoDebito := ASource.documentoDebito;
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCorrenteCredito := ASource.contaCorrenteCredito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fcontaPagamentoCredito := ASource.contaPagamentoCredito;
  ftipoBeneficiario := ASource.tipoBeneficiario;
  fcpfCnpjBeneficiario := ASource.cpfCnpjBeneficiario;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  fcodigoAutenticacaoPagamento := ASource.codigoAutenticacaoPagamento;
  fcodigoFinalidadeDOC := ASource.codigoFinalidadeDOC;
  fcodigoFinalidadeTED := ASource.codigoFinalidadeTED;
  fnumeroDepositoJudicial := ASource.numeroDepositoJudicial;
  fnumeroRequisicao := ASource.numeroRequisicao;
  fnumeroArquivoPagamento := ASource.numeroArquivoPagamento;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;
  finicioCartao := ASource.inicioCartao;
  ffimCartao := ASource.fimCartao;
  fdescricaoTransferencia := ASource.descricaoTransferencia;
  fformaTransmissao := ASource.formaTransmissao;
end;

{ TACBrPagamentosBBAlteracaoDataRetorno }

procedure TACBrPagamentosBBAlteracaoDataRetorno.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBAlteracaoDataRetorno) then
    Assign(TACBrPagamentosBBAlteracaoDataRetorno(ASource));
end;

procedure TACBrPagamentosBBAlteracaoDataRetorno.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('quantidadeLancamentoOriginal', fquantidadeLancamentoOriginal)
    .AddPair('quantidadeLancamentoAlterado', fquantidadeLancamentoAlterado);
end;

procedure TACBrPagamentosBBAlteracaoDataRetorno.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('quantidadeLancamentoAlterado', fquantidadeLancamentoAlterado)
    .Value('quantidadeLancamentoOriginal', fquantidadeLancamentoOriginal);
end;

procedure TACBrPagamentosBBAlteracaoDataRetorno.Clear;
begin
  fquantidadeLancamentoOriginal := 0;
  fquantidadeLancamentoAlterado := 0;
end;

function TACBrPagamentosBBAlteracaoDataRetorno.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fquantidadeLancamentoOriginal) and
    EstaZerado(fquantidadeLancamentoAlterado);
end;

procedure TACBrPagamentosBBAlteracaoDataRetorno.Assign(aSource: TACBrPagamentosBBAlteracaoDataRetorno);
begin 
  fquantidadeLancamentoOriginal := ASource.quantidadeLancamentoOriginal;
  fquantidadeLancamentoAlterado := ASource.quantidadeLancamentoAlterado;
end;

{ TACBrPagamentosBBAlteracaoData }

procedure TACBrPagamentosBBAlteracaoData.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBAlteracaoData) then
    Assign(TACBrPagamentosBBAlteracaoData(ASource));
end;

procedure TACBrPagamentosBBAlteracaoData.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .AddPair('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .AddPair('codigoProduto', fcodigoProduto)
    .AddPair('dataOriginalPagamento', DateToStr(fdataOriginalPagamento))
    .AddPair('dataNovoPagamento', DateToStr(fdataNovoPagamento));
end;

procedure TACBrPagamentosBBAlteracaoData.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .Value('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .Value('codigoProduto', fcodigoProduto)
    .Value('dataOriginalPagamento', fdataOriginalPagamento)
    .Value('dataNovoPagamento', fdataNovoPagamento);
end;

procedure TACBrPagamentosBBAlteracaoData.Clear;
begin
  fnumeroAgenciaDebito := 0;
  fnumeroContaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := EmptyStr;
  fcodigoProduto := 0;
  fdataOriginalPagamento := 0;
  fdataNovoPagamento := 0;
end;

function TACBrPagamentosBBAlteracaoData.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroAgenciaDebito) and
    EstaZerado(fnumeroContaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito) and
    EstaZerado(fcodigoProduto) and
    EstaZerado(fdataOriginalPagamento) and
    EstaZerado(fdataNovoPagamento);
end;

procedure TACBrPagamentosBBAlteracaoData.Assign(aSource: TACBrPagamentosBBAlteracaoData);
begin
  fnumeroAgenciaDebito := ASource.numeroAgenciaDebito;
  fnumeroContaCorrenteDebito := ASource.numeroContaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;
  fcodigoProduto := ASource.codigoProduto;
  fdataOriginalPagamento := ASource.dataOriginalPagamento;
  fdataNovoPagamento := ASource.dataNovoPagamento;
end;

{ TACBrPagamentosBBSolicitacaoLotePagamentosResposta }

function TACBrPagamentosBBSolicitacaoLotePagamentosResposta.Getpagamentos: TACBrPagamentosBBSolicitacaoLotePagamentos;
begin
  if (not Assigned(fpagamentos)) then
    fpagamentos := TACBrPagamentosBBSolicitacaoLotePagamentos.Create('pagamentos');
  Result := fpagamentos;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentosResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBSolicitacaoLotePagamentosResposta) then
    Assign(TACBrPagamentosBBSolicitacaoLotePagamentosResposta(ASource));
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentosResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('quantidadePagamentos', fquantidadePagamentos)
    .AddPair('valorPagamentos', fvalorPagamentos)
    .AddPair('quantidadePagamentosValidos', fquantidadePagamentosValidos)
    .AddPair('valorPagamentosValidos', fvalorPagamentosValidos);

  if Assigned(fpagamentos) then
    fpagamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentosResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('estadoRequisicao', i)
    .Value('quantidadePagamentos', fquantidadePagamentos)
    .Value('valorPagamentos', fvalorPagamentos)
    .Value('quantidadePagamentosValidos', fquantidadePagamentosValidos)
    .Value('valorPagamentosValidos', fvalorPagamentosValidos);

  if NaoEstaZerado(i) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i);

  if Assigned(fpagamentos) then
    fpagamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBSolicitacaoLotePagamentosResposta.Destroy;
begin
  if Assigned(fpagamentos) then
    fpagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentosResposta.Clear;
begin
  festadoRequisicao := perNenhum;
  fquantidadePagamentos := 0;
  fvalorPagamentos := 0;
  fquantidadePagamentosValidos := 0;
  fvalorPagamentosValidos := 0;

  if Assigned(fpagamentos) then
    fpagamentos.Clear;
end;

function TACBrPagamentosBBSolicitacaoLotePagamentosResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(fquantidadePagamentos) and
    EstaZerado(fvalorPagamentos) and
    EstaZerado(fquantidadePagamentosValidos) and
    EstaZerado(fvalorPagamentosValidos);

  if Assigned(fpagamentos) then
    Result := Result and fpagamentos.IsEmpty;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamentosResposta.Assign(aSource: TACBrPagamentosBBSolicitacaoLotePagamentosResposta);
begin
  festadoRequisicao := ASource.estadoRequisicao;
  fquantidadePagamentos := ASource.quantidadePagamentos;
  fvalorPagamentos := ASource.valorPagamentos;
  fquantidadePagamentosValidos := ASource.quantidadePagamentosValidos;
  fvalorPagamentosValidos := ASource.valorPagamentosValidos;
  pagamentos.Assign(ASource.pagamentos);
end;

{ TACBrPagamentosBBSolicitacaoLotePagamento }

function TACBrPagamentosBBSolicitacaoLotePagamento.Geterros: TACBrPagamentosBBLancamentoErros;
begin
  if (not Assigned(ferros)) then
    ferros := TACBrPagamentosBBLancamentoErros.Create('erros');
  Result := ferros;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBSolicitacaoLotePagamento) then
    Assign(TACBrPagamentosBBSolicitacaoLotePagamento(ASource));
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificadorPagamento', fidentificadorPagamento)
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCorrenteCredito', fcontaCorrenteCredito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('contaPagamentoCredito', fcontaPagamentoCredito)
    .AddPair('cpfBeneficiario', fcpfBeneficiario)
    .AddPair('cnpjBeneficiario', fcnpjBeneficiario)
    .AddPair('dataPagamento', DateToStr(fdataPagamento))
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('documentoCredito', fdocumentoCredito)
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .AddPair('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .AddPair('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('indicadorAceite', findicadorAceite);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('identificadorPagamento', fidentificadorPagamento)
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCorrenteCredito', fcontaCorrenteCredito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('contaPagamentoCredito', fcontaPagamentoCredito)
    .Value('cpfBeneficiario', fcpfBeneficiario)
    .Value('cnpjBeneficiario', fcnpjBeneficiario)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('documentoCredito', fdocumentoCredito)
    .Value('tipoCredito', i)
    .Value('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .Value('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .Value('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('indicadorAceite', findicadorAceite);

  if NaoEstaZerado(i) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBSolicitacaoLotePagamento.Destroy;
begin
  if Assigned(ferros) then
    ferros.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamento.Clear;
begin
  fidentificadorPagamento := 0;
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  fagenciaCredito := 0;
  fcontaCorrenteCredito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fcontaPagamentoCredito := EmptyStr;
  fcpfBeneficiario := 0;
  fcnpjBeneficiario := 0;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fdocumentoDebito := 0;
  fdocumentoCredito := 0;
  ftipoCredito := pcrNenhum;
  fcodigoFinalidadeDOC := 0;
  fcodigoFinalidadeTED := 0;
  fnumeroDepositoJudicial := EmptyStr;
  fdescricaoPagamento := EmptyStr;
  findicadorAceite := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBSolicitacaoLotePagamento.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificadorPagamento) and
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCorrenteCredito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaVazio(fcontaPagamentoCredito) and
    EstaZerado(fcpfBeneficiario) and
    EstaZerado(fcnpjBeneficiario) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(fdocumentoDebito) and
    EstaZerado(fdocumentoCredito) and
    EstaZerado(Ord(ftipoCredito)) and
    EstaZerado(fcodigoFinalidadeDOC) and
    EstaZerado(fcodigoFinalidadeTED) and
    EstaVazio(fnumeroDepositoJudicial) and
    EstaVazio(fdescricaoPagamento) and
    EstaVazio(findicadorAceite);

  if Assigned(ferros) then
    Result := Result and ferros.IsEmpty;
end;

procedure TACBrPagamentosBBSolicitacaoLotePagamento.Assign(aSource: TACBrPagamentosBBSolicitacaoLotePagamento);
begin
  fidentificadorPagamento := ASource.identificadorPagamento;
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCorrenteCredito := ASource.contaCorrenteCredito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fcontaPagamentoCredito := ASource.contaPagamentoCredito;
  fcpfBeneficiario := ASource.cpfBeneficiario;
  fcnpjBeneficiario := ASource.cnpjBeneficiario;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fdocumentoDebito := ASource.documentoDebito;
  fdocumentoCredito := ASource.documentoCredito;
  ftipoCredito := ASource.tipoCredito;
  fcodigoFinalidadeDOC := ASource.codigoFinalidadeDOC;
  fcodigoFinalidadeTED := ASource.codigoFinalidadeTED;
  fnumeroDepositoJudicial := ASource.numeroDepositoJudicial;
  fdescricaoPagamento := ASource.descricaoPagamento;
  findicadorAceite := ASource.indicadorAceite;
  erros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta }

function TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.GetlistaPagamentos: TACBrPagamentosBBTransferenciaPagamentoEspecificoLista;
begin
  if (not Assigned(flistaPagamentos)) then
    flistaPagamentos := TACBrPagamentosBBTransferenciaPagamentoEspecificoLista.Create('listaPagamentos');
  Result := flistaPagamentos;
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.GetlistaDevolucao: TACBrPagamentosBBDevolucaoLista;
begin
  if (not Assigned(flistaDevolucao)) then
    flistaDevolucao := TACBrPagamentosBBDevolucaoLista.Create('listaDevolucao');
  Result := flistaDevolucao;
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta) then
    Assign(TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .AddPair('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .AddPair('codigoFinalidadeTED', fcodigoFinalidadeTED);

  if Assigned(flistaPagamentos) then
    flistaPagamentos.WriteToJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('tipoCredito', i)
    .Value('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .Value('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .Value('codigoFinalidadeTED', fcodigoFinalidadeTED);

  if NaoEstaZerado(i) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i);

  if Assigned(flistaPagamentos) then
    flistaPagamentos.ReadFromJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.Destroy;
begin
  if Assigned(flistaDevolucao) then
    flistaDevolucao.Free;
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.Clear;
begin
  ftipoCredito := TACBrPagamentosBBTipoCredito(0);
  fnumeroDepositoJudicial := EmptyStr;
  fcodigoFinalidadeDOC := EmptyStr;
  fcodigoFinalidadeTED := EmptyStr;

  if Assigned(flistaPagamentos) then
    flistaPagamentos.Clear;

  if Assigned(flistaDevolucao) then
    flistaDevolucao.Clear;
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(Ord(ftipoCredito)) and
    EstaVazio(fnumeroDepositoJudicial) and
    EstaVazio(fcodigoFinalidadeDOC) and
    EstaVazio(fcodigoFinalidadeTED);

  if Assigned(flistaPagamentos) then
    Result := Result and flistaPagamentos.IsEmpty;

  if Assigned(flistaDevolucao) then
    Result := Result and flistaDevolucao.IsEmpty;
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta.Assign(aSource: TACBrPagamentosBBTransferenciaPagamentoEspecificoResposta);
begin 
  ftipoCredito := ASource.tipoCredito;
  fnumeroDepositoJudicial := ASource.numeroDepositoJudicial;
  fcodigoFinalidadeDOC := ASource.codigoFinalidadeDOC;
  fcodigoFinalidadeTED := ASource.codigoFinalidadeTED;
  listaPagamentos.Assign(ASource.listaPagamentos);
  listaDevolucao.Assign(ASource.listaDevolucao);
end;

{ TACBrPagamentosBBTransferenciaPagamentoEspecifico }

procedure TACBrPagamentosBBTransferenciaPagamentoEspecifico.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPagamentoEspecifico) then
    Assign(TACBrPagamentosBBTransferenciaPagamentoEspecifico(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecifico.DoWriteToJSon(aJSon: TACBrJSONObject);
begin 
  aJSon
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCorrenteCredito', fcontaCorrenteCredito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('numeroContaCredito', fnumeroContaCredito)
    .AddPair('tipoBeneficiario', PagamentosBBTipoBeneficiarioToInteger(ftipoBeneficiario))
    .AddPair('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('documentoCredito', fdocumentoCredito)
    .AddPair('texto', ftexto);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecifico.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCorrenteCredito', fcontaCorrenteCredito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('numeroContaCredito', fnumeroContaCredito)
    .Value('tipoBeneficiario', i)
    .Value('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('documentoCredito', fdocumentoCredito)
    .Value('texto', ftexto);

  if NaoEstaZerado(i) then
    ftipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecifico.Clear;
begin
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  fagenciaCredito := 0;
  fcontaCorrenteCredito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fnumeroContaCredito := EmptyStr;
  ftipoBeneficiario := ptbNenhum;
  fcpfCnpjBeneficiario := 0;
  fnomeBeneficiario := EmptyStr;
  fdocumentoCredito := 0;
  ftexto := EmptyStr;
end;

function TACBrPagamentosBBTransferenciaPagamentoEspecifico.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCorrenteCredito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaVazio(fnumeroContaCredito) and
    EstaZerado(Ord(ftipoBeneficiario)) and
    EstaZerado(fcpfCnpjBeneficiario) and
    EstaVazio(fnomeBeneficiario) and
    EstaZerado(fdocumentoCredito) and
    EstaVazio(ftexto);
end;

procedure TACBrPagamentosBBTransferenciaPagamentoEspecifico.Assign(aSource: TACBrPagamentosBBTransferenciaPagamentoEspecifico);
begin
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCorrenteCredito := ASource.contaCorrenteCredito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fnumeroContaCredito := ASource.numeroContaCredito;
  ftipoBeneficiario := ASource.tipoBeneficiario;
  fcpfCnpjBeneficiario := ASource.cpfCnpjBeneficiario;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  fdocumentoCredito := ASource.documentoCredito;
  ftexto := ASource.texto;
end;

{ TACBrPagamentosBBLoteTransferenciasRequisicaoResposta }

function TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.Gettransferencias: TACBrPagamentosBBTransferenciasRequisicaoResposta;
begin
  if (not Assigned(ftransferencias)) then
    ftransferencias := TACBrPagamentosBBTransferenciasRequisicaoResposta.Create('transferencias');
  Result := ftransferencias;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLoteTransferenciasRequisicaoResposta) then
    Assign(TACBrPagamentosBBLoteTransferenciasRequisicaoResposta(ASource));
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('quantidadeTransferencias', fquantidadeTransferencias)
    .AddPair('valorTransferencias', fvalorTransferencias)
    .AddPair('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .AddPair('valorTransferenciasValidas', fvalorTransferenciasValidas);

  if Assigned(ftransferencias) then
    ftransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('estadoRequisicao', i)
    .Value('quantidadeTransferencias', fquantidadeTransferencias)
    .Value('valorTransferencias', fvalorTransferencias)
    .Value('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .Value('valorTransferenciasValidas', fvalorTransferenciasValidas);

  if NaoEstaZerado(i) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i);

  if Assigned(ftransferencias) then
    ftransferencias.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.Destroy;
begin
  if Assigned(ftransferencias) then
    ftransferencias.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.Clear;
begin
  festadoRequisicao := perNenhum;
  fquantidadeTransferencias := 0;
  fvalorTransferencias := 0;
  fquantidadeTransferenciasValidas := 0;
  fvalorTransferenciasValidas := 0;

  if Assigned(ftransferencias) then
    ftransferencias.Clear;
end;

function TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(fquantidadeTransferencias) and
    EstaZerado(fvalorTransferencias) and
    EstaZerado(fquantidadeTransferenciasValidas) and
    EstaZerado(fvalorTransferenciasValidas);

  if Assigned(ftransferencias) then
    Result := Result and ftransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicaoResposta.Assign(aSource: TACBrPagamentosBBLoteTransferenciasRequisicaoResposta);
begin
  festadoRequisicao := ASource.estadoRequisicao;
  fquantidadeTransferencias := ASource.quantidadeTransferencias;
  fvalorTransferencias := ASource.valorTransferencias;
  fquantidadeTransferenciasValidas := ASource.quantidadeTransferenciasValidas;
  fvalorTransferenciasValidas := ASource.valorTransferenciasValidas;
  transferencias.Assign(ASource.transferencias);
end;

{ TACBrPagamentosBBTransferenciaRequisicaoResposta }

function TACBrPagamentosBBTransferenciaRequisicaoResposta.Geterros: TACBrPagamentosBBLancamentoErros;
begin
  if (not Assigned(ferros)) then
    ferros := TACBrPagamentosBBLancamentoErros.Create('erros');
  Result := ferros;
end;

procedure TACBrPagamentosBBTransferenciaRequisicaoResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaRequisicaoResposta) then
    Assign(TACBrPagamentosBBTransferenciaRequisicaoResposta(ASource));
end;

procedure TACBrPagamentosBBTransferenciaRequisicaoResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificadorTransferencia', fidentificadorTransferencia)
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('indicadorAceite', findicadorAceite);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaRequisicaoResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('identificadorTransferencia', fidentificadorTransferencia)
    .Value('tipoCredito', i)
    .Value('indicadorAceite', findicadorAceite);

  if NaoEstaZerado(i) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBTransferenciaRequisicaoResposta.Destroy;
begin
  if Assigned(ferros) then
    ferros.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBTransferenciaRequisicaoResposta.Clear;
begin
  fidentificadorTransferencia := 0;
  ftipoCredito := pcrNenhum;
  findicadorAceite := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBTransferenciaRequisicaoResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificadorTransferencia) and
    EstaZerado(Ord(ftipoCredito)) and
    EstaVazio(findicadorAceite);

  if Assigned(ferros) then
    Result := Result and ferros.IsEmpty;
end;

procedure TACBrPagamentosBBTransferenciaRequisicaoResposta.Assign(aSource: TACBrPagamentosBBTransferenciaRequisicaoResposta);
begin
  fidentificadorTransferencia := ASource.identificadorTransferencia;
  ftipoCredito := ASource.tipoCredito;
  findicadorAceite := ASource.indicadorAceite;
  erros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBLoteTransferenciasRequisicao }

function TACBrPagamentosBBLoteTransferenciasRequisicao.GetlistaTransferencias: TACBrPagamentosBBTransferenciasRequisicao;
begin
  if (not Assigned(flistaTransferencias)) then
    flistaTransferencias := TACBrPagamentosBBTransferenciasRequisicao.Create('listaTransferencias');
  Result := flistaTransferencias;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLoteTransferenciasRequisicao) then
    Assign(TACBrPagamentosBBLoteTransferenciasRequisicao(ASource));
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('numeroContratoPagamento', fnumeroContratoPagamento)
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('tipoPagamento', PagamentosBBTipoPagamentoToInteger(ftipoPagamento));

  if Assigned(flistaTransferencias) then
    flistaTransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicao.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('numeroContratoPagamento', fnumeroContratoPagamento)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('tipoPagamento', i);

  if NaoEstaZerado(i) then
    ftipoPagamento := IntegerToPagamentosBBTipoPagamento(i);

  if Assigned(flistaTransferencias) then
    flistaTransferencias.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLoteTransferenciasRequisicao.Destroy;
begin
  if Assigned(flistaTransferencias) then
    flistaTransferencias.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicao.Clear;
begin
  fnumeroRequisicao := 0;
  fnumeroContratoPagamento := 0;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  ftipoPagamento := ppgNenhum;

  if Assigned(flistaTransferencias) then
    flistaTransferencias.Clear;
end;

function TACBrPagamentosBBLoteTransferenciasRequisicao.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fnumeroContratoPagamento) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaZerado(Ord(ftipoPagamento));

  if Assigned(flistaTransferencias) then
    Result := Result and flistaTransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBLoteTransferenciasRequisicao.Assign(aSource: TACBrPagamentosBBLoteTransferenciasRequisicao);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  fnumeroContratoPagamento := ASource.numeroContratoPagamento;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  ftipoPagamento := ASource.tipoPagamento;
  listaTransferencias.Assign(ASource.listaTransferencias);
end;

{ TACBrPagamentosBBTransferenciaRequisicao }

procedure TACBrPagamentosBBTransferenciaRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaRequisicao) then
    Assign(TACBrPagamentosBBTransferenciaRequisicao(ASource));
end;

procedure TACBrPagamentosBBTransferenciaRequisicao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCorrenteCredito', fcontaCorrenteCredito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('contaPagamentoCredito', fcontaPagamentoCredito)
    .AddPair('cpfBeneficiario', fcpfBeneficiario)
    .AddPair('cnpjBeneficiario', fcnpjBeneficiario)
    .AddPair('dataTransferencia', DateToStr(fdataTransferencia))
    .AddPair('valorTransferencia', fvalorTransferencia)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('documentoCredito', fdocumentoCredito)
    .AddPair('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .AddPair('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .AddPair('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .AddPair('descricaoTransferencia', fdescricaoTransferencia);
end;

procedure TACBrPagamentosBBTransferenciaRequisicao.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCorrenteCredito', fcontaCorrenteCredito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('contaPagamentoCredito', fcontaPagamentoCredito)
    .Value('cpfBeneficiario', fcpfBeneficiario)
    .Value('cnpjBeneficiario', fcnpjBeneficiario)
    .Value('dataTransferencia', fdataTransferencia)
    .Value('valorTransferencia', fvalorTransferencia)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('documentoCredito', fdocumentoCredito)
    .Value('codigoFinalidadeDOC', fcodigoFinalidadeDOC)
    .Value('codigoFinalidadeTED', fcodigoFinalidadeTED)
    .Value('numeroDepositoJudicial', fnumeroDepositoJudicial)
    .Value('descricaoTransferencia', fdescricaoTransferencia);
end;

procedure TACBrPagamentosBBTransferenciaRequisicao.Clear;
begin
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  fagenciaCredito := 0;
  fcontaCorrenteCredito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fcontaPagamentoCredito := EmptyStr;
  fcpfBeneficiario := 0;
  fcnpjBeneficiario := 0;
  fdataTransferencia := 0;
  fvalorTransferencia := 0;
  fdocumentoDebito := 0;
  fdocumentoCredito := 0;
  fcodigoFinalidadeDOC := 0;
  fcodigoFinalidadeTED := 0;
  fnumeroDepositoJudicial := EmptyStr;
  fdescricaoTransferencia := EmptyStr;
end;

function TACBrPagamentosBBTransferenciaRequisicao.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCorrenteCredito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaVazio(fcontaPagamentoCredito) and
    EstaZerado(fcpfBeneficiario) and
    EstaZerado(fcnpjBeneficiario) and
    EstaZerado(fdataTransferencia) and
    EstaZerado(fvalorTransferencia) and
    EstaZerado(fdocumentoDebito) and
    EstaZerado(fdocumentoCredito) and
    EstaZerado(fcodigoFinalidadeDOC) and
    EstaZerado(fcodigoFinalidadeTED) and
    EstaVazio(fnumeroDepositoJudicial) and
    EstaVazio(fdescricaoTransferencia);
end;

procedure TACBrPagamentosBBTransferenciaRequisicao.Assign(aSource: TACBrPagamentosBBTransferenciaRequisicao);
begin 
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCorrenteCredito := ASource.contaCorrenteCredito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fcontaPagamentoCredito := ASource.contaPagamentoCredito;
  fcpfBeneficiario := ASource.cpfBeneficiario;
  fcnpjBeneficiario := ASource.cnpjBeneficiario;
  fdataTransferencia := ASource.dataTransferencia;
  fvalorTransferencia := ASource.valorTransferencia;
  fdocumentoDebito := ASource.documentoDebito;
  fdocumentoCredito := ASource.documentoCredito;
  fcodigoFinalidadeDOC := ASource.codigoFinalidadeDOC;
  fcodigoFinalidadeTED := ASource.codigoFinalidadeTED;
  fnumeroDepositoJudicial := ASource.numeroDepositoJudicial;
  fdescricaoTransferencia := ASource.descricaoTransferencia;
end;

{ TACBrPagamentosBBLoteTransferenciasResposta }

function TACBrPagamentosBBLoteTransferenciasResposta.Gettransferencias: TACBrPagamentosBBTransferencias;
begin
  if (not Assigned(ftransferencias)) then
    ftransferencias := TACBrPagamentosBBTransferencias.Create('transferencias');
  Result := ftransferencias;
end;

procedure TACBrPagamentosBBLoteTransferenciasResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLoteTransferenciasResposta) then
    Assign(TACBrPagamentosBBLoteTransferenciasResposta(ASource));
end;

procedure TACBrPagamentosBBLoteTransferenciasResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('indice', findice);

  if Assigned(ftransferencias) then
    ftransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLoteTransferenciasResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('indice', findice);

  if Assigned(ftransferencias) then
    ftransferencias.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLoteTransferenciasResposta.Destroy;
begin
  if Assigned(ftransferencias) then
    ftransferencias.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLoteTransferenciasResposta.Clear;
begin
  findice := 0;

  if Assigned(ftransferencias) then
    ftransferencias.Clear;
end;

function TACBrPagamentosBBLoteTransferenciasResposta.IsEmpty: Boolean;
begin
  Result := EstaZerado(findice);

  if Assigned(ftransferencias) then
    Result := Result and ftransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBLoteTransferenciasResposta.Assign(aSource: TACBrPagamentosBBLoteTransferenciasResposta);
begin 
  findice := ASource.indice;
  transferencias.Assign(ASource.transferencias);
end;

{ TACBrPagamentosBBTransferencia }

procedure TACBrPagamentosBBTransferencia.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferencia) then
    Assign(TACBrPagamentosBBTransferencia(ASource));
end;

procedure TACBrPagamentosBBTransferencia.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('dataRequisicao', DateToStr(fdataRequisicao))
    .AddPair('tipoPagamento', PagamentosBBTipoPagamentoToInteger(ftipoPagamento))
    .AddPair('identificacaoRequisitante', fidentificacaoRequisitante)
    .AddPair('quantidadeTransferencias', fquantidadeTransferencias)
    .AddPair('totalTransferencias', ftotalTransferencias)
    .AddPair('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .AddPair('totalTransferenciasValidas', ftotalTransferenciasValidas);
end;

procedure TACBrPagamentosBBTransferencia.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('estadoRequisicao', i1)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('dataRequisicao', fdataRequisicao)
    .Value('tipoPagamento', i2)
    .Value('identificacaoRequisitante', fidentificacaoRequisitante)
    .Value('quantidadeTransferencias', fquantidadeTransferencias)
    .Value('totalTransferencias', ftotalTransferencias)
    .Value('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .Value('totalTransferenciasValidas', ftotalTransferenciasValidas);

  if NaoEstaZerado(i1) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i1);

  if NaoEstaZerado(i2) then
    ftipoPagamento := IntegerToPagamentosBBTipoPagamento(i2);
end;

procedure TACBrPagamentosBBTransferencia.Clear;
begin
  fnumeroRequisicao := 0;
  festadoRequisicao := perNenhum;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fdataRequisicao := 0;
  ftipoPagamento := ppgNenhum;
  fidentificacaoRequisitante := EmptyStr;
  fquantidadeTransferencias := 0;
  ftotalTransferencias := 0;
  fquantidadeTransferenciasValidas := 0;
  ftotalTransferenciasValidas := 0;
end;

function TACBrPagamentosBBTransferencia.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaZerado(fdataRequisicao) and
    EstaZerado(Ord(ftipoPagamento)) and
    EstaVazio(fidentificacaoRequisitante) and
    EstaZerado(fquantidadeTransferencias) and
    EstaZerado(ftotalTransferencias) and
    EstaZerado(fquantidadeTransferenciasValidas) and
    EstaZerado(ftotalTransferenciasValidas);
end;

procedure TACBrPagamentosBBTransferencia.Assign(aSource: TACBrPagamentosBBTransferencia);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  festadoRequisicao := ASource.estadoRequisicao;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fdataRequisicao := ASource.dataRequisicao;
  ftipoPagamento := ASource.tipoPagamento;
  fidentificacaoRequisitante := ASource.identificacaoRequisitante;
  fquantidadeTransferencias := ASource.quantidadeTransferencias;
  ftotalTransferencias := ASource.totalTransferencias;
  fquantidadeTransferenciasValidas := ASource.quantidadeTransferenciasValidas;
  ftotalTransferenciasValidas := ASource.totalTransferenciasValidas;
end;

{ TACBrPagamentosBBLancamentosPeriodoResposta }

function TACBrPagamentosBBLancamentosPeriodoResposta.GetlistaMovimento: TACBrPagamentosBBMovimentoLista;
begin
  if (not Assigned(flistaMovimento)) then
    flistaMovimento := TACBrPagamentosBBMovimentoLista.Create('listaMovimento');
  Result := flistaMovimento;
end;

procedure TACBrPagamentosBBLancamentosPeriodoResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLancamentosPeriodoResposta) then
    Assign(TACBrPagamentosBBLancamentosPeriodoResposta(ASource));
end;

procedure TACBrPagamentosBBLancamentosPeriodoResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroDaposicaoDePesquisa', fnumeroDaposicaoDePesquisa)
    .AddPair('quantidadeOcorrenciasTotal', fquantidadeOcorrenciasTotal)
    .AddPair('quantidadeOcorrenciasTabeladas', fquantidadeOcorrenciasTabeladas);

  if Assigned(flistaMovimento) then
    flistaMovimento.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLancamentosPeriodoResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroDaposicaoDePesquisa', fnumeroDaposicaoDePesquisa)
    .Value('quantidadeOcorrenciasTotal', fquantidadeOcorrenciasTotal)
    .Value('quantidadeOcorrenciasTabeladas', fquantidadeOcorrenciasTabeladas);

  if Assigned(flistaMovimento) then
    flistaMovimento.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLancamentosPeriodoResposta.Destroy;
begin
  if Assigned(flistaMovimento) then
    flistaMovimento.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLancamentosPeriodoResposta.Clear;
begin 
  fnumeroDaposicaoDePesquisa := 0;
  fquantidadeOcorrenciasTotal := 0;
  fquantidadeOcorrenciasTabeladas := 0;

  if Assigned(flistaMovimento) then
    flistaMovimento.Clear;
end;

function TACBrPagamentosBBLancamentosPeriodoResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroDaposicaoDePesquisa) and
    EstaZerado(fquantidadeOcorrenciasTotal) and
    EstaZerado(fquantidadeOcorrenciasTabeladas);

  if Assigned(flistaMovimento) then
    Result := Result and flistaMovimento.IsEmpty;
end;

procedure TACBrPagamentosBBLancamentosPeriodoResposta.Assign(aSource: TACBrPagamentosBBLancamentosPeriodoResposta);
begin
  fnumeroDaposicaoDePesquisa := ASource.numeroDaposicaoDePesquisa;
  fquantidadeOcorrenciasTotal := ASource.quantidadeOcorrenciasTotal;
  fquantidadeOcorrenciasTabeladas := ASource.quantidadeOcorrenciasTabeladas;
  listaMovimento.Assign(ASource.listaMovimento);
end;

{ TACBrPagamentosBBMovimento }

procedure TACBrPagamentosBBMovimento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBMovimento) then
    Assign(TACBrPagamentosBBMovimento(ASource));
end;

procedure TACBrPagamentosBBMovimento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
  .AddPair('numeroRequisicaoPagamento', fnumeroRequisicaoPagamento)
  .AddPair('textoEstadoPagamento', ftextoEstadoPagamento)
  .AddPair('codigoIdentificadorDoPagamento', fcodigoIdentificadorDoPagamento)
  .AddPair('nomeDoFavorecido', fnomeDoFavorecido)
  .AddPair('codigoDoTipoDePessoa', PagamentosBBTipoBeneficiarioToInteger(fcodigoDoTipoDePessoa))
  .AddPair('numeroCPFouCNPJ', fnumeroCPFouCNPJ)
  .AddPair('dataPagamento', DateToStr(fdataPagamento))
  .AddPair('valorPagamento', fvalorPagamento)
  .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
  .AddPair('numeroDocumentoCredito', fnumeroDocumentoCredito)
  .AddPair('codigoFormaCredito', fcodigoFormaCredito)
  .AddPair('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento)
  .AddPair('dataDebito', DateToStr(fdataDebito))
  .AddPair('codigoTipoPagamento', PagamentosBBTipoPagamentoToInteger(fcodigoTipoPagamento));
end;

procedure TACBrPagamentosBBMovimento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('numeroRequisicaoPagamento', fnumeroRequisicaoPagamento)
    .Value('textoEstadoPagamento', ftextoEstadoPagamento)
    .Value('codigoIdentificadorDoPagamento', fcodigoIdentificadorDoPagamento)
    .Value('nomeDoFavorecido', fnomeDoFavorecido)
    .Value('codigoDoTipoDePessoa', i1)
    .Value('numeroCPFouCNPJ', fnumeroCPFouCNPJ)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('numeroDocumentoCredito', fnumeroDocumentoCredito)
    .Value('codigoFormaCredito', fcodigoFormaCredito)
    .Value('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento)
    .Value('dataDebito', fdataDebito)
    .Value('codigoTipoPagamento', i2);

  if NaoEstaZerado(i1) then
    fcodigoDoTipoDePessoa := IntegerToPagamentosBBTipoBeneficiario(i1);

  if NaoEstaZerado(i2) then
    fcodigoTipoPagamento := IntegerToPagamentosBBTipoPagamento(i2);
end;

procedure TACBrPagamentosBBMovimento.Clear;
begin
  fnumeroRequisicaoPagamento := 0;
  ftextoEstadoPagamento := EmptyStr;
  fcodigoIdentificadorDoPagamento := 0;
  fnomeDoFavorecido := EmptyStr;
  fcodigoDoTipoDePessoa := ptbNenhum;
  fnumeroCPFouCNPJ := 0;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fnumeroDocumentoDebito := 0;
  fnumeroDocumentoCredito := 0;
  fcodigoFormaCredito := 0;
  fcodigoAutenticacaoPagamento := EmptyStr;
  fdataDebito := 0;
  fcodigoTipoPagamento := ppgNenhum;
end;

function TACBrPagamentosBBMovimento.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicaoPagamento) and
    EstaVazio(ftextoEstadoPagamento) and
    EstaZerado(fcodigoIdentificadorDoPagamento) and
    EstaVazio(fnomeDoFavorecido) and
    EstaZerado(Ord(fcodigoDoTipoDePessoa)) and
    EstaZerado(fnumeroCPFouCNPJ) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(fnumeroDocumentoDebito) and
    EstaZerado(fnumeroDocumentoCredito) and
    EstaZerado(fcodigoFormaCredito) and
    EstaVazio(fcodigoAutenticacaoPagamento) and
    EstaZerado(fdataDebito) and
    EstaZerado(Ord(fcodigoTipoPagamento));
end;

procedure TACBrPagamentosBBMovimento.Assign(aSource: TACBrPagamentosBBMovimento);
begin
  fnumeroRequisicaoPagamento := ASource.numeroRequisicaoPagamento;
  ftextoEstadoPagamento := ASource.textoEstadoPagamento;
  fcodigoIdentificadorDoPagamento := ASource.codigoIdentificadorDoPagamento;
  fnomeDoFavorecido := ASource.nomeDoFavorecido;
  fcodigoDoTipoDePessoa := ASource.codigoDoTipoDePessoa;
  fnumeroCPFouCNPJ := ASource.numeroCPFouCNPJ;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fnumeroDocumentoDebito := ASource.numeroDocumentoDebito;
  fnumeroDocumentoCredito := ASource.numeroDocumentoCredito;
  fcodigoFormaCredito := ASource.codigoFormaCredito;
  fcodigoAutenticacaoPagamento := ASource.codigoAutenticacaoPagamento;
  fdataDebito := ASource.dataDebito;
  fcodigoTipoPagamento := ASource.codigoTipoPagamento;
end;

{ TACBrPagamentosBBLotePagamentosResposta }

function TACBrPagamentosBBLotePagamentosResposta.Getpagamentos: TACBrPagamentosBBLotePagamentos;
begin
  if (not Assigned(fpagamentos)) then
    fpagamentos := TACBrPagamentosBBLotePagamentos.Create('pagamentos');
  Result := fpagamentos;
end;

procedure TACBrPagamentosBBLotePagamentosResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLotePagamentosResposta) then
    Assign(TACBrPagamentosBBLotePagamentosResposta(ASource));
end;

procedure TACBrPagamentosBBLotePagamentosResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('indice', findice)
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('tipoPagamento', ftipoPagamento)
    .AddPair('dataRequisicao', DateToStr(fdataRequisicao))
    .AddPair('quantidadePagamentos', fquantidadePagamentos)
    .AddPair('valorPagamentos', fvalorPagamentos);

  if Assigned(fpagamentos) then
    fpagamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLotePagamentosResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('indice', findice)
    .Value('estadoRequisicao', i)
    .Value('tipoPagamento', ftipoPagamento)
    .Value('dataRequisicao', fdataRequisicao)
    .Value('quantidadePagamentos', fquantidadePagamentos)
    .Value('valorPagamentos', fvalorPagamentos);

  if NaoEstaZerado(i) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i);

  if Assigned(fpagamentos) then
    fpagamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLotePagamentosResposta.Destroy;
begin
  if Assigned(fpagamentos) then
    fpagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLotePagamentosResposta.Clear;
begin
  findice := 0;
  festadoRequisicao := perNenhum;
  ftipoPagamento := 0;
  fdataRequisicao := 0;
  fquantidadePagamentos := 0;
  fvalorPagamentos := 0;

  if Assigned(fpagamentos) then
    fpagamentos.Clear;
end;

function TACBrPagamentosBBLotePagamentosResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(findice) and
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(ftipoPagamento) and
    EstaZerado(fdataRequisicao) and
    EstaZerado(fquantidadePagamentos) and
    EstaZerado(fvalorPagamentos);

  if Assigned(fpagamentos) then
    Result := Result and fpagamentos.IsEmpty;
end;

procedure TACBrPagamentosBBLotePagamentosResposta.Assign(aSource: TACBrPagamentosBBLotePagamentosResposta);
begin 
  findice := ASource.indice;
  festadoRequisicao := ASource.estadoRequisicao;
  ftipoPagamento := ASource.tipoPagamento;
  fdataRequisicao := ASource.dataRequisicao;
  fquantidadePagamentos := ASource.quantidadePagamentos;
  fvalorPagamentos := ASource.valorPagamentos;
  pagamentos.Assign(ASource.pagamentos);
end;

{ TACBrPagamentosBBLotePagamento }

function TACBrPagamentosBBLotePagamento.Geterros: TACBrPagamentosBBLancamentoErros;
begin
  if (not Assigned(ferros)) then
    ferros := TACBrPagamentosBBLancamentoErros.Create('erros');
  Result := ferros;
end;

procedure TACBrPagamentosBBLotePagamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLotePagamento) then
    Assign(TACBrPagamentosBBLotePagamento(ASource));
end;

procedure TACBrPagamentosBBLotePagamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificadorPagamento', fidentificadorPagamento)
    .AddPair('dataPagamento', fdataPagamento)
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('tipoBeneficiario', PagamentosBBTipoBeneficiarioToInteger(ftipoBeneficiario))
    .AddPair('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('estadoPagamento', PagamentosBBLoteEstadoToInteger(festadoPagamento))
    .AddPair('descricaoPagamento', fdescricaoPagamento);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLotePagamento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2, i3: Integer;
begin
  aJSon
    .Value('identificadorPagamento', fidentificadorPagamento)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('tipoCredito', i1)
    .Value('tipoBeneficiario', i2)
    .Value('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('estadoPagamento', i3)
    .Value('descricaoPagamento', fdescricaoPagamento);

  if NaoEstaZerado(i1) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i1);

  if NaoEstaZerado(i2) then
    ftipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i2);

  if NaoEstaZerado(i3) then
    festadoPagamento := IntegerToPagamentosBBLoteEstado(i3);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLotePagamento.Destroy;
begin
  if Assigned(ferros) then
    ferros.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLotePagamento.Clear;
begin
  fidentificadorPagamento := 0;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  ftipoCredito := pcrNenhum;
  ftipoBeneficiario := ptbNenhum;
  fcpfCnpjBeneficiario := 0;
  fnomeBeneficiario := EmptyStr;
  festadoPagamento := pleNenhum;
  fdescricaoPagamento := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBLotePagamento.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificadorPagamento) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(Ord(ftipoCredito)) and
    EstaZerado(Ord(ftipoBeneficiario)) and
    EstaZerado(fcpfCnpjBeneficiario) and
    EstaVazio(fnomeBeneficiario) and
    EstaZerado(Ord(festadoPagamento)) and
    EstaVazio(fdescricaoPagamento);

    if Assigned(ferros) then
      ferros.IsEmpty;
end;

procedure TACBrPagamentosBBLotePagamento.Assign(aSource: TACBrPagamentosBBLotePagamento);
begin
  fidentificadorPagamento := ASource.identificadorPagamento;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  ftipoCredito := ASource.tipoCredito;
  ftipoBeneficiario := ASource.tipoBeneficiario;
  fcpfCnpjBeneficiario := ASource.cpfCnpjBeneficiario;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  festadoPagamento := ASource.estadoPagamento;
  fdescricaoPagamento := ASource.descricaoPagamento;
  erros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBCancelarLotePagamentos }

function TACBrPagamentosBBCancelarLotePagamentos.GetlistaPagamentos: TACBrPagamentosBBPagamentoCancelarLista;
begin
  if (not Assigned(flistaPagamentos)) then
    flistaPagamentos := TACBrPagamentosBBPagamentoCancelarLista.Create('listaPagamentos');
  Result := flistaPagamentos;
end;

procedure TACBrPagamentosBBCancelarLotePagamentos.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBCancelarLotePagamentos) then
    Assign(TACBrPagamentosBBCancelarLotePagamentos(ASource));
end;

procedure TACBrPagamentosBBCancelarLotePagamentos.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('numeroContratoPagamento', fnumeroContratoPagamento);

  if Assigned(flistaPagamentos) then
    flistaPagamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBCancelarLotePagamentos.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('numeroContratoPagamento', fnumeroContratoPagamento);

  if Assigned(flistaPagamentos) then
    flistaPagamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBCancelarLotePagamentos.Destroy;
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBCancelarLotePagamentos.Clear;
begin
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fnumeroContratoPagamento := 0;

  if Assigned(flistaPagamentos) then
    flistaPagamentos.Clear;
end;

function TACBrPagamentosBBCancelarLotePagamentos.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaZerado(fnumeroContratoPagamento) and
    flistaPagamentos.IsEmpty;
end;

procedure TACBrPagamentosBBCancelarLotePagamentos.Assign(aSource: TACBrPagamentosBBCancelarLotePagamentos);
begin 
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fnumeroContratoPagamento := ASource.numeroContratoPagamento;

  if Assigned(flistaPagamentos) then
    flistaPagamentos.Assign(ASource.listaPagamentos);
end;

{ TACBrPagamentosBBPagamentoCancelar }

procedure TACBrPagamentosBBPagamentoCancelar.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBPagamentoCancelar) then
    Assign(TACBrPagamentosBBPagamentoCancelar(ASource));
end;

procedure TACBrPagamentosBBPagamentoCancelar.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('codigoPagamento', fcodigoPagamento);
end;

procedure TACBrPagamentosBBPagamentoCancelar.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('codigoPagamento', fcodigoPagamento);
end;

procedure TACBrPagamentosBBPagamentoCancelar.Clear;
begin
  fcodigoPagamento := EmptyStr;
end;

function TACBrPagamentosBBPagamentoCancelar.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcodigoPagamento);
end;

procedure TACBrPagamentosBBPagamentoCancelar.Assign(aSource: TACBrPagamentosBBPagamentoCancelar);
begin
  fcodigoPagamento := ASource.codigoPagamento;
end;

{ TACBrPagamentosBBLiberarPagamentosRequisicao }

procedure TACBrPagamentosBBLiberarPagamentosRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLiberarPagamentosRequisicao) then
    Assign(TACBrPagamentosBBLiberarPagamentosRequisicao(ASource));
end;

procedure TACBrPagamentosBBLiberarPagamentosRequisicao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('indicadorFloat', findicadorFloat);
end;

procedure TACBrPagamentosBBLiberarPagamentosRequisicao.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('indicadorFloat', findicadorFloat);
end;

procedure TACBrPagamentosBBLiberarPagamentosRequisicao.Clear;
begin
  fnumeroRequisicao := 0;
  findicadorFloat := EmptyStr;
end;

function TACBrPagamentosBBLiberarPagamentosRequisicao.IsEmpty: Boolean;
begin
  Result := EstaZerado(fnumeroRequisicao) and EstaVazio(findicadorFloat);
end;

procedure TACBrPagamentosBBLiberarPagamentosRequisicao.Assign(aSource: TACBrPagamentosBBLiberarPagamentosRequisicao);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  findicadorFloat := ASource.indicadorFloat;
end;

{ TACBrPagamentoBBLotePagamentosRequisicao }

procedure TACBrPagamentoBBLotePagamentosRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentoBBLotePagamentosRequisicao) then
    Assign(TACBrPagamentoBBLotePagamentosRequisicao(ASource));
end;

procedure TACBrPagamentoBBLotePagamentosRequisicao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('codigoContrato', fcodigoContrato)
    .AddPair('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .AddPair('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito);
end;

procedure TACBrPagamentoBBLotePagamentosRequisicao.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('codigoContrato', fcodigoContrato)
    .Value('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .Value('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito);
end;

procedure TACBrPagamentoBBLotePagamentosRequisicao.Clear;
begin
  fnumeroRequisicao := 0;
  fcodigoContrato := 0;
  fnumeroAgenciaDebito := 0;
  fnumeroContaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := EmptyStr;
end;

function TACBrPagamentoBBLotePagamentosRequisicao.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fcodigoContrato) and
    EstaZerado(fnumeroAgenciaDebito) and
    EstaZerado(fnumeroContaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito);
end;

procedure TACBrPagamentoBBLotePagamentosRequisicao.Assign(aSource: TACBrPagamentoBBLotePagamentosRequisicao);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  fcodigoContrato := ASource.codigoContrato;
  fnumeroAgenciaDebito := ASource.numeroAgenciaDebito;
  fnumeroContaCorrenteDebito := ASource.numeroContaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;
end;

{ TACBrPagamentosBBPagamentosResposta }

function TACBrPagamentosBBPagamentosResposta.Getpagamentos: TACBrPagamentosBBPagamentos;
begin
  if (not Assigned(fpagamentos)) then
    fpagamentos := TACBrPagamentosBBPagamentos.Create('pagamentos');
  Result := fpagamentos;
end;

procedure TACBrPagamentosBBPagamentosResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBPagamentosResposta) then
    Assign(TACBrPagamentosBBPagamentosResposta(ASource));
end;

procedure TACBrPagamentosBBPagamentosResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('indice', findice)
    .AddPair('quantidadeTotalRegistros', fquantidadeTotalRegistros)
    .AddPair('quantidadeRegistros', fquantidadeRegistros);

  if Assigned(fpagamentos) then
    fpagamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBPagamentosResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('indice', findice)
    .Value('quantidadeTotalRegistros', fquantidadeTotalRegistros)
    .Value('quantidadeRegistros', fquantidadeRegistros);

  if Assigned(fpagamentos) then
    fpagamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBPagamentosResposta.Destroy;
begin
  if Assigned(fpagamentos) then
    fpagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBPagamentosResposta.Clear;
begin
  findice := 0;
  fquantidadeTotalRegistros := 0;
  fquantidadeRegistros := 0;

  if Assigned(fpagamentos) then
    fpagamentos.Clear;
end;

function TACBrPagamentosBBPagamentosResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(findice) and
    EstaZerado(fquantidadeTotalRegistros) and
    EstaZerado(fquantidadeRegistros) and
    fpagamentos.IsEmpty;
end;

procedure TACBrPagamentosBBPagamentosResposta.Assign(aSource: TACBrPagamentosBBPagamentosResposta);
begin
  findice := ASource.indice;
  fquantidadeTotalRegistros := ASource.quantidadeTotalRegistros;
  fquantidadeRegistros := ASource.quantidadeRegistros;

  if Assigned(fpagamentos) then
    fpagamentos.Assign(ASource.pagamentos);

end;

{ TACBrPagamentosBBPagamento }

procedure TACBrPagamentosBBPagamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBPagamento) then
    Assign(TACBrPagamentosBBPagamento(ASource));
end;

procedure TACBrPagamentosBBPagamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificadorPagamento', fidentificadorPagamento)
    .AddPair('tipoPagamento', PagamentosBBTipoPagamentoToInteger(ftipoPagamento))
    .AddPair('tipoCredito', PagamentosBBTipoCreditoToInteger(ftipoCredito))
    .AddPair('dataPagamento', DateToStr(fdataPagamento))
    .AddPair('compeCredito', fcompeCredito)
    .AddPair('ispbCredito', fispbCredito)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCredito', fcontaCredito)
    .AddPair('digitoVerificadorContaCredito', fdigitoVerificadorContaCredito)
    .AddPair('contaPagamentoCredito', fcontaPagamentoCredito)
    .AddPair('tipoBeneficiario', PagamentosBBTipoBeneficiarioToInteger(ftipoBeneficiario))
    .AddPair('cpfCnpj', fcpfCnpj)
    .AddPair('nome', fnome)
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('codigoBarrasBoleto', fcodigoBarrasBoleto)
    .AddPair('valorBoleto', fvalorBoleto)
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .AddPair('inicioCartao', finicioCartao)
    .AddPair('fimCartao', ffimCartao)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('dataDevolucao', fdataDevolucao)
    .AddPair('valorDevolucao', fvalorDevolucao)
    .AddPair('codigoDevolucao', PagamentosBBCodigoDevolucaoToInteger(fcodigoDevolucao))
    .AddPair('sequenciaDevolucao', fsequenciaDevolucao)
    .AddPair('tipoConta', PagamentosBBTipoContaToInteger(ftipoConta))
    .AddPair('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .AddPair('formaIdentificacao', PagamentosBBFormaIdentificacaoToInteger(fformaIdentificacao))
    .AddPair('dddTelefone', fdddTelefone)
    .AddPair('telefone', ftelefone)
    .AddPair('email', femail)
    .AddPair('identificacaoAleatoria', fidentificacaoAleatoria);
end;

procedure TACBrPagamentosBBPagamento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2, i3, i4, i5, i6: Integer;
begin
  aJSon
    .Value('identificadorPagamento', fidentificadorPagamento)
    .Value('tipoPagamento', i1)
    .Value('tipoCredito', i2)
    .Value('dataPagamento', fdataPagamento)
    .Value('compeCredito', fcompeCredito)
    .Value('ispbCredito', fispbCredito)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCredito', fcontaCredito)
    .Value('digitoVerificadorContaCredito', fdigitoVerificadorContaCredito)
    .Value('contaPagamentoCredito', fcontaPagamentoCredito)
    .Value('tipoBeneficiario', i3)
    .Value('cpfCnpj', fcpfCnpj)
    .Value('nome', fnome)
    .Value('valorPagamento', fvalorPagamento)
    .Value('codigoBarrasBoleto', fcodigoBarrasBoleto)
    .Value('valorBoleto', fvalorBoleto)
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .Value('inicioCartao', finicioCartao)
    .Value('fimCartao', ffimCartao)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('dataDevolucao', fdataDevolucao)
    .Value('valorDevolucao', fvalorDevolucao)
    .Value('codigoDevolucao', i4)
    .Value('sequenciaDevolucao', fsequenciaDevolucao)
    .Value('tipoConta', i5)
    .Value('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .Value('formaIdentificacao', i6)
    .Value('dddTelefone', fdddTelefone)
    .Value('telefone', ftelefone)
    .Value('email', femail)
    .Value('identificacaoAleatoria', fidentificacaoAleatoria);

  if NaoEstaZerado(i1) then
    ftipoPagamento := IntegerToPagamentosBBTipoPagamento(i1);

  if NaoEstaZerado(i2) then
    ftipoCredito := IntegerToPagamentosBBTipoCredito(i2);

  if NaoEstaZerado(i3) then
    ftipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i3);

  if NaoEstaZerado(i4) then
    fcodigoDevolucao := IntegerToPagamentosBBCodigoDevolucao(i4);

  if NaoEstaZerado(i5) then
    ftipoConta := IntegerToPagamentosBBTipoConta(i5);

  if NaoEstaZerado(i6) then
    fformaIdentificacao := IntegerToPagamentosBBFormaIdentificacao(i6);
end;

procedure TACBrPagamentosBBPagamento.Clear;
begin
  fidentificadorPagamento := 0;
  ftipoPagamento := ppgNenhum;
  ftipoCredito := pcrNenhum;
  fdataPagamento := 0;
  fcompeCredito := 0;
  fispbCredito := 0;
  fagenciaCredito := 0;
  fcontaCredito := 0;
  fdigitoVerificadorContaCredito := EmptyStr;
  fcontaPagamentoCredito := EmptyStr;
  ftipoBeneficiario := ptbNenhum;
  fcpfCnpj := 0;
  fnome := EmptyStr;
  fvalorPagamento := 0;
  fcodigoBarrasBoleto := EmptyStr;
  fvalorBoleto := 0;
  fnumeroRequisicao := 0;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := EmptyStr;
  finicioCartao := 0;
  ffimCartao := 0;
  fdocumentoDebito := 0;
  fdataDevolucao := 0;
  fvalorDevolucao := 0;
  fcodigoDevolucao := pcdNenhum;
  fsequenciaDevolucao := 0;
  ftipoConta := ptcNenhum;
  fdescricaoPagamentoInstantaneo := EmptyStr;
  fformaIdentificacao := pfiNenhum;
  fdddTelefone := 0;
  ftelefone := 0;
  femail := EmptyStr;
  fidentificacaoAleatoria := EmptyStr;
end;

function TACBrPagamentosBBPagamento.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificadorPagamento) and
    EstaZerado(Ord(ftipoPagamento)) and
    EstaZerado(Ord(ftipoCredito)) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fcompeCredito) and
    EstaZerado(fispbCredito) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCredito) and
    EstaVazio(fdigitoVerificadorContaCredito) and
    EstaVazio(fcontaPagamentoCredito) and
    EstaZerado(Ord(ftipoBeneficiario)) and
    EstaZerado(fcpfCnpj) and
    EstaVazio(fnome) and
    EstaZerado(fvalorPagamento) and
    EstaVazio(fcodigoBarrasBoleto) and
    EstaZerado(fvalorBoleto) and
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito) and
    EstaZerado(finicioCartao) and
    EstaZerado(ffimCartao) and
    EstaZerado(fdocumentoDebito) and
    EstaZerado(fdataDevolucao) and
    EstaZerado(fvalorDevolucao) and
    EstaZerado(Ord(fcodigoDevolucao)) and
    EstaZerado(fsequenciaDevolucao) and
    EstaZerado(Ord(ftipoConta)) and
    EstaVazio(fdescricaoPagamentoInstantaneo) and
    EstaZerado(Ord(fformaIdentificacao)) and
    EstaZerado(fdddTelefone) and
    EstaZerado(ftelefone) and
    EstaVazio(femail) and
    EstaVazio(fidentificacaoAleatoria);
end;

procedure TACBrPagamentosBBPagamento.Assign(aSource: TACBrPagamentosBBPagamento);
begin
  fidentificadorPagamento := ASource.identificadorPagamento;
  ftipoPagamento := ASource.tipoPagamento;
  ftipoCredito := ASource.tipoCredito;
  fdataPagamento := ASource.dataPagamento;
  fcompeCredito := ASource.compeCredito;
  fispbCredito := ASource.ispbCredito;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCredito := ASource.contaCredito;
  fdigitoVerificadorContaCredito := ASource.digitoVerificadorContaCredito;
  fcontaPagamentoCredito := ASource.contaPagamentoCredito;
  ftipoBeneficiario := ASource.tipoBeneficiario;
  fcpfCnpj := ASource.cpfCnpj;
  fnome := ASource.nome;
  fvalorPagamento := ASource.valorPagamento;
  fcodigoBarrasBoleto := ASource.codigoBarrasBoleto;
  fvalorBoleto := ASource.valorBoleto;
  fnumeroRequisicao := ASource.numeroRequisicao;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;
  finicioCartao := ASource.inicioCartao;
  ffimCartao := ASource.fimCartao;
  fdocumentoDebito := ASource.documentoDebito;
  fdataDevolucao := ASource.dataDevolucao;
  fvalorDevolucao := ASource.valorDevolucao;
  fcodigoDevolucao := ASource.codigoDevolucao;
  fsequenciaDevolucao := ASource.sequenciaDevolucao;
  ftipoConta := ASource.tipoConta;
  fdescricaoPagamentoInstantaneo := ASource.descricaoPagamentoInstantaneo;
  fformaIdentificacao := ASource.formaIdentificacao;
  fdddTelefone := ASource.dddTelefone;
  ftelefone := ASource.telefone;
  femail := ASource.email;
  fidentificacaoAleatoria := ASource.identificacaoAleatoria;

end;

{ TACBrPagamentosBBConsultaRespostaBase }

procedure TACBrPagamentosBBConsultaRespostaBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBConsultaRespostaBase) then
    Assign(TACBrPagamentosBBConsultaRespostaBase(ASource));
end;

procedure TACBrPagamentosBBConsultaRespostaBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', fid)
    .AddPair('estadoPagamento', PagamentosBBEstadoToInteger(festadoPagamento))
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .AddPair('inicioCartaoCredito', finicioCartaoCredito)
    .AddPair('fimCartaoCredito', ffimCartaoCredito)
    .AddPair('dataPagamento', DateToStr(fdataPagamento))
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento);
end;

procedure TACBrPagamentosBBConsultaRespostaBase.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('id', fid)
    .Value('estadoPagamento', i)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito)
    .Value('inicioCartaoCredito', finicioCartaoCredito)
    .Value('fimCartaoCredito', ffimCartaoCredito)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('codigoAutenticacaoPagamento', fcodigoAutenticacaoPagamento);

  if NaoEstaZerado(i) then
    festadoPagamento := IntegerToPagamentosBBEstado(i);
end;

procedure TACBrPagamentosBBConsultaRespostaBase.Clear;
begin
  fid := 0;
  festadoPagamento := pgeNenhum;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := EmptyStr;
  finicioCartaoCredito := 0;
  ffimCartaoCredito := 0;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fdocumentoDebito := 0;
  fcodigoAutenticacaoPagamento := EmptyStr;
end;

function TACBrPagamentosBBConsultaRespostaBase.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fid) and
    EstaZerado(Ord(festadoPagamento)) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito) and
    EstaZerado(finicioCartaoCredito) and
    EstaZerado(ffimCartaoCredito) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(fdocumentoDebito) and
    EstaVazio(fcodigoAutenticacaoPagamento);
end;

procedure TACBrPagamentosBBConsultaRespostaBase.Assign(aSource: TACBrPagamentosBBConsultaRespostaBase);
begin
  fid := ASource.id;
  festadoPagamento := ASource.estadoPagamento;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;
  finicioCartaoCredito := ASource.inicioCartaoCredito;
  ffimCartaoCredito := ASource.fimCartaoCredito;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fdocumentoDebito := ASource.documentoDebito;
  fcodigoAutenticacaoPagamento := ASource.codigoAutenticacaoPagamento;
end;

{ TACBrPagamentosBBOcorrenciaBase }

procedure TACBrPagamentosBBOcorrenciaBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBOcorrenciaBase) then
    Assign(TACBrPagamentosBBOcorrenciaBase(ASource));
end;

procedure TACBrPagamentosBBOcorrenciaBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('codigo', fcodigo);
end;

procedure TACBrPagamentosBBOcorrenciaBase.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('codigo', fcodigo);
end;

procedure TACBrPagamentosBBOcorrenciaBase.Clear;
begin
  fcodigo := 0;
end;

function TACBrPagamentosBBOcorrenciaBase.IsEmpty: Boolean;
begin
  Result := EstaZerado(fcodigo);
end;

procedure TACBrPagamentosBBOcorrenciaBase.Assign(aSource: TACBrPagamentosBBOcorrenciaBase);
begin
  fcodigo := aSource.codigo;
end;

{ TACBrPagamentosBBDevolucao }

procedure TACBrPagamentosBBDevolucao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBDevolucao) then
    Assign(TACBrPagamentosBBDevolucao(ASource));
end;

procedure TACBrPagamentosBBDevolucao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigoMotivo', fcodigoMotivo)
    .AddPair('dataDevolucao', DateToStr(fdataDevolucao))
    .AddPair('valorDevolucao', fvalorDevolucao);
end;

procedure TACBrPagamentosBBDevolucao.DoReadFromJSon(aJSon: TACBrJSONObject);
begin 
  aJSon
    .Value('codigoMotivo', fcodigoMotivo)
    .Value('dataDevolucao', fdataDevolucao)
    .Value('valorDevolucao', fvalorDevolucao);
end;

procedure TACBrPagamentosBBDevolucao.Clear;
begin
  fcodigoMotivo := 0;
  fdataDevolucao := 0;
  fvalorDevolucao := 0;
end;

function TACBrPagamentosBBDevolucao.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fcodigoMotivo) and
    EstaZerado(fdataDevolucao) and
    EstaZerado(fvalorDevolucao);
end;

procedure TACBrPagamentosBBDevolucao.Assign(aSource: TACBrPagamentosBBDevolucao);
begin
  fcodigoMotivo := ASource.codigoMotivo;
  fdataDevolucao := ASource.dataDevolucao;
  fvalorDevolucao := ASource.valorDevolucao;
end;

{ TACBrPagamentosBBDevolucaoBase }

procedure TACBrPagamentosBBDevolucaoBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBDevolucaoBase) then
    Assign(TACBrPagamentosBBDevolucaoBase(ASource));
end;

procedure TACBrPagamentosBBDevolucaoBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('codigoMotivo', fcodigoMotivo);
end;

procedure TACBrPagamentosBBDevolucaoBase.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('codigoMotivo', fcodigoMotivo);
end;

procedure TACBrPagamentosBBDevolucaoBase.Clear;
begin
  fcodigoMotivo := 0;
end;

function TACBrPagamentosBBDevolucaoBase.IsEmpty: Boolean;
begin
  Result := EstaZerado(fcodigoMotivo);
end;

procedure TACBrPagamentosBBDevolucaoBase.Assign(aSource: TACBrPagamentosBBDevolucaoBase);
begin
  fcodigoMotivo := aSource.codigoMotivo;
end;

{ TACBrPagamentosBBErroOAuth }

function TACBrPagamentosBBErroOAuth.Getattributes: TACBrPagamentosBBErroOAuthAttributes;
begin
  if (not Assigned(fattributes)) then
    fattributes := TACBrPagamentosBBErroOAuthAttributes.Create('attributes');
  Result := fattributes;
end;

procedure TACBrPagamentosBBErroOAuth.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBErroOAuth) then
    Assign(TACBrPagamentosBBErroOAuth(ASource));
end;

procedure TACBrPagamentosBBErroOAuth.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('statusCode', fstatusCode)
    .AddPair('error', ferror)
    .AddPair('message', fmessage);

  if Assigned(fattributes) then
    fattributes.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBErroOAuth.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('statusCode', fstatusCode)
    .Value('error', ferror)
    .Value('message', fmessage);

  if Assigned(fattributes) then
    fattributes.ReadFromJSon(aJSon);
end;

constructor TACBrPagamentosBBErroOAuth.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
end;

destructor TACBrPagamentosBBErroOAuth.Destroy;
begin
  if Assigned(fattributes) then
    fattributes.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBErroOAuth.Clear;
begin
  fstatusCode := 0;
  ferror := '';
  fmessage := '';

  if Assigned(fattributes) then
    fattributes.Clear;
end;

function TACBrPagamentosBBErroOAuth.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fstatusCode) and
    EstaVazio(ferror) and
    EstaVazio(fmessage) and
    fattributes.IsEmpty;
end;

procedure TACBrPagamentosBBErroOAuth.Assign(aSource: TACBrPagamentosBBErroOAuth);
begin
  fstatusCode := ASource.statusCode;
  ferror := ASource.error;
  fmessage := ASource.message;
  attributes.Assign(ASource.attributes);
end;

{ TACBrPagamentosBBErroOAuthAttribute }

procedure TACBrPagamentosBBErroOAuthAttribute.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBErroOAuthAttribute) then
    Assign(TACBrPagamentosBBErroOAuthAttribute(ASource));
end;

procedure TACBrPagamentosBBErroOAuthAttribute.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('error', ferror);
end;

procedure TACBrPagamentosBBErroOAuthAttribute.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('error', ferror);
end;

procedure TACBrPagamentosBBErroOAuthAttribute.Clear;
begin
  ferror := EmptyStr;
end;

function TACBrPagamentosBBErroOAuthAttribute.IsEmpty: Boolean;
begin
  Result := EstaVazio(ferror);
end;

procedure TACBrPagamentosBBErroOAuthAttribute.Assign(aSource: TACBrPagamentosBBErroOAuthAttribute);
begin
  ferror := aSource.error;
end;

{ TACBrPagamentosBBErro }

procedure TACBrPagamentosBBErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBErro) then
    Assign(TACBrPagamentosBBErro(ASource));
end;

procedure TACBrPagamentosBBErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigo', fcodigo)
    .AddPair('versao', fversao)
    .AddPair('mensagem', fmensagem)
    .AddPair('ocorrencia', focorrencia);
end;

procedure TACBrPagamentosBBErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigo', fcodigo)
    .Value('versao', fversao)
    .Value('mensagem', fmensagem)
    .Value('ocorrencia', focorrencia);
end;

procedure TACBrPagamentosBBErro.Clear;
begin
  fcodigo := '';
  fversao := '';
  fmensagem := '';
  focorrencia := '';
end;

function TACBrPagamentosBBErro.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcodigo) and
    EstaVazio(fversao) and
    EstaVazio(fmensagem) and
    EstaVazio(focorrencia);
end;

procedure TACBrPagamentosBBErro.Assign(aSource: TACBrPagamentosBBErro);
begin
  fcodigo := ASource.codigo;
  fversao := ASource.versao;
  fmensagem := ASource.mensagem;
  focorrencia := ASource.ocorrencia;
end;

end.
