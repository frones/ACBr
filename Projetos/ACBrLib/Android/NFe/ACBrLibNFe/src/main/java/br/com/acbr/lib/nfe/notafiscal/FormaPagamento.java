package br.com.acbr.lib.nfe.notafiscal;

import java.util.HashMap;
import java.util.Map;

public class FormaPagamento {

    private static final Map<String, FormaPagamento> lookup = new HashMap<>();

    public static final FormaPagamento fpDinheiro = new FormaPagamento("01", "Dinheiro");
    public static final FormaPagamento fpCheque = new FormaPagamento("02", "Cheque");
    public static final FormaPagamento fpCartaoCredito = new FormaPagamento("03", "Cartão de Crédito");
    public static final FormaPagamento fpCartaoDebito = new FormaPagamento("04", "Cartão de Débito");
    public static final FormaPagamento fpCreditoLoja = new FormaPagamento("05", "Crédito Loja");
    public static final FormaPagamento fpValeAlimentacao = new FormaPagamento("10", "Vale Alimentação");
    public static final FormaPagamento fpValeRefeicao = new FormaPagamento("11", "Vale Refeição");
    public static final FormaPagamento fpValePresente = new FormaPagamento("12", "Vale Presente");
    public static final FormaPagamento fpValeCombustivel = new FormaPagamento("13", "Vale Combustível");
    public static final FormaPagamento fpDuplicataMercantil = new FormaPagamento("14", "Duplicata Mercantil");
    public static final FormaPagamento fpBoletoBancario = new FormaPagamento("15", "Boleto Bancário");
    public static final FormaPagamento fpDepositoBancario = new FormaPagamento("16", "Depósito Bancário");
    public static final FormaPagamento fpPagamentoInstantaneo = new FormaPagamento("17", "Pagamento Instantâneo");
    public static final FormaPagamento fpTransfBancario = new FormaPagamento("18", "Transferência Bancária");
    public static final FormaPagamento fpProgramaFidelidade = new FormaPagamento("19", "Programa de Fidelidade");
    public static final FormaPagamento fpPagamentoInstantaneoEstatico = new FormaPagamento("20", "Pagamento Instantâneo Estático");
    public static final FormaPagamento fpCreditoEmLojaPorDevolucao = new FormaPagamento("21", "Crédito Loja por Devolução");
    public static final FormaPagamento fpFalhaHardware = new FormaPagamento("22", "Falha Hardware");
    public static final FormaPagamento fpSemPagamento = new FormaPagamento("90", "Sem Pagamento");
    public static final FormaPagamento fpRegimeEspecial = new FormaPagamento("98", "Regime Especial");
    public static final FormaPagamento fpOutro = new FormaPagamento("99", "Outro");

    private final String value;
    private final String description;

    static {
        addToLookup(fpDinheiro);
        addToLookup(fpCheque);
        addToLookup(fpCartaoCredito);
        addToLookup(fpCartaoDebito);
        addToLookup(fpCreditoLoja);
        addToLookup(fpValeAlimentacao);
        addToLookup(fpValeRefeicao);
        addToLookup(fpValePresente);
        addToLookup(fpValeCombustivel);
        addToLookup(fpDuplicataMercantil);
        addToLookup(fpBoletoBancario);
        addToLookup(fpDepositoBancario);
        addToLookup(fpPagamentoInstantaneo);
        addToLookup(fpTransfBancario);
        addToLookup(fpProgramaFidelidade);
        addToLookup(fpPagamentoInstantaneoEstatico);
        addToLookup(fpCreditoEmLojaPorDevolucao);
        addToLookup(fpFalhaHardware);
        addToLookup(fpSemPagamento);
        addToLookup(fpRegimeEspecial);
        addToLookup(fpOutro);
    }

    private FormaPagamento(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(FormaPagamento formaPagamento) {
        lookup.put(formaPagamento.value, formaPagamento);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static FormaPagamento fromValue(String value) {
        FormaPagamento formaPagamento = lookup.get(value);
        if(formaPagamento == null){
            throw new IllegalArgumentException("Forma de Pagamento inválida: " + value);
        } else {
            return formaPagamento;
        }
    }

    @Override
    public String toString() {
        return value;
    }
}
