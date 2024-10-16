using System;

namespace ACBrLib.Boleto
{
    public sealed class RetornoTituloWeb
    {
        public string CodBarras { get; set; }

        public string LinhaDig { get; set; }

        public string URL { get; set; }

        public string Instrucao1 { get; set; }

        public string Instrucao2 { get; set; }

        public string Instrucao3 { get; set; }

        public int Parcela { get; set; }

        public decimal PercentualMulta { get; set; }

        public bool MultaValorFixo { get; set; }

        public string SeuNumero { get; set; }

        public TipoDiasIntrucao TipoDiasProtesto { get; set; }

        public DateTime Vencimento { get; set; }

        public DateTime DataDocumento { get; set; }

        public string NumeroDocumento { get; set; }

        public string EspecieDoc { get; set; }

        public AceiteTitulo Aceite { get; set; }

        public DateTime DataProcessamento { get; set; }

        public string NossoNumero { get; set; }

        public string NossoNumeroCorrespondente { get; set; }

        public string UsoBanco { get; set; }

        public string Carteira { get; set; }

        public string EspecieMod { get; set; }

        public decimal ValorDocumento { get; set; }

        public string Mensagem { get; set; }

        public string Informativo { get; set; }

        public string Instrucoes { get; set; }

        public RetornoSacadoWeb Sacado { get; } = new RetornoSacadoWeb();

        public RetornoSacadoAvalistaWeb SacadoAvalista { get; } = new RetornoSacadoAvalistaWeb();

        public DateTime DataCredito { get; set; }

        public DateTime DataAbatimento { get; set; }

        public DateTime DataDesconto { get; set; }

        public DateTime DataDesconto2 { get; set; }

        public DateTime DataMoraJuros { get; set; }

        public DateTime DataMulta { get; set; }

        public DateTime DataProtesto { get; set; }

        public int DiasDeProtesto { get; set; }

        public DateTime DataBaixa { get; set; }

        public DateTime DataLimitePagto { get; set; }

        public decimal ValorDespesaCobranca { get; set; }

        public decimal ValorAbatimento { get; set; }

        public decimal ValorDesconto { get; set; }

        public decimal ValorDesconto2 { get; set; }

        public decimal ValorMoraJuros { get; set; }

        public decimal ValorIOF { get; set; }

        public decimal ValorOutrasDespesas { get; set; }

        public decimal ValorOutrosCreditos { get; set; }

        public decimal ValorRecebido { get; set; }

        public string CodigoMora { get; set; }

        public ACBrCarteiraEnvio CarteiraEnvio { get; set; }

        public ACBrCodigoNegativacao CodigoNegativacao { get; set; }

        public ACBrCodigoDesconto CodigoDesconto { get; set; }

        public CodigoJuros CodigoMoraJuros { get; set; }

        public ACBrCodigoMulta CodigoMulta { get; set; }

        public decimal ValorPago { get; set; }

        public ACBrCaracTitulo CaracTitulo { get; set; }

        public TipoPagamento TipoPagamento { get; set; }

        public int QtdePagamentoParcial { get; set; }

        public int QtdeParcelas { get; set; }

        public decimal ValorMinPagamento { get; set; }

        public decimal ValorMaxPagamento { get; set; }

        public decimal PercentualMinPagamento { get; set; }

        public decimal PercentualMaxPagamento { get; set; }

        public string emv { get; set; }

        public string url_Pix { get; set; }

        public string Tx_ID { get; set; }
    }
}