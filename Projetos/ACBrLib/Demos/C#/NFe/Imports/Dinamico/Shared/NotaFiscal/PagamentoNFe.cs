using System;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Formas de Pagamento
    /// </summary>
    public class PagamentoNFe
    {

        public IndicadorPagamento indPag { get; set; }

        /// <summary>
        /// Forma de pagamento
        /// <para>01=Dinheiro</para>
        /// <para>02=Cheque</para>
        /// <para>03=Cartão de Crédito</para>
        /// <para>04=Cartão de Débito</para>
        /// <para>05=Crédito Loja</para>
        /// <para>10=Vale Alimentação</para>
        /// <para>11=Vale Refeição</para>
        /// <para>12=Vale Presente</para>
        /// <para>13=Vale Combustível</para>
        /// <para>99=Outros</para>
        /// </summary>
        public FormaPagamento tPag { get; set; }

        public string xPag { get; set; }

        /// <summary>
        /// Valor do Pagamento
        /// </summary>
        public decimal vPag { get; set; }

        public DateTime dPag { get; set; }

        /// <summary>
        /// Grupo Opcional
        /// </summary>
        public string CNPJPag { get; set; }

        public string UFPag { get; set; }

        public TpIntegra tpIntegra { get; set; }

        /// <summary>
        /// CNPJ da Credenciadora de cartão de crédito e/ou débito
        /// </summary>
        public string CNPJ { get; set; }

        /// <summary>
        /// Bandeira da operadora de cartão de crédito e/ou débito
        /// <para>01=Visa</para>
        /// <para>02=Mastercard</para>
        /// <para>03=American Express</para>
        /// <para>04=Sorocred</para>
        /// <para>99=Outros</para>
        /// </summary>
        public BandeiraCartao tBand { get; set; }

        /// <summary>
        /// Número de autorização da operação cartão de crédito e/ou débito
        /// </summary>
        public string cAut { get; set; }

        public string CNPJReceb { get; set; }

        public string idTermPag { get; set; }

        public decimal vTroco { get; set; }
    }
}