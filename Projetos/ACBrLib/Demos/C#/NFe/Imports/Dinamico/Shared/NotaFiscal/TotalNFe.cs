namespace ACBrLib.NFe
{
    /// <summary>
    /// O grupo de valores totais da NF-e deve ser informado com o somatório do campo correspondente dos itens.
    /// </summary>
    public class TotalNFe
    {
        #region Properties

        /// <summary>
        /// Base de Cálculo do ICMS
        /// </summary>
        public decimal vBC { get; set; }

        /// <summary>
        /// Valor Total do ICMS
        /// </summary>
        public decimal vICMS { get; set; }

        /// <summary>
        /// Valor Total do ICMS desonerado
        /// </summary>
        public decimal vICMSDeson { get; set; }

        /// <summary>
        /// Base de Cálculo do ICMS ST
        /// </summary>
        public decimal vBCST { get; set; }

        /// <summary>
        /// Valor Total do ICMS ST
        /// </summary>
        public decimal vST { get; set; }

        /// <summary>
        /// Valor Total dos produtos e serviços
        /// </summary>
        public decimal vProd { get; set; }

        /// <summary>
        /// Valor Total do Frete
        /// </summary>
        public decimal vFrete { get; set; }

        /// <summary>
        /// Valor Total do Seguro
        /// </summary>
        public decimal vSeg { get; set; }

        /// <summary>
        /// Valor Total do Desconto
        /// </summary>
        public decimal vDesc { get; set; }

        /// <summary>
        /// Valor Total do II
        /// </summary>
        public decimal vII { get; set; }

        /// <summary>
        /// Valor Total do IPI
        /// </summary>
        public decimal vIPI { get; set; }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        public decimal vPIS { get; set; }

        /// <summary>
        /// Valor da COFINS
        /// </summary>
        public decimal vCOFINS { get; set; }

        /// <summary>
        /// Outras Despesas acessórias
        /// </summary>
        public decimal vOutro { get; set; }

        /// <summary>
        /// Valor Total da NF-e
        /// </summary>
        public decimal vNF { get; set; }

        /// <summary>
        /// Valor aproximado total de tributos federais, estaduais e municipais.
        /// </summary>
        public decimal? vTotTrib { get; set; }

        public decimal? vFCP { get; set; }

        public decimal? vFCPST { get; set; }

        public decimal? vFCPSTRet { get; set; }

        public decimal? vIPIDevol { get; set; }

        public decimal? vFCPUFDest { get; set; }

        public decimal? vICMSUFDest { get; set; }

        public decimal? vICMSUFRemet { get; set; }

        public decimal? qBCMono { get; set; }

        public decimal? vICMSMono { get; set; }

        public decimal? qBCMonoReten { get; set; }

        public decimal? vICMSMonoReten { get; set; }

        public decimal? qBCMonoRet { get; set; }

        public decimal? vICMSMonoRet { get; set; }

        #endregion Properties
    }
}