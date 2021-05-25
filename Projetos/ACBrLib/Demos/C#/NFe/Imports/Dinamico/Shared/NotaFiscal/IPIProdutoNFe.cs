namespace ACBrLib.NFe
{
    /// <summary>
    /// Imposto sobre Produtos Industrializados
    /// </summary>
    public class IPIProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Código da situação tributária do IPI
        /// <para>00=Entrada com recuperação de crédito</para>
        /// <para>01=Entrada tributada com alíquota zero</para>
        /// <para>02=Entrada isenta</para>
        /// <para>03=Entrada não-tributada</para>
        /// <para>04=Entrada imune</para>
        /// <para>05=Entrada com suspensão</para>
        /// <para>49=Outras entradas</para>
        /// <para>50=Saída tributada</para>
        /// <para>51=Saída tributada com alíquota zero</para>
        /// <para>52=Saída isenta</para>
        /// <para>53=Saída não-tributada</para>
        /// <para>54=Saída imune</para>
        /// <para>55=Saída com suspensão</para>
        /// <para>99=Outras saídas</para>
        /// </summary>
        public CSTIPI CST { get; set; } = CSTIPI.ipi00;

        /// <summary>
        /// Classe de enquadramento do IPI para Cigarros e Bebidas
        /// </summary>
        public string clEnq { get; set; }

        /// <summary>
        /// CNPJ do produtor da mercadoria, quando diferente do emitente.
        /// <para>Somente para os casos de exportação direta ou indireta.</para>
        /// <para>Informar os zeros não significativos</para>
        /// </summary>
        public string CNPJProd { get; set; }

        /// <summary>
        /// Código do selo de controle IPI
        /// <para>Preenchimento conforme Atos Normativos editados pela Receita Federal</para>
        /// </summary>
        public string cSelo { get; set; }

        /// <summary>
        /// Quantidade de selo de controle
        /// </summary>
        public int? qSelo { get; set; }

        /// <summary>
        /// Código de Enquadramento Legal do IPI
        /// </summary>
        public string cEnq { get; set; }

        /// <summary>
        /// Valor da BC do IPI
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Quantidade total na unidade padrão para tributação (somente para os produtos tributados por unidade)
        /// </summary>
        public decimal? qUnid { get; set; }

        /// <summary>
        /// Valor por Unidade Tributável
        /// </summary>
        public decimal? vUnid { get; set; }

        /// <summary>
        /// Alíquota do IPI
        /// </summary>
        public decimal? pIPI { get; set; }

        /// <summary>
        /// Valor do IPI
        /// </summary>
        public decimal? vIPI { get; set; }

        #endregion Properties
    }
}