using System.Collections.Generic;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Produtos e Serviços da NF-e
    /// </summary>
    public class ProdutoNFe
    {
        #region Constructors

        public ProdutoNFe()
        {

            NVE = new List<NVENFe>();
            DI = new List<DINFe>();
            DetExport = new List<DetExport>();
            Rastro = new List<RastroNFe>();
            Medicamento = new List<MedicamentoNFe>();
            Arma = new List<ArmaNFe>();
            ImpostoDevol = new ImpostoDevolNFe();
            Veiculo = new VeiculoNFe();
            Combustivel = new CombustivelNFe();
            ICMS = new ICMSProdutoNFe();
            ICMSUFDEST = new ICMSUFDESTNFe();
            IPI = new IPIProdutoNFe();
            II = new IIProdutoNFe();
            PIS = new PISProdutoNFe();
            PISST = new PISSTProdutoNFe();
            COFINS = new COFINSProdutoNFe();
            COFINSST = new COFINSSTProdutoNFe();
            ISSQN = new ISSQNNFe();
            gCred = new CreditoPresumidoNFe();
        }

        #endregion Constructors

        #region Properties

        /// <summary>
        /// Número do item (1-990)
        /// </summary>
        public int? nItem { get; set; }



        /// <summary>
        /// Código do produto ou serviço
        /// <para>Preencher com CFOP, caso se trate de itens não
        /// relacionados com mercadorias/produtos e que o
        /// contribuinte não possua codificação própria.</para>
        /// <para>Formato: "CFOP9999"</para>
        /// </summary>
        public string cProd { get; set; }

        /// <summary>
        /// GTIN (Global Trade Item Number) do produto, antigo código EAN ou código de barras
        /// <para>Preencher com o código GTIN-8, GTIN-12, GTIN-13 ou
        /// GTIN-14 (antigos códigos EAN, UPC e DUN-14), não
        /// informar o conteúdo da TAG em caso de o produto não
        /// possuir este código.</para>
        /// </summary>
        public string cEAN { get; set; }

        /// <summary>
        /// GTIN (Global Trade Item Number) da
        /// unidade tributável, antigo código EAN
        /// ou código de barras
        /// <para>Preencher com o código GTIN-8, GTIN-12, GTIN-13 ou
        /// GTIN-14 (antigos códigos EAN, UPC e DUN-14) da
        /// unidade tributável do produto, não informar o conteúdo da
        /// TAG em caso de o produto não possuir este código.</para>
        /// </summary>
        public string cEANTrib { get; set; }

        /// <summary>
        /// Código de barras diferente do padrão GTIN
        /// </summary>
        public string cBarra { get; set; }

        /// <summary>
        /// Código de Barras da unidade tributável que seja diferente do padrão GTIN
        /// </summary>
        public string cBarraTrib { get; set; }

        /// <summary>
        /// Descrição do produto ou serviço
        /// </summary>
        public string xProd { get; set; }

        /// <summary>
        /// Código NCM com 8 dígitos ou 2 dígitos(gênero)
        /// <para>Código NCM (8 posições), informar o Capítulo do NCM
        /// (gênero) quando a operação não for de comércio exterior
        /// (importação/exportação) ou o produto não seja tributado
        /// pelo IPI.</para>
        /// <para>Em caso de item de serviço ou item que não
        /// tenham produto (ex.transferência de crédito, crédito do
        /// ativo imobilizado, etc.), informar o código 00 (zeros)(v2.0)</para>
        /// </summary>
        public string NCM { get; set; }

        public string CEST { get; set; }

        /// <summary>
        /// EX_TIPI
        /// <para>Preencher de acordo com o código EX da TIPI. Em caso de serviço, não incluir a TAG.</para>
        /// </summary>
        public string EXTIPI { get; set; }

        /// <summary>
        /// Código Fiscal de Operações e Prestações.
        /// Utilizar Tabela de CFOP.
        /// </summary>
        public string CFOP { get; set; }

        /// <summary>
        /// Unidade Comercial
        /// </summary>
        public string uCom { get; set; }

        /// <summary>
        /// Quantidade Comercial
        /// </summary>
        public decimal qCom { get; set; }

        /// <summary>
        /// Valor Unitário de Comercialização
        /// <para>Informar o valor unitário de comercialização do produto,
        /// campo meramente informativo, o contribuinte pode utilizar
        /// a precisão desejada(0-10 decimais).</para>
        /// <para>Para efeitos de
        /// cálculo, o valor unitário será obtido pela divisão do valor
        /// do produto pela quantidade comercial. (v2.0)</para>
        /// </summary>
        public decimal vUnCom { get; set; }

        /// <summary>
        /// Valor Total Bruto dos Produtos ou Serviços
        /// </summary>
        public decimal vProd { get; set; }

        /// <summary>
        /// Unidade Tributável
        /// </summary>
        public string uTrib { get; set; }

        /// <summary>
        /// Quantidade Tributável
        /// </summary>
        public decimal qTrib { get; set; }

        /// <summary>
        /// Valor Unitário de tributação
        /// <para>Informar o valor unitário de tributação do produto, campo
        /// meramente informativo, o contribuinte pode utilizar a
        /// precisão desejada(0-10 decimais).</para>
        /// <para>Para efeitos de
        /// cálculo, o valor unitário será obtido pela divisão do valor
        /// do produto pela quantidade tributável (NT 2013/003).</para>
        /// </summary>
        public decimal vUnTrib { get; set; }

        /// <summary>
        /// Valor Total do Frete
        /// </summary>
        public decimal? vFrete { get; set; }

        /// <summary>
        /// Valor Total do Seguro
        /// </summary>
        public decimal? vSeg { get; set; }

        /// <summary>
        /// Valor do Desconto
        /// </summary>
        public decimal? vDesc { get; set; }

        /// <summary>
        /// Outras despesas acessórias
        /// </summary>
        public decimal? vOutro { get; set; }

        /// <summary>
        /// Indica se valor do Item (vProd) entra no valor total da NF-e (vProd)
        /// <para>0=Valor do item (vProd) não compõe o valor total da NF-e</para>
        /// <para>1=Valor do item (vProd) compõe o valor total da NF-e (vProd) (v2.0)</para>
        /// </summary>
        public IndicadorTotal indTot { get; set; }

        /// <summary>
        /// Número do Pedido de Compra (Opcional)
        /// </summary>
        public string xPed { get; set; }

        /// <summary>
        /// Item do Pedido de Compra (Opcional caso xPed esteja vazio)
        /// </summary>
        public int? nItemPed { get; set; }

        /// <summary>
        /// Número de controle da FCI - Ficha de Conteúdo de Importação (Opcional)
        /// <para>Informação relacionada com a Resolução 13/2012 do
        /// Senado Federal.</para>
        /// <para>Formato: Algarismos, letras maiúsculas
        /// de "A" a "F" e o caractere hífen. Exemplo:
        /// B01F70AF-10BF-4B1F-848C-65FF57F616FE</para>
        /// </summary>
        public string nFCI { get; set; }

        /// <summary>
        /// Número do RECOPI
        /// </summary>
        public string nRECOPI { get; set; }

        /// <summary>
        /// Percentual da mercadoria devolvida
        /// <para>Observação: O valor máximo deste percentual é 100%,
        /// no caso de devolução total da mercadoria.</para>
        /// </summary>
        public decimal pDevol { get; set; }

        /// <summary>
        /// Valor do IPI devolvido
        /// </summary>
        public decimal vIPIDevol { get; set; }

        /// <summary>
        /// Valor aproximado total de tributos federais, estaduais e municipais.
        /// </summary>
        public decimal? vTotTrib { get; set; }

        /// <summary>
        /// Informações Adicionais do Produto
        /// </summary>
        public string infAdProd { get; set; }

        public IndEscala indEscala { get; set; }

        public string CNPJFab { get; set; }

        public string cBenef { get; set; }

        public CreditoPresumidoNFe gCred { get; set; }

        public List<NVENFe> NVE { get; }

        public List<DINFe> DI { get; }

        public List<DetExport> DetExport { get; }

        public List<RastroNFe> Rastro { get; }

        public List<MedicamentoNFe> Medicamento { get; set; }

        public List<ArmaNFe> Arma { get; set; }

        public ImpostoDevolNFe ImpostoDevol { get; }

        public VeiculoNFe Veiculo { get; }

        public CombustivelNFe Combustivel { get; }

        public ICMSProdutoNFe ICMS { get; }

        public ICMSUFDESTNFe ICMSUFDEST { get; }

        public IPIProdutoNFe IPI { get; }

        public IIProdutoNFe II { get; }

        public PISProdutoNFe PIS { get; }

        public PISSTProdutoNFe PISST { get; }

        public COFINSProdutoNFe COFINS { get; }

        public COFINSSTProdutoNFe COFINSST { get; }

        public ISSQNNFe ISSQN { get; }

        #endregion Properties
    }

    public class ISSQNNFe
    {
        public decimal? vBC { get; set; }

        public decimal? vAliq { get; set; }

        public decimal? vISSQN { get; set; }

        public int cMunFG { get; set; }

        public string cListServ { get; set; }

        public ISSQNcSitTrib cSitTrib { get; set; }

        public decimal? vDeducao { get; set; }

        public decimal? vOutro { get; set; }

        public decimal? vDescIncond { get; set; }

        public decimal? vDescCond { get; set; }

        public decimal? vISSRet { get; set; }

        public IndISS indISS { get; set; }

        public string cServico { get; set; }

        public string cMun { get; set; }

        public string cPais { get; set; }

        public string nProcesso { get; set; }

        public IndIncentivo indIncentivo { get; set; }
    }
}