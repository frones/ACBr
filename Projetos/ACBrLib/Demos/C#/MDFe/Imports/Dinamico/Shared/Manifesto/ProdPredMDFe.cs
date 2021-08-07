namespace ACBrLib.MDFe
{
    public sealed class ProdPredMDFe
    {
        public TpCarga tpCarga { get; set; }

        public string xProd { get; set; }

        public string cEAN { get; set; }

        public string NCM { get; set; }

        public InfLocalMDFe InfLocalCarrega { get; } = new InfLocalMDFe();

        public InfLocalMDFe InfLocalDescarrega { get; } = new InfLocalMDFe();
    }
}