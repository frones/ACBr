namespace ACBrLib.MDFe
{
    public sealed class DispMDFe
    {
        public string CNPJForn { get; set; }

        public string CNPJPg { get; set; }

        public string nCompra { get; set; }

        public decimal vValePed { get; set; }

        public TpValePed tpValePed { get; set; } = TpValePed.tvpNenhum;
    }
}