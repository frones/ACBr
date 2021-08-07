namespace ACBrLib.MDFe
{
    public sealed class PropVeicMDFe
    {
        public string CNPJCPF { get; set; }

        public string RNTRC { get; set; }

        public string xNome { get; set; }

        public string IE { get; set; } = "ISENTO";

        public string UFProp { get; set; }

        public TpProp tpProp { get; set; }
    }
}