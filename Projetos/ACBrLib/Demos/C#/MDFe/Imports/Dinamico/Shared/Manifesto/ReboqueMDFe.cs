namespace ACBrLib.MDFe
{
    public sealed class ReboqueMDFe
    {
        public string cInt { get; set; }

        public string placa { get; set; }

        public string RENAVAM { get; set; }

        public int tara { get; set; }

        public int capKG { get; set; }

        public int capM3 { get; set; }

        public TipoCarroceria tpCar { get; set; }

        public string UF { get; set; }

        public PropVeicMDFe Proprietario { get; } = new PropVeicMDFe();
    }
}