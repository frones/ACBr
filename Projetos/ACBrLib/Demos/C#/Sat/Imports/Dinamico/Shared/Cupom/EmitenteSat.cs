using ACBrLib.Core.Sat;

namespace ACBrLib.Sat
{
    public sealed class EmitenteSat
    {
        #region Properties

        public string CNPJ { get; set; }

        public string xNome { get; set; }

        public string xFant { get; set; }

        public string IE { get; set; }

        public string IM { get; set; }

        public RegTrib cRegTrib { get; set; }

        public RegTribISSQN cRegTribISSQN { get; set; }

        public indRatISSQN indRatISSQN { get; set; }

        public string xLgr { get; set; }

        public string nro { get; set; }

        public string xCpl { get; set; }

        public string xBairro { get; set; }

        public string xMun { get; set; }

        public string CEP { get; set; }

        #endregion Properties
    }
}