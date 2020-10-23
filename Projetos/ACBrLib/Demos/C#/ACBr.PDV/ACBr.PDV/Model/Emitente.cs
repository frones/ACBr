using ACBrLib.Core.NFe;

namespace ACBr.PDV.Model
{
    public class Emitente
    {
        #region Constructors

        public Emitente()
        {
            CRT = CRT.crtSimplesNacional;
        }

        #endregion Constructors

        #region Properties

        public string CNPJ { get; set; }

        public string IE { get; set; }

        public string IM { get; set; }

        public string Razao { get; set; }

        public string Fantasia { get; set; }

        public string Fone { get; set; }

        public string CEP { get; set; }

        public string Logradouro { get; set; }

        public string Numero { get; set; }

        public string Complemento { get; set; }

        public string Bairro { get; set; }

        public string CidadeCod { get; set; }

        public string Cidade { get; set; }

        public string UF { get; set; }

        public CRT CRT { get; set; }

        #endregion Properties

        #region Methods

        public void Clear()
        {
            CNPJ = string.Empty;
            IE = string.Empty;
            IM = string.Empty;
            Razao = string.Empty;
            Fantasia = string.Empty;
            Fone = string.Empty;
            CEP = string.Empty;
            Logradouro = string.Empty;
            Numero = string.Empty;
            Complemento = string.Empty;
            Bairro = string.Empty;
            CidadeCod = string.Empty;
            Cidade = string.Empty;
            UF = string.Empty;
            CRT = CRT.crtSimplesNacional;
        }

        #endregion Methods
    }
}