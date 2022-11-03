using ACBrLib.Core.GNRe;
using ACBrLib.Core.DFe;

namespace ACBrLib.GNRe
{
    public class EmitenteGNRe
    {
        #region Properties
        public Tipo Tipo { get; set; }
        
        public string IE { get; set; }

        public string ID { get; set; }

        public string RazaoSocial { get; set; }

        public string Endereco { get; set; }

        public string Cidade { get; set; }

        public DFeUF UF { get; set; }

        public string CEP { get; set; }

        public string Telefone { get; set; }

        #endregion Properties
    }
}