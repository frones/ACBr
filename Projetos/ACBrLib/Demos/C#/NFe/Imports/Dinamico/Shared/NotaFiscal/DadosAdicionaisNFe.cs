using System.Collections.Generic;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Informações Adicionais da NF-e
    /// </summary>
    public class DadosAdicionaisNFe
    {
        #region Properties

        /// <summary>
        /// Informações Adicionais de Interesse do Fisco
        /// </summary>
        public string infAdFisco { get; set; }

        /// <summary>
        /// Informações Complementares de interesse do Contribuinte
        /// </summary>
        public string infCpl { get; set; }

        public List<InfoAdicionalNfe> ObsCont { get; } = new List<InfoAdicionalNfe>();

        public List<InfoAdicionalNfe> ObsFisco { get; } = new List<InfoAdicionalNfe>();

        public List<ProcRefNFe> ProcRef { get; } = new List<ProcRefNFe>();

        #endregion Properties
    }
}