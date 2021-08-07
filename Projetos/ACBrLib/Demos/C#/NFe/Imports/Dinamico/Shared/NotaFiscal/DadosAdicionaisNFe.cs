using System.Collections.Generic;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Informações Adicionais da NF-e
    /// </summary>
    public class DadosAdicionaisNFe : DadosAdicionais
    {
        #region Properties

        public List<InfoAdicionalNfe> ObsCont { get; } = new List<InfoAdicionalNfe>();

        public List<InfoAdicionalNfe> ObsFisco { get; } = new List<InfoAdicionalNfe>();

        public List<ProcRefNFe> ProcRef { get; } = new List<ProcRefNFe>();

        #endregion Properties
    }
}