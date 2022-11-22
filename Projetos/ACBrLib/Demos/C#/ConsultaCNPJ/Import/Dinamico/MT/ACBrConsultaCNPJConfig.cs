using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;

namespace ACBrLib.ConsultaCNPJ
{
    public sealed class ACBrCNPJConfig : ACBrLibDFeConfig<ACBrConsultaCNPJ>
    {
        #region Constructors

        public ACBrCNPJConfig(ACBrConsultaCNPJ acbrlib) : base(acbrlib, ACBrSessao.ConsultaCNPJ)
        {

        }

        #endregion Constructors

        #region Properties

        #endregion Properties
    }
}