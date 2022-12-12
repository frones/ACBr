using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.eSocial;
using ACBrLib.Core.DFe;

namespace ACBrLib.NCM
{
    public sealed class ACBrNCMConfig : ACBrLibDFeConfig<ACBrNCM>
    {
        #region Constructors

        public ACBrNCMConfig(ACBrNCM acbrlib) : base(acbrlib, ACBrSessao.NCM)
        {

        }

        #endregion Constructors

        #region Properties

        #endregion Properties
    }
}