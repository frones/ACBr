using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.eSocial;
using ACBrLib.Core.DFe;

namespace ACBrLib.GTIN
{
    public sealed class ACBrGTINConfig : ACBrLibDFeConfig<ACBrGTIN>
    {
        #region Constructors

        public ACBrGTINConfig(ACBrGTIN acbrlib) : base(acbrlib, ACBrSessao.GTIN)
        {

        }

        #endregion Constructors

        #region Properties

        public string PathGTIN
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}