using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class BradescoConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public BradescoConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.Bradesco)
        {

        }

        #endregion Constructors

        #region Properties

        public string ChavePIX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ClientID
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ClientSecret
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string SenhaPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}