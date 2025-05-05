using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class AppLessConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public AppLessConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.AppLess)
        {

        }

        #endregion Constructors

        #region Properties

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

		public string SecretKeyHMAC
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Scopes
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}