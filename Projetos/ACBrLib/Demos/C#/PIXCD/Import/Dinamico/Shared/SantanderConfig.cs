using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class SantanderConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public SantanderConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.Santander)
        {

        }

        #endregion Constructors

        #region Properties

        public string ChavePIX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ConsumerKey
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ConsumerSecret
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqCertificadoPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string SenhaCertificadoPFX
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}