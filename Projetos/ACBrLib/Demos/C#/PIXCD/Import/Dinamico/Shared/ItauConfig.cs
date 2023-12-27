using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class ItauConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public ItauConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.Itau)
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

        public string ArqChavePrivada
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqCertificado
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}