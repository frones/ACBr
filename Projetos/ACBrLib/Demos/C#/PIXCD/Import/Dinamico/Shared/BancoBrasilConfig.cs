using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class BancoBrasilConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public BancoBrasilConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.BancoBrasil)
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

        public string DeveloperApplicationKey
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

        public BBAPIVersao BBAPIVersao
        {
            get => GetProperty<BBAPIVersao>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}