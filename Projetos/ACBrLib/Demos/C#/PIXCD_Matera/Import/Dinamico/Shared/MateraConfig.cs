using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class MateraConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public MateraConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.Matera)
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

        public string SecretKey
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ClientSecret
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqCertificado
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqChavePrivada
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string AccountID
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string MediatorFee
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