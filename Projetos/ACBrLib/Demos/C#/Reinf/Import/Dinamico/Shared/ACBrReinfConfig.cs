using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.Reinf;

namespace ACBrLib.Reinf
{
    public sealed class ACBrReinfConfig : ACBrLibDFeConfig<ACBrReinf>
    {
        #region Constructors

        public ACBrReinfConfig(ACBrReinf acbrlib) : base(acbrlib, ACBrSessao.Reinf)
        {

        }

        #endregion Constructors

        #region Properties

        public string IdContribuinte
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string IdTransmissor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TipoContribuinte TipoContribuinte
        {
            get => GetProperty<TipoContribuinte>();
            set => SetProperty(value);
        }

        public string VersaoDF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool EmissaoPathReinf
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathReinf
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}