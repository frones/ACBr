using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.eSocial;
using ACBrLib.Core.DFe;

namespace ACBrLib.eSocial
{
    public sealed class ACBreSocialConfig : ACBrLibDFeConfig<ACBreSocial>
    {
        #region Constructors

        public ACBreSocialConfig(ACBreSocial acbrlib) : base(acbrlib, ACBrSessao.eSocial)
        {

        }

        #endregion Constructors

        #region Properties

        public string IdEmpregador
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string IdTransmissor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TipoEmpregador TipoEmpregador
        {
            get => GetProperty<TipoEmpregador>();
            set => SetProperty(value);
        }

        public string VersaoDF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool EmissaoPatheSocial
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PatheSocial
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}