using ACBrLib.Core;
using ACBrLib.Core.Config;

namespace ACBrLib.Boleto
{
    public sealed class CedenteWebserviceConfig : ACBrLibConfigBase<ACBrBoleto>
    {
        #region Constructors

        public CedenteWebserviceConfig(ACBrBoleto acbrlib) : base(acbrlib, ACBrSessao.BoletoCedenteWS)
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

        public string KeyUser
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Scope
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool IndicadorPix
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}