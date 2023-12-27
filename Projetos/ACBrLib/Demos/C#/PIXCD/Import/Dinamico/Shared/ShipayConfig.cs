using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.PIXCD;

namespace ACBrLib.PIXCD
{
    public sealed class ShipayConfig : ACBrLibDFeConfig<ACBrPIXCD>
    {
        #region Constructors

        public ShipayConfig(ACBrPIXCD acbrlib) : base(acbrlib, ACBrSessao.Shipay)
        {

        }

        #endregion Constructors

        #region Properties

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

        public string AccessKey
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }


        #endregion Properties
    }
}