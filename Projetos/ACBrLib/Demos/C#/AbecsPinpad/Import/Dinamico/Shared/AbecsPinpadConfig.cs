using ACBrLib.Core;
using ACBrLib.Core.AbecsPinpad;
using ACBrLib.Core.Config;

namespace ACBrLib.AbecsPinpad
{
    public sealed class AbecsPinpadConfig : ACBrLibConfig<ACBrAbecsPinpad>
    {
        #region Constructors

        public AbecsPinpadConfig(ACBrAbecsPinpad acbrlib) : base(acbrlib, ACBrSessao.AbecsPinpad)
        {
            Device = new DeviceConfig<ACBrAbecsPinpad>(acbrlib, ACBrSessao.AbecsPinpad_Device);
        }

        #endregion Constructors

        #region Properties

        public DeviceConfig<ACBrAbecsPinpad> Device { get; }

        public string PortaPinpad
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int TimeOut
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public string LogFile
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public MsgAlign MsgAlign
        {
            get => GetProperty<MsgAlign>();
            set => SetProperty(value);
        }

        public bool MsgWordWrap
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}