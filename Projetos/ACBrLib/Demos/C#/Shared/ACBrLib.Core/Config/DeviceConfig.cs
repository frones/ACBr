using ACBrLib.Core.Serial;

namespace ACBrLib.Core.Config
{
    public sealed class DeviceConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public DeviceConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
        }

        #endregion Constructors

        #region Properties

        public SerialBaud Baud
        {
            get => GetProperty<SerialBaud>();
            set => SetProperty(value);
        }

        public int Data
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TimeOut
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public SerialParity Parity
        {
            get => GetProperty<SerialParity>();
            set => SetProperty(value);
        }

        public SerialStopBytes Stop
        {
            get => GetProperty<SerialStopBytes>();
            set => SetProperty(value);
        }

        public int MaxBandwidth
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int SendBytesCount
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int SendBytesInterval
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public SerialHandShake HandShake
        {
            get => GetProperty<SerialHandShake>();
            set => SetProperty(value);
        }

        public bool SoftFlow
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool HardFlow
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}