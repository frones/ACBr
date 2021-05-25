namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterGavetaConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterGavetaConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter_QRCode)
        {
        }

        #endregion Constructors

        #region Properties

        public bool SinarInvertido
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public byte TempoON
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        public byte TempoOFF
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}