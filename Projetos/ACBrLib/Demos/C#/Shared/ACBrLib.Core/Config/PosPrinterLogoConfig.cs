namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterLogoConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterLogoConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter_Logo)
        {
        }

        #endregion Constructors

        #region Properties

        public bool IgnorarLogo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public byte KeyCode1
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        public byte KeyCode2
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        public byte FatorX
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        public byte FatorY
        {
            get => GetProperty<byte>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}