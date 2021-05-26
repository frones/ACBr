namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterQRCodeConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterQRCodeConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter_QRCode)
        {
        }

        #endregion Constructors

        #region Properties

        public int Tipo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int LarguraModulo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int ErrorLevel
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}