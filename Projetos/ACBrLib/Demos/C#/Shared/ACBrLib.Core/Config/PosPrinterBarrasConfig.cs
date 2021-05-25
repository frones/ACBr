namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterBarrasConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterBarrasConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter_Barras)
        {
        }

        #endregion Constructors

        #region Properties

        public bool MostrarCodigo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int LarguraLinha
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Altura
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Margem
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}