using ACBrLib.Core.PosPrinter;

namespace ACBrLib.Core.Config
{
    public sealed class PosPrinterMPaginaConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PosPrinterMPaginaConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.PosPrinter_QRCode)
        {
        }

        #endregion Constructors

        #region Properties

        public int Largura
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Altura
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Esquerda
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int Topo
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public PosDirecao Direcao
        {
            get => GetProperty<PosDirecao>();
            set => SetProperty(value);
        }

        public int EspacoEntreLinhas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}