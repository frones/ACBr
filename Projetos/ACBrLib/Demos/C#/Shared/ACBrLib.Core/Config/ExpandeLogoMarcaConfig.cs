namespace ACBrLib.Core.Config
{
    public sealed class ExpandeLogoMarcaConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public ExpandeLogoMarcaConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "ExpandeLogoMarca";
        }

        #endregion Constructors

        #region Properties

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

        public int Largura
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool Dimensionar
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Esticar
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}