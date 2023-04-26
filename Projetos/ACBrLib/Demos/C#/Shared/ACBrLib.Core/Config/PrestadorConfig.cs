namespace ACBrLib.Core.Config
{
    public sealed class PrestadorConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PrestadorConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "Prestador";
        }

        #endregion Constructors

        #region Properties

        public string Logo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}