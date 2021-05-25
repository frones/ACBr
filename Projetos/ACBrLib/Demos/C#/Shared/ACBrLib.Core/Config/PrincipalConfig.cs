namespace ACBrLib.Core.Config
{
    public sealed class PrincipalConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public PrincipalConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.Principal)
        {
        }

        #endregion Constructors

        #region Properties

        public TipoResposta TipoResposta
        {
            get => GetProperty<TipoResposta>();
            set => SetProperty(value);
        }

        public CodResposta CodificacaoResposta
        {
            get => GetProperty<CodResposta>();
            set => SetProperty(value);
        }

        public NivelLog LogNivel
        {
            get => GetProperty<NivelLog>();
            set => SetProperty(value);
        }

        public string LogPath
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}