namespace ACBrLib.Core.Config
{
    public sealed class CasasDecimaisConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public CasasDecimaisConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "CasasDecimais";
        }

        #endregion Constructors

        #region Properties

        public string Formato
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string MaskqCom
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string MaskvUnCom
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int qCom
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int vUnCom
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}