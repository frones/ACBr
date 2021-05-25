namespace ACBrLib.Core.Config
{
    public class DownloadConfig<TLib> : ACBrLibConfig<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public DownloadConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "Download";
        }

        #endregion Constructors

        #region Properties

        public string PathDownload
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string SepararPorNome
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}