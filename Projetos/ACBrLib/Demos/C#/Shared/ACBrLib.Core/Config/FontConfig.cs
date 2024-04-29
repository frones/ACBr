using ACBrLib.Core.DFe;

namespace ACBrLib.Core.Config
{
    public sealed class FontConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public FontConfig(TLib acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "Fonte";
        }

        #endregion Constructors

        #region Properties

        public FonteNome Nome
        {
            get => GetProperty<FonteNome>();
            set => SetProperty(value);
        }

        public bool Negrito
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int TamanhoFonteRazaoSocial
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoFonteEndereco
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoFonteDemaisCampos
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoFonteInformacoesComplementares
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}