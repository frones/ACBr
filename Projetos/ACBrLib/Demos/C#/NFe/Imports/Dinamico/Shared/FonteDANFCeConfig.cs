using ACBrLib.Core;
using ACBrLib.Core.Config;

namespace ACBrLib.NFe
{
    public sealed class FonteDANFCeConfig : ACBrLibConfigBase<ACBrNFe>
    {
        #region Constructors

        public FonteDANFCeConfig(ACBrNFe acbrlib, ACBrSessao sessao) : base(acbrlib, sessao)
        {
            SubName = "FonteLinhaItem";
        }

        #endregion Constructors

        #region Properties

        public string Name
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Color
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int Size
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool Bold
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Italic
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Underline
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool StrikeOut
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}