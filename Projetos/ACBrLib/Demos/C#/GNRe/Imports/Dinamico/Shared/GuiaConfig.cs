using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;

namespace ACBrLib.GNRe
{
    public sealed class GuiaConfig : ACBrLibConfigBase<ACBrGNRe>
    {
        #region Constructors

        public GuiaConfig(ACBrGNRe acbrlib) : base(acbrlib, ACBrSessao.Guia)
        {
        }

        #endregion Constructors

        #region Properties

        public string PathPDF
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool PrintDialog
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string Impressora
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool MostraPreview
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool MostraStatus
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int Copias
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int MargemInferior
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int MargemSuperior
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int MargemEsquerda
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int MargemDireita
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public TamanhoPapel TamanhoPapel
        {
            get => GetProperty<TamanhoPapel>();
            set => SetProperty(value);
        }

        public string Usuario
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}