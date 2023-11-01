using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.MDFe;

namespace ACBrLib.MDFe
{
    public sealed class MDFeConfig : ACBrLibDFeConfig<ACBrMDFe>
    {
        #region Constructors

        public MDFeConfig(ACBrMDFe acbrlib) : base(acbrlib, ACBrSessao.MDFe)
        {
            DAMDFe = new DAMDFeConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public DAMDFeConfig DAMDFe { get; }

        public bool EmissaoPathMDFe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathMDFe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathEvento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathArquivoMunicipios
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public VersaoMDFe VersaoDF
        {
            get => GetProperty<VersaoMDFe>();
            set => SetProperty(value);
        }

        public bool SalvarApenasMDFeProcessados
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool NormatizarMunicipios
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string IdCSRT
        {
            get => GetProperty<string>();
            set => SetProperty<string>(value);
        }

        public string CSRT
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }
        #endregion Properties
    }
}