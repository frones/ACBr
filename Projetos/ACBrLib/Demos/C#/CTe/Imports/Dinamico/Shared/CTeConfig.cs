using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public sealed class CTeConfig : ACBrLibDFeConfig<ACBrCTe>
    {
        #region Constructors

        public CTeConfig(ACBrCTe acbrlib) : base(acbrlib, ACBrSessao.CTe)
        {
            DACTe = new DACTeConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public DACTeConfig DACTe { get; }

        public bool EmissaoPathCTe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PathCTe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PathInu
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

        public ModeloCTe ModeloDF
        {
            get => GetProperty<ModeloCTe>();
            set => SetProperty(value);
        }

        public VersaoCTe VersaoDF
        {
            get => GetProperty<VersaoCTe>();
            set => SetProperty(value);
        }

        public bool SalvarApenasCTeProcessados
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool NormatizarMunicipios
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}