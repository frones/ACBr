using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class DANFeConfig : ReportConfig<ACBrNFe>
    {
        #region Constructors

        public DANFeConfig(ACBrNFe acbrlib) : base(acbrlib, ACBrSessao.DANFE)
        {
            NFe = new DANFeNFeConfig(acbrlib);
            NFCe = new DANFeNFCeConfig(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public DANFeNFeConfig NFe { get; }

        public DANFeNFCeConfig NFCe { get; }

        public TipoDANFE TipoDANFE
        {
            get => GetProperty<TipoDANFE>();
            set => SetProperty(value);
        }

        public bool ImprimeTotalLiquido
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public decimal vTribFed
        {
            get => GetProperty<decimal>();
            set => SetProperty(value);
        }

        public decimal vTribEst
        {
            get => GetProperty<decimal>();
            set => SetProperty(value);
        }

        public decimal vTribMun
        {
            get => GetProperty<decimal>();
            set => SetProperty(value);
        }

        public string FonteTributos
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ChaveTributos
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public ImprimeTributos ImprimeTributos
        {
            get => GetProperty<ImprimeTributos>();
            set => SetProperty(value);
        }

        public bool ExibeTotalTributosItem
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeCodigoEan
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public ExibeInfoAdicProduto ExibeInforAdicProduto
        {
            get => GetProperty<ExibeInfoAdicProduto>();
            set => SetProperty(value);
        }

        public bool QuebraLinhaEmDetalhamentos
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeNomeFantasia
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Cancelada
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string Protocolo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}