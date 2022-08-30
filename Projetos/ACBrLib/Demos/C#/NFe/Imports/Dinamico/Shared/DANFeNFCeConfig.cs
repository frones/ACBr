using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class DANFeNFCeConfig : ACBrLibConfigBase<ACBrNFe>
    {
        #region Constructors

        public DANFeNFCeConfig(ACBrNFe acbrlib) : base(acbrlib, ACBrSessao.DANFENFCe)
        {
            Fonte = new FonteDANFCeConfig(acbrlib, ACBrSessao.DANFENFCe);
        }

        #endregion Constructors

        #region Properties

        public TipoRelatorioBobina TipoRelatorioBobina
        {
            get => GetProperty<TipoRelatorioBobina>();
            set => SetProperty(value);
        }

        public TipoRelatorioEvento TipoRelatorioEvento
        {
            get => GetProperty<TipoRelatorioEvento>();
            set => SetProperty(value);
        }

        public int LarguraBobina
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool ImprimeDescAcrescItem
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeItens
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ViaConsumidor
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public decimal vTroco
        {
            get => GetProperty<decimal>();
            set => SetProperty(value);
        }

        public bool ImprimeQRCodeLateral
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeLogoLateral
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int EspacoFinal
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoLogoHeight
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int TamanhoLogoWidth
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public DescricaoPagamentos DescricaoPagamentos
        {
            get => GetProperty<DescricaoPagamentos>();
            set => SetProperty(value);
        }

        public bool ImprimeEmUmaLinha
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeEmDuasLinhas
        {
            get => GetProperty<bool>();
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

        public FonteDANFCeConfig Fonte { get; }

        #endregion Properties
    }
}