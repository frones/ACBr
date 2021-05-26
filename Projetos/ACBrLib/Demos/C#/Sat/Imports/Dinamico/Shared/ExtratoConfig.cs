using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.Sat;

namespace ACBrLib.Sat
{
    public sealed class ExtratoConfig : ReportConfig<ACBrSat>
    {
        #region Constructors

        public ExtratoConfig(ACBrSat acbrlib) : base(acbrlib, ACBrSessao.Extrato)
        {
        }

        #endregion Constructors

        #region Properties

        public TipoExtrato Tipo
        {
            get => GetProperty<TipoExtrato>();
            set => SetProperty(value);
        }

        public bool ImprimeQRCode
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeMsgOlhoNoImposto
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeCPFNaoInformado
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public SATExtratoFiltro Filtro
        {
            get => GetProperty<SATExtratoFiltro>();
            set => SetProperty(value);
        }

        public string MsgAppQRCode
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool ImprimeEmUmaLinha
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeDescAcrescItem
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeCodigoEan
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int LarguraBobina
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int EspacoFinal
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int LogoWidth
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public int LogoHeigth
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool LogoAutoSize
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool LogoCenter
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool LogoVisible
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimeChaveEmUmaLinha
        {
            get => GetProperty<bool>();
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

        #endregion Properties
    }
}