using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.CTe;
using ACBrLib.Core.DFe;

namespace ACBrLib.CTe
{
    public sealed class DACTeConfig : ReportConfig<ACBrCTe>
    {
        #region Constructors

        public DACTeConfig(ACBrCTe acbrlib) : base(acbrlib, ACBrSessao.DACTe)
        {
        }

        #endregion Constructors

        #region Properties

        public TipoDACTE TipoDACTe
        {
            get => GetProperty<TipoDACTE>();
            set => SetProperty(value);
        }

        public bool ExibeResumoCanhoto
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public PosCanhoto PosCanhoto
        {
            get => GetProperty<PosCanhoto>();
            set => SetProperty(value);
        }

        public bool CTeCancelada
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool EPECEnviado
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimirHoraSaida
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimirHoraSaida_Hora
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string ProtocoloCTe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Usuario
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public TamanhoPapel TamanhoPapel
        {
            get => GetProperty<TamanhoPapel>();
            set => SetProperty(value);
        }

        public bool ImprimeDescPorc
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}