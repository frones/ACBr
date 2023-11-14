using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.MDFe;

namespace ACBrLib.MDFe
{
    public sealed class DAMDFeConfig : ReportConfig<ACBrMDFe>
    {
        #region Constructors

        public DAMDFeConfig(ACBrMDFe acbrlib) : base(acbrlib, ACBrSessao.DAMDFe)
        {
        }

        #endregion Constructors

        #region Properties

        public bool ImprimeHoraSaida
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ImprimirHoraSaida_Hora
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public TipoDAMDFe TipoDAMDFe
        {
            get => GetProperty<TipoDAMDFe>();
            set => SetProperty(value);
        }

        public TamanhoPapel TamanhoPapel
        {
            get => GetProperty<TamanhoPapel>();
            set => SetProperty(value);
        }

        public string Protocolo
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool Cancelada
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool Encerrado
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExpandeLogoMarca
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool ExibirMunicipioDescarregamento
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}