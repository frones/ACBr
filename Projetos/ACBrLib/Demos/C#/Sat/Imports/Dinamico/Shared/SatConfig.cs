using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Sat;

namespace ACBrLib.Sat
{
    public sealed class SatConfig : ACBrLibConfigBase<ACBrSat>
    {
        #region Constructors

        public SatConfig(ACBrSat acbrlib) : base(acbrlib, ACBrSessao.SATConfig)
        {
        }

        #endregion Constructors

        #region Properties

        public decimal infCFe_versaoDadosEnt
        {
            get => GetProperty<decimal>();
            set => SetProperty(value);
        }

        public string ide_CNPJ
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public int ide_numeroCaixa
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public TipoAmbiente ide_tpAmb
        {
            get => GetProperty<TipoAmbiente>();
            set => SetProperty(value);
        }

        public string emit_CNPJ
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string emit_IE
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string emit_IM
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public RegTrib emit_cRegTrib
        {
            get => GetProperty<RegTrib>();
            set => SetProperty(value);
        }

        public RegTribISSQN emit_cRegTribISSQN
        {
            get => GetProperty<RegTribISSQN>();
            set => SetProperty(value);
        }

        public indRatISSQN emit_indRatISSQN
        {
            get => GetProperty<indRatISSQN>();
            set => SetProperty(value);
        }

        public bool EhUTF8
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public ushort PaginaDeCodigo
        {
            get => GetProperty<ushort>();
            set => SetProperty(value);
        }

        public string ArqSchema
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public SSLXmlSignLib SSLXmlSignLib
        {
            get => GetProperty<SSLXmlSignLib>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}