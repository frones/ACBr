using ACBrLib.Core;
using ACBrLib.Core.Config;

namespace ACBrLib.Sat
{
    public sealed class SATConfigArquivos : ACBrLibConfigBase<ACBrSat>
    {
        #region Constructors

        public SATConfigArquivos(ACBrSat acbrlib) : base(acbrlib, ACBrSessao.SATConfigArquivos)
        {
        }

        #endregion Constructors

        #region Properties

        public bool SalvarCFe
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SalvarCFeCanc
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SalvarEnvio
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorCNPJ
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorModelo
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorAno
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorMes
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SepararPorDia
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public string PastaCFeVenda
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PastaCFeCancelamento
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PastaEnvio
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PrefixoArqCFe
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string PrefixoArqCFeCanc
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}