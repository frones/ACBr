using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.Sat;

namespace ACBrLib.Sat
{
    public sealed class SatBaseConfig : ACBrLibConfig<ACBrSat>
    {
        #region Constructors

        public SatBaseConfig(ACBrSat acbrlib) : base(acbrlib, ACBrSessao.SAT)
        {
            SatConfig = new SatConfig(acbrlib);
            Arquivos = new SATConfigArquivos(acbrlib);
            Rede = new SATRede(acbrlib);
            Extrato = new ExtratoConfig(acbrlib);
            PosPrinter = new PosPrinterConfig<ACBrSat>(acbrlib);
            Email = new EmailConfig<ACBrSat>(acbrlib);
            DFe = new DFeConfig<ACBrSat>(acbrlib);
        }

        #endregion Constructors

        #region Properties

        public SatConfig SatConfig { get; }

        public SATConfigArquivos Arquivos { get; }

        public SATRede Rede { get; }

        public ExtratoConfig Extrato { get; }

        public PosPrinterConfig<ACBrSat> PosPrinter { get; }

        public EmailConfig<ACBrSat> Email { get; }

        public DFeConfig<ACBrSat> DFe { get; }

        public SATModelo Modelo
        {
            get => GetProperty<SATModelo>();
            set => SetProperty(value);
        }

        public string NomeDLL
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string ArqLog
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string CodigoDeAtivacao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string SignAC
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool ValidarNumeroSessaoResposta
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int NumeroTentativasValidarSessao
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}