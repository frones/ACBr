using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class ConsultarSessaoSatResposta : SatResposta<ConsultarSessaoSatResposta>
    {
        #region Properties

        public ConsultarSessaoResposta ConsultarSessao { get; } = new ConsultarSessaoResposta();

        public ConsultarSessaoCanceladoResposta ConsultarSessaoCancelado { get; } = new ConsultarSessaoCanceladoResposta();

        #endregion Properties

        #region Methods

        protected override void LerResposta(ACBrIniFile iniresposta)
        {
            iniresposta.ReadFromIni(ConsultarSessao, "CFE");
            iniresposta.ReadFromIni(ConsultarSessaoCancelado, "CANCELAMENTO");
        }

        #endregion Methods
    }
}