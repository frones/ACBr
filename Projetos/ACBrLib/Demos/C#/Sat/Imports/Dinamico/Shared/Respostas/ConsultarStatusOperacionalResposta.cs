using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class ConsultarStatusOperacionalResposta : SatResposta<ConsultarStatusOperacionalResposta>
    {
        #region Properties

        public RetornoStatusSat StatusSat { get; } = new RetornoStatusSat();

        #endregion Properties

        #region Methods

        protected override void LerResposta(ACBrIniFile iniresposta)
        {
            iniresposta.ReadFromIni(StatusSat, "STATUSSAT");
        }

        #endregion Methods
    }
}