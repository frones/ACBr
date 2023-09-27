using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EventoPagOpTrasnporte : EventoMDFeBase
    {
        #region Constructor

        public EventoPagOpTrasnporte()
        {
            tpEvento = TipoEventoMDFe.tePagamentoOperacao;
            versaoEvento = "3.00";
        }

        #endregion Constructor

        #region Properties

        public EventoInfViagens InfViagens { get; } = new EventoInfViagens();

        public List<InfPagMDFe> InfPag { get; } = new List<InfPagMDFe>();

        #endregion Properties

        #region Methods

        protected override ACBrIniFile WriteToIni()
        {
            var iniData = base.WriteToIni();

            iniData.WriteToIni(InfViagens, "infViagens");

            for (var i = 0; i < InfPag.Count; i++)
            {
                iniData.WriteToIni(InfPag[i], $"infPag{i + 1:000}");
                iniData.WriteToIni(InfPag[i].InfBanco, $"infBanc{i + 1:000}");

                for (var j = 0; j < InfPag[i].Comp.Count; j++)
                    iniData.WriteToIni(InfPag[i].Comp[j], $"Comp{i + 1:000}{j + 1:000}");

                for (var j = 0; j < InfPag[i].InfPrazo.Count; j++)
                    iniData.WriteToIni(InfPag[i].InfPrazo[j], $"infPrazo{i + 1:000}{j + 1:000}");
            }

            return iniData;
        }

        #endregion Methods
    }
}