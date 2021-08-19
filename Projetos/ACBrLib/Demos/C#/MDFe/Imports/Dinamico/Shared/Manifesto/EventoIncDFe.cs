using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EventoIncDFe : EventoMDFeBase
    {
        #region Constructor

        public EventoIncDFe()
        {
            tpEvento = TipoEvento.teInclusaoDFe;
            versaoEvento = "3.00";
        }

        #endregion Constructor

        #region Properties

        public string nProt { get; set; }

        public int cMunCarrega { get; set; }

        public string xMunCarrega { get; set; }

        public List<EventoInfDocNFe> Docs { get; } = new List<EventoInfDocNFe>(2000);

        #endregion Properties

        #region Methods

        protected override ACBrIniFile WriteToIni()
        {
            var iniData = base.WriteToIni();

            for (var i = 0; i < Docs.Count; i++)
                iniData.WriteToIni(Docs[i], $"infDoc{i:0000}");

            return iniData;
        }

        #endregion Methods
    }
}