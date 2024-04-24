using System;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class EventoEPEC : EventoNFeBase
    {
        #region Constructor

        public EventoEPEC()
        {
            tpEvento = TipoEventoNFe.teEPECNFe;
            DEST = new EventDEST();
        }

        #endregion Constructor

        #region Properties

        public int cOrgaoAutor { get; set; }

        public TipoAutor tpAutor { get; set; }

        public string verAplic { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoNFe tpNF { get; set; }

        public EventDEST DEST { get; }

        public decimal vNF { get; set; }

        public decimal vICMS { get; set; }

        public decimal vST { get; set; }

        public string IE { get; set; }

        #endregion Properties

        #region Methods

        protected override ACBrIniFile WriteToIni()
        {
            var iniData = base.WriteToIni();
            iniData.WriteToIni(DEST, "DEST");
            return iniData;
        }

        #endregion Methods
    }
}