using System;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Ini;

namespace ACBrLib.NFe
{
    public abstract class EventoBase
    {
        #region Constructor

        protected EventoBase()
        {
            nSeqEvento = 1;
            versaoEvento = "1.00";
        }

        #endregion Constructor

        #region Properties

        public int cOrgao { get; set; }

        public string CNPJ { get; set; }

        public string chNFe { get; set; }

        public DateTime dhEvento { get; set; }

        public TipoEvento tpEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string versaoEvento { get; set; }

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString() => WriteToIni().ToString();

        protected virtual ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();
            iniData["EVENTO"]["idLote"] = "1";
            iniData.WriteToIni(this, "EVENTO001");
            return iniData;
        }

        #endregion Methods
    }
}