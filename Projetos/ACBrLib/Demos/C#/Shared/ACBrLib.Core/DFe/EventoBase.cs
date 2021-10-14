using System;
using ACBrLib.Core.Extensions;

namespace ACBrLib.Core.DFe
{
    public abstract class EventoBase
    {
        #region Fields

        protected TipoEvento evento;

        #endregion

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

        public DateTime dhEvento { get; set; }

        public TipoEvento tpEvento => evento;

        public int nSeqEvento { get; set; }

        public string versaoEvento { get; set; }

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString() => WriteToIni().ToString();

        protected virtual ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile
            {
                ["EVENTO"] =
                {
                    ["idLote"] = "1"
                }
            };

            iniData.WriteToIni(GetType(), this, "EVENTO001");
            iniData["EVENTO001"]["tpEvento"] = evento.GetEnumValueOrInt();
            return iniData;
        }

        #endregion Methods
    }
}