using System;

namespace ACBrLib.Core.DFe
{
    public abstract class EventoBase<TEnum> where TEnum : Enum 
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

        public DateTime dhEvento { get; set; }

        public TEnum tpEvento { get; set; }

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
            return iniData;
        }

        #endregion Methods
    }
}