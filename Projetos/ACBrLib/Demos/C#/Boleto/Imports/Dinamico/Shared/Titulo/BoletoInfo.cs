using System;
using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public abstract class BoletoInfo
    {
        #region Constructor

        protected BoletoInfo()
        {
        }

        protected BoletoInfo(ACBrIniFile ini)
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Methods

        /// <inheritdoc/>
        public override string ToString() => WriteToIni().ToString();

        protected virtual ACBrIniFile WriteToIni()
        {
            var iniFile = new ACBrIniFile();
            WriteToIni(iniFile);
            return iniFile;
        }

        internal abstract void WriteToIni(ACBrIniFile iniFile);

        protected abstract void ReadFromIni(ACBrIniFile iniData);

        #endregion Methods
    }
}