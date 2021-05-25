using System;
using ACBrLib.Core;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed class EventoNFe
    {
        #region Constructor

        public EventoNFe()
        {
        }

        internal EventoNFe(ACBrIniFile ini)
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public int cOrgao { get; set; }

        public string CNPJ { get; set; }

        public DateTime dhEvento { get; set; }

        public string tpEvento { get; set; }

        public int nSeqEvento { get; set; }

        public string versaoEvento { get; set; }

        public int cOrgaoAutor { get; set; }

        public TipoAutor tpAutor { get; set; }

        public string verAplic { get; set; }

        public DateTime dhEmi { get; set; }

        public TipoNFe tpNF { get; set; }

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString()
        {
            return WriteToIni().ToString();
        }

        private ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
        }

        #endregion Methods

        #region Operators

        public static implicit operator EventoNFe(ACBrIniFile source) => new EventoNFe(source);

        public static implicit operator ACBrIniFile(EventoNFe source) => source.WriteToIni();

        #endregion Operators
    }
}