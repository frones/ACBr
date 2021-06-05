using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EventoAtorIntNFe : EventoBase
    {
        #region Constructor

        public EventoAtorIntNFe()
        {
            verAplic = "1.0";
            tpEvento = TipoEvento.teAtorInteressadoNFe;
            AutXml = new AutXML();
        }

        #endregion Constructor

        #region Properties

        public int cOrgaoAutor { get; set; }

        public TipoAutor tpAutor { get; set; }

        public string verAplic { get; set; }

        public TipoAutorizacao tpAutorizacao { get; set; }

        public AutXML AutXml { get; }

        #endregion Properties

        #region Methods

        protected override ACBrIniFile WriteToIni()
        {
            var iniData = base.WriteToIni();
            iniData.WriteToIni(AutXml, "autXML01");
            return iniData;
        }

        #endregion Methods
    }
}