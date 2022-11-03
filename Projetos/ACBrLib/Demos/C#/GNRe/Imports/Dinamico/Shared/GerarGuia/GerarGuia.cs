using System.Collections.Generic;
using System.IO;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.GNRe
{
    public sealed class GerarGuia
    {
        #region Constructor

        public GerarGuia()
        {
            Emitente = new EmitenteGNRe();
            Complemento = new ComplementoGNRe();
            Referencia = new ReferenciaGNRe();
            Destinatario = new DestinatarioGNRe();
            CampoExtra = new CampoExtraGNRe();
        }

        internal GerarGuia(ACBrIniFile ini) : this()
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public EmitenteGNRe Emitente { get; }

        public ComplementoGNRe Complemento { get; }

        public ReferenciaGNRe Referencia { get; }

        public DestinatarioGNRe Destinatario { get; }

        public CampoExtraGNRe CampoExtra { get; }

        #endregion Properties

        #region Methods

        public override string ToString()
        {
            return WriteToIni().ToString();
        }

        private ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();

            iniData.WriteToIni(Emitente, "Emitente");
            iniData.WriteToIni(Complemento, "Complemento");
            iniData.WriteToIni(Referencia, "Referencia");
            iniData.WriteToIni(Destinatario, "Destinatario");
            iniData.WriteToIni(CampoExtra, "CampoExtra");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(Emitente, "Emitente");
            iniData.ReadFromIni(Complemento, "Complemento");
            iniData.ReadFromIni(Referencia, "Referencia");
            iniData.ReadFromIni(Destinatario, "Destinatario");
            iniData.ReadFromIni(CampoExtra, "CampoExtra");
        }

        public static GerarGuia Load(string conteudo) => ACBrIniFile.Parse(conteudo);

        public static GerarGuia LoadFromFile(string filePath) => ACBrIniFile.Load(filePath);

        public static GerarGuia LoadFromFile(Stream stream) => ACBrIniFile.Load(stream);

        #endregion

        #region Operators

        public static implicit operator GerarGuia(ACBrIniFile source) => new GerarGuia(source);

        public static implicit operator ACBrIniFile(GerarGuia source) => source.WriteToIni();

        #endregion
    }
}