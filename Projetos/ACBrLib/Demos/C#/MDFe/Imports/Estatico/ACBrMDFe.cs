using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibMDFe
{
    public static class ACBrMDFe
    {
        #region DLL

        private const string ACBr = "ACBrMDFe32.dll";
        //private const string ACBr = "ACBrMDFe64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public delegate int MDFE_Inicializar(string eArqConfig, string eChaveCrypt);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Finalizar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Nome(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Versao(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigLer(string eArqConfig);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigGravar(string eArqConfig);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigGravarValor(string eSessao, string eChave, string valor);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarXML(string eArquivoOuXml);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarINI(string eArquivoOuIni);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarEventoXML(string eArquivoOuXml);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarEventoINI(string eArquivoOuIni);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_LimparLista();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_LimparListaEventos();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Assinar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Validar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_StatusServico(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Consultar(string eChaveOuCTe, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Enviar(int aLote, bool imprimir, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
                StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEmail(string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_Imprimir();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirPDF();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirEvento(string eChaveCTe, string eChaveEvento);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirEventoPDF(string eChaveCTe, string eChaveEvento);

        #endregion Imports

        #region Methods

        public static string ToUTF8(this string value)
        {
            return string.IsNullOrEmpty(value) ? value : Encoding.Default.GetString(Encoding.UTF8.GetBytes(value));
        }

        public static string FromUTF8(this StringBuilder value)
        {
            if (value == null) return null;
            return value.Length == 0 ? string.Empty : Encoding.UTF8.GetString(Encoding.Default.GetBytes(value.ToString()));
        }

        public static void CheckResult(int ret)
        {
            if (ret >= 0) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            NFE_UltimoRetorno(buffer, ref bufferLen);
            if (bufferLen > 256)
            {
                buffer.Capacity = bufferLen;
                NFE_UltimoRetorno(buffer, ref bufferLen);
            }

            switch (ret)
            {
                case -10:
                    throw new ApplicationException(FromUTF8(buffer));

                case -6:
                    throw new DirectoryNotFoundException(FromUTF8(buffer));

                case -5:
                    throw new FileNotFoundException(FromUTF8(buffer));

                default:
                    throw new Exception(FromUTF8(buffer));
            }
        }

        #endregion Methods
    }
}