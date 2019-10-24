using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibCTe
{
    public static class ACBrCTe
    {
        #region DLL

        private const string ACBr = "ACBrCTe32.dll";
        //private const string ACBr = "ACBrCTe64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public delegate int CTE_Inicializar(string eArqConfig, string eChaveCrypt);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Finalizar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Nome(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Versao(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ConfigLer(string eArqConfig);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ConfigGravar(string eArqConfig);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ConfigGravarValor(string eSessao, string eChave, string valor);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_CarregarXML(string eArquivoOuXml);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_CarregarINI(string eArquivoOuIni);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_CarregarEventoXML(string eArquivoOuXml);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_CarregarEventoINI(string eArquivoOuIni);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_LimparLista();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_LimparListaEventos();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Assinar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Validar();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_StatusServico(StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Consultar(string eChaveOuCTe, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
                int serie, int numeroInicial, int numeroFinal, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Enviar(int aLote, bool imprimir, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
                StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_EnviarEmail(string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_Imprimir();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ImprimirPDF();

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ImprimirEvento(string eChaveCTe, string eChaveEvento);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ImprimirEventoPDF(string eChaveCTe, string eChaveEvento);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ImprimirInutilizacao(string eChave);

            [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
            public delegate int CTE_ImprimirInutilizacaoPDF(string eChave);

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