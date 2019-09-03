using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib
{
    public static class AcbrNFe
    {
        #region DLL

        private const string ACBr = "ACBrNFe32.dll";
        //private const string ACBr = "ACBrNFe64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Inicializar(string eArqConfig, string eChaveCrypt);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Finalizar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Nome(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Versao(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ConfigLer(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ConfigGravar(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ConfigGravarValor(string eSessao, string eChave, string valor);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_CarregarXML(string eArquivoOuXml);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_CarregarINI(string eArquivoOuIni);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_CarregarEventoXML(string eArquivoOuXml);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_CarregarEventoINI(string eArquivoOuIni);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_LimparLista();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_LimparListaEventos();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Assinar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Validar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_StatusServico(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Consultar(string eChaveOuNFe, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Enviar(int aLote, bool imprimir, bool sincrono, bool zipado, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_EnviarEmailEvento(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_Imprimir(string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview, string cMarcaDagua, string bViaConsumidor, string bSimplificado);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ImprimirPDF();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ImprimirEvento(string eChaveNFe, string eChaveEvento);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ImprimirEventoPDF(string eChaveNFe, string eChaveEvento);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ImprimirInutilizacao(string eChave);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int NFE_ImprimirInutilizacaoPDF(string eChave);

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