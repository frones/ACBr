using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibPosPrinter
{
    public static class ACBrPosPrinter
    {
        #region DLL

        private const string ACBr = "ACBrPosPrinter32.dll";
        //private const string ACBr = "ACBrPosPrinter64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Inicializar(string eArqConfig, string eChaveCrypt);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Finalizar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Nome(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Versao(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ConfigLer(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ConfigGravar(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ConfigGravarValor(string eSessao, string eChave, string valor);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Ativar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Desativar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Imprimir(string aString, bool pulaLinha, bool decodificarTags, bool codificarPagina, int copias);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ImprimirLinha(string aString);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ImprimirCmd(string aString);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_ImprimirTags();
		
		[DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public static extern int POS_ImprimirImagemArquivo(string APath);
		
		[DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public static extern int POS_GravarLogoArquivo(string APath, int nAKC1, int nAKC2);

		[DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public static extern int POS_ImprimirLogo(int nAKC1, int nAKC2, int nFatorX, int nFatorY);

		[DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
		public static extern int POS_ApagarLogo(int nAKC1, int nAKC2);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_TxRx(string aString, byte bytesToRead, int aTimeOut, bool waitForTerminator, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Zerar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_InicializarPos();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_Reset();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_PularLinhas(int numLinhas);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_CortarPapel(bool parcial);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_AbrirGaveta();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_LerInfoImpressora(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_LerStatusImpressora(int tentativas, ref int status);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int POS_RetornarTags(StringBuilder buffer, ref int bufferSize, bool incluiAjuda);

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

            POS_UltimoRetorno(buffer, ref bufferLen);
            if (bufferLen > 256)
            {
                buffer.Capacity = bufferLen;
                POS_UltimoRetorno(buffer, ref bufferLen);
            }

            switch (ret)
            {
                case -10:
                    throw new ApplicationException(FromUTF8(buffer));

                case -6:
                    throw new DirectoryNotFoundException(FromUTF8(buffer));

                case -5:
                    throw new FileNotFoundException(FromUTF8(buffer));

                case -4:
                    throw new ApplicationException(FromUTF8(buffer));

                case -3:
                    throw new ApplicationException(FromUTF8(buffer));

                case -2:
                    throw new ApplicationException(FromUTF8(buffer));

                case -1:
                    throw new ApplicationException(FromUTF8(buffer));
            }
        }

        #endregion Methods
    }
}