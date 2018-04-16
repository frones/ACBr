using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibPosPrinter.Demo
{
    public class ACBrPosPrinter
    {
        #region DLL

        //private const string ACBr = "ACBrPosPrinter32.dll";
        private const string ACBr = "ACBrPosPrinter64.dll";

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
    }
}