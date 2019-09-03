using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib
{
    public static class ACBrBoleto
    {
        #region DLL

        private const string ACBr = "ACBrSAT32.dll";
        //private const string ACBr = "ACBrSAT64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_Inicializar(string eArqConfig, string eChaveCrypt);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_Finalizar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_Nome(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_Versao(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ConfigLer(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ConfigGravar(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ConfigGravarValor(string eSessao, string eChave, string valor);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ConfigurarDados(string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_IncluirTitulos(string eArquivoIni, string eTpSaida, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_LimparLista();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_TotalTitulosLista(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_Imprimir(string eNomeImpressora);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_GerarPDF();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_GerarHTML();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_GerarRemessa(string eDir, int eNumArquivo, string eNomeArquivo);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_LerRetorno(string eDir, string eNomeArq);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_EnviarEmail(string ePara, string eAssunto, string eMensagem, string eCC);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_SetDiretorioArquivo(string eDir, string eArq, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ListaBancos(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ListaCaractTitulo(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ListaOcorrencias(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_ListaOcorrenciasEX(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_TamNossoNumero(string eCarteira, string enossoNumero, string eConvenio, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_CodigosMoraAceitos(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_SelecionaBanco(string eCodBanco, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_MontarNossoNumero(int eIndice, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_RetornaLinhaDigitavel(int eIndice, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Boleto_RetornaCodigoBarras(int eIndice, StringBuilder buffer, ref int bufferSize);

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

            Boleto_UltimoRetorno(buffer, ref bufferLen);
            if (bufferLen > 256)
            {
                buffer.Capacity = bufferLen;
                Boleto_UltimoRetorno(buffer, ref bufferLen);
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