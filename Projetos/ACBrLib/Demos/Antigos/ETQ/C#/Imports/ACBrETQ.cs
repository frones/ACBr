using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibETQ
{
    public static class ACBrETQ
    {
        #region DLL

        private const string ACBr = "ACBrETQ32.dll";
        //private const string ACBr = "ACBrETQ64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Inicializar(string eArqConfig, string eChaveCrypt);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Finalizar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Nome(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Versao(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ConfigLer(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ConfigGravar(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ConfigGravarValor(string eSessao, string eChave, string valor);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Ativar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Desativar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_IniciarEtiqueta();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_FinalizarEtiqueta(int ACopias, int AAvancoEtq);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_CarregarImagem(string eArquivoImagem, string eNomeImagem, bool Flipped);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_Imprimir(int ACopias, int AAvancoEtq);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ImprimirTexto(int Orientacao, int Fonte, int MultiplicadorH, int MultiplicadorV,
                        int Vertical, int Horizontal, string eTexto, int SubFonte, bool ImprimirReverso);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl, EntryPoint = "ETQ_ImprimirTextoStr")]
        public static extern int ETQ_ImprimirTexto(int Orientacao, string Fonte, int MultiplicadorH, int MultiplicadorV,
                        int Vertical, int Horizontal, string eTexto, int SubFonte, bool ImprimirReverso);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ImprimirBarras(int Orientacao, int TipoBarras, int LarguraBarraLarga, int LarguraBarraFina,
                        int Vertical, int Horizontal, string eTexto, int AlturaCodBarras, int ExibeCodigo);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ImprimirLinha(int Vertical, int Horizontal, int Largura, int Altura);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ImprimirCaixa(int Vertical, int Horizontal, int Largura, int Altura, int EspessuraVertical,
                        int EspessuraHorizontal);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ETQ_ImprimirImagem(int MultiplicadorImagem, int Vertical, int Horizontal, string eNomeImagem);

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

            ETQ_UltimoRetorno(buffer, ref bufferLen);
            if (bufferLen > 256)
            {
                buffer.Capacity = bufferLen;
                ETQ_UltimoRetorno(buffer, ref bufferLen);
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