using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.ETQ
{
    public sealed partial class ACBrETQ
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Ativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Desativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_IniciarEtiqueta();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_FinalizarEtiqueta(int aCopias, int aAvancoEtq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_CarregarImagem(string eArquivoImagem, string eNomeImagem, bool flipped);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Imprimir(int aCopias, int aAvancoEtq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirTexto(int orientacao, int fonte, int multiplicadorH, int multiplicadorV,
                        int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirTextoStr(int orientacao, string fonte, int multiplicadorH, int multiplicadorV,
                        int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirBarras(int orientacao, int tipoBarras, int larguraBarraLarga, int larguraBarraFina,
                        int vertical, int horizontal, string eTexto, int alturaCodBarras, int exibeCodigo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirLinha(int vertical, int horizontal, int largura, int altura);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirCaixa(int vertical, int horizontal, int largura, int altura, int espessuraVertical,
                        int espessuraHorizontal);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirImagem(int multiplicadorImagem, int vertical, int horizontal, string eNomeImagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirQRCode(int vertical, int horizontal, string texto, int larguraModulo, int errorLevel, int tipo);
    }
}