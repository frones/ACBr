using System;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.ETQ
{
    public sealed partial class ACBrETQ
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Ativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Desativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_IniciarEtiqueta(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_FinalizarEtiqueta(IntPtr handle, int aCopias, int aAvancoEtq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_CarregarImagem(IntPtr handle, string eArquivoImagem, string eNomeImagem, bool flipped);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_Imprimir(IntPtr handle, int aCopias, int aAvancoEtq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirTexto(IntPtr handle, int orientacao, int fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirTextoStr(IntPtr handle, int orientacao, string fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirBarras(IntPtr handle, int orientacao, int tipoBarras, int larguraBarraLarga, int larguraBarraFina,
            int vertical, int horizontal, string eTexto, int alturaCodBarras, int exibeCodigo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirLinha(IntPtr handle, int vertical, int horizontal, int largura, int altura);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirCaixa(IntPtr handle, int vertical, int horizontal, int largura, int altura, int espessuraVertical,
            int espessuraHorizontal);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirImagem(IntPtr handle, int multiplicadorImagem, int vertical, int horizontal, string eNomeImagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int ETQ_ImprimirQRCode(IntPtr handle, int vertical, int horizontal, string texto, int larguraModulo, int errorLevel, int tipo);
    }
}