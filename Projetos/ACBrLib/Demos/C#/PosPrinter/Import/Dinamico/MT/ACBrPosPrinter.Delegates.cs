using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.PosPrinter
{
    public sealed partial class ACBrPosPrinter
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Ativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Desativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Imprimir(IntPtr handle, string aString, bool pulaLinha, bool decodificarTags, bool codificarPagina, int copias);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirLinha(IntPtr handle, string aString);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirCmd(IntPtr handle, string aString);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirTags(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirImagemArquivo(IntPtr handle, string aPath);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirLogo(IntPtr handle, int nAKC1, int nAKC2, int nFatorX, int nFatorY);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirCheque(IntPtr handle, int CodBanco, string AValor, string ADataEmissao, string AFavorecido,
           string ACidade, string AComplemento, bool LerCMC7, int SegundosEspera);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ImprimirTextoCheque(IntPtr handle, int X, int Y, string AString, bool AguardaCheque, int SegundosEspera);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_TxRx(IntPtr handle, string aString, byte bytesToRead, int aTimeOut, bool waitForTerminator, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Zerar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_InicializarPos(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_Reset(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_PularLinhas(IntPtr handle, int numLinhas);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_CortarPapel(IntPtr handle, bool parcial);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_AbrirGaveta(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_LerInfoImpressora(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_LerStatusImpressora(IntPtr handle, int tentativas, ref int status);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_RetornarTags(IntPtr handle, bool incluiAjuda, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_AcharPortas(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_GravarLogoArquivo(IntPtr handle, string aPath, int nAKC1, int nAKC2);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_ApagarLogo(IntPtr handle, int nAKC1, int nAKC2);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_LeituraCheque(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_LerCMC7(IntPtr handle, bool AguardaCheque, int SegundosEspera, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_EjetarCheque(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_PodeLerDaPorta(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int POS_LerCaracteristicas(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<POS_Inicializar>("POS_Inicializar");
            AddMethod<POS_Finalizar>("POS_Finalizar");
            AddMethod<POS_Nome>("POS_Nome");
            AddMethod<POS_Versao>("POS_Versao");
            AddMethod<POS_UltimoRetorno>("POS_UltimoRetorno");
            AddMethod<POS_ConfigImportar>("POS_ConfigImportar");
            AddMethod<POS_ConfigExportar>("POS_ConfigExportar");
            AddMethod<POS_ConfigLer>("POS_ConfigLer");
            AddMethod<POS_ConfigGravar>("POS_ConfigGravar");
            AddMethod<POS_ConfigLerValor>("POS_ConfigLerValor");
            AddMethod<POS_ConfigGravarValor>("POS_ConfigGravarValor");
            AddMethod<POS_Ativar>("POS_Ativar");
            AddMethod<POS_Desativar>("POS_Desativar");
            AddMethod<POS_Imprimir>("POS_Imprimir");
            AddMethod<POS_ImprimirLinha>("POS_ImprimirLinha");
            AddMethod<POS_ImprimirCmd>("POS_ImprimirCmd");
            AddMethod<POS_ImprimirTags>("POS_ImprimirTags");
            AddMethod<POS_ImprimirImagemArquivo>("POS_ImprimirImagemArquivo");
            AddMethod<POS_ImprimirLogo>("POS_ImprimirLogo");
            AddMethod<POS_ImprimirCheque>("POS_ImprimirCheque");
            AddMethod<POS_ImprimirTextoCheque>("POS_ImprimirTextoCheque");
            AddMethod<POS_TxRx>("POS_TxRx");
            AddMethod<POS_Zerar>("POS_Zerar");
            AddMethod<POS_InicializarPos>("POS_InicializarPos");
            AddMethod<POS_Reset>("POS_Reset");
            AddMethod<POS_PularLinhas>("POS_PularLinhas");
            AddMethod<POS_CortarPapel>("POS_CortarPapel");
            AddMethod<POS_AbrirGaveta>("POS_AbrirGaveta");
            AddMethod<POS_LerInfoImpressora>("POS_LerInfoImpressora");
            AddMethod<POS_LerStatusImpressora>("POS_LerStatusImpressora");
            AddMethod<POS_RetornarTags>("POS_RetornarTags");
            AddMethod<POS_AcharPortas>("POS_AcharPortas");
            AddMethod<POS_GravarLogoArquivo>("POS_GravarLogoArquivo");
            AddMethod<POS_ApagarLogo>("POS_ApagarLogo");
            AddMethod<POS_LeituraCheque>("POS_LeituraCheque");
            AddMethod<POS_LerCMC7>("POS_LerCMC7");
            AddMethod<POS_EjetarCheque>("POS_EjetarCheque");
            AddMethod<POS_PodeLerDaPorta>("POS_PodeLerDaPorta");
            AddMethod<POS_LerCaracteristicas>("POS_LerCaracteristicas");
        }
    }
}