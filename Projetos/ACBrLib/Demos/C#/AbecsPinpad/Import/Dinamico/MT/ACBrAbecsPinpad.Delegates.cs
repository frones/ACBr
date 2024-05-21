using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace ACBrLib.AbecsPinpad
{
    public sealed partial class ACBrAbecsPinpad
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Ativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Desativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_OPN(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CLO(IntPtr handle, string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CLX(IntPtr handle, string sMensagemOuNomeImagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GIX(IntPtr handle, string PP_DATA, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GIN(IntPtr handle, int GIN_ACQIDX, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_PinPadCapabilities(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DSP(IntPtr handle, string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DEX(IntPtr handle, string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GKY(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_RMC(IntPtr handle, string sMensagemRMC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GCD(IntPtr handle, int aMSGIDX, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CEX(IntPtr handle, bool VerifyKey, bool VerifyMagnetic, bool VerifyICCInsertion, bool VerifyICCRemoval, bool VerifyCTLSPresence, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_MNU(IntPtr handle, string sMNUOPT, string sDSPMSG, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_LoadMedia(IntPtr handle, string sCaminhoImagem, int aTipoImagem, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_LMF(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DSI(IntPtr handle, string sNomeArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DMF(IntPtr handle, string sNomeArquivo);

        protected override void InitializeMethods()
        {
            AddMethod<AbecsPinpad_Inicializar>("AbecsPinpad_Inicializar");
            AddMethod<AbecsPinpad_Finalizar>("AbecsPinpad_Finalizar");
            AddMethod<AbecsPinpad_Nome>("AbecsPinpad_Nome");
            AddMethod<AbecsPinpad_Versao>("AbecsPinpad_Versao");
            AddMethod<AbecsPinpad_UltimoRetorno>("AbecsPinpad_UltimoRetorno");
            AddMethod<AbecsPinpad_ConfigImportar>("AbecsPinpad_ConfigImportar");
            AddMethod<AbecsPinpad_ConfigExportar>("AbecsPinpad_ConfigExportar");
            AddMethod<AbecsPinpad_ConfigLer>("AbecsPinpad_ConfigLer");
            AddMethod<AbecsPinpad_ConfigGravar>("AbecsPinpad_ConfigGravar");
            AddMethod<AbecsPinpad_ConfigLerValor>("AbecsPinpad_ConfigLerValor");
            AddMethod<AbecsPinpad_ConfigGravarValor>("AbecsPinpad_ConfigGravarValor");
            AddMethod<AbecsPinpad_Ativar>("AbecsPinpad_Ativar");
            AddMethod<AbecsPinpad_Desativar>("AbecsPinpad_Desativar");
            AddMethod<AbecsPinpad_OPN>("AbecsPinpad_OPN");
            AddMethod<AbecsPinpad_CLO>("AbecsPinpad_CLO");
            AddMethod<AbecsPinpad_CLX>("AbecsPinpad_CLX");
            AddMethod<AbecsPinpad_GIX>("AbecsPinpad_GIX");
            AddMethod<AbecsPinpad_GIN>("AbecsPinpad_GIN");
            AddMethod<AbecsPinpad_PinPadCapabilities>("AbecsPinpad_PinPadCapabilities");
            AddMethod<AbecsPinpad_DSP>("AbecsPinpad_DSP");
            AddMethod<AbecsPinpad_DEX>("AbecsPinpad_DEX");
            AddMethod<AbecsPinpad_GKY>("AbecsPinpad_GKY");
            AddMethod<AbecsPinpad_RMC>("AbecsPinpad_RMC");
            AddMethod<AbecsPinpad_GCD>("AbecsPinpad_GCD");
            AddMethod<AbecsPinpad_CEX>("AbecsPinpad_CEX");
            AddMethod<AbecsPinpad_MNU>("AbecsPinpad_MNU");
            AddMethod<AbecsPinpad_LoadMedia>("AbecsPinpad_LoadMedia");
            AddMethod<AbecsPinpad_LMF>("AbecsPinpad_LMF");
            AddMethod<AbecsPinpad_DSI>("AbecsPinpad_DSI");
            AddMethod<AbecsPinpad_DMF>("AbecsPinpad_DMF");
        }
    }
}